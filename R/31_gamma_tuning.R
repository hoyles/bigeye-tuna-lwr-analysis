# ==============================================================================
# 31_gamma_tuning.R -- the smoothing multiplier gamma on withheld trips
#
# Section 4.1; Supplementary Text S1. Trip-blocked ten-fold cross-validation of Model A at gamma = 1, 1.4,
# 2, 2.65, 4 and 6, with and without the spatial term, on the analysis dataset, scored by the full
# predictive log score. Only fold differences and mean scores are reported; 85_ gives the gam(ML) fits.
# ENGINE AND TIMING: bam(discrete = TRUE); 100 CV fits and 12 full fits in parallel, 10-30 min.
# ==============================================================================

source("R/00_setup.R")
library(parallel); library(scoringRules)
set.seed(20260826)

NFOLD <- 10; NWORKER <- 16
JOBDIR <- file.path(OUT, "gamma_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

allf <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
         setf = factor(setid), tripf = factor(tripID), sexf = factor(sex)) %>%
  filter(complete.cases(logwt, loglen, hbf, lon, lat, mon, obs_name, yrf)) %>%
  droplevels() %>% as.data.frame()
allf <- allf[!allf$obs_name %in% c("obs_091", "obs_113", "obs_175"), ]
allf$obs_name <- droplevels(allf$obs_name)
d <- readRDS(file.path(OUT, "final_dataset.rds"))$d   # the analysis dataset (32_)
# centred within THIS dataset, which is the pre-exclusion pool rather than the
# final analysis set; the centring point is immaterial to a variance component
d$loglen_c <- d$loglen - mean(d$loglen)
trips <- levels(d$tripf)
d$fold <- setNames(sample(rep_len(1:NFOLD, length(trips))), trips)[as.character(d$tripf)]
cat(sprintf("data: %d fish | %d trips | %d observers\n\n", nrow(d),
            nlevels(d$tripf), nlevels(d$obs_name)))

# Spatial basis and trip random slope both follow the adopted specification (32_).
# gamma is a property of the model it is chosen for, so it must be tuned on the
# model the paper uses -- and gamma penalises every smooth equally, which now
# includes the trip slope.
SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE <- "s(obs_name, bs='re') + s(tripf, bs='re') + s(loglen_c, tripf, bs='re')"
form_of <- function(spatial) {
  bits <- c("s(loglen, k=20)", RE, "sexf", if (spatial) SP else NULL, "yrf")
  as.formula(paste("logwt ~", paste(bits, collapse = " + ")))
}

GAMMAS <- c(1.0, 1.4, 2, 2.65, 4, 6)
SPECS <- c(
  lapply(GAMMAS, function(g) list(id = sprintf("sp_g%.2f", g), gamma = g, spatial = TRUE)),
  lapply(c(1.0, 1.4, 4, 6), function(g) list(id = sprintf("nosp_g%.2f", g), gamma = g, spatial = FALSE)))
names(SPECS) <- sapply(SPECS, `[[`, "id")

run_cv <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(scoringRules)})
  outf <- file.path(JOBDIR, sprintf("cv_%s_f%02d.rds", job$id, job$fold))
  if (file.exists(outf)) return(readRDS(outf))
  s <- SPECS[[job$id]]
  tr <- d[d$fold != job$fold, ]; te <- d[d$fold == job$fold, ]
  te <- te[te$yrf %in% unique(tr$yrf), , drop = FALSE]
  if (nrow(te) < 5) return(NULL)
  tr$obs_name <- factor(tr$obs_name); tr$tripf <- factor(tr$tripf)
  tr$yrf <- factor(as.character(tr$yrf))
  te$yrf <- factor(as.character(te$yrf), levels = levels(tr$yrf))
  te$obs_name <- factor(levels(tr$obs_name)[1], levels = levels(tr$obs_name))
  te$tripf    <- factor(levels(tr$tripf)[1],    levels = levels(tr$tripf))
  m <- try(bam(form_of(s$spatial), data = tr, family = scat, method = "fREML",
               discrete = TRUE, gamma = s$gamma), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  lab <- sapply(m$smooth, function(z) z$label)
  excl <- intersect(c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)"), lab)
  pr <- predict(m, te, se.fit = TRUE, exclude = excl)
  mu <- as.numeric(pr$fit); sef <- as.numeric(pr$se.fit); y <- te$logwt
  th <- m$family$getTheta(TRUE); nu <- th[1]
  # The random effects are EXCLUDED from the fitted mean because a held-out trip
  # and its observer are unseen, so their variance must be added back or the
  # predictive distribution is too narrow. This matters more here than anywhere
  # else in the analysis: gamma shrinks the random effects, so a model fitted at
  # high gamma is penalised least by omitting them, and a comparison that omits
  # them will favour high gamma for a reason that has nothing to do with
  # prediction. 37_ and 61_ both hit this; scoring both ways makes it visible.
  blup_sd <- function(l) { i <- match(l, lab); if (is.na(i)) return(0)
    sd(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]) }
  v_re <- blup_sd("s(obs_name)")^2 + blup_sd("s(tripf)")^2 +
          blup_sd("s(loglen_c,tripf)")^2 * te$loglen_c^2
  sc      <- sqrt(th[2]^2 + sef^2)              # residual only: NOT comparable across gamma
  sc_full <- sqrt(th[2]^2 + sef^2 + v_re)       # honest predictive scale
  gete <- function(p) { i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
    sum(m$edf[m$smooth[[i[1]]]$first.para:m$smooth[[i[1]]]$last.para]) }
  res <- data.frame(id = job$id, gamma = s$gamma, spatial = s$spatial, fold = job$fold,
    n = nrow(te), edf_tot = sum(m$edf), edf_len = gete("s(loglen)"),
    edf_sp = gete("te("), edf_trip = gete("s(tripf)"), edf_obs = gete("s(obs_name)"),
    rmse_kg = sqrt(mean((exp(y)-exp(mu))^2)),
    logscore = mean(dt((y-mu)/sc, df=nu, log=TRUE) - log(sc)),
    logscore_full = mean(dt((y-mu)/sc_full, df=nu, log=TRUE) - log(sc_full)),
    crps = mean(crps_t(y, df=nu, location=mu, scale=sc)),
    crps_full = mean(crps_t(y, df=nu, location=mu, scale=sc_full)))
  saveRDS(res, outf); res
}

# full-data fits: what does gamma do to the length smooth and the maturation dip?
run_full <- function(g) {
  .libPaths(LIB); suppressMessages(library(mgcv))
  outf <- file.path(JOBDIR, sprintf("full_g%.2f.rds", g))
  if (file.exists(outf)) return(readRDS(outf))
  m  <- bam(form_of(TRUE), data = d, family = scat, method = "fREML",
            discrete = TRUE, gamma = g)
  ml <- bam(update(form_of(TRUE), . ~ . - s(loglen, k = 20) + loglen),
            data = d, family = scat, method = "fREML", discrete = TRUE, gamma = g)
  L <- c(80, 95, 110, 130, 150, 175)
  nd <- data.frame(loglen = log(L), loglen_c = log(L) - mean(d$loglen),
                   lat = median(d$lat), lon = median(d$lon), mon = 6,
                   yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                   sexf = factor("F", levels = levels(d$sexf)),
                   obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                   tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
  ex <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
  ps <- as.numeric(predict(m,  nd, exclude = ex))
  pl <- as.numeric(predict(ml, nd, exclude = ex))
  lab <- sapply(m$smooth, function(z) z$label)
  gete <- function(p) { i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
    sum(m$edf[m$smooth[[i[1]]]$first.para:m$smooth[[i[1]]]$last.para]) }
  res <- list(gamma = g, edf_len = gete("s(loglen)"), edf_sp = gete("te("),
              edf_tot = sum(m$edf), L = L, nonlin_pct = 100*(exp(ps - pl) - 1))
  saveRDS(res, outf); res
}

jobs <- unlist(lapply(names(SPECS), function(s)
  lapply(1:NFOLD, function(f) list(id = s, fold = f))), recursive = FALSE)
cat(sprintf("=== CV: %d jobs (%d specs x %d folds) ===\n", length(jobs), length(SPECS), NFOLD))
cl <- makeCluster(NWORKER)
clusterExport(cl, c("d", "SPECS", "LIB", "JOBDIR", "SP", "RE", "form_of"), envir = environment())
t0 <- Sys.time()
res  <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, run_cv)))
full <- parLapplyLB(cl, as.list(GAMMAS), run_full)
stopCluster(cl)
cat(sprintf("elapsed %.1f min\n\n", as.numeric(difftime(Sys.time(), t0, units="mins"))))

smry <- res %>% group_by(id) %>%
  summarise(gamma = first(gamma), spatial = first(spatial), folds = dplyr::n(),
            edf_tot = mean(edf_tot), edf_len = mean(edf_len), edf_sp = mean(edf_sp),
            rmse_kg = weighted.mean(rmse_kg, n), logscore = weighted.mean(logscore, n),
            logscore_full = weighted.mean(logscore_full, n),
            crps = weighted.mean(crps, n), crps_full = weighted.mean(crps_full, n),
            .groups = "drop") %>% arrange(desc(logscore_full))
cat("=== out-of-sample performance ===\n")
print(as.data.frame(smry %>% mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)

cat("\n=== is gamma's benefit independent of the spatial term? ===\n")
for (g in c(1.0, 1.4, 4, 6)) {
  a <- smry %>% filter(spatial, abs(gamma - g) < .01)
  b <- smry %>% filter(!spatial, abs(gamma - g) < .01)
  if (nrow(a) && nrow(b))
    cat(sprintf("  gamma %.2f : with spatial %.5f | without spatial %.5f | diff %+.5f\n",
                g, a$logscore_full, b$logscore_full,
                a$logscore_full - b$logscore_full))
}
cat("  (on the FULL predictive score; if the with/without gap shrinks as gamma\n")
cat("   rises, gamma is simply deleting the spatial term rather than improving\n")
cat("   calibration)\n")

cat("\n=== does the ranking depend on how the predictive scale is computed? ===\n")
for (g in sort(unique(smry$gamma))) {
  r <- smry %>% filter(spatial, abs(gamma - g) < .01)
  if (nrow(r)) cat(sprintf("  gamma %.2f : residual-only %.5f | full predictive %.5f\n",
                           g, r$logscore, r$logscore_full))
}
cat("  Higher gamma shrinks the random effects, so omitting their variance flatters\n")
cat("  it. Read the full column; the residual-only column is shown to expose that.\n")

# ------------------------------------------------------------------------------
# PAIRED ACROSS FOLDS. A mean difference is not enough. Every other specification
# decision in this paper is reported with a fold count and a paired test, and
# gamma should meet the same standard. The comparison that matters is against
# gamma = 1.0 -- the value with no ad hoc inflation -- not against the bottom of
# an arbitrary grid. The second comparison asks whether the apparent optimum is
# real: if 2.65 does not beat 1.4 significantly, the peak is within noise of 1.4
# and there is no case for moving off it in either direction.
# ------------------------------------------------------------------------------
cat("\n=== paired across folds (spatial spec) ===\n")
paired <- function(g1, g0, col, better = "higher") {
  a <- res %>% filter(spatial, abs(gamma - g1) < .01) %>% arrange(fold) %>% pull(.data[[col]])
  b <- res %>% filter(spatial, abs(gamma - g0) < .01) %>% arrange(fold) %>% pull(.data[[col]])
  if (!length(a) || length(a) != length(b)) return(invisible(NULL))
  d <- a - b
  w <- if (better == "higher") sum(d > 0) else sum(d < 0)
  cat(sprintf("  %-14s g=%.2f vs g=%.2f | mean diff %+.5f | favours %.2f in %2d/%d folds | t p=%.3f | Wilcoxon p=%.3f\n",
              col, g1, g0, mean(d), g1, w, length(d),
              t.test(a, b, paired = TRUE)$p.value,
              suppressWarnings(wilcox.test(a, b, paired = TRUE))$p.value))
}
paired(1.4,  1.0, "logscore_full")
paired(1.4,  1.0, "crps_full", "lower")
paired(1.4,  1.0, "rmse_kg",   "lower")
paired(2.65, 1.4, "logscore_full")
cat("  crps and rmse are lower-better; the fold counts already account for that.\n")

cat("\n=== what does gamma do to the length smooth and the maturation dip? ===\n")
ft <- bind_rows(lapply(full, function(x) tibble(gamma = x$gamma, edf_len = x$edf_len,
  edf_sp = x$edf_sp, edf_tot = x$edf_tot,
  dip_110 = x$nonlin_pct[x$L == 110], at_80 = x$nonlin_pct[x$L == 80])))
print(as.data.frame(ft %>% mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("  dip_110 is the % deviation from a log-linear fit at 110 cm.\n")
cat("  The published estimate is -2.5% (95% CrI -2.9 to -2.1) from 2,935 fish.\n")
cat("  If it shrinks toward zero as gamma rises, gamma is erasing a real signal.\n")

write_csv(res,  file.path(OUT, "TableR49_gamma_cv_raw.csv"))
write_csv(smry, file.path(OUT, "TableR50_gamma_cv_summary.csv"))
write_csv(ft,   file.path(OUT, "TableR51_gamma_nonlinearity.csv"))
saveRDS(list(res = res, smry = smry, ft = ft), file.path(OUT, "gamma_tuning.rds"))
message("31_gamma_tuning.R complete")
