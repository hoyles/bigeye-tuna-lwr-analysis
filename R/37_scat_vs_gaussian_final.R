# ==============================================================================
# 37_scat_vs_gaussian_final.R -- Scaled-t against Gaussian errors, by trip-blocked cross-validation
#
# Section 4.5. Each withheld fish is scored against the predictive distribution for a new trip, with the
# observer, trip and trip-slope variance added back.
#
# ENGINE AND TIMING: 20 gam(ML) fits in parallel; 30-90 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
library(scoringRules)

set.seed(20260828)
NFOLD   <- 10
NWORKER <- 10
THREADS <- 2
JOBDIR  <- file.path(OUT, "scatgaus_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

dat <- readRDS(file.path(OUT, "final_dataset.rds"))
d <- dat$d
d$tripf <- factor(d$tripf); d$obs_name <- factor(d$obs_name); d$yrf <- factor(d$yrf)
cat(sprintf("final dataset: %d fish | %d trips | %d observers\n",
            nrow(d), nlevels(d$tripf), nlevels(d$obs_name)))

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE <- "s(obs_name, bs='re') + s(tripf, bs='re')"
# Trip random slope (32_). Both families must carry it, or
# the comparison confounds the error distribution with the slope term -- the same
# reason it was added to the Gaussian twin in 38_.
FORM <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE,
                         "+ s(loglen_c, tripf, bs='re') + sexf +", SP, "+ yrf"))
cat("formula: "); print(FORM)

run_one <- function(job) {
  .libPaths(LIB)
  suppressMessages({ library(mgcv); library(scoringRules); library(digest) })
  Sys.setenv(OMP_NUM_THREADS = THREADS, OPENBLAS_NUM_THREADS = THREADS)
  # Cache key includes a formula hash: without it, changing the specification
  # (as when the trip slope was adopted) silently returns the previous run's fits
  # under the new name. Same trap 32_ carried.
  fh <- substr(digest::digest(deparse(FORM)), 1, 8)
  outf <- file.path(JOBDIR, paste0(job$fam, "__F", job$fold, "_", fh, ".rds"))
  if (file.exists(outf)) return(readRDS(outf))            # resume support

  tr <- job$train; te <- job$test
  # a held-out year cannot be predicted from a yrf factor level never trained on
  tr$yrf <- factor(as.character(tr$yrf))
  te <- te[as.character(te$yrf) %in% levels(tr$yrf), , drop = FALSE]
  if (nrow(te) < 5) return(NULL)
  te$yrf <- factor(as.character(te$yrf), levels = levels(tr$yrf))
  tr$obs_name <- factor(tr$obs_name); tr$tripf <- factor(tr$tripf)
  # park the unseen levels on a training level; both terms are excluded below
  te$obs_name <- factor(levels(tr$obs_name)[1], levels = levels(tr$obs_name))
  te$tripf    <- factor(levels(tr$tripf)[1],    levels = levels(tr$tripf))

  t0 <- Sys.time()
  m <- try(gam(FORM, data = tr, family = job$fam, method = "ML", gamma = 1.4),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)

  labs <- vapply(m$smooth, function(s) s$label, character(1))
  excl <- intersect(c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)"), labs)
  pr <- try(predict(m, te, se.fit = TRUE, exclude = excl), silent = TRUE)
  if (inherits(pr, "try-error")) return(NULL)
  mu <- as.numeric(pr$fit); sef <- as.numeric(pr$se.fit); y <- te$logwt

  # variance the excluded random effects would contribute for a NEW trip.
  # SD of the BLUPs -- the same estimator the manuscript quotes for the observer
  # (4.9%) and trip (3.2%) SD, so the numbers stay mutually consistent.
  #
  # The trip SLOPE contributes too, and unlike the intercepts its contribution is
  # LENGTH-DEPENDENT: a new trip's exponent is unknown, so the uncertainty it adds
  # grows with distance from the centring point. v_re is therefore a vector, not a
  # scalar. Omitting it would make the predictive distribution too narrow for a
  # model that carries the slope.
  blup_sd <- function(lab) {
    i <- match(lab, labs); if (is.na(i)) return(0)
    sd(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para])
  }
  sd_obs <- blup_sd("s(obs_name)"); sd_trip <- blup_sd("s(tripf)")
  sd_slope <- blup_sd("s(loglen_c,tripf)")
  v_re <- sd_obs^2 + sd_trip^2 + sd_slope^2 * te$loglen_c^2

  # scale of the observation-level error, on the log scale
  if (job$fam == "scat") {
    th <- m$family$getTheta(TRUE); nu <- th[1]; v_obs_err <- th[2]^2
  } else {
    nu <- NA_real_; v_obs_err <- m$sig2
  }

  score <- function(v_extra) {
    sc <- sqrt(v_obs_err + sef^2 + v_extra)
    if (job$fam == "scat") {
      ls <- dt((y - mu) / sc, df = nu, log = TRUE) - log(sc)
      cr <- crps_t(y, df = nu, location = mu, scale = sc)
    } else {
      ls <- dnorm(y, mu, sc, log = TRUE)
      cr <- crps_norm(y, mean = mu, sd = sc)
    }
    c(logscore = mean(ls), crps = mean(cr))
  }
  s_fit <- score(0); s_full <- score(v_re)

  res <- data.frame(
    fam = job$fam, fold = job$fold, n = nrow(te),
    secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    edf = sum(m$edf), nu = nu,
    sd_obs_pct = 100*(exp(sd_obs) - 1), sd_trip_pct = 100*(exp(sd_trip) - 1),
    logscore_fit  = s_fit["logscore"],  crps_fit  = s_fit["crps"],
    logscore_full = s_full["logscore"], crps_full = s_full["crps"],
    rmse_log = sqrt(mean((y - mu)^2)), mae_log = mean(abs(y - mu)),
    bias_log = mean(mu - y),
    rmse_kg = sqrt(mean((exp(y) - exp(mu))^2)),
    mae_kg  = mean(abs(exp(y) - exp(mu))),
    bias_kg = mean(exp(mu) - exp(y)),
    row.names = NULL)
  saveRDS(res, outf)                                      # incremental save
  res
}

# --- trip-blocked folds -------------------------------------------------------
trips <- levels(d$tripf)
tripfold <- setNames(sample(rep_len(1:NFOLD, length(trips))), trips)
d$fold <- tripfold[as.character(d$tripf)]
cat("\n=== 10-fold CV blocked by trip ===\n"); print(table(d$fold))

jobs <- list()
for (f in 1:NFOLD) for (fm in c("scat", "gaussian"))
  jobs[[length(jobs) + 1]] <- list(fam = fm, fold = f,
                                   train = d[d$fold != f, ], test = d[d$fold == f, ])
FH <- substr(digest::digest(deparse(FORM)), 1, 8)
cached <- sum(file.exists(file.path(JOBDIR,
  vapply(jobs, function(j) paste0(j$fam, "__F", j$fold, "_", FH, ".rds"), character(1)))))
cat(sprintf("jobs: %d (%d already cached, %d to fit)\n", length(jobs), cached,
            length(jobs) - cached))

cl <- makeCluster(NWORKER)
clusterExport(cl, c("FORM", "LIB", "JOBDIR", "THREADS"), envir = environment())
t0 <- Sys.time()
res <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, run_one)))
stopCluster(cl)
cat(sprintf("elapsed: %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
saveRDS(res, file.path(OUT, "scat_vs_gaussian_final.rds"))

# --- summary ------------------------------------------------------------------
sumr <- res %>% group_by(fam) %>%
  summarise(folds = dplyr::n(), nobs = sum(n), edf = mean(edf), nu = mean(nu),
            logscore_fit  = weighted.mean(logscore_fit,  n),
            logscore_full = weighted.mean(logscore_full, n),
            crps_fit  = weighted.mean(crps_fit,  n),
            crps_full = weighted.mean(crps_full, n),
            rmse_log = weighted.mean(rmse_log, n),
            rmse_kg  = weighted.mean(rmse_kg,  n),
            bias_kg  = weighted.mean(bias_kg,  n),
            secs = mean(secs), .groups = "drop")
cat("\n--- scaled-t vs Gaussian, final specification ---\n")
cat("    (higher logscore better; lower crps/rmse better)\n")
print(as.data.frame(sumr %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

# --- paired by fold, which is the test that matters ---------------------------
w <- res %>% select(fam, fold, n, logscore_fit, logscore_full,
                    crps_fit, crps_full, rmse_kg) %>%
  pivot_wider(names_from = fam, values_from = c(logscore_fit, logscore_full,
                                                crps_fit, crps_full, rmse_kg))
pairtab <- tibble(
  metric = c("log score (fit only)", "log score (full predictive)",
             "CRPS (fit only)", "CRPS (full predictive)", "RMSE kg"),
  scat_better_in = c(
    sum(w$logscore_fit_scat  > w$logscore_fit_gaussian),
    sum(w$logscore_full_scat > w$logscore_full_gaussian),
    sum(w$crps_fit_scat      < w$crps_fit_gaussian),
    sum(w$crps_full_scat     < w$crps_full_gaussian),
    sum(w$rmse_kg_scat       < w$rmse_kg_gaussian)),
  folds = nrow(w),
  mean_diff = c(
    mean(w$logscore_fit_scat  - w$logscore_fit_gaussian),
    mean(w$logscore_full_scat - w$logscore_full_gaussian),
    mean(w$crps_fit_scat      - w$crps_fit_gaussian),
    mean(w$crps_full_scat     - w$crps_full_gaussian),
    mean(w$rmse_kg_scat       - w$rmse_kg_gaussian)))
pairtab$p_wilcoxon <- c(
  wilcox.test(w$logscore_fit_scat,  w$logscore_fit_gaussian,  paired = TRUE)$p.value,
  wilcox.test(w$logscore_full_scat, w$logscore_full_gaussian, paired = TRUE)$p.value,
  wilcox.test(w$crps_fit_scat,      w$crps_fit_gaussian,      paired = TRUE)$p.value,
  wilcox.test(w$crps_full_scat,     w$crps_full_gaussian,     paired = TRUE)$p.value,
  wilcox.test(w$rmse_kg_scat,       w$rmse_kg_gaussian,       paired = TRUE)$p.value)
cat("\n--- paired by fold (scaled-t vs Gaussian) ---\n")
cat("    mean_diff sign: log score positive = scat better; CRPS/RMSE negative = scat better\n")
print(as.data.frame(pairtab %>%
  mutate(mean_diff = signif(mean_diff, 3),
         p_wilcoxon = format.pval(p_wilcoxon, digits = 2))), row.names = FALSE)

cat(sprintf("\nestimated scaled-t degrees of freedom (nu): %.2f\n",
            mean(res$nu, na.rm = TRUE)))
cat("  low nu = genuinely heavy tails; nu above ~30 is effectively Gaussian\n")

cat("\n--- per fold, log score (full predictive) ---\n")
print(as.data.frame(w %>% select(fold, scat = logscore_full_scat,
                                 gaussian = logscore_full_gaussian) %>%
  mutate(diff = round(scat - gaussian, 4),
         scat = round(scat, 4), gaussian = round(gaussian, 4))), row.names = FALSE)

write_csv(sumr,    file.path(OUT, "TableR60_scat_vs_gaussian_final.csv"))
write_csv(pairtab, file.path(OUT, "TableR61_scat_vs_gaussian_paired.csv"))
write_csv(res,     file.path(OUT, "TableR62_scat_vs_gaussian_folds.csv"))
message("37_scat_vs_gaussian_final.R complete")
