# ==============================================================================
# 30_specification_cv.R -- Specification choices by trip-blocked cross-validation, and the ICC of each covariate within trip
#
# Sections 3.2, 3.5 and 4.1; Table 4 (spatial and gear rows).
#
# ENGINE AND TIMING: about 100 bam fits in parallel; 1-4 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel); library(scoringRules)
set.seed(20260826)

NFOLD <- 10; NWORKER <- 16
JOBDIR <- file.path(OUT, "speccv_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

# --- data: one fish per set, either sex ---------------------------------------
allf <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
         year = as.numeric(year), setf = factor(setid), tripf = factor(tripID),
         sexf = factor(sex)) %>%
  filter(complete.cases(logwt, loglen, hbf, lon, lat, mon, obs_name, yrf)) %>%
  droplevels() %>% as.data.frame()
allf <- allf[!allf$obs_name %in% c("obs_091", "obs_113", "obs_175"), ]
allf$obs_name <- droplevels(allf$obs_name)
d <- droplevels(draw_one_per_group(allf, "setf"))
cat(sprintf("one fish per set: %d fish (%d F, %d M) | %d trips | %d observers\n\n",
            nrow(d), sum(d$sexf == "F"), sum(d$sexf == "M"),
            nlevels(d$tripf), nlevels(d$obs_name)))

# ---------------------------------------------------------------------------
# PART 0: can space be separated from trip?
# ---------------------------------------------------------------------------
cat("=== PART 0: spatial / trip identifiability ===\n")
icc <- function(x, g) {
  s <- split(x, g); s <- s[lengths(s) >= 2]
  n <- lengths(s); mn <- sapply(s, mean); gm <- sum(n*mn)/sum(n)
  msb <- sum(n*(mn-gm)^2)/(length(s)-1)
  msw <- sum(sapply(s, function(z) sum((z-mean(z))^2)))/(sum(n)-length(s))
  n0 <- (sum(n)-sum(n^2)/sum(n))/(length(s)-1)
  (msb-msw)/(msb+(n0-1)*msw)
}
cat(sprintf("  ICC within trip: longitude %.4f | latitude %.4f | month %.4f\n",
            icc(d$lon, d$tripf), icc(d$lat, d$tripf), icc(d$mon, d$tripf)))
cat(sprintf("  (for comparison, HBF was 0.968 -- effectively a trip-level covariate)\n"))
tsd <- d %>% group_by(tripf) %>%
  summarise(n = dplyr::n(), lon_sd = sd(lon), lat_sd = sd(lat), .groups = "drop") %>%
  filter(n >= 5)
cat(sprintf("  within-trip SD: longitude median %.1f deg (overall %.1f) | latitude %.1f (overall %.1f)\n",
            median(tsd$lon_sd, na.rm=TRUE), sd(d$lon),
            median(tsd$lat_sd, na.rm=TRUE), sd(d$lat)))

for (CELL in c(5, 10)) {
  d$cell <- paste0(floor(d$lon/CELL)*CELL, "_", floor(d$lat/CELL)*CELL)
  cs <- d %>% group_by(cell) %>%
    summarise(n = dplyr::n(), trips = n_distinct(tripf), .groups = "drop") %>%
    filter(n >= 20)
  # median() returns a double, so use %.0f rather than %d
  cat(sprintf("  %2d-deg cells with >=20 fish: %d | with >=2 trips: %d | >=4 trips: %d | median trips/cell %.0f\n",
              CELL, nrow(cs), sum(cs$trips >= 2), sum(cs$trips >= 4), median(cs$trips)))
}
tc <- d %>% group_by(tripf) %>% summarise(nc = n_distinct(cell), .groups = "drop")
cat(sprintf("  10-deg cells visited per trip: median %.0f (range %d-%d)\n",
            median(tc$nc), min(tc$nc), max(tc$nc)))
cat("  => spatial effects are identifiable only from cells visited by SEVERAL trips\n\n")

# ---------------------------------------------------------------------------
# PART 1: specification cross-validation
# ---------------------------------------------------------------------------
trips <- levels(d$tripf)
d$fold <- setNames(sample(rep_len(1:NFOLD, length(trips))), trips)[as.character(d$tripf)]

RE   <- "s(obs_name, bs='re') + s(tripf, bs='re')"
SP3  <- "te(lon, lat, mon, k = c(10,10,4), bs = c('cr','cr','cc'))"
SP3s <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
SP2  <- "te(lon, lat, k = c(10,10), bs = c('cr','cr'))"
SPa  <- "s(lon, k=10) + s(lat, k=10)"

SPECS <- list(
  ref          = list(sp = SP3,  hbf = "",              yr = "yrf",         gamma = 1.4),
  sp_noMonth   = list(sp = SP2,  hbf = "",              yr = "yrf",         gamma = 1.4),
  sp_lowk      = list(sp = SP3s, hbf = "",              yr = "yrf",         gamma = 1.4),
  sp_additive  = list(sp = SPa,  hbf = "",              yr = "yrf",         gamma = 1.4),
  sp_none      = list(sp = "",   hbf = "",              yr = "yrf",         gamma = 1.4),
  hbf_linear   = list(sp = SP3,  hbf = "hbf",           yr = "yrf",         gamma = 1.4),
  hbf_smooth   = list(sp = SP3,  hbf = "s(hbf, k=5)",   yr = "yrf",         gamma = 1.4),
  yr_none      = list(sp = SP3,  hbf = "",              yr = "",            gamma = 1.4),
  yr_smooth    = list(sp = SP3,  hbf = "",              yr = "s(year, k=6)", gamma = 1.4),
  gamma1       = list(sp = SP3,  hbf = "",              yr = "yrf",         gamma = 1.0),
  gamma1_spNone= list(sp = "",   hbf = "",              yr = "yrf",         gamma = 1.0))

build <- function(s) {
  bits <- c("s(loglen, k=20)", RE, "sexf", s$sp, s$hbf, s$yr)
  bits <- bits[nzchar(bits)]
  as.formula(paste("logwt ~", paste(bits, collapse = " + ")))
}

run_one <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(scoringRules)})
  outf <- file.path(JOBDIR, sprintf("%s_f%02d.rds", job$spec, job$fold))
  if (file.exists(outf)) return(readRDS(outf))
  s <- SPECS[[job$spec]]
  tr <- d[d$fold != job$fold, ]; te <- d[d$fold == job$fold, ]
  te <- te[te$yrf %in% unique(tr$yrf), , drop = FALSE]
  if (nrow(te) < 5) return(NULL)
  tr$obs_name <- factor(tr$obs_name); tr$tripf <- factor(tr$tripf)
  tr$yrf <- factor(as.character(tr$yrf))
  te$yrf <- factor(as.character(te$yrf), levels = levels(tr$yrf))
  te$obs_name <- factor(levels(tr$obs_name)[1], levels = levels(tr$obs_name))
  te$tripf    <- factor(levels(tr$tripf)[1],    levels = levels(tr$tripf))
  t0 <- Sys.time()
  m <- try(bam(build(s), data = tr, family = scat, method = "fREML",
               discrete = TRUE, gamma = s$gamma), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  lab <- sapply(m$smooth, function(z) z$label)
  excl <- intersect(c("s(obs_name)", "s(tripf)"), lab)
  pr <- try(predict(m, te, se.fit = TRUE, exclude = excl), silent = TRUE)
  if (inherits(pr, "try-error")) return(NULL)
  mu <- as.numeric(pr$fit); sef <- as.numeric(pr$se.fit); y <- te$logwt
  th <- m$family$getTheta(TRUE); nu <- th[1]; sc <- sqrt(th[2]^2 + sef^2)
  gete <- function(p) { i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA)
    sum(m$edf[m$smooth[[i[1]]]$first.para:m$smooth[[i[1]]]$last.para]) }
  res <- data.frame(spec = job$spec, fold = job$fold, n = nrow(te),
    gamma = s$gamma, secs = as.numeric(difftime(Sys.time(), t0, units="secs")),
    edf_tot = sum(m$edf), edf_sp = suppressWarnings(max(gete("te("), gete("s(lon)"), na.rm=TRUE)),
    edf_trip = gete("s(tripf)"), edf_obs = gete("s(obs_name)"),
    rmse_kg = sqrt(mean((exp(y)-exp(mu))^2)), bias_kg = mean(exp(mu)-exp(y)),
    logscore = mean(dt((y-mu)/sc, df=nu, log=TRUE) - log(sc)),
    crps = mean(crps_t(y, df=nu, location=mu, scale=sc)))
  saveRDS(res, outf); res
}

jobs <- unlist(lapply(names(SPECS), function(s)
  lapply(1:NFOLD, function(f) list(spec = s, fold = f))), recursive = FALSE)
cat(sprintf("=== PART 1: %d jobs (%d specs x %d folds) ===\n",
            length(jobs), length(SPECS), NFOLD))
cl <- makeCluster(NWORKER)
clusterExport(cl, c("d", "SPECS", "LIB", "JOBDIR", "RE", "build"), envir = environment())
t0 <- Sys.time()
res <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, run_one)))
stopCluster(cl)
cat(sprintf("elapsed %.1f min for %d fits\n\n",
            as.numeric(difftime(Sys.time(), t0, units="mins")), nrow(res)))

smry <- res %>% group_by(spec) %>%
  summarise(gamma = first(gamma), folds = dplyr::n(), edf_tot = mean(edf_tot),
            edf_sp = mean(edf_sp), edf_trip = mean(edf_trip), edf_obs = mean(edf_obs),
            rmse_kg = weighted.mean(rmse_kg, n), bias_kg = weighted.mean(bias_kg, n),
            logscore = weighted.mean(logscore, n), crps = weighted.mean(crps, n),
            .groups = "drop") %>% arrange(desc(logscore))
cat("=== out-of-sample performance (higher logscore better) ===\n")
print(as.data.frame(smry %>% mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)

ref <- res %>% filter(spec == "ref") %>% select(fold, ls0 = logscore, cr0 = crps, rm0 = rmse_kg)
paired <- res %>% inner_join(ref, by = "fold") %>% group_by(spec) %>%
  summarise(d_logscore = mean(logscore - ls0), d_crps = mean(crps - cr0),
            d_rmse_kg = mean(rmse_kg - rm0), wins = sum(logscore > ls0),
            folds = dplyr::n(), .groups = "drop") %>% arrange(desc(d_logscore))
cat("\n=== paired against the reference spec (positive = better than reference) ===\n")
print(as.data.frame(paired %>% mutate(across(where(is.numeric), ~round(.x,5)))), row.names=FALSE)

cat("\n=== verdicts ===\n")
g <- function(s) smry$logscore[smry$spec == s]
cat(sprintf("  spatial term      : ref %.5f vs sp_none %.5f -> %s\n", g("ref"), g("sp_none"),
            ifelse(g("ref") > g("sp_none"), "KEEP spatial", "DROP spatial")))
cat(sprintf("  HBF               : ref(none) %.5f vs linear %.5f vs k=5 %.5f -> %s\n",
            g("ref"), g("hbf_linear"), g("hbf_smooth"),
            ifelse(g("ref") >= max(g("hbf_linear"), g("hbf_smooth")), "DROP HBF", "RETAIN HBF")))
cat(sprintf("  year              : yrf %.5f vs none %.5f vs s(year) %.5f\n",
            g("ref"), g("yr_none"), g("yr_smooth")))
cat(sprintf("  gamma             : 1.4 %.5f vs 1.0 %.5f -> %s\n", g("ref"), g("gamma1"),
            ifelse(g("ref") > g("gamma1"), "keep gamma=1.4", "gamma=1 is fine (Wood's 1.4 targets GCV)")))

write_csv(res,  file.path(OUT, "TableR46_speccv_raw.csv"))
write_csv(smry, file.path(OUT, "TableR47_speccv_summary.csv"))

# ---------------------------------------------------------------------------
# PART 2: outlier observers with trip in the model
# ---------------------------------------------------------------------------
cat("\n=== PART 2: outlier observers, re-checked with trip RE ===\n")
dall <- droplevels(draw_one_per_group(
  da2 %>% filter(processtype=="RGT", year>=2015, year<=2022, sex %in% c("F","M"),
                 length>=70, length<=200, hbf>10) %>%
    mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
           setf = factor(setid), tripf = factor(tripID), sexf = factor(sex)) %>%
    filter(complete.cases(logwt, loglen, hbf, lon, lat, mon, obs_name, yrf)) %>%
    as.data.frame(), "setf"))
m_all <- bam(build(SPECS$ref), data = dall, family = scat, method = "fREML",
             discrete = TRUE, gamma = 1.4)
re <- extract_ranef(m_all, "s(obs_name)")
re$z <- (re$value - mean(re$value)) / sd(re$value)
cat(sprintf("  observers: %d | RE sd = %.4f (%.2f%% in weight)\n",
            nrow(re), sd(re$value), 100*(exp(sd(re$value))-1)))
cat("  previously excluded (identified WITHOUT trip in the model):\n")
print(as.data.frame(re %>% filter(group %in% c("obs_091","obs_113","obs_175")) %>%
        select(group, value, z) %>% mutate(across(where(is.numeric), ~round(.x,4)))),
      row.names = FALSE)
cat(sprintf("  observers now beyond |z| > 2.5: %d -> %s\n",
            sum(abs(re$z) > 2.5),
            paste(re$group[abs(re$z) > 2.5], collapse = ", ")))
write_csv(re, file.path(OUT, "TableR48_observer_re_with_trip.csv"))

saveRDS(list(res = res, smry = smry, paired = paired, re = re),
        file.path(OUT, "specification_cv.rds"))
message("30_specification_cv.R complete")
