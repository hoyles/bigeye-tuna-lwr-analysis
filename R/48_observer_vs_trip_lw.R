# ==============================================================================
# 48_observer_vs_trip_lw.R -- Do observer effects replicate across an observer's own trips?
#
# Section 4.6. Split-half test across each observer's trips, with a permutation null, and a bootstrap over
# observers for the observer and trip standard deviations.
#
# ENGINE AND TIMING: three gam(ML) fits and several hundred bam fits; under an hour.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260830)

JOBDIR <- file.path(OUT, "obstrip_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
NPART <- 100; NPERM <- 100; NBOOT <- 300; NWORKER <- 6

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
# Trip random slope (32_). The intercept variance components
# must be estimated in its presence: with the slope in the model the observer SD
# rises from 4.28% to 4.86% while trip is unchanged at 3.22%, so intervals
# computed without it no longer describe the fitted model.
SL     <- "s(loglen_c, tripf, bs='re')"
F_FULL <- as.formula(paste("logwt ~ s(loglen, k=20) + s(obs_name, bs='re') +",
                           "s(tripf, bs='re') +", SL, "+ sexf +", SP, "+ yrf"))
F_TRIP <- as.formula(paste("logwt ~ s(loglen, k=20) +",
                           "s(tripf, bs='re') +", SL, "+ sexf +", SP, "+ yrf"))
F_OBS  <- as.formula(paste("logwt ~ s(loglen, k=20) + s(obs_name, bs='re') +",
                           "sexf +", SP, "+ yrf"))

sd_of <- function(m, lab) {
  labs <- vapply(m$smooth, function(z) z$label, character(1))
  i <- match(lab, labs); if (is.na(i)) return(NA_real_)
  sd(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para])
}
blup_of <- function(m, lab) {
  labs <- vapply(m$smooth, function(z) z$label, character(1))
  i <- match(lab, labs); if (is.na(i)) return(NULL)
  b <- coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]
  names(b) <- levels(m$model[[m$smooth[[i]]$term]]); b
}
pct <- function(sd_log) 100 * (exp(sd_log) - 1)   # log scale -> % in weight

# ------------------------------------------------------------------------------
# 0. the design
# ------------------------------------------------------------------------------
ob <- d %>% group_by(obs_name) %>%
  summarise(ntrip = n_distinct(tripf), nfish = dplyr::n(), .groups = "drop")
ntr <- n_distinct(d$tripf); nob <- n_distinct(d$obs_name)
cat(sprintf("FINAL WEIGHT DATASET: %d fish | %d trips | %d observers\n", nrow(d), ntr, nob))
cat(sprintf("  one observer per trip: %s\n",
            all((d %>% group_by(tripf) %>% summarise(k = n_distinct(obs_name)))$k == 1)))
cat(sprintf("  observers with 1 trip: %d (%.0f%% of fish) | >=2 trips: %d (%.0f%% of fish)\n",
            sum(ob$ntrip == 1), 100*sum(ob$nfish[ob$ntrip==1])/sum(ob$nfish),
            sum(ob$ntrip >= 2), 100*sum(ob$nfish[ob$ntrip>=2])/sum(ob$nfish)))
cat(sprintf("  residual df separating observer from trip: %d - %d = %d\n", ntr, nob, ntr - nob))
MULTI <- as.character(ob$obs_name[ob$ntrip >= 2])

# ------------------------------------------------------------------------------
# 1. headline fits under gam(ML): does observer add anything beyond trip?
# ------------------------------------------------------------------------------
run_gam <- function(nm, form, data) {
  f <- file.path(JOBDIR, paste0("gam_", nm, ".rds"))
  if (file.exists(f)) { cat(sprintf("  gam %s: cached\n", nm)); return(readRDS(f)) }
  cat(sprintf("\nfitting gam %s ...\n", nm)); t0 <- Sys.time()
  m <- gam(form, data = data, family = scat, method = "ML", gamma = 1.4)
  cat(sprintf("  %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  o <- list(aic = AIC(m), sd_obs = sd_of(m, "s(obs_name)"), sd_trip = sd_of(m, "s(tripf)"))
  saveRDS(o, f); o
}
g_full <- run_gam("full",  F_FULL, d)
g_trip <- run_gam("triponly", F_TRIP, d)
g_obs  <- run_gam("obsonly",  F_OBS,  d)
cat("\n=== does each level earn its place? (gam ML, scat, gamma=1.4) ===\n")
cat(sprintf("  full (observer + trip)  AIC %10.1f | observer SD %.2f%% | trip SD %.2f%%\n",
            g_full$aic, pct(g_full$sd_obs), pct(g_full$sd_trip)))
cat(sprintf("  trip only               AIC %10.1f | dAIC vs full %+8.1f\n",
            g_trip$aic, g_trip$aic - g_full$aic))
cat(sprintf("  observer only           AIC %10.1f | dAIC vs full %+8.1f\n",
            g_obs$aic,  g_obs$aic  - g_full$aic))
cat("  Read the observer row carefully. A model that already carries a free effect\n")
cat("  for every trip has absorbed all trip-level variation, so adding observer can\n")
cat("  only change how those 232 effects are shrunk, not how well the data are fitted.\n")
cat("  A near-zero dAIC for observer is therefore expected whether or not observer\n")
cat("  effects are real, and is NOT evidence against them. The evidence that decides\n")
cat("  it is replication across an observer's own disjoint trips, below.\n")

# ------------------------------------------------------------------------------
# 2. replication of observer effects across disjoint trips
# ------------------------------------------------------------------------------
# One replicate: partition each multi-trip observer's trips into halves, fit with
# a random effect on observer x half, correlate the two halves across observers.
one_rep <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(dplyr)})
  set.seed(job$seed)
  dd <- D
  key <- unique(dd[, c("tripf", "obs_name")])
  key$obs_use <- as.character(key$obs_name)
  if (job$permute) {                      # null: reshuffle which observer owns
    tab <- table(key$obs_use)             # each trip, keeping trips-per-observer
    key$obs_use <- rep(names(tab), tab)[sample(nrow(key))]
  }
  key$half <- NA_integer_
  for (o in unique(key$obs_use)) {
    idx <- which(key$obs_use == o); k <- length(idx)
    if (k < 2) { key$half[idx] <- 1L; next }
    h <- rep(1:2, length.out = k)[sample(k)]
    if (length(unique(h)) < 2) h[sample(k, 1)] <- setdiff(1:2, h[1])
    key$half[idx] <- h
  }
  key$obs_half <- factor(paste0(key$obs_use, "__", key$half))
  dd <- dd %>% left_join(key[, c("tripf", "obs_half", "obs_use")], by = "tripf")
  dd$obs_half <- factor(dd$obs_half)
  m <- try(bam(logwt ~ s(loglen, k=20) + s(obs_half, bs='re') + s(tripf, bs='re') +
                 s(loglen_c, tripf, bs='re') +
                 sexf + te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc')) + yrf,
               data = dd, family = scat, method = "fREML", discrete = TRUE),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  labs <- vapply(m$smooth, function(z) z$label, character(1))
  i <- match("s(obs_half)", labs)
  b <- coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]
  names(b) <- levels(dd$obs_half)
  sp <- do.call(rbind, strsplit(names(b), "__", fixed = TRUE))
  w <- data.frame(obs = sp[,1], half = sp[,2], eff = as.numeric(b)) %>%
    filter(obs %in% MULTI2) %>%
    tidyr::pivot_wider(names_from = half, values_from = eff, names_prefix = "h")
  w <- w[stats::complete.cases(w), ]
  if (nrow(w) < 10) return(NULL)
  data.frame(permute = job$permute, rep = job$rep, n_obs = nrow(w),
             r = cor(w$h1, w$h2), rho = cor(w$h1, w$h2, method = "spearman"))
}

REPF <- file.path(JOBDIR, "replication.rds")
if (file.exists(REPF)) { rep_res <- readRDS(REPF); cat("\n  (replication cached)\n") } else {
  jobs <- c(lapply(1:NPART, function(i) list(rep = i, seed = 1000+i, permute = FALSE)),
            lapply(1:NPERM, function(i) list(rep = i, seed = 5000+i, permute = TRUE)))
  cat(sprintf("\nreplication: %d fits on %d workers ...\n", length(jobs), NWORKER))
  cl <- makeCluster(NWORKER)
  clusterExport(cl, c("D", "LIB", "MULTI2"),
                envir = list2env(list(D = d, LIB = LIB, MULTI2 = MULTI)))
  t0 <- Sys.time()
  rep_res <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, one_rep)))
  stopCluster(cl)
  cat(sprintf("  %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  saveRDS(rep_res, REPF)
}
obs_r  <- rep_res %>% filter(!permute); nul_r <- rep_res %>% filter(permute)
cat("\n=== do observer effects replicate across that observer's own trips? ===\n")
cat(sprintf("  observers compared per split: %d (median)\n", median(obs_r$n_obs)))
cat(sprintf("  real split-half   : mean r %+.3f (sd %.3f) over %d partitions\n",
            mean(obs_r$r), sd(obs_r$r), nrow(obs_r)))
cat(sprintf("  permuted observers: mean r %+.3f (sd %.3f) over %d partitions\n",
            mean(nul_r$r), sd(nul_r$r), nrow(nul_r)))
zr <- (mean(obs_r$r) - mean(nul_r$r)) /
      sqrt(var(obs_r$r)/nrow(obs_r) + var(nul_r$r)/nrow(nul_r))
cat(sprintf("  difference: %+.3f, Z = %.2f, p = %.3g\n",
            mean(obs_r$r) - mean(nul_r$r), zr, 2*pnorm(-abs(zr))))
cat(sprintf("  fraction of real partitions with r > the 95th centile of the null: %.2f\n",
            mean(obs_r$r > quantile(nul_r$r, .95))))
# what would we expect if the observer effect were entirely real?
mt <- ob$ntrip[ob$ntrip >= 2]
r_exp <- g_full$sd_obs^2 / (g_full$sd_obs^2 + g_full$sd_trip^2/mean(pmax(1, mt/2)))
cat(sprintf("  for reference, r expected if the fitted split were exactly true: %.2f\n", r_exp))

# ------------------------------------------------------------------------------
# 3. block bootstrap over trips: intervals for the two standard deviations
# ------------------------------------------------------------------------------
one_boot <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(dplyr)})
  set.seed(job$seed)
  dd <- D
  # resample OBSERVERS with replacement, carrying all of that observer's trips;
  # a repeated observer becomes a distinct person with distinct trips
  obs <- levels(dd$obs_name); pick <- sample(obs, length(obs), replace = TRUE)
  z <- bind_rows(lapply(seq_along(pick), function(i) {
    x <- dd[dd$obs_name == pick[i], ]
    x$obs_name <- paste0(pick[i], "_", i)
    x$tripf    <- paste0(as.character(x$tripf), "_", i); x }))
  z$tripf <- factor(z$tripf); z$obs_name <- factor(z$obs_name)
  z$sexf <- factor(z$sexf); z$yrf <- factor(z$yrf)
  m <- try(bam(logwt ~ s(loglen, k=20) + s(obs_name, bs='re') + s(tripf, bs='re') +
                 s(loglen_c, tripf, bs='re') +
                 sexf + te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc')) + yrf,
               data = z, family = scat, method = "fREML", discrete = TRUE),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  labs <- vapply(m$smooth, function(x) x$label, character(1))
  g <- function(lab) { i <- match(lab, labs)
    sd(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]) }
  data.frame(rep = job$rep, sd_obs = g("s(obs_name)"), sd_trip = g("s(tripf)"),
             n_obs = nlevels(z$obs_name), n_trip = nlevels(z$tripf),
             converged = isTRUE(m$converged))
}
BOOTF <- file.path(JOBDIR, "bootstrap.rds")
if (file.exists(BOOTF)) { bt <- readRDS(BOOTF); cat("\n  (bootstrap cached)\n") } else {
  cat(sprintf("\nbootstrap: %d refits on %d workers ...\n", NBOOT, NWORKER))
  cl <- makeCluster(NWORKER)
  clusterExport(cl, c("D", "LIB"), envir = list2env(list(D = d, LIB = LIB)))
  t0 <- Sys.time()
  bt <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl,
         lapply(1:NBOOT, function(i) list(rep = i, seed = 9000+i)), one_boot)))
  stopCluster(cl)
  cat(sprintf("  %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  saveRDS(bt, BOOTF)
}
cat("\n=== observer-block bootstrap intervals for the variance components ===\n")
cat("    (bam refits: use the SPREAD, not the centre; the point estimates are gam)\n")
nbad <- sum(!bt$converged | bt$sd_obs > 1 | bt$sd_trip > 1)
if (nbad) cat(sprintf("    %d of %d replicates discarded (non-convergent or degenerate)\n",
                      nbad, nrow(bt)))
bt <- bt[bt$converged & bt$sd_obs <= 1 & bt$sd_trip <= 1, ]
qi <- function(x) sprintf("median %.2f%% (95%% CI %.2f-%.2f)", pct(median(x)),
                          pct(quantile(x, .025)), pct(quantile(x, .975)))
cat(sprintf("  observer SD: point %.2f%%, bootstrap %s\n", pct(g_full$sd_obs), qi(bt$sd_obs)))
cat(sprintf("  trip SD    : point %.2f%%, bootstrap %s\n", pct(g_full$sd_trip), qi(bt$sd_trip)))
rat <- bt$sd_obs / bt$sd_trip
cat(sprintf("  observer/trip ratio: point %.2f, bootstrap median %.2f (95%% CI %.2f-%.2f)\n",
            g_full$sd_obs/g_full$sd_trip, median(rat), quantile(rat,.025), quantile(rat,.975)))
cat(sprintf("  bootstrap replicates in which observer SD exceeds trip SD: %.0f%% of %d\n",
            100*mean(bt$sd_obs > bt$sd_trip), nrow(bt)))

# ------------------------------------------------------------------------------
# 4. sensitivity: drop the observers who worked a single trip
# ------------------------------------------------------------------------------
d_multi <- droplevels(d[d$obs_name %in% MULTI, ])
cat(sprintf("\nmulti-trip observers only: %d fish, %d trips, %d observers\n",
            nrow(d_multi), nlevels(d_multi$tripf), nlevels(d_multi$obs_name)))
g_multi <- run_gam("multitrip", F_FULL, d_multi)
cat(sprintf("  observer SD %.2f%% (was %.2f%%) | trip SD %.2f%% (was %.2f%%)\n",
            pct(g_multi$sd_obs), pct(g_full$sd_obs),
            pct(g_multi$sd_trip), pct(g_full$sd_trip)))

# ------------------------------------------------------------------------------
cat("\n=== READING THIS ===\n")
if (mean(obs_r$r) - mean(nul_r$r) > 0.15 && zr > 3) {
  cat("  Observer effects REPLICATE across the observer's own disjoint trips, and\n")
  cat("  the permutation null does not. The observer level is a property of the\n")
  cat("  person, and the fitted observer/trip split is supported rather than assumed.\n")
} else if (zr > 2) {
  cat("  Observer effects replicate WEAKLY. Report the correlation and the interval\n")
  cat("  on the split; do not describe observer and trip as cleanly separable.\n")
} else {
  cat("  Observer effects DO NOT replicate across an observer's own trips. What the\n")
  cat("  model attributes to observers cannot be distinguished from trip-to-trip\n")
  cat("  variation, and the claim that the two are separable must be withdrawn.\n")
}
cat("  In every case the split rests on 63 observers with a median of 2 trips;\n")
cat("  the bootstrap interval, not the point estimate, is the honest summary.\n")

write_csv(rep_res, file.path(OUT, "TableR75_observer_replication.csv"))
write_csv(bt,      file.path(OUT, "TableR76_obstrip_bootstrap.csv"))
saveRDS(list(design = ob, gam = list(full = g_full, trip = g_trip, obs = g_obs,
                                     multi = g_multi),
             replication = rep_res, bootstrap = bt),
        file.path(OUT, "observer_vs_trip_lw.rds"))
message("48_observer_vs_trip_lw.R complete")
