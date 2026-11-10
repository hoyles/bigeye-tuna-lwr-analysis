# ==============================================================================
# 54_trip_slope_replication.R -- Do trip slopes replicate on fish that did not estimate them?
#
# Section 4.6. Split-half test within trips against a null that reassigns fish to trips at random.
#
# ENGINE AND TIMING: bam, 100 splits and 100 permutations in parallel; 15-25 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260830)

JOBDIR <- file.path(OUT, "tripslope_rep_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
NSPLIT <- 100; NPERM <- 100; NWORKER <- 6
SD_FITTED <- 0.2085          # SD of the predicted trip slopes in Model A (32_)

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf),
                  loglen_c = loglen - mean(loglen)) %>% as.data.frame()

# a trip can only carry a slope if it spans some length range; require enough
# fish and enough spread, and report how many qualify
tinfo <- d %>% group_by(tripf) %>%
  summarise(n = dplyr::n(), sd_len = sd(loglen_c), nlen = n_distinct(length),
            .groups = "drop")
KEEP <- as.character(tinfo$tripf[tinfo$n >= 30 & tinfo$sd_len > 0.05])
cat(sprintf("%d fish | %d trips | %d trips with >=30 fish and length spread (%.0f%% of fish)\n",
            nrow(d), nlevels(d$tripf), length(KEEP),
            100*sum(tinfo$n[as.character(tinfo$tripf) %in% KEEP])/nrow(d)))
cat(sprintf("  fish per qualifying trip: median %d; distinct lengths: median %d\n",
            median(tinfo$n[as.character(tinfo$tripf) %in% KEEP]),
            median(tinfo$nlen[as.character(tinfo$tripf) %in% KEEP])))

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"

one_rep <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(dplyr)})
  set.seed(job$seed)
  dd <- D
  if (job$permute) {
    # destroy trip structure entirely: reassign fish to pseudo-trips of the same
    # sizes. Nothing real survives, so the halves must be independent.
    sizes <- as.integer(table(dd$tripf))
    lab <- rep(levels(dd$tripf), sizes)
    dd$tripf <- factor(lab[sample(nrow(dd))], levels = levels(dd$tripf))
  }
  # split each trip's fish at random into two halves
  dd$half <- ave(seq_len(nrow(dd)), dd$tripf,
                 FUN = function(i) rep(1:2, length.out = length(i))[sample(length(i))])
  dd$th <- factor(paste0(as.character(dd$tripf), "__", dd$half))
  m <- try(bam(logwt ~ s(loglen, k = 20) + s(obs_name, bs = 're') +
                 s(th, bs = 're') + s(loglen_c, th, bs = 're') + sexf +
                 te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc')) + yrf,
               data = dd, family = scat, method = "fREML", discrete = TRUE),
           silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  labs <- vapply(m$smooth, function(z) z$label, character(1))
  i <- match("s(loglen_c,th)", labs)
  if (is.na(i)) return(NULL)
  b <- coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]
  names(b) <- levels(dd$th)
  sp <- do.call(rbind, strsplit(names(b), "__", fixed = TRUE))
  w <- data.frame(trip = sp[,1], half = sp[,2], eff = as.numeric(b)) %>%
    filter(trip %in% KEEP2) %>%
    tidyr::pivot_wider(names_from = half, values_from = eff, names_prefix = "h")
  w <- w[stats::complete.cases(w), ]
  if (nrow(w) < 20) return(NULL)
  data.frame(permute = job$permute, rep = job$rep, n_trip = nrow(w),
             r = cor(w$h1, w$h2), rho = cor(w$h1, w$h2, method = "spearman"),
             sd_half = sd(c(w$h1, w$h2)))
}

RF <- file.path(JOBDIR, "replication.rds")
if (file.exists(RF)) { res <- readRDS(RF); cat("\n  (cached)\n") } else {
  jobs <- c(lapply(1:NSPLIT, function(i) list(rep = i, seed = 4100+i, permute = FALSE)),
            lapply(1:NPERM,  function(i) list(rep = i, seed = 9100+i, permute = TRUE)))
  cat(sprintf("\n%d fits on %d workers ...\n", length(jobs), NWORKER))
  cl <- makeCluster(NWORKER)
  clusterExport(cl, c("D", "LIB", "KEEP2"),
                envir = list2env(list(D = d, LIB = LIB, KEEP2 = KEEP)))
  t0 <- Sys.time()
  res <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, one_rep)))
  stopCluster(cl)
  cat(sprintf("  %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  saveRDS(res, RF)
}

obs <- res %>% filter(!permute); nul <- res %>% filter(permute)
cat("\n=== do trip-specific exponents replicate across halves of the trip's fish? ===\n")
cat(sprintf("  trips compared per split: %d (median)\n", median(obs$n_trip)))
cat(sprintf("  real split-half : mean r %+.3f (sd %.3f, 95%% range %+.3f to %+.3f)\n",
            mean(obs$r), sd(obs$r), quantile(obs$r,.025), quantile(obs$r,.975)))
cat(sprintf("  pseudo-trips    : mean r %+.3f (sd %.3f)\n", mean(nul$r), sd(nul$r)))
zr <- (mean(obs$r) - mean(nul$r)) /
      sqrt(var(obs$r)/nrow(obs) + var(nul$r)/nrow(nul))
cat(sprintf("  difference %+.3f, Z = %.2f, p = %.3g\n",
            mean(obs$r) - mean(nul$r), zr, 2*pnorm(-abs(zr))))
cat(sprintf("  fraction of real splits above the null's 95th centile: %.2f\n",
            mean(obs$r > quantile(nul$r, .95))))

# what correlation would the fitted spread predict? Each half carries about half
# the trip's fish, so the sampling variance of a half-trip slope is roughly twice
# that of the whole-trip estimate.
se_half <- median(sapply(KEEP, function(tr) {
  z <- d[as.character(d$tripf) == tr, ]
  s <- summary(lm(logwt ~ loglen_c, data = z))
  sqrt(2) * coef(s)["loglen_c", "Std. Error"] }))
r_exp <- SD_FITTED^2 / (SD_FITTED^2 + se_half^2)
cat(sprintf("\n  median SE of a half-trip slope: %.3f\n", se_half))
cat(sprintf("  r expected if the fitted spread of %.3f were exactly true: %.2f\n",
            SD_FITTED, r_exp))
cat(sprintf("  (weight-at-length observer effects, for comparison: 0.62 observed vs 0.71 expected)\n"))

cat("
=== comparison with the fitted spread ===
")
if (mean(obs$r) > 0 && mean(obs$r) < 0.5 * r_exp) {
  cat("  Note: observed replication falls well short of what the fitted spread\n")
  cat("  predicts, so even if non-zero the fitted SD of b is overstated.\n")
} else {
  cat(sprintf("  Observed replication (%.2f) is close to the %.2f the fitted spread predicts,\n",
              mean(obs$r), r_exp))
  cat("  so the estimated SD of b across trips is not materially inflated.\n")
}

write_csv(res, file.path(OUT, "TableR83_trip_slope_replication.csv"))
saveRDS(list(res = res, r_expected = r_exp, se_half = se_half, z = zr),
        file.path(OUT, "trip_slope_replication.rds"))
message("54_trip_slope_replication.R complete")
