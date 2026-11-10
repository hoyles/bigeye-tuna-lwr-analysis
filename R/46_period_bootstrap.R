# ==============================================================================
# 46_period_bootstrap.R -- Uncertainty in b for each period by resampling trips
#
# Sections 3.6 and 4.9.
#
# ENGINE AND TIMING: bam, 2 periods x 300 resamples in parallel; 10-25 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260830)

NBOOT <- 300; NWORKER <- 6

rgt <- readRDS(file.path(OUT, "final_dataset.rds"))$d
rgg <- readRDS(file.path(OUT, "rgg_dataset.rds"))$d
prep <- function(x) {
  x$tripf <- factor(x$tripf); x$obs_name <- factor(x$obs_name); x$sexf <- factor(x$sexf)
  as.data.frame(x)
}
rgt <- prep(rgt); rgg <- prep(rgg)
cat(sprintf("RGG: %d fish, %d trips | RGT: %d fish, %d trips\n",
            nrow(rgg), nlevels(rgg$tripf), nrow(rgt), nlevels(rgt$tripf)))

F_EQ1 <- logwt ~ loglen + s(obs_name, bs = "re") + s(tripf, bs = "re") + sexf

one_boot <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(dplyr)})
  Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
  d <- if (job$period == "RGG") RGG else RGT
  if (job$rep == 0) {                       # rep 0 = the unresampled fit
    dd <- d
  } else {
    set.seed(7000 + job$rep + ifelse(job$period == "RGG", 0, 100000))
    tr <- levels(d$tripf)
    pick <- sample(tr, length(tr), replace = TRUE)
    # rebuild with duplicated trips given distinct labels, so a trip drawn twice
    # contributes two independent random-effect levels, as a new sample would
    dd <- bind_rows(lapply(seq_along(pick), function(i) {
      z <- d[d$tripf == pick[i], ]; z$tripf <- paste0(pick[i], "_", i); z }))
    dd$tripf <- factor(dd$tripf); dd$obs_name <- factor(dd$obs_name)
    dd$sexf <- factor(dd$sexf)
  }
  m <- try(bam(F_EQ1, data = dd, family = scat, method = "fREML",
               discrete = TRUE, gamma = 1.4), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  data.frame(period = job$period, rep = job$rep,
             b = unname(coef(m)["loglen"]),
             a = unname(exp(coef(m)["(Intercept)"])),
             se_nominal = unname(summary(m)$se["loglen"]))
}

jobs <- c(lapply(0:NBOOT, function(i) list(period = "RGG", rep = i)),
          lapply(0:NBOOT, function(i) list(period = "RGT", rep = i)))
cat(sprintf("\n%d fits on %d workers ...\n", length(jobs), NWORKER))
cl <- makeCluster(NWORKER)
clusterExport(cl, c("RGG", "RGT", "F_EQ1", "LIB"),
              envir = list2env(list(RGG = rgg, RGT = rgt, F_EQ1 = F_EQ1, LIB = LIB)))
t0 <- Sys.time()
res <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, one_boot)))
stopCluster(cl)
cat(sprintf("elapsed %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
saveRDS(res, file.path(OUT, "period_bootstrap_raw.rds"))

pt  <- res %>% filter(rep == 0)
bs  <- res %>% filter(rep > 0)
summ <- bs %>% group_by(period) %>%
  summarise(nboot = dplyr::n(), b_mean = mean(b), se_boot = sd(b),
            lo = quantile(b, .025), hi = quantile(b, .975), .groups = "drop") %>%
  left_join(pt %>% select(period, b_point = b, se_nominal), by = "period") %>%
  mutate(inflation = se_boot / se_nominal)
cat("\n=== b by period: nominal vs trip-bootstrap uncertainty ===\n")
print(as.data.frame(summ %>% select(period, b_point, se_nominal, se_boot, inflation,
                                    lo, hi, nboot) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

bg <- summ$b_point[summ$period == "RGG"]; bt <- summ$b_point[summ$period == "RGT"]
sg <- summ$se_nominal[summ$period == "RGG"]; st <- summ$se_nominal[summ$period == "RGT"]
Bg <- summ$se_boot[summ$period == "RGG"];    Bt <- summ$se_boot[summ$period == "RGT"]
d_nom  <- (bt - bg) / sqrt(sg^2 + st^2)
d_boot <- (bt - bg) / sqrt(Bg^2 + Bt^2)
cat(sprintf("\n  difference in b: %+.4f\n", bt - bg))
cat(sprintf("  in NOMINAL standard errors      : %.1f sigma\n", abs(d_nom)))
cat(sprintf("  in TRIP-BOOTSTRAP standard errors: %.1f sigma\n", abs(d_boot)))
cat(sprintf("  the nominal SE understates the uncertainty by a factor of %.1f (RGG), %.1f (RGT)\n",
            Bg/sg, Bt/st))

cat("\n=== READING THIS ===\n")
if (abs(d_boot) > 3) {
  cat("  The difference between periods survives resampling trips. It is not\n")
  cat("  merely an artefact of which trips each decade happened to contain.\n")
} else if (abs(d_boot) > 2) {
  cat("  The difference is marginal once trip resampling is allowed for. Report\n")
  cat("  the bootstrap interval, not the nominal one, and do not call it decisive.\n")
} else {
  cat("  The difference between periods is WITHIN what resampling trips produces.\n")
  cat("  A decade of new sampling would generate a difference of this size with no\n")
  cat("  change in the fish at all. This demonstrates the update trap on our own\n")
  cat("  data rather than arguing it from variance components.\n")
}
cat("\n  NOTE: the periods also differ in recorded process type, so even a\n")
cat("  surviving difference could not be attributed to the fish without further\n")
cat("  argument.\n")

write_csv(summ, file.path(OUT, "TableR71_period_bootstrap.csv"))
saveRDS(list(summ = summ, res = res), file.path(OUT, "period_bootstrap.rds"))
message("46_period_bootstrap.R complete")
