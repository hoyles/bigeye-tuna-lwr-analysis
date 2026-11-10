# ==============================================================================
# 24_set_icc.R -- Within-set intra-class correlation, design effect and effective sample size
#
# Section 3.1 and Table 3.
#
# ENGINE AND TIMING: residual-based moments; a few minutes.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

allf <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
         setf = factor(setid), tripf = factor(tripID)) %>%
  filter(complete.cases(logwt, loglen, hbf, lon, lat, mon, obs_name, yrf)) %>%
  droplevels() %>% as.data.frame()
cat(sprintf("fish %d | sets %d | trips %d | observers %d | mean fish/set %.2f\n\n",
            nrow(allf), nlevels(allf$setf), nlevels(allf$tripf),
            nlevels(allf$obs_name), nrow(allf)/nlevels(allf$setf)))

# --- fit the base model on ALL fish (no set RE -- that is what failed) --------
FORM <- logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") +
  te(lon, lat, mon, k = c(10, 10, 4), bs = c("cr", "cr", "cc")) + s(hbf, k = 15) + yrf
cat("fitting base model on all fish with bam(discrete=TRUE) ...\n"); flush.console()
t0 <- Sys.time()
m <- bam(FORM, data = allf, family = scat, method = "fREML",
         discrete = TRUE, nthreads = 8)
cat(sprintf("  done in %.1f min | dev.expl %.2f%% | edf %.1f\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")),
            summary(m)$dev.expl * 100, sum(m$edf)))
allf$res <- residuals(m, type = "response")

# --- ICC of the residuals -----------------------------------------------------
icc <- function(x, g) {
  s <- split(x, g); s <- s[lengths(s) >= 2]
  n <- lengths(s); mn <- sapply(s, mean)
  gm <- sum(n * mn) / sum(n)
  msb <- sum(n * (mn - gm)^2) / (length(s) - 1)
  msw <- sum(sapply(s, function(z) sum((z - mean(z))^2))) / (sum(n) - length(s))
  n0 <- (sum(n) - sum(n^2) / sum(n)) / (length(s) - 1)
  c(icc = (msb - msw) / (msb + (n0 - 1) * msw), n0 = n0)
}
cat("\n=== intra-cluster correlation of residual weight-at-length ===\n")
i_set  <- icc(allf$res, allf$setf)
i_trip <- icc(allf$res, allf$tripf)
cat(sprintf("  within SET  : rho = %.4f (mean cluster size %.2f)\n", i_set["icc"], i_set["n0"]))
cat(sprintf("  within TRIP : rho = %.4f (mean cluster size %.1f)\n", i_trip["icc"], i_trip["n0"]))

mbar <- nrow(allf) / nlevels(allf$setf)
deff <- 1 + (mbar - 1) * i_set["icc"]
cat(sprintf("\n  design effect from set clustering = 1 + (%.2f-1) x %.4f = %.2f\n",
            mbar, i_set["icc"], deff))
cat(sprintf("  effective n, all fish        = %d / %.2f = %.0f\n",
            nrow(allf), deff, nrow(allf) / deff))
cat(sprintf("  effective n, one fish per set= %d\n", nlevels(allf$setf)))
cat(sprintf("  ratio (all fish / one per set) = %.2f\n",
            (nrow(allf) / deff) / nlevels(allf$setf)))

# for comparison: how big is the set-level variation relative to observer/trip?
cat("\n=== set-level SD in context ===\n")
sd_res <- sd(allf$res)
cat(sprintf("  residual SD                = %.4f (%.2f%% in weight)\n",
            sd_res, 100 * (exp(sd_res) - 1)))
sd_set <- sqrt(max(0, i_set["icc"]) ) * sd_res
cat(sprintf("  implied set-level SD       = %.4f (%.2f%% in weight)\n",
            sd_set, 100 * (exp(sd_set) - 1)))

# --- does pseudo-replication under-smooth? ------------------------------------
# Compare the smoothing chosen on all fish against one fish per set.
cat("\n=== effect of pseudo-replication on smoothing parameter selection ===\n")
one <- allf %>% group_by(setf) %>% slice_sample(n = 1) %>% ungroup() %>%
  droplevels() %>% as.data.frame()
m1 <- bam(FORM, data = one, family = scat, method = "fREML",
          discrete = TRUE, nthreads = 8)
lab <- sapply(m$smooth, function(s) s$label)
e_all <- sapply(m$smooth,  function(s) sum(m$edf[s$first.para:s$last.para]))
e_one <- sapply(m1$smooth, function(s) sum(m1$edf[s$first.para:s$last.para]))
cat(sprintf("  %-22s %10s %10s\n", "term", "all fish", "one/set"))
for (i in seq_along(lab))
  cat(sprintf("  %-22s %10.2f %10.2f\n", lab[i], e_all[i], e_one[i]))
cat(sprintf("  %-22s %10.2f %10.2f\n", "TOTAL edf", sum(m$edf), sum(m1$edf)))
cat("\n  (markedly higher edf on all fish = pseudo-replication buying wiggliness\n")
cat("   that the effective sample size does not support)\n")

saveRDS(list(icc_set = i_set, icc_trip = i_trip, deff = deff,
             edf_all = setNames(e_all, lab), edf_one = setNames(e_one, lab)),
        file.path(OUT, "set_icc.rds"))
message("24_set_icc.R complete")
