# ==============================================================================
# 15_uniform_baseline.R -- Rounding propensity and direction against the uniform baseline
#
# Sections 3.1 and 4.6; Table 12. Both sexes, pre-exclusion pool.
#
# ENGINE AND TIMING: no model fitting; about 10 min (observer bootstrap).
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

allfish <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(wrem = weight %% 5, lrem = length %% 5) %>% as.data.frame()
cat("fish:", nrow(allfish), "| observers:", n_distinct(allfish$obs_name), "\n")

# ---------------------------------------------------------------------------
# 1. IS 20% PER REMAINDER CORRECT FOR THESE DISTRIBUTIONS?
# ---------------------------------------------------------------------------
cat("\n=== 1. does a smooth underlying density imply 20% per remainder? ===\n")
check_uniform <- function(x, lab, bw_mult = 4) {
  # bandwidth much wider than the 5-unit rounding interval, so the smooth fit is
  # insensitive to rounding but still tracks the real shape of the distribution
  d <- density(x, bw = bw_mult * 5, from = min(x) - 20, to = max(x) + 20, n = 4096)
  grid <- seq(floor(min(x)) - 10, ceiling(max(x)) + 10, by = 1)
  f <- approx(d$x, d$y, xout = grid)$y; f[is.na(f)] <- 0; f <- f / sum(f)
  p <- tapply(f, grid %% 5, sum)
  cat(sprintf("  %-7s implied remainder probs: %s  (max deviation from 0.20: %.5f)\n",
              lab, paste(sprintf("%.4f", p), collapse = " "), max(abs(p - 0.2))))
  p
}
pw <- check_uniform(allfish$weight, "weight")
pl <- check_uniform(allfish$length, "length")
cat("  => deviations are tiny, so 20% is an excellent approximation for both.\n")

# ---------------------------------------------------------------------------
# 2. ROUNDING PROPENSITY AGAINST THE 20% BASELINE
# ---------------------------------------------------------------------------
cat("\n=== 2. rounding propensity (fraction of measurements rounded) ===\n")
prop_rounded <- function(p0) pmax(0, (p0 - 0.2) / 0.8)
obs <- allfish %>% group_by(obs_name) %>%
  summarise(n = n(), p0_wt = mean(wrem == 0), p0_len = mean(lrem == 0), .groups = "drop") %>%
  filter(n >= 100) %>%
  mutate(pr_wt = prop_rounded(p0_wt), pr_len = prop_rounded(p0_len))
cat(sprintf("  weight: pooled p0 = %.4f => %.1f%% of measurements rounded\n",
            mean(allfish$wrem == 0), 100 * prop_rounded(mean(allfish$wrem == 0))))
cat(sprintf("  length: pooled p0 = %.4f => %.1f%% of measurements rounded\n",
            mean(allfish$lrem == 0), 100 * prop_rounded(mean(allfish$lrem == 0))))
cat("\n  per-observer rounding fraction:\n")
print(summary(obs$pr_wt)); cat("  (weight)\n")
print(summary(obs$pr_len)); cat("  (length)\n")
write_csv(obs, file.path(OUT, "TableR29_observer_rounding_uniform_baseline.csv"))

# ---------------------------------------------------------------------------
# 3. DIRECTION AGAINST THE 20% BASELINE
# ---------------------------------------------------------------------------
# a_r = fraction of measurements with true remainder r that were rounded away.
# Under a uniform baseline each class holds 0.20 of the mass, so
#     a_r = (0.20 - observed_r) / 0.20
# displacement: rem 1 -> -1, rem 2 -> -2, rem 3 -> +2, rem 4 -> +1
cat("\n=== 3. rounding direction against the 20% baseline ===\n")
direction <- function(rem, unit, lab) {
  o <- as.numeric(table(factor(rem, levels = 0:4))); o <- o / sum(o)
  a <- (0.2 - o[2:5]) / 0.2
  net <- 0.2 * (a[1]*(-1) + a[2]*(-2) + a[3]*(+2) + a[4]*(+1))
  cat(sprintf("\n  --- %s ---\n", lab))
  cat(sprintf("  observed remainders: %s\n", paste(sprintf("%.4f", o), collapse = " ")))
  cat(sprintf("  rem 1 -> DOWN 1: a1 = %+.4f\n  rem 2 -> DOWN 2: a2 = %+.4f\n", a[1], a[2]))
  cat(sprintf("  rem 3 -> UP   2: a3 = %+.4f\n  rem 4 -> UP   1: a4 = %+.4f\n", a[3], a[4]))
  # bootstrap over observers (cluster bootstrap, so CIs respect clustering)
  ids <- unique(allfish$obs_name)
  bs <- replicate(2000, {
    s <- sample(ids, length(ids), replace = TRUE)
    r <- unlist(lapply(s, function(i) rem[allfish$obs_name == i]))
    oo <- as.numeric(table(factor(r, levels = 0:4))); oo <- oo / sum(oo)
    aa <- (0.2 - oo[2:5]) / 0.2
    0.2 * (aa[1]*(-1) + aa[2]*(-2) + aa[3]*(+2) + aa[4]*(+1))
  })
  ci <- quantile(bs, c(.025, .975))
  cat(sprintf("  NET DISPLACEMENT = %+.4f %s  (95%% CI [%+.4f, %+.4f])\n",
              net, unit, ci[1], ci[2]))
  cat(sprintf("  symmetry test (a1 vs a4, a2 vs a3): |a1-a4| = %.4f, |a2-a3| = %.4f\n",
              abs(a[1]-a[4]), abs(a[2]-a[3])))
  c(net = net, lo = ci[1], hi = ci[2])
}
nw <- direction(allfish$wrem, "kg", "WEIGHT")
nl <- direction(allfish$lrem, "cm", "LENGTH")
cat(sprintf("\n  as %% of the mean: weight %+.3f%% (mean %.1f kg), length %+.3f%% (mean %.1f cm)\n",
            100*nw["net"]/mean(allfish$weight), mean(allfish$weight),
            100*nl["net"]/mean(allfish$length), mean(allfish$length)))

# ---------------------------------------------------------------------------
# 4. HOW MUCH DOES THE BASELINE CHOICE MATTER?
# ---------------------------------------------------------------------------
cat("\n=== 4. net displacement under different baseline choices ===\n")
# The two empirical baselines are from analyses not included in this repository.
cmp <- tribble(
  ~baseline,                              ~weight_kg, ~length_cm,
  "uniform 20% (adopted)",                nw["net"],  nl["net"],
  "empirical: low-rounding observers",    -0.0592,    -0.0453,
  "empirical, split-sample corrected",   -0.0098, -0.0460)
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
cat("\n  NOTE: for WEIGHT the sign depends on the baseline. The empirical baseline\n")
cat("  has remainder 4 depleted (15.8%) among low rounders, which masks the deficit\n")
cat("  at 4 that a uniform baseline exposes. Magnitudes are under 0.1% of mean\n")
cat("  weight either way, so this is a sign flip on a negligible quantity.\n")
cat("  Residual-based tests, which need no baseline, found no consistent\n")
cat("  direction in either variable -- that conclusion is unaffected.\n")

saveRDS(list(pw = pw, pl = pl, obs = obs, nw = nw, nl = nl),
        file.path(OUT, "uniform_baseline.rds"))
message("15_uniform_baseline.R complete")
