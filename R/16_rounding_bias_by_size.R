# ==============================================================================
# 16_rounding_bias_by_size.R -- Rounding bias by length class and the implied change in b
#
# Section 4.6. Both sexes, pre-exclusion pool.
#
# ENGINE AND TIMING: no model fitting; under a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

allfish <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(wrem = weight %% 5) %>% as.data.frame()

BRK <- c(70, 100, 112, 122, 132, 145, 200)
allfish$lclass <- cut(allfish$length, BRK, include.lowest = TRUE)
cat("fish:", nrow(allfish), "| length classes:", nlevels(allfish$lclass), "\n\n")

# --- class-specific baseline from a smooth density ---------------------------
smooth_baseline <- function(w) {
  if (length(w) < 200) return(rep(0.2, 5))
  d <- density(w, bw = max(4 * 5, bw.nrd0(w)), from = min(w) - 20, to = max(w) + 20, n = 4096)
  g <- seq(floor(min(w)) - 10, ceiling(max(w)) + 10, by = 1)
  f <- approx(d$x, d$y, xout = g)$y; f[is.na(f)] <- 0; f <- f / sum(f)
  as.numeric(tapply(f, g %% 5, sum))
}

cat("=== class-specific baselines (smooth density) vs uniform 0.20 ===\n")
for (lc in levels(allfish$lclass)) {
  w <- allfish$weight[allfish$lclass == lc]
  b <- smooth_baseline(w)
  cat(sprintf("  %-10s n=%5d  baseline: %s  (max dev %.4f)\n", lc, length(w),
              paste(sprintf("%.4f", b), collapse = " "), max(abs(b - 0.2))))
}

# --- a1..a4 and net displacement by length class -----------------------------
calc <- function(w, rem, base) {
  o <- as.numeric(table(factor(rem, levels = 0:4))); o <- o / sum(o)
  a <- (base[2:5] - o[2:5]) / base[2:5]
  net <- sum(base[2:5] * a * c(-1, -2, 2, 1))
  c(a1 = a[1], a2 = a[2], a3 = a[3], a4 = a[4], net = net,
    p0 = o[1], asym = a[4] - a[1])
}

cat("\n=== weight rounding by length class ===\n")
rows <- list()
for (lc in levels(allfish$lclass)) {
  s <- allfish[allfish$lclass == lc, ]
  base <- smooth_baseline(s$weight)
  v <- calc(s$weight, s$wrem, base)
  # cluster bootstrap over observers for the net displacement
  ids <- unique(s$obs_name)
  bs <- replicate(1000, {
    ss <- s[s$obs_name %in% sample(ids, length(ids), replace = TRUE), ]
    calc(ss$weight, ss$wrem, base)["net"]
  })
  rows[[lc]] <- tibble(lclass = lc, n = nrow(s), mean_len = mean(s$length),
    mean_wt = mean(s$weight), p0 = v["p0"],
    a1_down = v["a1"], a4_up = v["a4"], asym = v["asym"],
    net_kg = v["net"], lo = quantile(bs, .025), hi = quantile(bs, .975),
    net_pct = 100 * v["net"] / mean(s$weight))
}
R <- bind_rows(rows)
print(as.data.frame(R %>% select(lclass, n, mean_len, mean_wt, p0, a1_down, a4_up,
                                 asym, net_kg, net_pct) %>%
        mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

cat("\n--- net displacement with 95% CI, by class ---\n")
print(as.data.frame(R %>% select(lclass, mean_len, net_kg, lo, hi, net_pct) %>%
        mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

# --- does the ASYMMETRY vary with size? --------------------------------------
cat("\n=== does the direction of rounding vary with fish size? ===\n")
ct <- cor.test(R$mean_len, R$asym)
cat(sprintf("  correlation of (a4 - a1) with mean length: r = %+.3f, p = %s\n",
            ct$estimate, format.pval(ct$p.value, digits = 3)))
cta <- cor.test(R$mean_len, R$p0)
cat(sprintf("  correlation of p0 (frequency) with mean length: r = %+.3f, p = %s\n",
            cta$estimate, format.pval(cta$p.value, digits = 3)))
cat("  (a rise in p0 without a rise in asymmetry = more rounding, not more bias)\n")

# --- implied effect on b ------------------------------------------------------
# proportional bias in each class ~ net_kg / mean_wt; on the log scale the
# induced change in b is the slope of that bias against log(length)
cat("\n=== implied effect on the LWR slope b ===\n")
R$logbias <- log1p(R$net_kg / R$mean_wt)
fit <- lm(logbias ~ log(mean_len), data = R, weights = n)
cf <- coef(fit); ci <- confint(fit)
cat(sprintf("  slope of log-bias on log(length) = %+.5f  (95%% CI %+.5f to %+.5f)\n",
            cf[2], ci[2, 1], ci[2, 2]))
cat(sprintf("  => induced change in b = %+.5f (%.3f%% of b = 3.06)\n",
            cf[2], 100 * cf[2] / 3.06))
cat(sprintf("  direction: %s\n",
            if (cf[2] > 0) "UPWARD tilt in b" else "DOWNWARD tilt in b"))
cat(sprintf("  proportional bias: %.3f%% at %.0f cm vs %.3f%% at %.0f cm\n",
            R$net_pct[1], R$mean_len[1], R$net_pct[nrow(R)], R$mean_len[nrow(R)]))

# --- direct simulation check --------------------------------------------------
# Apply the ESTIMATED size-specific rounding to unrounded-ish data and refit.
cat("\n=== direct check: simulate the estimated size-specific rounding ===\n")
obsr <- allfish %>% group_by(obs_name) %>%
  summarise(n = n(), p0 = mean(wrem == 0), .groups = "drop") %>% filter(n >= 100)
clean <- allfish %>% filter(obs_name %in% obsr$obs_name[obsr$p0 <= quantile(obsr$p0, 1/4)])
cat(sprintf("  pseudo-clean subset: %d fish (p0 = %.3f)\n", nrow(clean),
            mean(clean$wrem == 0)))
b_before <- coef(lm(log(weight) ~ log(length), data = clean))[2]
sim_b <- replicate(200, {
  w <- clean$weight
  lc <- cut(clean$length, BRK, include.lowest = TRUE)
  for (i in seq_len(nrow(R))) {
    k <- which(lc == R$lclass[i]); if (!length(k)) next
    r <- w[k] %% 5
    u <- runif(length(k))
    # apply the estimated per-remainder rounding probabilities
    w[k][r == 1 & u < R$a1_down[i]] <- w[k][r == 1 & u < R$a1_down[i]] - 1
    w[k][r == 4 & u < R$a4_up[i]]   <- w[k][r == 4 & u < R$a4_up[i]]   + 1
  }
  coef(lm(log(w) ~ log(clean$length)))[2]
})
cat(sprintf("  b before = %.5f | after simulated rounding = %.5f (SD %.5f)\n",
            b_before, mean(sim_b), sd(sim_b)))
cat(sprintf("  change in b = %+.5f (%.3f%%)  <- compare with the analytic estimate above\n",
            mean(sim_b) - b_before, 100 * (mean(sim_b) - b_before) / b_before))

write_csv(R, file.path(OUT, "TableR30_rounding_bias_by_lengthclass.csv"))
saveRDS(list(R = R, fit = fit, sim_b = sim_b), file.path(OUT, "rounding_bias_by_size.rds"))
message("16_rounding_bias_by_size.R complete")
