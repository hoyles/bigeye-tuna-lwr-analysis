# ==============================================================================
# 17_rounding_bias_by_weight.R -- Rounding bias by weight class and the implied change in b
#
# Sections 3.1 and 4.6. Classes are defined on recorded weight and on weight predicted from length.
# Both sexes, pre-exclusion pool.
#
# ENGINE AND TIMING: no model fitting; about a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

allfish <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(wrem = weight %% 5) %>% as.data.frame()

# bin edges and widths are all multiples of 5
WBRK <- c(0, 15, 25, 35, 45, 60, 400)
allfish$wclass <- cut(allfish$weight, WBRK, include.lowest = TRUE)
cat("fish:", nrow(allfish), "\n")
cat("bin widths (kg):", paste(diff(WBRK)[-length(diff(WBRK))], collapse = ", "),
    "- all multiples of 5\n\n")

smooth_baseline <- function(w) {
  if (length(w) < 200) return(rep(0.2, 5))
  d <- density(w, bw = max(20, bw.nrd0(w)), from = min(w) - 20, to = max(w) + 20, n = 4096)
  g <- seq(floor(min(w)) - 10, ceiling(max(w)) + 10, by = 1)
  f <- approx(d$x, d$y, xout = g)$y; f[is.na(f)] <- 0; f <- f / sum(f)
  as.numeric(tapply(f, g %% 5, sum))
}
calc <- function(rem, base) {
  o <- as.numeric(table(factor(rem, levels = 0:4))); o <- o / sum(o)
  a <- (base[2:5] - o[2:5]) / base[2:5]
  c(a1 = a[1], a2 = a[2], a3 = a[3], a4 = a[4],
    net = sum(base[2:5] * a * c(-1, -2, 2, 1)), p0 = o[1], asym = a[4] - a[1])
}

analyse <- function(cls, wt, rem, obs, lab) {
  cat("===", lab, "===\n")
  rows <- list()
  for (lc in levels(cls)) {
    k <- which(cls == lc); if (length(k) < 300) next
    base <- smooth_baseline(wt[k])
    v <- calc(rem[k], base)
    ids <- unique(obs[k])
    bs <- replicate(1000, {
      kk <- k[obs[k] %in% sample(ids, length(ids), replace = TRUE)]
      calc(rem[kk], base)["net"]
    })
    rows[[lc]] <- tibble(class = lc, n = length(k), mean_wt = mean(wt[k]),
      p0 = v["p0"], a1_down = v["a1"], a4_up = v["a4"], asym = v["asym"],
      net_kg = v["net"], lo = quantile(bs, .025), hi = quantile(bs, .975),
      net_pct = 100 * v["net"] / mean(wt[k]),
      base_dev = max(abs(base - 0.2)))
  }
  R <- bind_rows(rows)
  print(as.data.frame(R %>% select(class, n, mean_wt, p0, a1_down, a4_up, asym,
                                   net_kg, lo, hi, net_pct, base_dev) %>%
          mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
  if (nrow(R) >= 4) {
    ct <- cor.test(log(R$mean_wt), R$asym)
    cat(sprintf("\n  asymmetry (a4-a1) vs log(weight): r = %+.3f, p = %s\n",
                ct$estimate, format.pval(ct$p.value, digits = 3)))
    ct2 <- cor.test(log(R$mean_wt), R$p0)
    cat(sprintf("  p0 (frequency)      vs log(weight): r = %+.3f, p = %s\n",
                ct2$estimate, format.pval(ct2$p.value, digits = 3)))
    # implied tilt in b: bias is on log(weight); b relates log(wt) to log(len),
    # and log(wt) ~ b*log(len), so d(bias)/d(log wt) maps to db/b
    R$logbias <- log1p(R$net_kg / R$mean_wt)
    fit <- lm(logbias ~ log(mean_wt), data = R, weights = n)
    s <- coef(fit)[2]; ci <- confint(fit)[2, ]
    cat(sprintf("  slope of log-bias on log(weight) = %+.5f (95%% CI %+.5f to %+.5f)\n",
                s, ci[1], ci[2]))
    cat(sprintf("  => implied change in b = %+.5f (%.2f%% of b = 3.06); direction: %s\n",
                s * 3.06, 100 * s, if (s > 0) "UPWARD" else "DOWNWARD"))
  }
  cat("\n")
  R
}

Rw <- analyse(allfish$wclass, allfish$weight, allfish$wrem, allfish$obs_name,
              "A. bins on RECORDED weight (edges/widths multiples of 5)")

# --- cross-check: bins on PREDICTED weight from length ------------------------
lm0 <- lm(log(weight) ~ log(length), data = allfish)
allfish$wpred <- exp(predict(lm0))
allfish$pclass <- cut(allfish$wpred, WBRK, include.lowest = TRUE)
Rp <- analyse(allfish$pclass, allfish$wpred, allfish$wrem, allfish$obs_name,
              "B. bins on PREDICTED weight from length (immune to weight rounding)")

# --- how much did the grid-alignment fix matter? ------------------------------
cat("=== C. effect of bin alignment (why unaligned bins look non-monotonic) ===\n")
allfish$qclass <- cut(allfish$weight, quantile(allfish$weight, 0:4/4), include.lowest = TRUE)
cat("  quartile bins (edges NOT multiples of 5):\n")
print(allfish %>% group_by(qclass) %>%
        summarise(n = n(), p0 = round(mean(wrem == 0), 4), .groups = "drop") %>%
        as.data.frame(), row.names = FALSE)
cat("  aligned bins (edges multiples of 5):\n")
print(allfish %>% group_by(wclass) %>%
        summarise(n = n(), p0 = round(mean(wrem == 0), 4), .groups = "drop") %>%
        as.data.frame(), row.names = FALSE)

write_csv(Rw, file.path(OUT, "TableR31_rounding_by_weightclass.csv"))
write_csv(Rp, file.path(OUT, "TableR32_rounding_by_predicted_weightclass.csv"))
saveRDS(list(Rw = Rw, Rp = Rp), file.path(OUT, "rounding_bias_by_weight.rds"))
message("17_rounding_bias_by_weight.R complete")
