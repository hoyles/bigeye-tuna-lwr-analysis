# ==============================================================================
# 10_rounding_consequences.R -- Effect of rounding on b, by simulation
#
# Section 4.6 and Table 13. Rounding is imposed at chosen rates and intervals on the fish of observers who
# rarely rounded, and the power law refitted. Both sexes.
#
# ENGINE AND TIMING: simple refits; under a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

NREP <- 25

allfish <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(wrem = weight %% 5, lrem = length %% 5)

obsr <- allfish %>% group_by(obs_name) %>%
  summarise(n = n(), p0_wt = mean(wrem == 0), p0_len = mean(lrem == 0), .groups = "drop") %>%
  filter(n >= 100)
low  <- obsr$obs_name[obsr$p0_wt <= 0.24]
high <- obsr$obs_name[obsr$p0_wt >= 0.40]

base <- allfish %>% filter(obs_name %in% low) %>%
  select(obs_name, setid, length, weight) %>% as.data.frame()
cat("clean baseline: ", nrow(base), " fish from ", length(low),
    " low-rounding observers\n", sep = "")
cat("baseline p(weight mult of 5) = ", round(mean(base$weight %% 5 == 0), 4),
    " | p(length mult of 5) = ", round(mean(base$length %% 5 == 0), 4), "\n\n", sep = "")

# fish are recorded as whole kg / whole cm, so interval 1 is the existing
# resolution and acts as a null condition
round_to <- function(x, R, p) {
  if (R <= 1 || p <= 0) return(x)
  i <- runif(length(x)) < p
  x[i] <- round(x[i] / R) * R
  x
}

fit_b <- function(len, wt) {
  ok <- len > 0 & wt > 0
  cf <- coef(lm(log(wt[ok]) ~ log(len[ok])))
  c(a = unname(cf[1]), b = unname(cf[2]))
}

b0 <- fit_b(base$length, base$weight)
cat(sprintf("baseline LWR: b = %.5f, log(a) = %.5f\n\n", b0["b"], b0["a"]))
LENS <- c(80, 120, 160)
pred0 <- exp(b0["a"] + b0["b"] * log(LENS))

# ---------------------------------------------------------------------------
# 1. SIMULATION GRID
# ---------------------------------------------------------------------------
grid <- expand.grid(target = c("weight", "length", "both"),
                    R = c(1, 2, 5, 10), p = c(0.25, 0.50, 1.00),
                    stringsAsFactors = FALSE)
cat("=== 1. paired rounding simulation (", NREP, " replicates per cell) ===\n", sep = "")
sim <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  rr <- t(replicate(NREP, {
    len <- base$length; wt <- base$weight
    if (g$target %in% c("length", "both")) len <- round_to(len, g$R, g$p)
    if (g$target %in% c("weight", "both")) wt  <- round_to(wt,  g$R, g$p)
    est <- fit_b(len, wt)
    pr  <- exp(est["a"] + est["b"] * log(LENS))
    c(b = est["b"], a = est["a"], pr)
  }))
  colnames(rr) <- c("b", "a", paste0("wt", LENS))
  sim[[i]] <- tibble(target = g$target, R = g$R, p = g$p,
                     b = mean(rr[, "b"]), b_sd = sd(rr[, "b"]),
                     b_pct = 100 * (mean(rr[, "b"]) - b0["b"]) / b0["b"],
                     wt80_pct  = 100 * (mean(rr[, "wt80"])  - pred0[1]) / pred0[1],
                     wt120_pct = 100 * (mean(rr[, "wt120"]) - pred0[2]) / pred0[2],
                     wt160_pct = 100 * (mean(rr[, "wt160"]) - pred0[3]) / pred0[3])
}
S <- bind_rows(sim)

for (tg in c("weight", "length", "both")) {
  cat("\n--- rounding", toupper(tg), "---\n")
  print(as.data.frame(S %>% filter(target == tg) %>%
    select(R, p, b, b_pct, wt80_pct, wt120_pct, wt160_pct) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
}

# ---------------------------------------------------------------------------
# 2. CLASSICAL ATTENUATION PREDICTION (length rounding only)
# ---------------------------------------------------------------------------
cat("\n=== 2. simulated vs theoretical attenuation of b (length rounding) ===\n")
sd_logx <- sd(log(base$length)); Lbar <- mean(base$length)
theo <- S %>% filter(target == "length") %>%
  mutate(sd_u = sqrt(p) * (R / sqrt(12)) / Lbar,
         lambda = sd_logx^2 / (sd_logx^2 + sd_u^2),
         b_pct_theory = 100 * (lambda - 1))
print(as.data.frame(theo %>% select(R, p, b_pct_sim = b_pct, b_pct_theory) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

# where do the real data sit?
excess <- mean(allfish$wrem == 0) - 0.20
cat(sprintf("\nobserved weight rounding: p0 = %.3f => ~%.1f%% of measurements rounded to 5\n",
            mean(allfish$wrem == 0), 100 * excess / 0.80))
excess_l <- mean(allfish$lrem == 0) - 0.20
cat(sprintf("observed length rounding: p0 = %.3f => ~%.1f%% of measurements rounded to 5\n",
            mean(allfish$lrem == 0), 100 * excess_l / 0.80))
p_l <- excess_l / 0.80
sd_u <- sqrt(p_l) * (5 / sqrt(12)) / Lbar
cat(sprintf("=> predicted attenuation of b from OBSERVED length rounding: %.3f%%\n",
            100 * (sd_logx^2 / (sd_logx^2 + sd_u^2) - 1)))

# ---------------------------------------------------------------------------
# 3. EMPIRICAL: low- vs high-rounding observers
# ---------------------------------------------------------------------------
cat("\n=== 3. LWR fitted to low- vs high-rounding observers (real data) ===\n")
emp <- lapply(list(low = low, high = high), function(o) {
  dd <- allfish %>% filter(obs_name %in% o)
  m <- gam(logwt ~ loglen + s(obs_name, bs = "re"), data = droplevels(dd),
           family = scat, method = "ML", gamma = 1.4)
  tibble(n = nrow(dd), nobs = length(o), b = coef(m)["loglen"],
         se = summary(m)$se["loglen"],
         resid_sd = sd(residuals(m, type = "response")))
}) %>% bind_rows(.id = "group")
print(as.data.frame(emp %>% mutate(across(where(is.numeric), ~round(.x, 5)))), row.names = FALSE)
dz <- (emp$b[1] - emp$b[2]) / sqrt(emp$se[1]^2 + emp$se[2]^2)
cat(sprintf("difference in b: %+.4f, z = %.2f, p = %s\n",
            emp$b[1] - emp$b[2], dz, format.pval(2 * pnorm(-abs(dz)), digits = 3)))
cat(sprintf("residual SD ratio (high/low) = %.3f  <- rounding inflates VARIANCE\n",
            emp$resid_sd[2] / emp$resid_sd[1]))

# ---------------------------------------------------------------------------
# 4. DOES AN OBSERVER ROUND UNIFORMLY ACROSS SIZES?
# ---------------------------------------------------------------------------
cat("\n=== 4. per-observer rounding by length class ===\n")
lc <- allfish %>%
  mutate(lclass = cut(length, c(70, 100, 120, 140, 200),
                      labels = c("70-100", "100-120", "120-140", "140-200"))) %>%
  group_by(obs_name, lclass) %>%
  summarise(n = n(), p0 = mean(wrem == 0), .groups = "drop") %>%
  filter(n >= 25)
wide <- lc %>% select(obs_name, lclass, p0) %>%
  pivot_wider(names_from = lclass, values_from = p0) %>%
  filter(complete.cases(.))
cat("observers with >=25 fish in all four length classes:", nrow(wide), "\n")
cat("\nwithin-observer spread of p0 across length classes:\n")
rng <- apply(as.matrix(wide[, -1]), 1, function(z) max(z) - min(z))
print(summary(rng))
cat(sprintf("\nobservers whose p0 varies by >0.20 across length classes: %d of %d (%.0f%%)\n",
            sum(rng > 0.20), length(rng), 100 * mean(rng > 0.20)))
cat("\n10 observers with the most size-dependent rounding:\n")
print(as.data.frame(wide %>% mutate(range = rng) %>% arrange(desc(range)) %>% head(10) %>%
        mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
# is rounding consistent within observer? correlation across classes
cat("\ncorrelation of p0 between length classes (across observers):\n")
print(round(cor(as.matrix(wide[, -1])), 3))
cat("\ncorrelation between weight- and length-rounding propensity across observers:\n")
ctwl <- cor.test(obsr$p0_wt, obsr$p0_len, method = "spearman", exact = FALSE)
cat(sprintf("  rho = %+.3f, p = %s\n", ctwl$estimate, format.pval(ctwl$p.value, digits = 3)))

write_csv(S,    file.path(OUT, "TableR23_rounding_simulation.csv"))
write_csv(wide, file.path(OUT, "TableR24_observer_rounding_by_lengthclass.csv"))
saveRDS(list(S = S, emp = emp, wide = wide, theo = theo),
        file.path(OUT, "rounding_consequences.rds"))
message("10_rounding_consequences.R complete")
