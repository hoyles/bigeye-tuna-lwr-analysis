# ==============================================================================
# 34_regional.R -- Region-by-region power laws against the continuous spatial model
#
# Section 4.3 (correlation between regional fits and the spatial model).
#
# ENGINE AND TIMING: 15-20 bam fits; 1-3 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260826)

fin <- readRDS(file.path(OUT, "final_model.rds"))
dat <- readRDS(file.path(OUT, "final_dataset.rds"))
m_pool <- fin$m; d <- dat$d
LENS <- c(90, 100, 120, 140, 160)

cov_tab <- d %>% group_by(region) %>%
  summarise(fish = dplyr::n(), trips = n_distinct(tripf), obs = n_distinct(obs_name),
            months = n_distinct(mon), .groups = "drop") %>% arrange(desc(fish))
cat("=== coverage by region (final dataset) ===\n")
print(as.data.frame(cov_tab), row.names = FALSE)
REGS <- cov_tab$region[cov_tab$fish >= 800 & cov_tab$trips >= 20]
cat(sprintf("\nregions carried forward (>=800 fish, >=20 trips): %s\n\n",
            paste(REGS, collapse = ", ")))

# trip random SLOPE; loglen_c must exist on the fitting data.
d$loglen_c <- d$loglen - mean(d$loglen)

F_EQ1 <- logwt ~ loglen + s(obs_name, bs = "re") + s(tripf, bs = "re") +
  s(loglen_c, tripf, bs = "re") + sexf
F_REG <- logwt ~ s(loglen, k = 15) + s(obs_name, bs = "re") + s(tripf, bs = "re") +
  s(loglen_c, tripf, bs = "re") + sexf + te(lon, lat, k = c(5, 5), bs = c("cr", "cr"))

fitb <- function(f, dd) bam(f, data = dd, family = scat, method = "fREML",
                            discrete = TRUE, gamma = 1.4)
predof <- function(mm, dd) {
  nd <- data.frame(loglen = log(LENS), loglen_c = log(LENS) - mean(d$loglen),
                   lat = median(dd$lat), lon = median(dd$lon),
                   mon = 6, sexf = factor("F", levels = levels(dd$sexf)),
                   yrf = factor(levels(dd$yrf)[min(5, nlevels(dd$yrf))], levels = levels(dd$yrf)),
                   obs_name = factor(levels(dd$obs_name)[1], levels = levels(dd$obs_name)),
                   tripf = factor(levels(dd$tripf)[1], levels = levels(dd$tripf)))
  ex <- intersect(c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)"), sapply(mm$smooth, function(z) z$label))
  exp(as.numeric(predict(mm, nd, exclude = ex)))
}

# pooled eq.1 reference
m_eq1 <- fitb(F_EQ1, d)
b_pool <- unname(coef(m_eq1)["loglen"])
p_pool_eq1 <- predof(m_eq1, d)
cat(sprintf("pooled eq.1: b = %.4f (SE %.4f)\n\n", b_pool, summary(m_eq1)$se["loglen"]))

rows <- list(); preds <- list()
for (r in REGS) {
  dr <- droplevels(d[d$region == r, ])
  m1 <- try(fitb(F_EQ1, dr), silent = TRUE); if (inherits(m1, "try-error")) next
  m2 <- try(fitb(F_REG, dr), silent = TRUE)
  b <- unname(coef(m1)["loglen"]); se <- unname(summary(m1)$se["loglen"])
  p1 <- predof(m1, dr)
  p2 <- if (inherits(m2, "try-error")) rep(NA_real_, length(LENS)) else predof(m2, dr)
  rows[[as.character(r)]] <- tibble(region = r, fish = nrow(dr),
    trips = n_distinct(dr$tripf), b = b, se = se, lo = b - 1.96*se, hi = b + 1.96*se,
    z_vs_pool = (b - b_pool)/se,
    dAIC_gam_eq1 = if (inherits(m2, "try-error")) NA else AIC(m2) - AIC(m1))
  preds[[as.character(r)]] <- tibble(region = r, length_cm = LENS,
                                     eq1 = p1, gam = p2, pooled = p_pool_eq1)
  cat(sprintf("  R%-3s n=%5d trips=%3d  b = %.4f (%.4f-%.4f)  z vs pooled = %+5.2f\n",
              r, nrow(dr), n_distinct(dr$tripf), b, b - 1.96*se, b + 1.96*se,
              (b - b_pool)/se))
}
tab <- bind_rows(rows); pr <- bind_rows(preds)

cat("\n=== regional eq.1 slopes vs the pooled slope ===\n")
print(as.data.frame(tab %>% mutate(p = 2*pnorm(-abs(z_vs_pool))) %>%
  select(region, fish, trips, b, se, z_vs_pool, p, dAIC_gam_eq1) %>%
  mutate(across(c(b, se, z_vs_pool), ~round(.x, 4)), p = format.pval(p, digits = 2),
         dAIC_gam_eq1 = round(dAIC_gam_eq1, 1))), row.names = FALSE)
cat(sprintf("\n  spread of regional slopes: %.4f to %.4f (pooled %.4f)\n",
            min(tab$b), max(tab$b), b_pool))
cat(sprintf("  regions differing from pooled at p < 0.05: %d of %d\n",
            sum(2*pnorm(-abs(tab$z_vs_pool)) < 0.05), nrow(tab)))

diffs <- pr %>% mutate(pct = 100*(eq1 - pooled)/pooled)
cat("\n=== % difference of the REGIONAL eq.1 from the POOLED eq.1 ===\n")
print(as.data.frame(diffs %>% select(region, length_cm, pct) %>%
  pivot_wider(names_from = region, values_from = pct, names_prefix = "R") %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)
cat(sprintf("\n  max |regional - pooled| across regions and lengths: %.1f%%\n",
            max(abs(diffs$pct), na.rm = TRUE)))
cat(sprintf("  spread across regions at 120 cm: %.1f%%\n",
            diff(range(diffs$pct[diffs$length_cm == 120], na.rm = TRUE))))
cat("  compare: the continuous spatial model gives 3.6% between regions,\n")
cat("  and the split-half consistent spatial SD is 0.77%\n")

# does the pooled spatial model agree with the independent regional fits?
cat("\n=== corroboration: pooled spatial model vs independent regional eq.1 ===\n")
sp <- lapply(REGS, function(r) {
  dr <- d[d$region == r, ]
  nd <- expand.grid(length_cm = LENS, mon = 1:12)
  nd$loglen <- log(nd$length_cm); nd$loglen_c <- nd$loglen - mean(d$loglen)
  nd$lat <- median(dr$lat); nd$lon <- median(dr$lon)
  nd$sexf <- factor("F", levels = levels(d$sexf))
  nd$yrf <- factor(levels(d$yrf)[5], levels = levels(d$yrf))
  nd$obs_name <- factor(levels(d$obs_name)[1], levels = levels(d$obs_name))
  nd$tripf <- factor(levels(d$tripf)[1], levels = levels(d$tripf))
  nd$fit <- as.numeric(predict(m_pool, nd, exclude = c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")))
  nd %>% group_by(length_cm) %>% summarise(spatial = exp(mean(fit)), .groups = "drop") %>%
    mutate(region = r)
}) %>% bind_rows()
# Both series must be expressed as deviations across REGIONS AT EACH LENGTH.
# Centring pct_spatial on mean(spatial) over the whole table mixes in the length
# effect, which is two orders of magnitude larger than any regional difference
# and swamps the comparison. Group by length before centring.
cmp <- pr %>% select(region, length_cm, eq1, pooled) %>%
  inner_join(sp, by = c("region", "length_cm")) %>%
  group_by(length_cm) %>%
  mutate(pct_eq1     = 100*(eq1     - mean(eq1))    /mean(eq1),
         pct_spatial = 100*(spatial - mean(spatial))/mean(spatial),
         pct_eq1_vs_pooled = 100*(eq1 - pooled)/pooled) %>%
  ungroup()
rr  <- cor(cmp$pct_eq1, cmp$pct_spatial, use = "complete.obs")
rsp <- cor(cmp$pct_eq1, cmp$pct_spatial, method = "spearman", use = "complete.obs")
cat("  correlation of regional deviations (independent eq.1 vs pooled spatial model):\n")
cat(sprintf("    Pearson r = %+.3f, Spearman = %+.3f  (%d region x length pairs)\n",
            rr, rsp, nrow(cmp)))
# The pairs are not independent: the same regions repeat across lengths. Average
# over length and correlate at region level, which is the honest n.
rm_ <- cmp %>% group_by(region) %>%
  summarise(eq1 = mean(pct_eq1), spatial = mean(pct_spatial), .groups = "drop")
rct <- cor.test(rm_$eq1, rm_$spatial)
cat(sprintf("    region level (length-averaged): r = %+.3f, p = %s, n = %d regions\n",
            rct$estimate, format.pval(rct$p.value, digits = 2), nrow(rm_)))
r_by_len <- cmp %>% group_by(length_cm) %>%
  summarise(r = cor(pct_eq1, pct_spatial),
            range_eq1 = diff(range(pct_eq1)),
            range_spatial = diff(range(pct_spatial)), .groups = "drop")
cat("\n  by length. Note range_spatial < range_eq1: the penalised spatial smooth\n")
cat("  shrinks regional differences relative to independent per-region fits, so\n")
cat("  the two agree in pattern while the pooled model is the more conservative.\n")
print(as.data.frame(r_by_len %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)
cat("\n  regional deviations at 120 cm:\n")
print(as.data.frame(cmp %>% filter(length_cm == 120) %>%
  select(region, pct_eq1, pct_spatial, pct_eq1_vs_pooled) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)

write_csv(tab,  file.path(OUT, "TableR54_regional_slopes.csv"))
write_csv(diffs, file.path(OUT, "TableR55_regional_vs_pooled.csv"))
saveRDS(list(tab = tab, pr = pr, cmp = cmp, r = rr, r_spearman = rsp,
             r_by_length = r_by_len), file.path(OUT, "regional_final.rds"))
message("34_regional.R complete")
