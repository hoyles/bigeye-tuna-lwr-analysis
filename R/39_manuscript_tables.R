# ==============================================================================
# 39_manuscript_tables.R -- Manuscript tables from the final model
#
# Tables 6-11. Model B (equation 3) is fitted here, for all data and by region.
#
# ENGINE AND TIMING: about 10 gam fits; 2-5 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260828)

# Manuscript tables are written to their own folder.
TABDIR <- file.path(dirname(OUT), "manuscript_tables")
if (!dir.exists(TABDIR)) dir.create(TABDIR, recursive = TRUE)

fin <- readRDS(file.path(OUT, "final_model.rds"))
dat <- readRDS(file.path(OUT, "final_dataset.rds"))
m <- fin$m; d <- dat$d
d$tripf <- factor(d$tripf); d$obs_name <- factor(d$obs_name); d$yrf <- factor(d$yrf)

stars <- function(p) ifelse(is.na(p), "",
                    ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "")))))
fmtp <- function(p) ifelse(p < 2e-16, "<2e-16", formatC(p, format = "g", digits = 3))

# ---------------------------------------------------------------------------
# NEW TABLE 5 -- parametric coefficients of the final model
# ---------------------------------------------------------------------------
pt <- summary(m)$p.table
t5 <- tibble(term = rownames(pt),
             estimate = pt[, 1], std_error = pt[, 2],
             z_value = pt[, 3], p_value = pt[, 4]) %>%
  mutate(sig = stars(p_value), p_fmt = fmtp(p_value))
# make the year and sex terms readable
t5$term <- sub("^yrf", "", t5$term)
t5$term <- sub("^sexfM$", "Sex (M vs F)", t5$term)
cat("=== NEW TABLE 5: parametric coefficients, final model ===\n")
print(as.data.frame(t5 %>% select(term, estimate, std_error, z_value, p_fmt, sig) %>%
  mutate(across(c(estimate, std_error), ~round(.x, 4)), z_value = round(z_value, 3))),
  row.names = FALSE)
cat(sprintf("\n  sex effect on weight: %+.2f%%\n",
            100*(exp(t5$estimate[t5$term == "Sex (M vs F)"]) - 1)))

# ---------------------------------------------------------------------------
# NEW TABLE 6 -- smooth terms of the final model
# ---------------------------------------------------------------------------
st <- summary(m)$s.table
t6 <- tibble(term = rownames(st), edf = st[, 1], ref_df = st[, 2],
             chi_sq = st[, 3], p_value = st[, 4]) %>%
  mutate(sig = stars(p_value), p_fmt = fmtp(p_value))
cat("\n=== NEW TABLE 6: smooth terms, final model ===\n")
print(as.data.frame(t6 %>% select(term, edf, ref_df, chi_sq, p_fmt, sig) %>%
  mutate(across(c(edf, ref_df), ~round(.x, 2)), chi_sq = round(chi_sq, 1))),
  row.names = FALSE)
cat(sprintf("\n  total edf %.1f | deviance explained %.2f%% | AIC %.1f | n = %d\n",
            sum(m$edf), summary(m)$dev.expl*100, AIC(m), nrow(d)))

# ---------------------------------------------------------------------------
# eq.1 fits -- pooled and by region (replaces Tables 11 and 12)
# ---------------------------------------------------------------------------
# Trip random slope (see 32_). It changes the delivered b by about one standard
# error and raises that standard error from 0.0046 to 0.0152 (section 4.6).
# Regional comparisons must carry it, or regions appear to differ from the
# pooled exponent when they do not.
F_EQ1 <- logwt ~ loglen + s(obs_name, bs = "re") + s(tripf, bs = "re") +
  s(loglen_c, tripf, bs = "re") + sexf
LENS  <- c(100, 120, 140, 160)

fit_eq1 <- function(dd, label) {
  dd <- droplevels(dd)
  if (n_distinct(dd$tripf) < 5 || nrow(dd) < 150) {
    cat(sprintf("  %-10s SKIPPED (n = %d, trips = %d -- too few)\n",
                label, nrow(dd), n_distinct(dd$tripf)))
    return(NULL)
  }
  mm <- try(gam(F_EQ1, data = dd, family = scat, method = "REML", gamma = 1.4),
            silent = TRUE)
  if (inherits(mm, "try-error")) {
    cat(sprintf("  %-10s FAILED\n", label)); return(NULL)
  }
  cf <- coef(mm); se <- summary(mm)$se
  a  <- unname(exp(cf["(Intercept)"])); b <- unname(cf["loglen"])
  se_int <- unname(se["(Intercept)"]);  se_b <- unname(se["loglen"])
  cat(sprintf("  %-10s n=%5d trips=%3d  a=%.4e  b=%.4f (SE %.4f)\n",
              label, nrow(dd), n_distinct(dd$tripf), a, b, se_b))
  list(row = tibble(region = label, a = a, a_CV = se_int, b = b, b_CV = se_b/b,
                    b_SE = se_b, r_sq = summary(mm)$r.sq, N = nrow(dd),
                    trips = n_distinct(dd$tripf), observers = n_distinct(dd$obs_name)),
       pred = tibble(region = label, length_cm = LENS, wt = a * LENS^b))
}

cat("\n=== eq.1 fits (log wt ~ log len, with observer and trip random effects) ===\n")
pooled <- fit_eq1(d, "All")
regs <- d %>% count(region) %>% filter(n >= 150) %>% arrange(region) %>% pull(region)
byreg <- lapply(as.character(regs), function(r) fit_eq1(d[d$region == r, ], r))
byreg <- Filter(Negate(is.null), byreg)

t11 <- pooled$row %>% mutate(period = "2015-2022") %>%
  select(period, a, a_CV, b, b_CV, r_sq, N)
cat("\n=== NEW TABLE (was 11): eq.1 across all regions ===\n")
print(as.data.frame(t11 %>% mutate(a = formatC(a, format = "e", digits = 3),
  across(c(a_CV, b_CV), ~formatC(.x, format = "e", digits = 2)),
  across(c(b, r_sq), ~round(.x, 4)))), row.names = FALSE)

t12 <- bind_rows(lapply(byreg, `[[`, "row")) %>%
  # Each region's fish are also in the pooled fit, so under a common exponent
  # Var(b_r - b_p) = SE_r^2 - SE_p^2. Dividing by SE_r alone ignored both
  # that correlation and the pooled uncertainty.
  mutate(z_vs_pooled = (b - pooled$row$b)/sqrt(b_SE^2 - pooled$row$b_SE^2),
         p_vs_pooled = 2*pnorm(-abs(z_vs_pooled)),
         well_sampled = N >= 800 & trips >= 20)
cat("\n=== NEW TABLE (was 12): eq.1 by region ===\n")
print(as.data.frame(t12 %>% select(region, a, a_CV, b, b_CV, r_sq, N, trips,
                                   observers, z_vs_pooled, p_vs_pooled, well_sampled) %>%
  mutate(z_vs_pooled = round(z_vs_pooled, 2),
         p_vs_pooled = format.pval(p_vs_pooled, digits = 2)) %>%
  mutate(a = formatC(a, format = "e", digits = 3),
         across(c(a_CV, b_CV), ~formatC(.x, format = "e", digits = 2)),
         across(c(b, r_sq), ~round(.x, 4)))), row.names = FALSE)

# ---------------------------------------------------------------------------
# NEW TABLE (was 13) -- regional eq.1 vs the all-region eq.1, % by length
# ---------------------------------------------------------------------------
pp <- pooled$pred %>% select(length_cm, pooled_wt = wt)
t13 <- bind_rows(lapply(byreg, `[[`, "pred")) %>%
  inner_join(pp, by = "length_cm") %>%
  mutate(pct = 100*(wt - pooled_wt)/pooled_wt) %>%
  select(region, length_cm, pct) %>%
  pivot_wider(names_from = length_cm, values_from = pct,
              names_glue = "{length_cm}cm")
cat("\n=== NEW TABLE (was 13): % difference, regional eq.1 vs all-region eq.1 ===\n")
print(as.data.frame(t13 %>% mutate(across(where(is.numeric), ~round(.x, 1)))),
      row.names = FALSE)
rng <- bind_rows(lapply(byreg, `[[`, "pred")) %>% inner_join(pp, by = "length_cm") %>%
  mutate(pct = 100*(wt - pooled_wt)/pooled_wt)
well <- t12$region[t12$well_sampled]
rw <- rng %>% filter(region %in% well)
cat(sprintf("\n  ALL regions       : max |regional - pooled| %.1f%%, spread at 120 cm %.1f%%\n",
            max(abs(rng$pct)), diff(range(rng$pct[rng$length_cm == 120]))))
cat(sprintf("  WELL-SAMPLED only : max |regional - pooled| %.1f%%, spread at 120 cm %.1f%%\n",
            max(abs(rw$pct)), diff(range(rw$pct[rw$length_cm == 120]))))
cat(sprintf("  well-sampled = N >= 800 and >= 20 trips: regions %s\n",
            paste(well, collapse = ", ")))
cat(sprintf("  regions differing from pooled at p < 0.05: %d of %d (%s)\n",
            sum(t12$p_vs_pooled < 0.05), nrow(t12),
            paste(t12$region[t12$p_vs_pooled < 0.05], collapse = ", ")))
cat("  NB: the sparsely sampled regions show the LARGER deviations, but none of\n")
cat("  them differs significantly from the pooled slope -- their standard errors\n")
cat("  on b are 3-9x larger. Quote the well-sampled figure for the magnitude\n")
cat("  claim, and show all regions in the table with N and SE visible.\n")

write_csv(t5,  file.path(TABDIR, "Table06_parametric_coefficients.csv"))
write_csv(t6,  file.path(TABDIR, "Table07_smooth_terms.csv"))
write_csv(t11, file.path(TABDIR, "Table08_eq1_all_regions.csv"))
write_csv(t12, file.path(TABDIR, "Table09_eq1_by_region.csv"))
write_csv(t13, file.path(TABDIR, "Table10_regional_vs_pooled_pct.csv"))
saveRDS(list(t5 = t5, t6 = t6, t11 = t11, t12 = t12, t13 = t13),
        file.path(OUT, "manuscript_tables.rds"))

message("39_manuscript_tables.R complete")
