# ==============================================================================
# 45_rgg_supplementary.R -- The 2009-2014 (RGG) period under the same approach
#
# Supplementary Text S2 and section 4.9. Same filters, exclusion rules and model as 32_.
#
# ENGINE AND TIMING: gam(REML); 35-60 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260830)

NWORKER <- 4; THREADS <- 2
JOBDIR  <- file.path(OUT, "rgg_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

SP  <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE  <- "s(obs_name, bs='re') + s(tripf, bs='re')"
# Trip random slope (32_). RGG must be fitted under the same
# specification as RGT or the period comparison compares two different models.
SL          <- "s(loglen_c, tripf, bs='re')"
# As in 32_, the SCREENING formula omits the slope: rule (a) derives the observer
# exclusions from it, so changing it would change which observers are dropped and
# hence the RGG dataset itself.
FORM_SCREEN <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE, "+ sexf +", SP, "+ yrf"))
FORM_SMOOTH <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE, "+", SL,
                                "+ sexf +", SP, "+ yrf"))
FORM_LINEAR <- as.formula(paste("logwt ~ loglen +", RE, "+", SL,
                                "+ sexf +", SP, "+ yrf"))
F_EQ1       <- logwt ~ loglen + s(obs_name, bs = "re") + s(tripf, bs = "re") +
  s(loglen_c, tripf, bs = "re") + sexf

# --- the RGG pool, filtered exactly as RGT is ---------------------------------
allf <- da2 %>%
  filter(processtype == "RGG", year >= 2009, year <= 2014, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
         setf = factor(setid), tripf = factor(tripID), sexf = factor(sex),
         wrem = weight %% 5, lrem = length %% 5) %>%
  filter(complete.cases(logwt, loglen, lon, lat, mon, obs_name, yrf)) %>%
  droplevels() %>% as.data.frame()
cat(sprintf("RGG pool: %d fish | %d sets | %d trips | %d observers\n",
            nrow(allf), nlevels(allf$setf), nlevels(allf$tripf), nlevels(allf$obs_name)))

# --- rules (0) and (b), then (a) by screening fit -----------------------------
MIN_SETS <- 10
contrib <- allf %>% group_by(obs_name) %>%
  summarise(n_fish = dplyr::n(), n_sets = n_distinct(setf), .groups = "drop")
drop_small <- contrib$obs_name[contrib$n_sets <= MIN_SETS]
cat(sprintf("\nrule (0) <= %d sets: %d observers\n", MIN_SETS, length(drop_small)))

prec <- allf %>% group_by(obs_name) %>%
  summarise(n = dplyr::n(), p0_wt = mean(wrem == 0), p0_len = mean(lrem == 0),
            .groups = "drop") %>% filter(!obs_name %in% drop_small)
drop_prec <- prec$obs_name[prec$p0_wt > 0.8 | prec$p0_len > 0.8]
cat(sprintf("rule (b) p0 > 0.8: %d observers\n", length(drop_prec)))
if (length(drop_prec))
  print(as.data.frame(prec %>% filter(obs_name %in% drop_prec) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)

allf <- allf[!allf$obs_name %in% drop_small, ]
allf$obs_name <- droplevels(allf$obs_name)
d_all <- droplevels(draw_one_per_group(allf, "setf"))
cat(sprintf("\nscreening fit on %d fish, %d observers ...\n",
            nrow(d_all), nlevels(d_all$obs_name)))
m_scr <- bam(FORM_SCREEN, data = d_all, family = scat, method = "fREML",
             discrete = TRUE, gamma = 1.4)
re <- extract_ranef(m_scr, "s(obs_name)")
re$z <- (re$value - mean(re$value)) / sd(re$value)
drop_bias <- re$group[abs(re$z) > 2.5]
cat(sprintf("rule (a) |z| > 2.5: %d observers\n", length(drop_bias)))

DROP <- Reduce(union, list(as.character(drop_small), as.character(drop_bias),
                           as.character(drop_prec)))
d <- droplevels(d_all[!d_all$obs_name %in% DROP, ])
d$loglen_c <- d$loglen - mean(d$loglen)   # centred within RGG, as 32_ does for RGT
cat(sprintf("\nRGG analysis dataset: %d fish (%d F, %d M) | %d trips | %d observers\n",
            nrow(d), sum(d$sexf == "F"), sum(d$sexf == "M"),
            nlevels(d$tripf), nlevels(d$obs_name)))
cat(sprintf("  %d observers excluded, %.2f%% of fish\n",
            length(DROP), 100*(1 - nrow(d)/nrow(d_all))))
saveRDS(list(d = d, DROP = DROP, prec = prec, re = re),
        file.path(OUT, "rgg_dataset.rds"))

# --- the heavy fits, in parallel, each cached ---------------------------------
run_fit <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(digest)})
  Sys.setenv(OMP_NUM_THREADS = THREADS, OPENBLAS_NUM_THREADS = THREADS)
  # Cache key includes a formula hash. Keyed on job$id alone it would silently
  # return the previous specification's fits after the trip slope was adopted --
  # the same trap 32_ and 37_ carried.
  # The METHOD is part of the key as well as the formula. Keyed on the formula
  # alone, switching a fit from ML to REML returns the ML fit from cache and
  # changes nothing -- the same silent-stale-cache trap, one level along.
  fh <- substr(digest::digest(deparse(job$form)), 1, 8)
  outf <- file.path(JOBDIR, paste0(job$id, "_", fh, "_", job$method, ".rds"))
  if (file.exists(outf)) return(readRDS(outf))
  t0 <- Sys.time()
  m <- try(gam(job$form, data = job$dat, family = scat, method = job$method,
               gamma = 1.4), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  attr(m, "mins") <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(m, outf); m
}
# ENGINE BY PURPOSE. Everything REPORTED comes from a REML fit: REML integrates
# out the fixed effects when estimating smoothing parameters, so the variance
# components are less biased, and variance components are what this reports. The
# ML pair below exists only to compare the smooth length term with the linear
# one, which differ in their FIXED effects and so cannot be compared on REML.
jobs <- list(list(id = "rgg_smooth", form = FORM_SMOOTH, dat = d, method = "REML"),
             list(id = "rgg_smooth", form = FORM_SMOOTH, dat = d, method = "ML"),
             list(id = "rgg_linear", form = FORM_LINEAR, dat = d, method = "ML"),
             list(id = "rgg_eq1",    form = F_EQ1,       dat = d, method = "REML"))
regs <- d %>% count(region) %>% filter(n >= 150) %>% arrange(region) %>% pull(region)
for (r in as.character(regs))
  jobs[[length(jobs)+1]] <- list(id = paste0("rgg_eq1_R", r), form = F_EQ1,
                                 dat = droplevels(d[d$region == r, ]), method = "REML")
cat(sprintf("\nfitting %d models on %d workers ...\n", length(jobs), NWORKER))
cl <- makeCluster(NWORKER)
clusterExport(cl, c("LIB", "JOBDIR", "THREADS"), envir = environment())
t0 <- Sys.time()
fits <- parLapplyLB(cl, jobs, run_fit)
stopCluster(cl)
names(fits) <- vapply(jobs, function(j) paste0(j$id, "_", j$method), character(1))
cat(sprintf("elapsed %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

want <- c("rgg_smooth_REML", "rgg_smooth_ML", "rgg_linear_ML", "rgg_eq1_REML",
          paste0("rgg_eq1_R", as.character(regs), "_REML"))
miss <- setdiff(want, names(fits))
if (length(miss))
  stop("fits missing: ", paste(miss, collapse = ", "),
       "
  names present: ", paste(names(fits), collapse = ", "))

m   <- fits$rgg_smooth_REML         # everything reported below comes from this
mML <- fits$rgg_smooth_ML            # the ML pair, used only for the dAIC
ml  <- fits$rgg_linear_ML
stopifnot(!is.null(m), !is.null(mML), !is.null(ml))

# --- supplementary tables ------------------------------------------------------
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***",
                     ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
pt <- summary(m)$p.table
tS1 <- tibble(term = sub("^yrf", "", sub("^sexfM$", "Sex (M vs F)", rownames(pt))),
              estimate = pt[,1], std_error = pt[,2], z = pt[,3], p = pt[,4]) %>%
  mutate(sig = stars(p))
st <- summary(m)$s.table
tS2 <- tibble(term = rownames(st), edf = st[,1], ref_df = st[,2],
              chi_sq = st[,3], p = st[,4]) %>% mutate(sig = stars(p))
cat("\n=== RGG parametric coefficients ===\n")
print(as.data.frame(tS1 %>% mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
cat("\n=== RGG smooth terms ===\n")
print(as.data.frame(tS2 %>% mutate(across(where(is.numeric), ~round(.x, 2)))), row.names = FALSE)

sd_of <- function(mm, lab) {
  labs <- vapply(mm$smooth, function(z) z$label, character(1))
  i <- match(lab, labs); if (is.na(i)) return(NA_real_)
  sd(coef(mm)[mm$smooth[[i]]$first.para:mm$smooth[[i]]$last.para])
}
vc <- tibble(component = c("observer", "trip"),
             sd_log = c(sd_of(m, "s(obs_name)"), sd_of(m, "s(tripf)"))) %>%
  mutate(pct = 100*(exp(sd_log) - 1))
cat("\n=== RGG variance components ===\n")
print(as.data.frame(vc %>% mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
cat(sprintf("\n  deviance explained %.2f%% | total edf %.1f | AIC %.1f  (REML)\n",
            summary(m)$dev.expl*100, sum(m$edf), AIC(m)))
cat(sprintf("  dAIC smooth vs linear length %+.1f  (from the ML pair: the two\n",
            AIC(mML) - AIC(ml)))
cat("  differ in fixed effects, and REML likelihoods do not compare across those)\n")

# --- eq.1, pooled and by region ------------------------------------------------
ab <- function(mm, dd) {
  cf <- coef(mm); se <- summary(mm)$se
  tibble(a = unname(exp(cf["(Intercept)"])), b = unname(cf["loglen"]),
         b_SE = unname(se["loglen"]), a_CV = unname(se["(Intercept)"]),
         r_sq = summary(mm)$r.sq, N = nrow(dd), trips = n_distinct(dd$tripf))
}
tS3 <- bind_rows(c(list(ab(fits$rgg_eq1_REML, d) %>% mutate(region = "All")),
  lapply(as.character(regs), function(r) {
    f <- fits[[paste0("rgg_eq1_R", r, "_REML")]]
    if (is.null(f)) return(NULL)
    ab(f, d[d$region == r, ]) %>% mutate(region = r) }))) %>%
  select(region, everything())
cat("\n=== RGG eq.1, pooled and by region ===\n")
print(as.data.frame(tS3 %>% mutate(a = formatC(a, format="e", digits=3),
  across(c(b, b_SE, r_sq), ~round(.x, 4)))), row.names = FALSE)

# --- the period comparison -----------------------------------------------------
rgt <- readRDS(file.path(OUT, "manuscript_tables.rds"))$t11
LENS <- seq(70, 200, by = 10)
a_rgg <- tS3$a[tS3$region == "All"]; b_rgg <- tS3$b[tS3$region == "All"]
a_rgt <- rgt$a[1];                   b_rgt <- rgt$b[1]
cmp <- tibble(length_cm = LENS,
              RGG = a_rgg * LENS^b_rgg, RGT = a_rgt * LENS^b_rgt) %>%
  mutate(pct_diff = 100*(RGT - RGG)/RGG)
cat("\n=== the same power law fitted to both periods, identically ===\n")
cat(sprintf("  RGG 2009-2014: a = %.4e, b = %.4f (SE %.4f), n = %d, %d trips\n",
            a_rgg, b_rgg, tS3$b_SE[tS3$region == "All"], tS3$N[tS3$region == "All"],
            tS3$trips[tS3$region == "All"]))
cat(sprintf("  RGT 2015-2022: a = %.4e, b = %.4f, n = %d\n", a_rgt, b_rgt, rgt$N[1]))
cat(sprintf("  difference in b: %+.4f, or %.1f RGG standard errors\n",
            b_rgt - b_rgg, (b_rgt - b_rgg)/tS3$b_SE[tS3$region == "All"]))
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~round(.x, 3)))), row.names = FALSE)
cat(sprintf("\n  mean |difference| in predicted weight: %.2f%%   max: %.2f%%\n",
            mean(abs(cmp$pct_diff)), max(abs(cmp$pct_diff))))
cat("  The two periods share a fishery and a species but almost no trips or\n")
cat("  observers. Read this as the scale of difference a decade of new sampling\n")
cat("  produces, NOT as demonstrated change in the fish -- the periods also\n")
cat("  differ in recorded process type.\n")

write_csv(tS1, file.path(OUT, "TableS_RGG_parametric.csv"))
write_csv(tS2, file.path(OUT, "TableS_RGG_smooth.csv"))
write_csv(tS3, file.path(OUT, "TableS_RGG_eq1_by_region.csv"))
write_csv(cmp, file.path(OUT, "TableS_RGG_vs_RGT_predicted.csv"))
saveRDS(list(m = m, ml = ml, tS1 = tS1, tS2 = tS2, tS3 = tS3, cmp = cmp,
             vc = vc, DROP = DROP), file.path(OUT, "rgg_supplementary.rds"))
message("45_rgg_supplementary.R complete")
