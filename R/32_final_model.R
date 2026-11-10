# ==============================================================================
# 32_final_model.R -- Analysis dataset and the final model (Model A, and Model C for comparison)
#
# Sections 2.4, 3.1, 3.4, 4.2 and 4.4; Tables 2 and 11. Builds the pre-exclusion pool, applies the three observer
# exclusion rules in order, draws one fish per set, and fits Model A (equation 2) and Model C under gam(ML).
# Writes output/final_dataset.rds and output/final_model.rds, which most later scripts read.
#
# ENGINE AND TIMING: one bam screening fit, then two gam(ML) fits in parallel; 10-25 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260826)

JOBDIR <- file.path(OUT, "final_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

SP  <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE  <- "s(obs_name, bs='re') + s(tripf, bs='re')"

# TRIP RANDOM SLOPE. Trips differ in the slope of log weight on log length as
# well as in level. The term replicates on fish that did not estimate it (54_)
# and improves prediction of withheld trips (78_); see section 4.6.
#
# loglen is centred so that the slope is not confounded with the intercept.
SLOPE <- "s(loglen_c, tripf, bs='re')"

# The SCREENING formula deliberately omits the slope. Rule (a) below derives the
# observer exclusions from this fit, so changing it would change DROP and hence
# the analysis dataset. The exclusion rule is about observer bias, precision and
# contribution, none of which the slope bears on; keeping the screen fixed leaves
# the dataset bit-identical to every earlier run.
FORM_SCREEN <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE, "+ sexf +", SP, "+ yrf"))
FORM_SMOOTH <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE, "+", SLOPE,
                                "+ sexf +", SP, "+ yrf"))
FORM_LINEAR <- as.formula(paste("logwt ~ loglen +", RE, "+", SLOPE,
                                "+ sexf +", SP, "+ yrf"))

# --- pool ---------------------------------------------------------------------
allf <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year),
         setf = factor(setid), tripf = factor(tripID), sexf = factor(sex),
         wrem = weight %% 5, lrem = length %% 5) %>%
  filter(complete.cases(logwt, loglen, lon, lat, mon, obs_name, yrf)) %>%
  droplevels() %>% as.data.frame()

# --- rule (b): precision -------------------------------------------------------
# Rule (0) MINIMUM CONTRIBUTION. An observer with a handful of fish contributes
# nothing and forces a random-effect level to be estimated from almost no data.
# The manuscript already applies ">10 sets"; it is applied explicitly here so the
# three rules are separable and stated. It also removes the degenerate case that
# broke the first run, where an observer with ONE fish whose length happened to
# be a multiple of 5 scored p0 = 1.0 and was excluded for "rounding".
MIN_SETS <- 10
contrib <- allf %>% group_by(obs_name) %>%
  summarise(n_fish = dplyr::n(), n_sets = n_distinct(setf), .groups = "drop")
drop_small <- contrib$obs_name[contrib$n_sets <= MIN_SETS]
cat(sprintf("=== rule (0) minimum contribution: <= %d sets ===\n", MIN_SETS))
print(as.data.frame(contrib %>% filter(obs_name %in% drop_small) %>%
        arrange(n_sets)), row.names = FALSE)

# Rule (b) PRECISION. p0 is a binomial proportion, so it needs a usable
# denominator; rule (0) already removes the degenerate cases.
prec <- allf %>% group_by(obs_name) %>%
  summarise(n = dplyr::n(), p0_wt = mean(wrem == 0), p0_len = mean(lrem == 0),
            .groups = "drop") %>% filter(!obs_name %in% drop_small)
drop_prec <- prec$obs_name[prec$p0_wt > 0.8 | prec$p0_len > 0.8]
cat("\n=== rule (b) precision: p0 > 0.8 for weight or length ===\n")
print(as.data.frame(prec %>% filter(obs_name %in% drop_prec) %>%
        mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)

# --- rule (a): bias, screened with bam ----------------------------------------
allf <- allf[!allf$obs_name %in% drop_small, ]
allf$obs_name <- droplevels(allf$obs_name)
d_all <- droplevels(draw_one_per_group(allf, "setf"))
cat(sprintf("\nscreening fit on %d fish, %d observers (after rule 0) ...\n",
            nrow(d_all), nlevels(d_all$obs_name)))
t0 <- Sys.time()
m_scr <- bam(FORM_SCREEN, data = d_all, family = scat, method = "fREML",
             discrete = TRUE, gamma = 1.4)
cat(sprintf("  done in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
re <- extract_ranef(m_scr, "s(obs_name)")
re$z <- (re$value - mean(re$value)) / sd(re$value)
drop_bias <- re$group[abs(re$z) > 2.5]
cat("\n=== observer exclusion, rule (a) bias: |z| > 2.5 ===\n")
print(as.data.frame(re %>% filter(group %in% drop_bias) %>% arrange(z) %>%
        select(group, value, z) %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)

DROP <- Reduce(union, list(as.character(drop_small), as.character(drop_bias),
                           as.character(drop_prec)))
cat(sprintf("\nexcluded: %d observers total (%d minimum-contribution, %d bias, %d precision)\n",
            length(DROP), length(drop_small), length(drop_bias), length(drop_prec)))
cat(sprintf("  -> %s\n", paste(sort(DROP), collapse = ", ")))
d <- droplevels(d_all[!d_all$obs_name %in% DROP, ])
# centred log length, for the trip random slope; carried in the saved dataset so
# downstream scripts use the same centring rather than each computing their own
d$loglen_c <- d$loglen - mean(d$loglen)
cat(sprintf("final dataset: %d fish (%d F, %d M) | %d trips | %d observers | %.2f%% of fish removed\n\n",
            nrow(d), sum(d$sexf == "F"), sum(d$sexf == "M"), nlevels(d$tripf),
            nlevels(d$obs_name), 100 * (1 - nrow(d) / nrow(d_all))))
saveRDS(list(d = d, DROP = DROP, prec = prec, re = re),
        file.path(OUT, "final_dataset.rds"))

# --- gam(ML) fits, in parallel -------------------------------------------------
run_fit <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(digest)})
  # Cache key MUST encode the dataset AND the formula. Keying on job$id alone
  # would silently return a fit from a previous run after the exclusion rules
  # changed the data -- or, as when the trip slope was adopted, after the
  # formula changed while the data did not.
  fh <- substr(digest::digest(deparse(if (job$id == "smooth") FORM_SMOOTH else FORM_LINEAR)), 1, 8)
  outf <- file.path(JOBDIR, sprintf("%s_n%d_o%d_t%d_%s.rds", job$id, nrow(d),
                                    nlevels(d$obs_name), nlevels(d$tripf), fh))
  if (file.exists(outf)) return(readRDS(outf))
  f <- if (job$id == "smooth") FORM_SMOOTH else FORM_LINEAR
  t0 <- Sys.time()
  m <- try(gam(f, data = d, family = scat, method = job$method, gamma = 1.4), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  saveRDS(m, outf)
  m
}
jobs <- list(list(id = "smooth", method = "ML"), list(id = "linear", method = "ML"))
cat("fitting the final model under gam(ML) (smooth and linear length) ...\n")
cl <- makeCluster(2)
clusterExport(cl, c("d", "FORM_SMOOTH", "FORM_LINEAR", "LIB", "JOBDIR"), envir = environment())
t0 <- Sys.time()
fits <- parLapplyLB(cl, jobs, run_fit); stopCluster(cl)
names(fits) <- sapply(jobs, `[[`, "id")
cat(sprintf("  elapsed %.1f min\n\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
m <- fits$smooth; ml <- fits$linear
stopifnot(!is.null(m), !is.null(ml))

# --- model summary -------------------------------------------------------------
lab <- sapply(m$smooth, function(z) z$label)
gete <- function(p) { i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
  sum(m$edf[m$smooth[[i[1]]]$first.para:m$smooth[[i[1]]]$last.para]) }
gesd <- function(p) { i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
  sd(coef(m)[m$smooth[[i[1]]]$first.para:m$smooth[[i[1]]]$last.para]) }
cat("=== final model, gam(ML) ===\n")
cat(sprintf("  n = %d | AIC = %.1f | dev.expl = %.2f%% | total edf = %.1f\n",
            nrow(d), AIC(m), summary(m)$dev.expl * 100, sum(m$edf)))
cat(sprintf("  edf: length %.2f | spatial %.2f | observer %.2f | trip %.2f\n",
            gete("s(loglen)"), gete("te("), gete("s(obs_name)"), gete("s(tripf)")))
cat(sprintf("  random effect SD: observer %.4f (%.2f%%) | trip %.4f (%.2f%%)\n",
            gesd("s(obs_name)"), 100*(exp(gesd("s(obs_name)"))-1),
            gesd("s(tripf)"), 100*(exp(gesd("s(tripf)"))-1)))
cat(sprintf("  trip random SLOPE: SD of b across trips %.4f | edf %.2f\n",
            gesd("s(loglen_c,tripf)"), gete("s(loglen_c,tripf)")))
sx <- coef(m)[grep("^sexf", names(coef(m)))]
cat(sprintf("  sex effect (M vs F): %+.4f log units = %+.2f%%\n", sx, 100*(exp(sx)-1)))
cat(sprintf("  dAIC (smooth - linear length) = %+.1f\n", AIC(m) - AIC(ml)))

# --- the maturation dip --------------------------------------------------------
cat("\n=== nonlinearity: departure from a log-linear relationship ===\n")
LENS <- c(80, 90, 100, 110, 120, 130, 140, 150, 160, 175)
nd <- data.frame(loglen = log(LENS), loglen_c = log(LENS) - mean(d$loglen),
                 lat = median(d$lat), lon = median(d$lon), mon = 6,
                 yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                 sexf = factor("F", levels = levels(d$sexf)),
                 obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                 tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
Xp <- predict(m, nd, type = "lpmatrix", exclude = EX)
B  <- MASS::mvrnorm(4000, coef(m), vcov(m))
lp <- B %*% t(Xp)
wts <- sapply(LENS, function(L) sum(abs(d$length - L) <= 7.5)); wts <- wts / sum(wts)
x <- log(LENS)
dev <- t(apply(lp, 1, function(y) 100 * (exp(y - fitted(lm(y ~ x, weights = wts))) - 1)))
q <- t(apply(dev, 2, quantile, c(.025, .5, .975)))
nfish <- sapply(LENS, function(L) sum(abs(d$length - L) <= 5))
out <- data.frame(length_cm = LENS, n_within_5cm = nfish,
                  median = q[, 2], lo = q[, 1], hi = q[, 3])
print(out %>% mutate(across(where(is.numeric), ~round(.x, 2))), row.names = FALSE)
i110 <- which(LENS == 110)
cat(sprintf("\n  dip at 110 cm: %+.2f%% (95%% CrI %+.2f to %+.2f) from %d fish within 5 cm\n",
            out$median[i110], out$lo[i110], out$hi[i110], out$n_within_5cm[i110]))
cat("  published specification gave -2.5%% (CrI -2.9 to -2.1); bam on the new spec gave -1.27%%\n")

write_csv(out, file.path(OUT, "TableR52_final_nonlinearity.csv"))
saveRDS(list(m = m, ml = ml, out = out, DROP = DROP),
        file.path(OUT, "final_model.rds"))
message("32_final_model.R complete")
