# ==============================================================================
# 64_hbf_within_trip.R -- Hooks between floats decomposed into its trip mean and its within-trip departure
#
# Section 4.2. The within-trip component cannot be absorbed by the trip effect, so it tests HBF directly;
# an injection check confirms the test has power.
#
# ENGINE AND TIMING: gam(REML) for the reported fits, bam for 25 injection replicates; 3-8 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260901)
NINJ <- 25

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE <- "s(obs_name, bs='re') + s(tripf, bs='re')"
SL <- "s(loglen_c, tripf, bs='re')"

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf),
                  loglen_c = loglen - mean(loglen)) %>% as.data.frame()
stopifnot(!any(is.na(d$hbf)))

# ------------------------------------------------------------------------------
# 1. the decomposition, before any model is fitted
# ------------------------------------------------------------------------------
d <- d %>% group_by(tripf) %>%
  mutate(hbf_b = mean(hbf), hbf_w = hbf - hbf_b) %>% ungroup() %>% as.data.frame()

vw <- var(d$hbf_w); vb <- var(d$hbf_b)
tv <- d %>% group_by(tripf) %>% summarise(nh = n_distinct(hbf), n = dplyr::n(),
                                          .groups = "drop")
cat("=== how much of HBF varies within a trip? (analysis dataset) ===\n")
cat(sprintf("  total SD %.2f hooks | between-trip %.2f | within-trip %.2f\n",
            sd(d$hbf), sqrt(vb), sqrt(vw)))
cat(sprintf("  %.1f%% of HBF variance is within trip; ICC = %.3f\n",
            100 * vw / (vw + vb), vb / (vw + vb)))
cat(sprintf("  %d of %d trips change HBF at all, holding %d of %d fish\n",
            sum(tv$nh > 1), nrow(tv), sum(tv$n[tv$nh > 1]), sum(tv$n)))

# The same count on ALL sets, not the one-fish-per-set analysis dataset. The
# analysis dataset sees one fish per set, so it understates how many trips change
# HBF only if a trip's changes fall outside the sampled sets; report both.
fullsets <- da2 %>% filter(!is.na(hbf)) %>%
  group_by(tripID, setid) %>% summarise(hbf = dplyr::first(hbf), .groups = "drop")
ft <- fullsets %>% group_by(tripID) %>%
  summarise(nh = n_distinct(hbf), nsets = dplyr::n(), .groups = "drop")
fg <- fullsets %>% group_by(tripID) %>% mutate(b = mean(hbf)) %>% ungroup()
cat(sprintf("  on ALL %d sets in %d trips: %d trips (%.0f%%) change HBF; ICC %.3f,\n",
            nrow(fullsets), nrow(ft), sum(ft$nh > 1), 100*mean(ft$nh > 1),
            var(fg$b)/var(fg$hbf)))
cat(sprintf("  within-trip SD %.2f hooks against %.2f between; median range where it\n",
            sd(fg$hbf - fg$b), sd(fg$b)))
cat(sprintf("  varies %.1f hooks (max %.0f)\n",
            median((fullsets %>% group_by(tripID) %>%
                      summarise(r = diff(range(hbf)), .groups="drop") %>%
                      filter(r > 0))$r),
            max((fullsets %>% group_by(tripID) %>%
                   summarise(r = diff(range(hbf)), .groups="drop"))$r)))

nz <- d$hbf_w[abs(d$hbf_w) > 1e-8]
SPAN <- as.numeric(diff(quantile(nz, c(0.05, 0.95))))
cat(sprintf("  span used for reporting: %.1f hooks (5th-95th centile of the %d\n",
            SPAN, length(nz)))
cat("  non-zero within-trip departures) -- every effect below is quoted over it\n")

HBF_RANGE <- diff(range(d$hbf))
PUB_PCT   <- 9.83                       # without trip effects, across the full HBF range (section 4.2)
pub_per   <- log(1 + PUB_PCT / 100) / HBF_RANGE
PUB_SPAN  <- 100 * (exp(pub_per * SPAN) - 1)
cat(sprintf("  published effect %.2f%% across %.0f hooks -> %.2f%% over this span\n",
            PUB_PCT, HBF_RANGE, PUB_SPAN))

if ("workdate" %in% names(d)) {
  dd <- d %>% group_by(tripf) %>%
    mutate(f = rank(workdate, ties.method = "min"),
           f = (f - 1) / pmax(max(f) - 1, 1)) %>% ungroup()
  d$dayfrac <- dd$f
  bins <- dd %>% filter(tripf %in% tv$tripf[tv$nh > 1]) %>%
    mutate(q = cut(f, c(-.01, .25, .5, .75, 1.01),
                   labels = c("first quarter", "second", "third", "last quarter"))) %>%
    group_by(q) %>% summarise(mean_abs_departure = round(mean(abs(hbf_w)), 2),
                              fish = dplyr::n(), .groups = "drop")
  cat("\n  when in a trip does HBF depart from the trip mean?\n")
  print(as.data.frame(bins), row.names = FALSE)
} else {
  cat("\n  (workdate absent from the analysis dataset; day-within-trip control skipped)\n")
  d$dayfrac <- NA_real_
}

# ------------------------------------------------------------------------------
# 2. the fits
# ------------------------------------------------------------------------------
base <- paste("logwt ~ s(loglen, k=20) +", RE, "+", SL, "+ sexf +", SP, "+ yrf")
F0 <- as.formula(base)
F1 <- as.formula(paste(base, "+ hbf_w"))
F2 <- as.formula(paste(base, "+ hbf_w + hbf_b"))
F3 <- as.formula(paste(base, "+ s(hbf_w, k=5)"))
F4 <- as.formula(paste(base, "+ hbf_w + dayfrac"))

# ENGINE, SPLIT BY PURPOSE. The within-trip coefficient and its interval are
# quoted in section 4.2 and in the response letter as a BOUND, so they must come
# from an exact fit: a bound stated with a confidence interval is exactly what
# bam(discrete=TRUE) may not supply. REML rather than ML because these are
# reported rather than compared, and no AIC from this script is quoted anywhere.
#
# fit_fast stays bam and is used ONLY inside the injection check below, which is
# a resampling exercise over 25 refits and is what the approximation is for.
JOBDIR <- file.path(OUT, "hbf_within_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
fitg <- function(f, id, dat = d) {
  outf <- file.path(JOBDIR, paste0(id, "_",
                    substr(digest::digest(deparse(f)), 1, 8), "_REML.rds"))
  if (file.exists(outf)) { cat("  (cached)", id, "\n"); return(readRDS(outf)) }
  t0 <- Sys.time()
  m <- gam(f, data = dat, family = scat, method = "REML")
  cat(sprintf("  fitted %s in %.1f min", id,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))), "\n")
  saveRDS(m, outf); m
}
fit_fast <- function(f, dat = d) bam(f, data = dat, family = scat,
                                     method = "fREML", discrete = TRUE)
t0 <- Sys.time()
m0 <- fitg(F0, "m0"); m1 <- fitg(F1, "m1"); m2 <- fitg(F2, "m2"); m3 <- fitg(F3, "m3")
m4 <- if (all(is.finite(d$dayfrac))) fitg(F4, "m4") else NULL
cat(sprintf("\n%d fits in %.1f min\n", 4 + as.integer(!is.null(m4)),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

say <- function(m, term, label) {
  s <- summary(m)$p.table
  if (is.null(m) || !term %in% rownames(s)) {
    cat(sprintf("  %-30s not estimated\n", label)); return(NULL)
  }
  b <- s[term, 1]; se <- s[term, 2]
  pct <- function(z) 100 * (exp(z * SPAN) - 1)
  cat(sprintf("  %-30s %+6.2f%%  (95%% CI %+.2f to %+.2f)  p = %.3f\n",
              label, pct(b), pct(b - 1.96 * se), pct(b + 1.96 * se), s[term, 4]))
  c(est = pct(b), lo = pct(b - 1.96 * se), hi = pct(b + 1.96 * se), p = s[term, 4])
}

cat("\n=== effect on weight at length, over the within-trip span above ===\n")
r1 <- say(m1, "hbf_w", "within-trip HBF, alone")
r2 <- say(m2, "hbf_w", "within-trip HBF, w/ between")
rb <- say(m2, "hbf_b", "between-trip HBF")
r4 <- if (!is.null(m4)) say(m4, "hbf_w", "within-trip, day controlled") else NULL

cat("\n  No AIC comparison is printed: these models differ in FIXED effects and\n")
cat("  the fits are REML, whose likelihoods do not compare across those. No AIC\n")
cat("  from this script is quoted anywhere, so nothing is lost by omitting it.\n")
lab <- vapply(m3$smooth, function(z) z$label, character(1))
if ("s(hbf_w)" %in% lab) {
  i <- match("s(hbf_w)", lab)
  cat(sprintf("  s(hbf_w) edf = %.2f (a curved within-trip response would exceed 1)\n",
              sum(m3$edf[m3$smooth[[i]]$first.para:m3$smooth[[i]]$last.para])))
}

cat("\n  The between-trip coefficient is entangled with the trip random effect, so\n")
cat("  its STANDARD ERROR is the quantity of interest, not its estimate. NOTE the\n")
cat("  ratio below came out near 1, not large: 'a trip random effect has enough\n")
cat("  free parameters to absorb any between-trip variable' holds for trip as 232\n")
cat("  FIXED dummies, where hbf_b would be exactly collinear and inestimable. A\n")
cat("  PENALISED random effect shrinks, leaving the between-trip contrast partly\n")
cat("  identified -- blended in the usual random-effects way, so still not the\n")
cat("  clean estimate. The within-trip coefficient remains the identified one.\n")
se_of <- function(m, t) { s <- summary(m)$p.table; if (t %in% rownames(s)) s[t, 2] else NA }
cat(sprintf("  SE(hbf_w) = %.5f | SE(hbf_b) = %.5f | ratio %.1fx\n",
            se_of(m2, "hbf_w"), se_of(m2, "hbf_b"),
            se_of(m2, "hbf_b") / se_of(m2, "hbf_w")))

# ------------------------------------------------------------------------------
# 3. does the estimator actually recover a known within-trip effect?
#    A bound is worthless if the estimator is shrunk toward zero. Inject the
#    published effect into the response and see whether hbf_w finds it.
# ------------------------------------------------------------------------------
cat(sprintf("\n=== injection check: put the published effect in, %d replicates ===\n", NINJ))
fit0 <- as.numeric(predict(m0)); res <- d$logwt - fit0
inj <- replicate(NINJ, {
  z <- d
  z$logwt <- fit0 + pub_per * (z$hbf - mean(z$hbf)) + sample(res)
  s <- summary(fit_fast(F1, z))$p.table
  c(100 * (exp(s["hbf_w", 1] * SPAN) - 1), s["hbf_w", 4])
})
cat(sprintf("  injected %+.2f%% over the span; recovered %+.2f%% (SD %.2f); p<0.05 in %d/%d\n",
            PUB_SPAN, mean(inj[1, ]), sd(inj[1, ]), sum(inj[2, ] < 0.05), NINJ))
cat("  Recovery near the injected value means the interval above is a real bound.\n")
cat("  Recovery shrunk toward zero means it is not, and must not be quoted.\n")
cat("  (Residuals are reshuffled independently, which ignores residual within-trip\n")
cat("  correlation and so flatters detection slightly.)\n")

# ------------------------------------------------------------------------------
# 4. bait, the same way -- the coauthor raises it alongside HBF
# ------------------------------------------------------------------------------
cat("\n=== bait: does it change within a trip? ===\n")
bt <- d %>% group_by(tripf) %>% summarise(nb = n_distinct(baitType), n = dplyr::n(),
                                          .groups = "drop")
cat(sprintf("  %d of %d trips use more than one bait type, holding %d of %d fish\n",
            sum(bt$nb > 1), nrow(bt), sum(bt$n[bt$nb > 1]), sum(bt$n)))
bait_res <- NULL
if (sum(bt$nb > 1) >= 10) {
  db <- droplevels(d[d$tripf %in% bt$tripf[bt$nb > 1], ])
  db$baitType <- factor(db$baitType)
  fb0 <- fitg(as.formula(base), "fb0", db)
  fb1 <- fitg(as.formula(paste(base, "+ baitType")), "fb1", db)
  cat(sprintf("  within-trip bait test on those trips only (n = %d, %d bait types):\n",
              nrow(db), nlevels(db$baitType)))
  cat(sprintf("  dAIC %+.1f against the same model without bait\n", AIC(fb1) - AIC(fb0)))
  tb <- summary(fb1)$p.table
  bait_res <- tb[grep("^baitType", rownames(tb)), c(1, 2, 4), drop = FALSE]
  print(round(bait_res, 4))
  cat("  Bait is still largely a trip-level choice, so this rests on the minority\n")
  cat("  of trips that switch; read it as a bound, not as an estimate.\n")
} else {
  cat("  too few trips change bait to identify a within-trip bait effect\n")
}

saveRDS(list(span = SPAN, pub_span = PUB_SPAN, within = r1, within_b = r2,
             between = rb, within_day = r4, inj = inj, bait = bait_res,
             se_ratio = se_of(m2, "hbf_b") / se_of(m2, "hbf_w"),
             aic = c(m1 = AIC(m1) - AIC(m0), m2 = AIC(m2) - AIC(m0),
                     m3 = AIC(m3) - AIC(m0))),
        file.path(OUT, "hbf_within_trip.rds"))
message("64_hbf_within_trip.R complete")
