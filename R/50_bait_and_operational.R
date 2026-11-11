# ==============================================================================
# 50_bait_and_operational.R -- Bait type and the operational covariates, tested with and without trip effects
#
# Sections 4.6 and 4.1; Table 4 (operational rows) and Table 5 (bait row).
#
# ENGINE AND TIMING: six gam(ML) fits and 20 bam cross-validation fits; about 25 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260830)

JOBDIR <- file.path(OUT, "bait_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
cat(sprintf("final dataset: %d fish | %d trips | %d observers\n",
            nrow(d), nlevels(d$tripf), nlevels(d$obs_name)))
cat(sprintf("baitType missing for %.1f%% of fish\n", 100*mean(is.na(d$baitType))))
print(table(d$baitType, useNA = "ifany"))

db <- d %>% filter(!is.na(baitType), !is.na(weather), !is.na(wavescale),
                   !is.na(lightstick), !is.na(lunar_illumination)) %>%
  mutate(baitf = factor(baitType), weatherf = factor(weather),
         wavef = factor(wavescale), lightf = factor(lightstick),
         lunar = as.numeric(lunar_illumination)) %>%
  droplevels() %>% as.data.frame()
cat(sprintf("\ncomplete-case set for these tests: %d fish, %d trips, %d observers\n",
            nrow(db), nlevels(factor(db$tripf)), nlevels(factor(db$obs_name))))

# ------------------------------------------------------------------------------
# 1. at what level do these covariates vary?  (cf. 30_: HBF 0.967, length 0.188)
# ------------------------------------------------------------------------------
icc_cat <- function(x, g) {                    # mean ICC over indicator columns
  g <- factor(g); lv <- sort(unique(x))
  v <- vapply(lv, function(l) {
    y <- as.integer(x == l)
    mm <- tapply(y, g, mean); nn <- tapply(y, g, length); gm <- mean(y)
    msb <- sum(nn*(mm - gm)^2)/(nlevels(g) - 1)
    msw <- sum((y - mm[as.character(g)])^2)/(length(y) - nlevels(g))
    n0  <- (sum(nn) - sum(nn^2)/sum(nn))/(nlevels(g) - 1)
    s   <- max(0, (msb - msw)/n0); s/(s + msw)
  }, numeric(1))
  weighted.mean(v, table(x)[as.character(lv)])
}
icc_num <- function(y, g) {                    # ICC for a continuous covariate
  g <- factor(g); mm <- tapply(y, g, mean); nn <- tapply(y, g, length); gm <- mean(y)
  msb <- sum(nn*(mm - gm)^2)/(nlevels(g) - 1)
  msw <- sum((y - mm[as.character(g)])^2)/(length(y) - nlevels(g))
  n0  <- (sum(nn) - sum(nn^2)/sum(nn))/(nlevels(g) - 1)
  s   <- max(0, (msb - msw)/n0); s/(s + msw)
}
cat("\n=== at what level does each covariate vary? ===\n")
icc_rows <- list()   # saved, not only printed: these numbers are cited in Table 4
for (nm in c("baitf", "weatherf", "wavef", "lightf")) {
  per_trip <- db %>% group_by(tripf) %>%
    summarise(k = n_distinct(.data[[nm]]), .groups = "drop")
  cat(sprintf("  %-9s ICC within trip %.3f | distinct values per trip: median %d, mean %.2f, max %d\n",
              nm, icc_cat(as.character(db[[nm]]), db$tripf),
              median(per_trip$k), mean(per_trip$k), max(per_trip$k)))
  icc_rows[[nm]] <- data.frame(covariate = nm,
                               icc_within_trip = icc_cat(as.character(db[[nm]]), db$tripf),
                               median_per_trip = median(per_trip$k),
                               mean_per_trip   = mean(per_trip$k),
                               max_per_trip    = max(per_trip$k))
}
cat(sprintf("  %-9s ICC within trip %.3f | continuous; SD within trip %.3f of total %.3f\n",
            "lunar", icc_num(db$lunar, db$tripf),
            sqrt(mean(tapply(db$lunar, factor(db$tripf), var), na.rm = TRUE)), sd(db$lunar)))
icc_rows[["lunar"]] <- data.frame(covariate = "lunar",
                                  icc_within_trip = icc_num(db$lunar, db$tripf),
                                  median_per_trip = NA_real_,
                                  mean_per_trip   = NA_real_,
                                  max_per_trip    = NA_real_)
icc_tab <- bind_rows(icc_rows)
write_csv(icc_tab, file.path(OUT, "TableR105_operational_icc.csv"))
cat("  (an ICC near 1 means the covariate is a property of the trip, so its\n")
cat("   evidence is the number of trips, not the number of fish)\n")

# ------------------------------------------------------------------------------
# 2. the same term, with and without the trip random effect
# ------------------------------------------------------------------------------
SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
# Trip random slope (32_): the specification these results
# are compared against must be the one the paper uses.
BASE_TRIP <- paste("logwt ~ s(loglen, k=20) + s(obs_name, bs='re') +",
                   "s(tripf, bs='re') + s(loglen_c, tripf, bs='re') + sexf +", SP, "+ yrf")
BASE_NO   <- paste("logwt ~ s(loglen, k=20) + s(obs_name, bs='re') +",
                   "sexf +", SP, "+ yrf")

run <- function(nm, form, data) {
  f <- file.path(JOBDIR, paste0("gam_", nm, ".rds"))
  if (file.exists(f)) {
    o <- readRDS(f)
    # AICs are only comparable across fits made on identical rows; a cache built
    # on a different complete-case set must not be silently reused
    if (!is.null(o$n) && o$n != nrow(data))
      stop(sprintf("cached %s was fitted on %d rows, now %d -- clear %s",
                   nm, o$n, nrow(data), JOBDIR))
    cat(sprintf("  %s: cached\n", nm)); return(o)
  }
  cat(sprintf("  fitting %s ...\n", nm)); t0 <- Sys.time()
  m <- gam(as.formula(form), data = data, family = scat, method = "ML", gamma = 1.4)
  cat(sprintf("    %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  s <- summary(m)
  o <- list(aic = AIC(m), ptab = s$p.table, stab = s$s.table, n = nrow(data),
            coefs = coef(m)[grep("baitf|weatherf|wavef|lightf", names(coef(m)))])
  saveRDS(o, f); o
}

cat("\nfitting (complete-case data throughout so AICs are comparable) ...\n")
fits <- list(
  no_trip_base = run("no_trip_base", BASE_NO,   db),
  no_trip_bait = run("no_trip_bait", paste(BASE_NO,   "+ baitf"), db),
  trip_base    = run("trip_base",    BASE_TRIP, db),
  trip_bait    = run("trip_bait",    paste(BASE_TRIP, "+ baitf"), db),
  trip_weather = run("trip_weather", paste(BASE_TRIP, "+ weatherf"), db),
  trip_wave    = run("trip_wave",    paste(BASE_TRIP, "+ wavef"), db),
  trip_light   = run("trip_light",   paste(BASE_TRIP, "+ lightf"), db),
  trip_lunar   = run("trip_lunar",   paste(BASE_TRIP, "+ s(lunar, k=8)"), db))

cat("\n=== bait: apparent support depends entirely on whether trip is modelled ===\n")
cat(sprintf("  WITHOUT trip effects: dAIC for adding bait %+8.1f\n",
            fits$no_trip_bait$aic - fits$no_trip_base$aic))
cat(sprintf("  WITH    trip effects: dAIC for adding bait %+8.1f\n",
            fits$trip_bait$aic - fits$trip_base$aic))
show_terms <- function(o, lab) {
  cf <- o$ptab[grep("baitf|weatherf|wavef|lightf", rownames(o$ptab)), , drop = FALSE]
  if (!nrow(cf)) { cat(sprintf("  %s: no terms\n", lab)); return(invisible()) }
  cat(sprintf("\n  %s\n", lab))
  for (i in seq_len(nrow(cf)))
    cat(sprintf("    %-18s %+7.3f%% in weight (SE %.3f%%)  p = %.4g\n",
                rownames(cf)[i], 100*(exp(cf[i,1]) - 1), 100*cf[i,2], cf[i,4]))
}
show_terms(fits$no_trip_bait, "bait effects WITHOUT trip in the model")
show_terms(fits$trip_bait,    "bait effects WITH trip in the model")
show_terms(fits$trip_weather, "weather effects, with trip in the model")
show_terms(fits$trip_wave,    "wave scale effects, with trip in the model")
show_terms(fits$trip_light,   "light stick effect, with trip in the model")
lun <- fits$trip_lunar$stab[grep("lunar", rownames(fits$trip_lunar$stab)), , drop = FALSE]
if (nrow(lun))
  cat(sprintf("\n  lunar illumination, with trip in the model: edf %.2f, p = %.4g\n",
              lun[1, "edf"], lun[1, ncol(lun)]))
cat("
  The four operational covariates under the final specification (scaled-t,
")
cat("  trip random effect, RGT, both sexes).
")
cat(sprintf("\n  dAIC with trip modelled: weather %+.1f | wave %+.1f | light stick %+.1f | lunar %+.1f\n",
            fits$trip_weather$aic - fits$trip_base$aic,
            fits$trip_wave$aic    - fits$trip_base$aic,
            fits$trip_light$aic   - fits$trip_base$aic,
            fits$trip_lunar$aic   - fits$trip_base$aic))

# ------------------------------------------------------------------------------
# 3. trip-blocked cross-validation -- the paper's own standard
# ------------------------------------------------------------------------------
CVF <- file.path(JOBDIR, "cv.rds")
if (file.exists(CVF)) { cv <- readRDS(CVF); cat("\n  (CV cached)\n") } else {
  cat("\ntrip-blocked 10-fold CV (bam), base vs base + bait ...\n")
  trips <- levels(factor(db$tripf)); K <- 10
  fold <- setNames(sample(rep(1:K, length.out = length(trips))), trips)
  db$fold <- fold[as.character(db$tripf)]
  t0 <- Sys.time(); rows <- list()
  for (k in 1:K) {
    tr <- db[db$fold != k, ]; te <- db[db$fold == k, ]
    tr$tripf <- factor(tr$tripf); tr$obs_name <- factor(tr$obs_name)
    te <- te[te$obs_name %in% levels(tr$obs_name), ]      # observers must be seen
    if (!nrow(te)) next
    te$obs_name <- factor(te$obs_name, levels = levels(tr$obs_name))
    # predict.bam cannot handle unseen random-effect levels even with exclude=,
    # so point every test row at a training trip; exclude= then drops its effect
    te$tripf <- factor(levels(tr$tripf)[1], levels = levels(tr$tripf))
    for (lab in c("base", "bait")) {
      f <- if (lab == "base") BASE_TRIP else paste(BASE_TRIP, "+ baitf")
      m <- try(bam(as.formula(f), data = tr, family = scat, method = "fREML",
                   discrete = TRUE, gamma = 1.4), silent = TRUE)
      if (inherits(m, "try-error")) next
      # predict with trip effects excluded (test trips are unseen)
      p <- predict(m, newdata = te, type = "link",
                   exclude = c("s(tripf)"), discrete = FALSE)
      rows[[length(rows)+1]] <- data.frame(fold = k, model = lab,
        rmse = sqrt(mean((te$logwt - p)^2)), n = nrow(te))
    }
  }
  cv <- bind_rows(rows)
  cat(sprintf("  %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  saveRDS(cv, CVF)
}
if (nrow(cv)) {
  w <- cv %>% tidyr::pivot_wider(id_cols = fold, names_from = model, values_from = rmse)
  w <- w[stats::complete.cases(w), ]
  cat("\n=== trip-blocked CV: does bait predict withheld trips better? ===\n")
  cat(sprintf("  mean RMSE  base %.5f | bait %.5f | difference %+.5f\n",
              mean(w$base), mean(w$bait), mean(w$bait - w$base)))
  cat(sprintf("  folds won by bait: %d of %d", sum(w$bait < w$base), nrow(w)))
  tt <- try(t.test(w$bait, w$base, paired = TRUE), silent = TRUE)
  if (!inherits(tt, "try-error")) cat(sprintf(" | paired p = %.3f", tt$p.value))
  cat("\n")
}

cat("\n=== READING THIS ===\n")
dn <- fits$no_trip_bait$aic - fits$no_trip_base$aic
dt <- fits$trip_bait$aic    - fits$trip_base$aic
if (dn < -10 && dt > -10) {
  cat("  Bait looks strongly supported until trip effects are modelled, and then\n")
  cat("  does not. It is a LABEL FOR THE TRIP, not a mechanism acting on the fish --\n")
  cat("  the same pattern as hooks between floats. This is a sixth instance of the\n")
  cat("  paper's central phenomenon, on a covariate that had not been examined.\n")
} else if (dt < -10) {
  cat("  Bait survives the trip random effect. It is a candidate MECHANISM for part\n")
  cat("  of the trip effect and should be reported as such, with the caveat that its\n")
  cat("  evidence is the number of trips rather than the number of fish.\n")
} else {
  cat("  Bait is unsupported either way; it does not explain the trip effect.\n")
}

write_csv(cv, file.path(OUT, "TableR79_bait_cv.csv"))
saveRDS(list(fits = fits, cv = cv), file.path(OUT, "bait_and_operational.rds"))
message("50_bait_and_operational.R complete")
