# ==============================================================================
# 56_trip_slope_delivered.R -- What the trip slope does to the delivered a and b of the power law
#
# Section 4.6: the exponent and its standard error with and without the trip slope.
#
# ENGINE AND TIMING: four gam fits; 20-30 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260831)

JOBDIR <- file.path(OUT, "delivered_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf),
                  loglen_c = loglen - mean(loglen)) %>% as.data.frame()
cat(sprintf("%d fish | %d trips | %d observers\n",
            nrow(d), nlevels(d$tripf), nlevels(d$obs_name)))

SP    <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE    <- "s(obs_name, bs='re') + s(tripf, bs='re')"
SLOPE <- "s(loglen_c, tripf, bs='re')"
FORMS <- list(
  pow_base   = paste("logwt ~ loglen +", RE, "+ sexf +", SP, "+ yrf"),
  pow_slope  = paste("logwt ~ loglen +", RE, "+", SLOPE, "+ sexf +", SP, "+ yrf"),
  sm_base    = paste("logwt ~ s(loglen, k=20) +", RE, "+ sexf +", SP, "+ yrf"),
  sm_slope   = paste("logwt ~ s(loglen, k=20) +", RE, "+", SLOPE, "+ sexf +", SP, "+ yrf"),
  # The DELIVERED relationship is not the full model with a linear length term.
  # 39_ fits equation (1) marginally -- no spatial surface, no year -- because that
  # is what a user applies, and under REML. Quoting a and b from anything else
  # gives a different number: this pair reproduces 39_ (b = 3.0679) and adds the
  # trip slope to it, which is the comparison that answers "what changes for a user".
  eq1_base   = paste("logwt ~ loglen +", RE, "+ sexf"),
  eq1_slope  = paste("logwt ~ loglen +", RE, "+", SLOPE, "+ sexf"))

run <- function(nm) {
  f <- file.path(JOBDIR, paste0(nm, ".rds"))
  if (file.exists(f)) { cat(sprintf("  %s: cached\n", nm)); return(readRDS(f)) }
  cat(sprintf("  fitting %s ...\n", nm)); t0 <- Sys.time()
  # 39_ fits equation (1) under REML; the model-structure comparisons use ML.
  # Match whichever convention the number will be quoted against.
  meth <- if (grepl("^eq1", nm)) "REML" else "ML"
  m <- gam(as.formula(FORMS[[nm]]), data = d, family = scat,
           method = meth, gamma = 1.4)
  cat(sprintf("    %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  # keep only what is needed downstream; a full scat gam is large
  o <- list(aic = AIC(m), coefs = coef(m), ptab = summary(m)$p.table,
            pred = NULL)
  if (grepl("^sm", nm)) {
    nd <- data.frame(loglen = log(seq(70, 200, 5)), loglen_c = log(seq(70,200,5)) - mean(d$loglen),
                     length = seq(70, 200, 5),
                     obs_name = d$obs_name[1], tripf = d$tripf[1],
                     sexf = d$sexf[1], yrf = factor("2019", levels = levels(d$yrf)),
                     lon = median(d$lon), lat = median(d$lat), mon = 6)
    ex <- c("s(obs_name)", "s(tripf)")
    if (grepl("slope", nm)) ex <- c(ex, "s(loglen_c,tripf)")
    o$pred <- data.frame(length = nd$length,
                         logwt = as.numeric(predict(m, nd, exclude = ex)))
  }
  saveRDS(o, f); o
}
fits <- setNames(lapply(names(FORMS), run), names(FORMS))

# ------------------------------------------------------------------------------
# 1. the delivered power law
# ------------------------------------------------------------------------------
grab <- function(o) {
  pt <- o$ptab
  c(b = unname(pt["loglen", 1]), se = unname(pt["loglen", 2]),
    a = unname(exp(pt["(Intercept)", 1])))
}
pb <- grab(fits$pow_base); ps <- grab(fits$pow_slope)
cat("\n=== the delivered relationship, equation (1) ===\n")
cat(sprintf("  trip intercept only : b = %.4f (SE %.4f)   a = %.4e\n", pb["b"], pb["se"], pb["a"]))
cat(sprintf("  + trip slope        : b = %.4f (SE %.4f)   a = %.4e\n", ps["b"], ps["se"], ps["a"]))
cat(sprintf("  change in b: %+.4f, which is %.2f of the ORIGINAL standard error\n",
            ps["b"] - pb["b"], abs(ps["b"] - pb["b"]) / pb["se"]))
cat(sprintf("  standard error inflated by %.2fx\n", ps["se"] / pb["se"]))
cat(sprintf("  for comparison, the trip-block bootstrap in 46_ gave 0.0130 (2.6x nominal)\n"))

# weight at length implied by each power law
L <- seq(70, 200, 5)
w1 <- pb["a"] * L^pb["b"]; w2 <- ps["a"] * L^ps["b"]
cat(sprintf("\n  predicted weight differs by mean %.2f%%, max %.2f%% (at %d cm)\n",
            mean(abs(100*(w2-w1)/w1)), max(abs(100*(w2-w1)/w1)), L[which.max(abs(w2-w1)/w1)]))

# --- the DELIVERED relationship, as 39_ defines it ----------------------------
eb <- grab(fits$eq1_base); es <- grab(fits$eq1_slope)
cat("\n=== equation (1) as delivered to users (no spatial term, no year; REML) ===\n")
cat(sprintf("  trip intercept only : b = %.4f (SE %.4f)   a = %.4e   [39_ reports 3.0679]\n",
            eb["b"], eb["se"], eb["a"]))
cat(sprintf("  + trip slope        : b = %.4f (SE %.4f)   a = %.4e\n", es["b"], es["se"], es["a"]))
cat(sprintf("  change in b: %+.4f = %.2f original SE, but only %.2f of the HONEST SE\n",
            es["b"] - eb["b"], abs(es["b"] - eb["b"])/eb["se"], abs(es["b"] - eb["b"])/es["se"]))
cat(sprintf("  standard error inflated %.2fx (nominal %.4f -> %.4f; 46_ bootstrap 0.0130)\n",
            es["se"]/eb["se"], eb["se"], es["se"]))
we1 <- eb["a"] * L^eb["b"]; we2 <- es["a"] * L^es["b"]
cat(sprintf("  predicted weight differs by mean %.2f%%, max %.2f%% (at %d cm)\n",
            mean(abs(100*(we2-we1)/we1)), max(abs(100*(we2-we1)/we1)),
            L[which.max(abs(we2-we1)/we1)]))
cat("\n  NOTE on judging the shift in b. Measuring it against the ORIGINAL standard\n")
cat("  error is circular: that is the standard error this analysis shows to be too\n")
cat("  small by a factor of three. The honest comparison is against the new one.\n")

# ------------------------------------------------------------------------------
# 2. the smooth model's predictions
# ------------------------------------------------------------------------------
if (!is.null(fits$sm_base$pred) && !is.null(fits$sm_slope$pred)) {
  cmp <- fits$sm_base$pred %>% rename(base = logwt) %>%
    inner_join(fits$sm_slope$pred %>% rename(slope = logwt), by = "length") %>%
    mutate(pct = 100*(exp(slope) - exp(base))/exp(base))
  cat("\n=== predicted weight at length, smooth model, trip effects excluded ===\n")
  print(as.data.frame(cmp %>% filter(length %% 20 == 0 | length %in% c(70, 200)) %>%
    mutate(across(where(is.numeric), ~round(.x, 4)))), row.names = FALSE)
  cat(sprintf("\n  mean |difference| %.2f%%, max %.2f%% at %d cm\n",
              mean(abs(cmp$pct)), max(abs(cmp$pct)), cmp$length[which.max(abs(cmp$pct))]))
  cat(sprintf("  dAIC for the trip slope, smooth model: %+.1f\n",
              fits$sm_slope$aic - fits$sm_base$aic))
}

cat("\n=== READING THIS ===\n")
dchg <- abs(es["b"] - eb["b"]) / es["se"]      # judged against the HONEST SE
infl <- es["se"] / eb["se"]
cat(sprintf("  Delivered b moves %+.4f, which is %.2f of the honest standard error, and\n",
            es["b"] - eb["b"], dchg))
cat(sprintf("  changes predicted weight by at most %.2f%% across 70-200 cm.\n",
            max(abs(100*(we2-we1)/we1))))
if (dchg < 1.5) {
  cat("  So a trip slope changes what we say about UNCERTAINTY, not about the fish.\n")
  cat("  Users applying equation (1) would get essentially the same answer.\n")
} else {
  cat("  The delivered exponent moves by more than the honest uncertainty allows, so\n")
  cat("  the reported relationship itself would have to be revised.\n")
}
if (infl > 1.5) {
  cat(sprintf("  The standard error rises %.1fx, towards the trip-block bootstrap value.\n", infl))
  cat("  Modelling the slope and resampling trips are two routes to the same honest\n")
  cat("  interval, which is a much stronger position than a nominal SE with a caveat.\n")
} else {
  cat(sprintf("  The standard error rises only %.2fx, well short of the bootstrap's 2.6x,\n", infl))
  cat("  so the trip slope does not by itself deliver an honest interval for b.\n")
}

write_csv(tibble(model = c("trip intercept only", "+ trip slope"),
                 b = c(pb["b"], ps["b"]), se = c(pb["se"], ps["se"]),
                 a = c(pb["a"], ps["a"])),
          file.path(OUT, "TableR85_delivered_with_trip_slope.csv"))
saveRDS(list(pow = list(base = pb, slope = ps), fits = fits),
        file.path(OUT, "trip_slope_delivered.rds"))
message("56_trip_slope_delivered.R complete")
