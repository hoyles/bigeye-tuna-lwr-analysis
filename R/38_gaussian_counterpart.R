# ==============================================================================
# 38_gaussian_counterpart.R -- The Gaussian counterpart of Model A, for Figure 6 and the supplementary distribution figure
#
# Section 4.5 and Figure 6.
#
# ENGINE AND TIMING: one gam(ML) fit; 3-10 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260828)

dat <- readRDS(file.path(OUT, "final_dataset.rds"))
d <- dat$d
d$tripf <- factor(d$tripf); d$obs_name <- factor(d$obs_name); d$yrf <- factor(d$yrf)

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE <- "s(obs_name, bs='re') + s(tripf, bs='re')"
# The Gaussian twin must match the final specification in EVERY respect except
# the error distribution, or the comparison confounds two changes. The trip
# random slope was (32_), so it belongs here too.
FORM <- as.formula(paste("logwt ~ s(loglen, k=20) +", RE,
                         "+ s(loglen_c, tripf, bs='re') + sexf +", SP, "+ yrf"))

cat(sprintf("fitting Gaussian counterpart on %d fish ...\n", nrow(d)))
t0 <- Sys.time()
mg <- gam(FORM, data = d, family = gaussian, method = "ML", gamma = 1.4)
cat(sprintf("done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

fin <- readRDS(file.path(OUT, "final_model.rds"))
ms <- fin$m

gete <- function(mm, p) {
  lab <- vapply(mm$smooth, function(z) z$label, character(1))
  i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
  sum(mm$edf[mm$smooth[[i[1]]]$first.para:mm$smooth[[i[1]]]$last.para])
}
gesd <- function(mm, p) {
  lab <- vapply(mm$smooth, function(z) z$label, character(1))
  i <- grep(p, lab, fixed = TRUE); if (!length(i)) return(NA_real_)
  sd(coef(mm)[mm$smooth[[i[1]]]$first.para:mm$smooth[[i[1]]]$last.para])
}

cmp <- tibble(
  quantity = c("AIC", "deviance explained %", "total edf", "length edf",
               "spatial edf", "observer SD %", "trip SD %"),
  scaled_t = c(AIC(ms), summary(ms)$dev.expl*100, sum(ms$edf),
               gete(ms, "s(loglen)"), gete(ms, "te("),
               100*(exp(gesd(ms, "s(obs_name)"))-1), 100*(exp(gesd(ms, "s(tripf)"))-1)),
  gaussian = c(AIC(mg), summary(mg)$dev.expl*100, sum(mg$edf),
               gete(mg, "s(loglen)"), gete(mg, "te("),
               100*(exp(gesd(mg, "s(obs_name)"))-1), 100*(exp(gesd(mg, "s(tripf)"))-1)))
cat("\n=== scaled-t vs Gaussian, same specification, whole data ===\n")
print(as.data.frame(cmp %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)
cat("\n  NB: AIC is not comparable across families with different likelihoods.\n")
cat("  The out-of-sample comparison in 37_ is the one to quote.\n")

# predicted weight at length, both families, random effects excluded
LENS <- seq(70, 200, by = 5)
nd <- data.frame(loglen = log(LENS), loglen_c = log(LENS) - mean(d$loglen),
                 lat = median(d$lat), lon = median(d$lon),
                 mon = 6, sexf = factor("F", levels = levels(d$sexf)),
                 yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                 obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                 tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
ex <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
pred <- tibble(length_cm = LENS,
               scat = exp(as.numeric(predict(ms, nd, exclude = ex))),
               gaus = exp(as.numeric(predict(mg, nd, exclude = ex)))) %>%
  mutate(pct_diff = 100*(gaus - scat)/scat)
cat("\n=== predicted weight (kg): Gaussian vs scaled-t ===\n")
print(as.data.frame(pred %>% mutate(across(where(is.numeric), ~round(.x, 4)))),
      row.names = FALSE)
cat(sprintf("\n  max |difference| in predicted weight: %.2f%%\n", max(abs(pred$pct_diff))))

saveRDS(list(mg = mg, cmp = cmp, pred = pred),
        file.path(OUT, "final_model_gaussian.rds"))
write_csv(cmp,  file.path(OUT, "TableR63_scat_vs_gaussian_insample.csv"))
write_csv(pred, file.path(OUT, "TableR64_predicted_weight_by_family.csv"))
message("38_gaussian_counterpart.R complete")
