# ==============================================================================
# 77_observer_trip_slope_reml.R -- How the variation in slope divides between trips and observers
#
# Section 4.6. Model A with an observer slope added, fitted under gam(REML).
#
# ENGINE AND TIMING: one gam(REML) fit; about 40 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
suppressMessages(library(digest))
JOBDIR <- file.path(OUT, "random_smooth_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()

F_OS <- "logwt ~ s(loglen, k = 20) + s(obs_name, bs = 're') + sexf +
  te(lon, lat, mon, k = c(6, 6, 4), bs = c('cr', 'cr', 'cc')) + yrf + s(tripf, bs = 're') + s(loglen_c, tripf, bs = 're') + s(loglen_c, obs_name, bs = 're')"
outf <- file.path(JOBDIR, sprintf("OS_n%d_t%d_%s.rds", nrow(d), nlevels(d$tripf), substr(digest(F_OS), 1, 8)))
if (file.exists(outf)) m <- readRDS(outf) else {
  t0 <- Sys.time()
  m <- gam(as.formula(F_OS), data = d, family = scat, method = "REML", gamma = 1.4)
  m$fit_minutes <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(m, outf)
}
cat(sprintf("two-slope model: %.1f min | REML %.2f | AIC %.1f\n", m$fit_minutes, -m$gcv.ubre, AIC(m)))

summ <- function(m, id) {
  vc <- gam.vcomp(m)
  vc <- if (is.list(vc)) vc$vc[, "std.dev"] else vc[, "std.dev"]
  lab <- sapply(m$smooth, function(z) z$label)
  bind_rows(lapply(which(sapply(m$smooth, inherits, "random.effect")), function(i) {
    ii <- m$smooth[[i]]$first.para:m$smooth[[i]]$last.para
    data.frame(model = id, term = lab[i], sd_vcomp = unname(vc[lab[i]]),
               sd_predicted = sd(coef(m)[ii]), edf = sum(m$edf[ii]))
  }))
}
fA <- list.files(JOBDIR, "^A_n", full.names = TRUE)
res <- summ(m, "A + observer slope")
if (length(fA)) { mA <- readRDS(fA[1]); res <- bind_rows(summ(mA, "A"), res)
  cat(sprintf("Model A (REML): REML %.2f | AIC %.1f  => adding the observer slope: dREML %+.2f, dAIC %+.1f\n",
              -mA$gcv.ubre, AIC(mA), (-m$gcv.ubre) - (-mA$gcv.ubre), AIC(m) - AIC(mA))) }
res$pct_vcomp <- ifelse(grepl("loglen_c", res$term), NA, 100 * (exp(res$sd_vcomp) - 1))
res$pct_predicted <- ifelse(grepl("loglen_c", res$term), NA, 100 * (exp(res$sd_predicted) - 1))
print(res, row.names = FALSE, digits = 4)
sl <- res[res$model == "A + observer slope" & grepl("loglen_c", res$term), ]
cat(sprintf("\nslope variance: trip %.4f + observer %.4f = %.4f (combined SD %.4f); observer share %.0f%%\n",
            sl$sd_vcomp[grepl("tripf", sl$term)]^2, sl$sd_vcomp[grepl("obs", sl$term)]^2,
            sum(sl$sd_vcomp^2), sqrt(sum(sl$sd_vcomp^2)),
            100 * sl$sd_vcomp[grepl("obs", sl$term)]^2 / sum(sl$sd_vcomp^2)))
write_csv(res, file.path(OUT, "TableR121_slope_components_reml.csv"))
message("77_observer_trip_slope_reml.R complete")
