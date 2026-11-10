# ==============================================================================
# 72_distribution_figure.R -- Supplementary figure comparing the scaled-t and Gaussian residual distributions
#
# Supplementary material.
#
# ENGINE AND TIMING: prediction only; under a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")

fig_dir <- file.path(FIG, "manuscript")
fin  <- readRDS(file.path(OUT, "final_model.rds"))
gaus <- readRDS(file.path(OUT, "final_model_gaussian.rds"))
d    <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
m <- fin$m; mg <- gaus$mg

th   <- m$family$getTheta(TRUE); NU <- th[1]; SIG_T <- th[2]
SIG_G <- sqrt(mg$sig2)
cat(sprintf("scaled-t: df %.2f, scale %.4f | Gaussian: sd %.4f\n", NU, SIG_T, SIG_G))

ps <- predict(m,  d, se.fit = TRUE)
pg <- predict(mg, d, se.fit = TRUE)
mu_s <- as.numeric(ps$fit); se_s <- as.numeric(ps$se.fit)
mu_g <- as.numeric(pg$fit); se_g <- as.numeric(pg$se.fit)

# --- interval half-widths for a new observation from a seen trip --------------
w_s <- 2 * qt(0.975, NU) * sqrt(SIG_T^2 + se_s^2)
w_g <- 2 * qnorm(0.975)  * sqrt(SIG_G^2 + se_g^2)
ratio <- w_s / w_g
cat(sprintf("\n95%% prediction interval width, log scale:\n"))
cat(sprintf("  scaled-t  median %.4f (range %.4f-%.4f)\n", median(w_s), min(w_s), max(w_s)))
cat(sprintf("  Gaussian  median %.4f (range %.4f-%.4f)\n", median(w_g), min(w_g), max(w_g)))
cat(sprintf("  RATIO     median %.3f (range %.3f-%.3f)  ->  scaled-t is %.0f%% %s\n",
            median(ratio), min(ratio), max(ratio),
            100*abs(median(ratio) - 1), ifelse(median(ratio) > 1, "WIDER", "NARROWER")))
cat("  The caption inherited from the previous version says 'approximately 50%\n")
cat("  wider'. Compare that with the measured value above before using it.\n")

# --- PIT ----------------------------------------------------------------------
pit_s <- pt((d$logwt - mu_s)/SIG_T, df = NU)
pit_g <- pnorm((d$logwt - mu_g)/SIG_G)
ks_s <- suppressWarnings(ks.test(pit_s, "punif")$statistic)
ks_g <- suppressWarnings(ks.test(pit_g, "punif")$statistic)
cat(sprintf("\nPIT departure from uniform (Kolmogorov-Smirnov D, smaller is better):\n"))
cat(sprintf("  scaled-t %.4f | Gaussian %.4f\n", ks_s, ks_g))

# --- the figure ---------------------------------------------------------------
ord <- order(d$length)
bin <- d %>% mutate(w_s = w_s, w_g = w_g, lb = round(length/5)*5) %>%
  group_by(lb) %>% summarise(n = dplyr::n(), w_s = median(w_s), w_g = median(w_g),
                             .groups = "drop") %>% filter(n >= 20)

save_fig <- function(name, width, height, expr, res_png = 300, res_tiff = 600) {
  png(file.path(fig_dir, paste0(name, ".png")), width = width, height = height,
      units = "in", res = res_png); eval(expr); dev.off()
  tiff(file.path(fig_dir, paste0(name, ".tiff")), width = width, height = height,
       units = "in", res = res_tiff, compression = "lzw"); eval(expr); dev.off()
  cat("Saved:", name, "\n")
}

save_fig("FigS_distribution_comparison", width = 10, height = 8, quote({
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
  # qq.gam compares each model's residuals with ITS OWN reference distribution,
  # by simulation from the fitted family. Overlaying raw deviance residuals from
  # the two models on shared normal axes is meaningless: mgcv does not
  # standardise them, so the Gaussian set has SD ~0.098 and the scaled-t set is
  # on a different scale again.
  qq.gam(mg, rep = 100, level = 0.9, pch = 16, cex = 0.4, rl.col = "#A03A3A",
         col = adjustcolor("#A03A3A", 0.25), main = "(A) Gaussian errors")
  qq.gam(m,  rep = 100, level = 0.9, pch = 16, cex = 0.4, rl.col = "#2E7D5B",
         col = adjustcolor("#2E7D5B", 0.25), main = "(B) Scaled-t errors")

  hist(pit_g, breaks = 20, freq = FALSE, col = adjustcolor("#A03A3A", 0.5),
       border = NA, main = "(C) Probability integral transform", las = 1,
       xlab = "PIT value", ylim = c(0, 2))
  hist(pit_s, breaks = 20, freq = FALSE, col = adjustcolor("#2E7D5B", 0.5),
       border = NA, add = TRUE)
  abline(h = 1, lty = 2, lwd = 2)
  legend("topright", c("Gaussian", "Scaled-t", "uniform"),
         fill = c(adjustcolor("#A03A3A", .5), adjustcolor("#2E7D5B", .5), NA),
         border = NA, lty = c(NA, NA, 2), bty = "n", cex = 0.9)

  plot(bin$lb, bin$w_g, type = "l", lwd = 2, col = "#A03A3A", las = 1,
       ylim = range(c(bin$w_g, bin$w_s)),
       main = "(D) 95% prediction interval width",
       xlab = "Fork length (cm)", ylab = "Width, log-weight scale")
  lines(bin$lb, bin$w_s, lwd = 2, col = "#2E7D5B")
  legend("topright", c("Gaussian", "Scaled-t"), col = c("#A03A3A", "#2E7D5B"),
         lwd = 2, bty = "n", cex = 0.9)
}))

saveRDS(list(nu = NU, sig_t = SIG_T, sig_g = SIG_G,
             ratio_median = median(ratio), ratio_range = range(ratio),
             ks_scat = ks_s, ks_gauss = ks_g, binned = bin),
        file.path(OUT, "distribution_figure.rds"))
write_csv(bin, file.path(OUT, "TableS_interval_widths.csv"))
message("72_distribution_figure.R complete")
