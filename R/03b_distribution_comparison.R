# ==============================================================================
# DISTRIBUTION COMPARISON: GAUSSIAN VS SCALED-T
# Quantitative diagnostics for reviewer response
# Inputs: mod_gauss (Gaussian family GAM), mod_scat (scat family GAM)
# ==============================================================================

library(tidyverse)
library(mgcv)

compare_distributions <- function(mod_gauss, mod_scat, output_dir = "./output/") {
  
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # ---------------------------------------------------------------------------
  # 1. EXTRACT PARAMETERS
  # ---------------------------------------------------------------------------
  theta <- mod_scat$family$getTheta(TRUE)
  scat_df <- theta[1]
  
  # Use empirical SD of response residuals for prediction intervals
  sigma_gauss <- sd(residuals(mod_gauss, type = "response"))
  sigma_scat <- sd(residuals(mod_scat, type = "response"))
  
  cat("Gaussian sigma:", round(sigma_gauss, 4), "\n")
  cat("Scaled-t sigma:", round(sigma_scat, 4), ", df:", round(scat_df, 2), "\n")
  
  # ---------------------------------------------------------------------------
  # 2. COMPUTE DEVIANCE RESIDUALS (as used by gam.check)
  # ---------------------------------------------------------------------------
  resid_gauss <- residuals(mod_gauss, type = "deviance")
  resid_scat <- residuals(mod_scat, type = "deviance")
  
  # ---------------------------------------------------------------------------
  # 3. Q-Q DEVIATION SUMMARIES (both vs normal, as per gam.check)
  # ---------------------------------------------------------------------------
  calc_qq_deviations <- function(resid) {
    n <- length(resid)
    theoretical_q <- qnorm(ppoints(n))
    sample_q <- sort(resid)
    
    probs <- c(0.01, 0.05, 0.95, 0.99)
    idx <- round(probs * n)
    deviations <- sample_q[idx] - theoretical_q[idx]
    names(deviations) <- paste0("q", probs * 100)
    
    list(
      max_abs_dev = max(abs(sample_q - theoretical_q)),
      prop_beyond_2sd = mean(abs(resid) > 2),
      prop_beyond_3sd = mean(abs(resid) > 3),
      tail_deviations = deviations
    )
  }
  
  qq_gauss <- calc_qq_deviations(resid_gauss)
  qq_scat <- calc_qq_deviations(resid_scat)
  
  # ---------------------------------------------------------------------------
  # 4. PROBABILITY INTEGRAL TRANSFORM (PIT)
  # ---------------------------------------------------------------------------
  pit_gauss <- pnorm(resid_gauss)
  pit_scat <- pnorm(resid_scat)
  
  calc_pit_chisq <- function(pit_vals, n_bins = 10) {
    expected <- length(pit_vals) / n_bins
    observed <- hist(pit_vals, breaks = seq(0, 1, length.out = n_bins + 1), plot = FALSE)$counts
    sum((observed - expected)^2 / expected)
  }
  
  pit_chisq_gauss <- calc_pit_chisq(pit_gauss)
  pit_chisq_scat <- calc_pit_chisq(pit_scat)
  
  # ---------------------------------------------------------------------------
  # 5. PREDICTION INTERVAL WIDTHS ACROSS LENGTH RANGE
  # ---------------------------------------------------------------------------
  dat <- mod_scat$model
  pred_grid <- data.frame(loglen = seq(min(dat$loglen), max(dat$loglen), length.out = 50))
  
  for(v in names(dat)) {
    if(v %in% c("logwt", "loglen")) next
    if(is.factor(dat[[v]])) {
      pred_grid[[v]] <- factor(names(sort(table(dat[[v]]), decreasing = TRUE))[1], 
                               levels = levels(dat[[v]]))
    } else if(is.numeric(dat[[v]])) {
      pred_grid[[v]] <- median(dat[[v]], na.rm = TRUE)
    }
  }
  
  excl <- "s(obs_name)"
  pred_gauss <- predict(mod_gauss, pred_grid, se.fit = TRUE, exclude = excl)
  pred_scat <- predict(mod_scat, pred_grid, se.fit = TRUE, exclude = excl)
  
  # 95% prediction intervals
  pi_width_gauss <- 2 * qnorm(0.975) * sqrt(pred_gauss$se.fit^2 + sigma_gauss^2)
  pi_width_scat <- 2 * qt(0.975, df = scat_df) * sqrt(pred_scat$se.fit^2 + sigma_scat^2)
  
  pred_grid$length_cm <- exp(pred_grid$loglen)
  pred_grid$pi_width_gauss <- pi_width_gauss
  pred_grid$pi_width_scat <- pi_width_scat
  pred_grid$pi_ratio <- pi_width_scat / pi_width_gauss
  pred_grid$pi_width_gauss_kg <- exp(pred_gauss$fit + pi_width_gauss/2) - 
    exp(pred_gauss$fit - pi_width_gauss/2)
  pred_grid$pi_width_scat_kg <- exp(pred_scat$fit + pi_width_scat/2) - 
    exp(pred_scat$fit - pi_width_scat/2)
  
  # ---------------------------------------------------------------------------
  # 6. CREATE MULTI-PANEL FIGURE
  # ---------------------------------------------------------------------------
  png(paste0(output_dir, "/Fig_distribution_comparison.png"), 
      width = 10, height = 8, units = "in", res = 300)
  
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))
  
  # Panel A: Q-Q plot comparison
  n_gauss <- length(resid_gauss)
  n_scat <- length(resid_scat)
  theor_gauss <- qnorm(ppoints(n_gauss))
  theor_scat <- qnorm(ppoints(n_scat))
  yl <- range(c(sort(resid_gauss), sort(resid_scat)))
  xl <- range(c(theor_gauss, theor_scat))
  
  plot(theor_gauss, sort(resid_gauss), pch = 16, cex = 0.3, col = "red",
       xlab = "Theoretical quantiles", ylab = "Deviance residuals",
       main = "A. Q-Q plots", ylim = yl, xlim = xl)
  abline(0, 1, lty = 2, lwd = 1.5)
  points(theor_scat, sort(resid_scat), pch = 16, cex = 0.3, col = "blue")
  legend("topleft", legend = c("Gaussian", "Scaled-t"), col = c("red", "blue"), 
         pch = 16, bty = "n")
  
  # Panel B: PIT histograms
  hist(pit_gauss, breaks = 20, freq = FALSE, col = rgb(1, 0, 0, 0.4),
       main = "B. PIT histograms", xlab = "PIT value", ylim = c(0, 10))
  hist(pit_scat, breaks = 20, freq = FALSE, col = rgb(0, 0, 1, 0.4), add = TRUE)
  abline(h = 1, lty = 2, lwd = 2)
  legend("topright", legend = c("Gaussian", "Scaled-t"), 
         fill = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)), bty = "n")
  
  # Panel C: Prediction interval widths
  plot(pred_grid$length_cm, pi_width_gauss, type = "l", col = "red", lwd = 2,
       xlab = "Length (cm)", ylab = "95% PI width (log-weight)",
       main = "C. Prediction interval widths", 
       ylim = c(0,max(c(pi_width_gauss, pi_width_scat))))
  lines(pred_grid$length_cm, pi_width_scat, col = "blue", lwd = 2)
  legend("topright", legend = c("Gaussian", "Scaled-t"), col = c("red", "blue"), 
         lwd = 2, bty = "n")
  
  # Panel D: PI width ratio
  plot(pred_grid$length_cm, pred_grid$pi_ratio, type = "l", col = "black", lwd = 2,
       xlab = "Length (cm)", ylab = "PI width ratio (scat / Gaussian)",
       main = "D. Relative interval width", ylim=c(1,1.1*max(pred_grid$pi_ratio)))
  abline(h = 1, lty = 2, col = "gray50")
  
  mtext("Gaussian vs Scaled-t Distribution Comparison", outer = TRUE, cex = 1.2)
  dev.off()
  
  # ---------------------------------------------------------------------------
  # 7. CREATE SUMMARY TABLE
  # ---------------------------------------------------------------------------
  summary_table <- tibble(
    Metric = c(
      "AIC", "Residual SD", "Estimated df (scat)", "Max Q-Q deviation",
      "Proportion |resid| > 2", "Proportion |resid| > 3", 
      "Q-Q deviation at 1st percentile", "Q-Q deviation at 99th percentile",
      "PIT chi-squared (10 bins)", "Mean 95% PI width (log scale)",
      "95% PI width at 80 cm (kg)", "95% PI width at 150 cm (kg)"
    ),
    Gaussian = c(
      round(AIC(mod_gauss), 1), round(sigma_gauss, 4), NA, round(qq_gauss$max_abs_dev, 3),
      round(qq_gauss$prop_beyond_2sd, 4), round(qq_gauss$prop_beyond_3sd, 4),
      round(qq_gauss$tail_deviations["q1"], 3), round(qq_gauss$tail_deviations["q99"], 3),
      round(pit_chisq_gauss, 1), round(mean(pi_width_gauss), 3),
      round(pred_grid$pi_width_gauss_kg[which.min(abs(pred_grid$length_cm - 80))], 2),
      round(pred_grid$pi_width_gauss_kg[which.min(abs(pred_grid$length_cm - 150))], 2)
    ),
    Scaled_t = c(
      round(AIC(mod_scat), 1), round(sigma_scat, 4), round(scat_df, 2), round(qq_scat$max_abs_dev, 3),
      round(qq_scat$prop_beyond_2sd, 4), round(qq_scat$prop_beyond_3sd, 4),
      round(qq_scat$tail_deviations["q1"], 3), round(qq_scat$tail_deviations["q99"], 3),
      round(pit_chisq_scat, 1), round(mean(pi_width_scat), 3),
      round(pred_grid$pi_width_scat_kg[which.min(abs(pred_grid$length_cm - 80))], 2),
      round(pred_grid$pi_width_scat_kg[which.min(abs(pred_grid$length_cm - 150))], 2)
    )
  )
  
  write_csv(summary_table, paste0(output_dir, "Table_distribution_comparison.csv"))
  cat("\nOutputs saved to:", output_dir, "\n")
  
  invisible(list(scat_df = scat_df, sigma_scat = sigma_scat, sigma_gauss = sigma_gauss,
                 qq_gauss = qq_gauss, qq_scat = qq_scat,
                 pred_intervals = pred_grid, summary_table = summary_table))
}
