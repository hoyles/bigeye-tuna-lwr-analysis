# ==============================================================================
# 04_generate_figures.R
# Generate all manuscript figures for Fisheries Research submission
# Outputs to ../figures_resubmit/ in both PNG (preview) and TIFF (submission)
#
# Prerequisites:
#   - Run 01_initialise.R, 02_load_characterise_prepare_data.R first
#   - Model objects saved by 03_analysis_resubmit.R must exist in ./resubmit/store/
#   - Requires: tidyverse, mgcv, maps, readxl, sf, metR, magick, gammit, jsonlite
#
# Best models (from backward AIC selection, ML, scat family):
#   Female RGT:    mod10rgtfx_ml_scat
#   Male RGT:      mod10rgtmx_ml_scat
#   Joint RGT:     mod10rgtbx_ml_scat (2-sex equivalent of female best)
#   Gaussian equiv: mod10rgtfx_ml_gaus (for distribution comparison)
# ==============================================================================

library(tidyverse)
library(mgcv)
library(maps)
library(readxl)
library(magick)
library(gammit)
library(jsonlite)

source("support_functions.R")
source("comprehensive_plot.R")
source("03b_distribution_comparison.R")

# --- Output directory ---------------------------------------------------------
fig_dir <- "../figures_resubmit2"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# --- Load data ----------------------------------------------------------------
load("../resubmit/store/data_for_gams_cleanobs2.RData")
# Provides: drggm_ro, drggf_ro, drgtf_ro, drgtm_ro, drgtb_ro

# Full dataset (needed for Figs 1-7)
load("../data/processed/dat.Rdata")
# Provides: dat, da2

# --- Load model objects -------------------------------------------------------
load("../resubmit/store/mods_gtf_ml_scat_cleanobs.RData")
# Provides: mod10rgtfx_ml_scat (+ other female RGT scat models)
load("../resubmit/store/mod10rgtfx_ml_gaus.RData")
# Provides: mod10rgtfx_ml_gaus (Gaussian equivalent for Fig 8a)
load("../resubmit/store/mods_gtm_ml_scat_cleanobs.RData")
# Provides: mod10rgtmx_ml_scat (+ other male RGT scat models)
load("../resubmit/store/mods_gtb_ml_scat_cleanobs.RData")
# Provides: mod10rgtbx_ml_scat, mod10rgtbx_ml_scat_lin, mod13crgtbxy_ml (+ others)

# --- Helper: save to both PNG and TIFF ----------------------------------------
# Wraps a plotting expression, writing to both formats.
# Usage: save_fig("Fig01_name", width=10, height=8, { plot code here })
save_fig <- function(name, width, height, expr, res_png = 300, res_tiff = 600) {
  # PNG (for preview / quick checks)
  png(file.path(fig_dir, paste0(name, ".png")),
      width = width, height = height, units = "in", res = res_png)
  eval(expr)
  dev.off()
  
  # TIFF (for journal submission, LZW compressed)
  tiff(file.path(fig_dir, paste0(name, ".tiff")),
       width = width, height = height, units = "in", res = res_tiff,
       compression = "lzw")
  eval(expr)
  dev.off()
  
  cat("Saved:", name, "\n")
}

# For ggplot objects, use ggsave instead
save_ggfig <- function(name, p, width, height, dpi_png = 300, dpi_tiff = 600) {
  ggsave(file.path(fig_dir, paste0(name, ".png")),  p, width = width, height = height, dpi = dpi_png)
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), p, width = width, height = height, dpi = dpi_tiff,
         compression = "lzw")
  cat("Saved:", name, "\n")
}


# ==============================================================================
# FIGURE 1: Published LWRs for bigeye tuna
# Source: supplementary LWR spreadsheet
# ==============================================================================

lwr_all <- read_excel("../tables/TableS1_supp_LWR_list.xlsx")
lwr_all <- data.frame(lwr_all)
lwr_all[, 1] <- NULL
names(lwr_all) <- tolower(names(lwr_all))
lwr_all <- filter(lwr_all, use.not == "O")

getfl <- function(x, ii = 1) {
  if (is.na(x)) return(NA)
  a <- as.numeric(strsplit(x, "-")[[1]])
  return(a[ii])
}
lwr_all$flmin <- sapply(lwr_all$fl.range, getfl, ii = 1)
lwr_all$flmax <- sapply(lwr_all$fl.range, getfl, ii = 2)
lwr_all$ocean <- factor(lwr_all$ocean)

save_fig("Fig01_published_LWRs", width = 9, height = 9, quote({
  plot(c(20, max(lwr_all$flmax, na.rm = TRUE)), c(0, 290),
       type = "n", xlab = "Length (cm)", ylab = "Weight (kg)",
       cex.lab = 1.2)
  for (i in 1:nrow(lwr_all)) {
    if (!is.na(lwr_all$flmin[i])) {
      len_seq <- seq(lwr_all$flmin[i], lwr_all$flmax[i], by = 1)
      wt_seq  <- lwr_all$a[i] * len_seq^lwr_all$b[i]
      lines(len_seq, wt_seq, col = i, lty = as.numeric(lwr_all$ocean[i]))
    }
  }
  use <- !is.na(lwr_all$flmin)
  legend("topleft", legend = lwr_all$ref.code[use],
         col = (1:nrow(lwr_all))[use],
         lty = as.numeric(lwr_all$ocean)[use], lwd = 1, ncol = 2, cex = 1,
         y.intersp = 1)
}))


# ==============================================================================
# FIGURE 2: Sampling location maps by process type + region map
# NOTE: Requires 'dat' from 02_load_characterise_prepare_data.R
# Bottom-right panel shows stock assessment regions (R1-R11)
# ==============================================================================

# Define region polygons (also used later for Fig 12)
regions_json <- '{
  "fishery_regions": [
    {"id": "R1", "nodes": [[120, 50], [170, 50], [170, 10], [140, 10], [140, 20], [120, 20], [120, 50]]},
    {"id": "R2", "nodes": [[170, 50], [-150, 50], [-150, 10], [170, 10], [170, 50]]},
    {"id": "R3", "nodes": [[140, 10], [170, 10], [170, -10], [160, -10], [160, -5], [155, -5], [155, 0], [140, 0], [140, 10]]},
    {"id": "R4", "nodes": [[170, 10], [-150, 10], [-150, -10], [170, -10], [170, 10]]},
    {"id": "R5", "nodes": [[140, -10], [170, -10], [170, -40], [140, -40], [140, -20], [150, -20], [150, -15], [140, -15], [140, -10]]},
    {"id": "R6", "nodes": [[170, -10], [-150, -10], [-150, -40], [170, -40], [170, -10]]},
    {"id": "R7", "nodes": [[110, 20], [140, 20], [140, -10], [110, -10], [110, 20]]},
    {"id": "R8", "nodes": [[140, 0], [155, 0], [155, -5], [160, -5], [160, -10]]},
    {"id": "R9", "nodes": [[140, -15], [150, -15], [150, -20], [140, -20], [140, -15]]},
    {"id": "R10", "nodes": [[-150, 10], [-90, 10], [-90, -40], [-150, -40], [-150, 10]]},
    {"id": "R11", "nodes": [[-150, 50], [-110, 50], [-110, 10], [-150, 10], [-150, 50]]}
  ]
}'

regions_list <- fromJSON(regions_json)$fishery_regions

save_fig("Fig02_samplemap_process", width = 12, height = 11, quote({
  ptypes <- sort(unique(dat$processtype))
  par(mfrow = c(3, 2), mar = c(5, 4, 3, 2))
  for (pr in ptypes) {
    datx <- filter(dat, processtype == pr)
    a <- tapply(datx$length, list(datx$lon, datx$lat), length)
    image(sort(unique(datx$lon)), sort(unique(datx$lat)), log10(a),
          main = pr, xlim = c(120, 250), ylim = c(-40, 40),
          xlab = "Longitude", ylab = "Latitude")
    contour(sort(unique(datx$lon)), sort(unique(datx$lat)), a,
            add = TRUE, labcex = 0.8,
            levels = c(10, 100, 1000, 2000, 3000, 5000, 7000, 10000))
    map(add = TRUE, database = "world2", fill = TRUE)
  }
  
  # 6th panel: Stock assessment regions
  plot(1, type = "n", xlim = c(110, 270), ylim = c(-40, 50),
       xlab = "Longitude", ylab = "Latitude", main = "Assessment regions",
       xaxt = "n")
  axis(1, at = seq(120, 260, 20))
  map(database = "world2", fill = TRUE, col = "grey80", add = TRUE)
  
  # Region colours (semi-transparent fills)
  reg_cols <- c(
    R1  = "#E41A1C40", R2  = "#377EB840", R3  = "#4DAF4A40",
    R4  = "#984EA340", R5  = "#FF7F0040", R6  = "#FFFF3340",
    R7  = "#A6562840", R8  = "#F781BF40", R9  = "#99999940",
    R10 = "#66C2A540", R11 = "#FC8D6240"
  )
  
  # Fix R8: original nodes don't close properly (diagonal from bottom-right
  
  # back to top-left). Add closing nodes along the bottom edge.
  for (i in 1:nrow(regions_list)) {
    nodes <- regions_list$nodes[[i]]
    lon <- ifelse(nodes[, 1] < 0, nodes[, 1] + 360, nodes[, 1])
    lat <- nodes[, 2]
    rid <- regions_list$id[i]
    # R8 needs extra nodes to close via bottom edge
    if (rid == "R8") {
      lon <- c(lon, 140)
      lat <- c(lat, -10)
    }
    polygon(lon, lat, col = reg_cols[rid], border = "black", lwd = 1.5)
  }
  
  # Manual label positions (lon in 0-360)
  lab_pos <- data.frame(
    id  = c("R1",  "R2",  "R3",  "R4",  "R5",  "R6",  "R7",  "R8",  "R9",  "R10", "R11"),
    lon = c( 145,   185,   155,   185,   160,   185,   135,   152,   147,    230,   230),
    lat = c(  30,    30,     3,     0,   -25,   -25,    15,    -7,   -17.5,    -15,    30)
  )
  text(lab_pos$lon, lab_pos$lat, lab_pos$id, cex = 1.1, font = 2)
}))


# ==============================================================================
# FIGURE 3: Summary plots (samples/year, observers/year, etc.)
# NOTE: Requires 'dat' from 02_load_characterise_prepare_data.R
# ==============================================================================

save_fig("Fig03_summary_plots", width = 8, height = 10, quote({
  par(mfrow = c(3, 2))
  barplot(table(dat$year[dat$year > 2008]),
          xlab = "Year", ylab = "Samples per year")
  
  a <- table(dat$year, dat$obs_name)
  tmp <- function(x) sum(x > 0)
  barplot(apply(a[rownames(a) != "2008", ], 1, tmp),
          xlab = "Year", ylab = "Unique observers per year")
  
  a <- table(dat$year, dat$tripID)
  barplot(apply(a[rownames(a) != "2008", ], 1, tmp),
          xlab = "Year", ylab = "Unique trips per year")
  
  a <- table(dat$year, paste(dat$obs_name, dat$workdate))
  barplot(apply(a[rownames(a) != "2008", ], 1, tmp),
          xlab = "Year", ylab = "Unique sets per year")
  
  barplot(table(dat$year) / apply(a, 1, tmp),
          xlab = "Year", ylab = "Mean samples per set")
}))


# ==============================================================================
# FIGURE 4: Samples by sex and process type per year
# ==============================================================================

save_fig("Fig04_samples_by_sex_process", width = 14, height = 7, quote({
  par(mfrow = c(1, 2))
  with(dat[dat$year > 2008, ],
       barplot(table(sex, year), legend.text = TRUE, col = 1:5,
               args.legend = list(x = "topleft", title = "Sex"),
               xlab = "Year", ylab = "Number of samples", ylim = c(0, 15000)))
  with(dat[dat$year > 2008, ],
       barplot(table(processtype, year), legend.text = TRUE, col = 1:5,
               args.legend = list(x = "topleft", title = "Process type"),
               xlab = "Year", ylab = "Number of samples", ylim = c(0, 15000)))
}))


# ==============================================================================
# FIGURE 5: Length frequency by process type
# ==============================================================================

save_fig("Fig05_lenfreq_process", width = 10, height = 7.5, quote({
  ptypes <- sort(unique(dat$processtype))
  i <- 1
  plot(1:10, 1:10, xlim = c(60, 200), ylim = c(0, 1), type = "n",
       xlab = "Fork length (cm)", ylab = "Relative frequency")
  for (pr in ptypes) {
    datx <- filter(dat, processtype == pr, length > 60, length < 200)
    a <- table(datx$length)
    a <- a / max(a)
    points(as.numeric(names(a)), a, col = i, pch = i, type = "b", lty = 2)
    i <- i + 1
  }
  legend("topright", legend = ptypes, col = 1:5, pch = 1:5, lty = 2)
}))


# ==============================================================================
# FIGURE 6: Weight and length frequency histograms (rounding visible)
# NOTE: Requires 'da2' from 02_load_characterise_prepare_data.R
# ==============================================================================

save_fig("Fig06_weight_length_freq", width = 10, height = 12, quote({
  par(mfrow = c(2, 1))
  hist(da2$weight, nclass = 250, xlim = c(0, 120), axes = FALSE,
       main = "Histogram of weights", xlab = "Weight (kg)")
  axis(1, at = seq(0, 120, 5)); axis(2); box()
  for (xx in seq(0, 120, 5)) abline(v = xx, lty = 2, col = "grey70")
  
  hist(da2$length, nclass = 141, xlim = c(60, 200), axes = FALSE,
       main = "Histogram of lengths", xlab = "Length (cm)")
  axis(1, at = seq(60, 200, 5)); axis(2); box()
  for (xx in seq(60, 200, 5)) abline(v = xx, lty = 2, col = "grey70")
}))


# ==============================================================================
# FIGURE 7: Rounding patterns by observer
# NOTE: Requires 'da2' from 02_load_characterise_prepare_data.R
# ==============================================================================

save_fig("Fig07_rounding_by_observer", width = 7, height = 7, quote({
  nmax <- 200
  par(mfrow = c(1, 2), mar = c(5, 2, 3, 1), oma = c(0, 2, 0, 0))
  
  # Weight rounding
  a <- table(da2$obs_name, da2$weight %% 5)
  a <- a[apply(a, 1, sum) > nmax, ]
  a <- a[, 1:5]
  wv5 <- a / apply(a, 1, sum)
  plot(0:4, wv5[1, ], type = "n", ylim = c(0, 1),
       xlab = "Remainder", ylab = "", main = "Weights", cex.lab = 1.2)
  for (i in 1:nrow(wv5)) points(0:4, wv5[i, ], col = i, pch = 1, type = "b")
  points(0:4, table(da2$weight %% 5) / length(da2$weight),
         col = 1, pch = 0, type = "b", lwd = 2, cex = 1.5)
  
  # Length rounding
  a <- table(da2$obs_name, da2$length %% 5)
  a <- a[apply(a, 1, sum) > nmax, ]
  a <- a[, 1:5]
  wv5 <- a / apply(a, 1, sum)
  plot(0:4, wv5[1, ], type = "n", ylim = c(0, 1),
       xlab = "Remainder", ylab = "", main = "Lengths", cex.lab = 1.2)
  for (i in 1:nrow(wv5)) points(0:4, wv5[i, ], col = i, pch = 1, type = "b")
  points(0:4, table(da2$length %% 5) / length(da2$length),
         col = 1, pch = 0, type = "b", lwd = 2, cex = 1.2)
  
  mtext("Proportions", side = 2, line = 0.8, outer = TRUE, cex = 1.2)
}))


# ==============================================================================
# FIGURE 8: Two-panel comparison
#   Panel a: Gaussian vs scaled-t weight differences
#   Panel b: Sex-specific vs joint model weight differences
# CORRECTED: Uses mod10rgtfx_ml_gaus vs mod10rgtfx_ml_scat (not WGG models)
# ==============================================================================

save_fig("Fig08_gauss_vs_scat_and_sex", width = 12, height = 6, quote({
  par(mfrow = c(1, 2), mar = c(5, 4.5, 3, 1))
  
  # --- Shared prediction grid ---
  excl <- c("s(obs_name)", "te(lon,lat,mon)", "s(hbf)", "yrf")
  new_data <- with(drgtf_ro,
                   expand.grid(
                     hbf    = median(hbf),
                     loglen = seq(min(loglen), max(loglen), length.out = 200),
                     logwt  = median(logwt),
                     obs_name = factor(obs_name[1], levels = levels(obs_name)),
                     mon    = median(mon),
                     lat    = median(lat),
                     lon    = median(lon),
                     yrf    = yrf[1],
                     year   = median(year)
                   ))
  len <- exp(new_data$loglen)
  
  # --- Panel a: Gaussian vs scaled-t ---
  pred_scat  <- predict(mod10rgtfx_ml_scat, new_data, exclude = excl,
                        type = "link", se.fit = TRUE, unconditional = TRUE)
  pred_gauss <- predict(mod10rgtfx_ml_gaus, new_data, exclude = excl,
                        type = "link", se.fit = TRUE, unconditional = TRUE)
  
  wt_diff_a <- exp(pred_gauss$fit) - exp(pred_scat$fit)
  
  # Reference band: ±2SE of the preferred (scat) model on weight scale
  scat_ci_up <- exp(pred_scat$fit + 2 * pred_scat$se.fit) - exp(pred_scat$fit)
  scat_ci_lo <- exp(pred_scat$fit - 2 * pred_scat$se.fit) - exp(pred_scat$fit)
  
  yl_a <- range(c(wt_diff_a, scat_ci_up, scat_ci_lo))
  plot(len, wt_diff_a, type = "l", lwd = 2, ylim = yl_a,
       xlab = "Length (cm)", ylab = "Weight difference (kg)",
       main = "a) Gaussian minus scaled-t")
  lines(len, scat_ci_up, lty = 2, col = "grey50")
  lines(len, scat_ci_lo, lty = 2, col = "grey50")
  abline(h = 0, lty = 2, col = "grey50")
  
  # --- Panel b: Sex-specific vs joint ---
  predF <- predict(mod10rgtfx_ml_scat, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  predM <- predict(mod10rgtmx_ml_scat, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  predJ <- predict(mod10rgtbx_ml_scat, new_data, exclude = excl,
                   type = "link", se.fit = TRUE, unconditional = TRUE)
  
  wt_diffF <- exp(predF$fit) - exp(predJ$fit)
  wt_diffM <- exp(predM$fit) - exp(predJ$fit)
  
  # Reference band: ±2SE of the preferred (joint) model on weight scale
  joint_ci_up <- exp(predJ$fit + 2 * predJ$se.fit) - exp(predJ$fit)
  joint_ci_lo <- exp(predJ$fit - 2 * predJ$se.fit) - exp(predJ$fit)
  
  yl_b <- range(c(wt_diffF, wt_diffM, joint_ci_up, joint_ci_lo))
  plot(len, wt_diffF, type = "l", lwd = 2, col = 2, ylim = yl_b,
       xlab = "Length (cm)", ylab = "Weight difference (kg)",
       main = "b) Sex-specific minus joint")
  lines(len, wt_diffM,    lwd = 2, col = 4)
  lines(len, joint_ci_up, lty = 2, col = "grey50")
  lines(len, joint_ci_lo, lty = 2, col = "grey50")
  abline(h = 0, lty = 2, col = "grey50")
  legend("topleft", legend = c("Female - Joint", "Male - Joint"),
         col = c(2, 4), lwd = 2, bty = "n")
}))


# ==============================================================================
# FIGURE 9: Comprehensive diagnostics — best female RGT model
# Uses create_comprehensive_plot() from comprehensive_plot.R
# Model: mod10rgtfx_ml_scat
# Output: PNG + TIFF via combine_plots() (already produces both)
# ==============================================================================

doplots(mod = mod10rgtfx_ml_scat, dat = drgtf_ro,
        direc = fig_dir, modn = "Fig09_mod10rgtfx_ml_scat",
        diag = TRUE, loglen = TRUE, maps = TRUE, hbf = TRUE,
        year = FALSE, yrf = TRUE, wv = FALSE, obs = TRUE,
        comprehensive = TRUE)
cat("Saved: Fig09 (comprehensive, via doplots)\n")


# ==============================================================================
# FIGURE 10: Nonlinearity — smooth vs linear loglen
# Panel a: Females (mod10rgtfx_ml_scat vs mod10rgtfx_ml_scat_lin)
# Panel b: Joint  (mod10rgtbx_ml_scat vs mod10rgtbx_ml_scat_lin)
# Generates weight-difference plots showing practical impact of nonlinearity
# ==============================================================================

# --- Prediction helper --------------------------------------------------------
predict_lw_pair <- function(mod, modlin, dat, excl) {
  new_data <- with(dat,
                   expand.grid(
                     hbf    = median(hbf),
                     loglen = seq(min(loglen), max(loglen), length.out = 200),
                     logwt  = median(logwt),
                     obs_name = factor(obs_name[1], levels = levels(obs_name)),
                     mon    = median(mon),
                     lat    = median(lat),
                     lon    = median(lon),
                     yrf    = yrf[1],
                     year   = median(year)
                   ))
  p1  <- predict(mod,    new_data, exclude = excl, type = "link", se.fit = TRUE, unconditional = TRUE)
  p1t <- predict(mod,    new_data, exclude = excl, type = "terms", se.fit = TRUE, unconditional = TRUE)
  p2  <- predict(modlin, new_data, exclude = excl, type = "link", se.fit = TRUE, unconditional = TRUE)
  
  loglen_col <- grep("loglen", colnames(p1t$se.fit))
  se_loglen <- if (length(loglen_col) > 0) p1t$se.fit[, loglen_col[1]] else p1t$se.fit[, 1]
  
  list(
    len     = exp(new_data$loglen),
    loglen  = new_data$loglen,
    wt      = exp(p1$fit),
    wt_up   = exp(p1$fit + 2 * se_loglen),
    wt_lo   = exp(p1$fit - 2 * se_loglen),
    wt_lin  = exp(p2$fit),
    fit     = p1$fit,
    fit_up  = p1$fit + 2 * se_loglen,
    fit_lo  = p1$fit - 2 * se_loglen,
    fit_lin = p2$fit
  )
}

excl_10 <- c("s(obs_name)", "te(lon,lat,mon)", "s(hbf)", "yrf")

pf <- predict_lw_pair(mod10rgtfx_ml_scat, mod10rgtfx_ml_scat_lin, drgtf_ro, excl_10)
pb <- predict_lw_pair(mod10rgtbx_ml_scat, mod10rgtbx_ml_scat_lin, drgtb_ro, excl_10)

save_fig("Fig10_nonlinearity", width = 10, height = 5, quote({
  par(mfrow = c(1, 2), mar = c(5, 4.5, 3, 1))
  
  # Panel a: Females (weight difference)
  yl <- range(c(pf$wt_up - pf$wt_lin, pf$wt_lo - pf$wt_lin))
  plot(pf$len, pf$wt - pf$wt_lin, type = "l", lwd = 2, ylim = yl,
       xlab = "Length (cm)", ylab = "Weight difference (kg)",
       main = "a) Females: nonlinear minus linear")
  lines(pf$len, pf$wt_up - pf$wt_lin, lty = 2)
  lines(pf$len, pf$wt_lo - pf$wt_lin, lty = 2)
  abline(h = 0, col = 2, lty = 2)
  
  # Panel b: Joint (weight difference)
  yl <- range(c(pb$wt_up - pb$wt_lin, pb$wt_lo - pb$wt_lin))
  plot(pb$len, pb$wt - pb$wt_lin, type = "l", lwd = 2, ylim = yl,
       xlab = "Length (cm)", ylab = "Weight difference (kg)",
       main = "b) Both sexes: nonlinear minus linear")
  lines(pb$len, pb$wt_up - pb$wt_lin, lty = 2)
  lines(pb$len, pb$wt_lo - pb$wt_lin, lty = 2)
  abline(h = 0, col = 2, lty = 2)
}))


# ==============================================================================
# FIGURE 11: Comprehensive diagnostics — best joint (both sexes) RGT model
# Model: mod10rgtbx_ml_scat
# ==============================================================================

doplots(mod = mod10rgtbx_ml_scat, dat = drgtb_ro,
        direc = fig_dir, modn = "Fig11_mod10rgtbx_ml_scat",
        diag = TRUE, loglen = TRUE, maps = TRUE, hbf = TRUE,
        year = FALSE, yrf = TRUE, wv = FALSE, obs = TRUE,
        comprehensive = TRUE)
cat("Saved: Fig11 (comprehensive, via doplots)\n")


# ==============================================================================
# FIGURES 12 & 13: Spatial LWR comparison maps (separate figures)
# Fig 12: Spatial model vs pooled a*L^b
# Fig 13: Spatial model vs regional LWRs
# Model: mod10rgtbx_ml_scat (joint)
# ==============================================================================

library(sf)
library(metR)

# --- Create prediction grid (shared by both figures) ---
grid <- expand_grid(lon = seq(130, 230, 1), lat = seq(-35, 35, 1))

# Filter to locations with ≥2 samples within 3 degrees
grid_filtered <- grid %>%
  rowwise() %>%
  filter(sum(abs(drgtb_ro$lon - lon) <= 3 & abs(drgtb_ro$lat - lat) <= 3) >= 2) %>%
  ungroup()

pred_data <- grid_filtered %>%
  expand_grid(length = c(100, 120, 140, 160), mon = 1:12) %>%
  mutate(
    loglen   = log(length),
    hbf      = mean(drgtb_ro$hbf, na.rm = TRUE),
    yrf      = drgtb_ro$yrf[1],
    obs_name = drgtb_ro$obs_name[1]
  )

pred_data$pred_logwt <- predict(
  mod10rgtbx_ml_scat, newdata = pred_data,
  type = "response", exclude = c("s(yrf)", "s(obs_name)")
)

pred_summary <- pred_data %>%
  group_by(lon, lat, length) %>%
  summarise(mean_logwt = mean(pred_logwt), .groups = "drop") %>%
  mutate(pred_wt = exp(mean_logwt))

# --- Region geometry (reuse regions_list from Fig 2) ---
regions_df <- map_dfr(1:nrow(regions_list), function(i) {
  nodes <- regions_list$nodes[[i]]
  tibble(region = regions_list$id[i],
         lon = ifelse(nodes[, 1] < 0, nodes[, 1] + 360, nodes[, 1]),
         lat = nodes[, 2])
})

# Plain dataframe coastline for use with coord_cartesian (avoids coord_sf axis issues)
world_df <- map_data("world2")

# --- Shared map builder ---
make_map <- function(dat) {
  ggplot() +
    geom_tile(data = dat, aes(x = lon, y = lat, fill = pct_diff)) +
    geom_contour(data = dat, aes(x = lon, y = lat, z = pct_diff),
                 breaks = seq(-10, 10, 1), colour = "black", linewidth = 0.2) +
    geom_text_contour(data = dat, aes(x = lon, y = lat, z = pct_diff),
                      breaks = seq(-10, 10, 1), stroke = 0.2, size = 2) +
    geom_polygon(data = world_df, aes(x = long, y = lat, group = group),
                 fill = "grey80", colour = "grey50", linewidth = 0.2) +
    geom_path(data = regions_df, aes(x = lon, y = lat, group = region),
              colour = "salmon", linewidth = 1) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, name = "% diff") +
    scale_x_continuous(breaks = seq(120, 240, 20)) +
    scale_y_continuous(breaks = seq(-40, 40, 20)) +
    facet_wrap(~ length, labeller = labeller(length = function(x) paste(x, "cm"))) +
    coord_fixed(ratio = 1, xlim = c(130, 230), ylim = c(-45, 55)) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal(base_family = "Arial")
}

# --- FIGURE 12: Spatial model vs pooled a*L^b ---
a_pool <- 1.274e-5
b_pool <- 3.058

pred_compare_pool <- pred_summary %>%
  filter(length %in% c(100, 120, 140, 160)) %>%
  mutate(
    simple_wt = a_pool * length^b_pool,
    pct_diff  = 100 * (pred_wt - simple_wt) / simple_wt,
    lon       = ifelse(lon < 0, lon + 360, lon)
  )

save_ggfig("Fig12_spatial_vs_pooled", make_map(pred_compare_pool),
           width = 10, height = 6)

# --- FIGURE 13: Spatial model vs regional LWRs ---
# Create sf polygons for regions
region_polys <- map_dfr(1:nrow(regions_list), function(i) {
  nodes <- regions_list$nodes[[i]]
  lon <- ifelse(nodes[, 1] < 0, nodes[, 1] + 360, nodes[, 1])
  lat <- nodes[, 2]
  # Close R8 polygon via bottom edge (needs extra node at 140,-10)
  if (regions_list$id[i] == "R8") {
    lon <- c(lon, 140); lat <- c(lat, -10)
  }
  if (lon[1] != lon[length(lon)] | lat[1] != lat[length(lat)]) {
    lon <- c(lon, lon[1]); lat <- c(lat, lat[1])
  }
  poly <- st_polygon(list(cbind(lon, lat)))
  st_sf(region = regions_list$id[i], geometry = st_sfc(poly, crs = 4326))
})

# Assign grid points to regions
pred_pts <- pred_summary %>%
  mutate(lon360 = ifelse(lon < 0, lon + 360, lon)) %>%
  st_as_sf(coords = c("lon360", "lat"), crs = 4326, remove = FALSE)

pred_with_region <- st_join(pred_pts, region_polys, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(region_num = as.numeric(gsub("R", "", region)))

# Regional LWR coefficients (2015-2022, from base models fitted by region)
regional_coefs <- tibble(
  region_num = c(1,       2,       3,       4,       6,       10,      11),
  a_reg      = c(1.66e-5, 1.10e-5, 1.26e-5, 1.01e-5, 1.22e-5, 1.07e-5, 2.39e-5),
  b_reg      = c(3.001,   3.090,   3.062,   3.104,   3.065,   3.090,   2.931)
)

pred_compare_reg <- pred_with_region %>%
  filter(length %in% c(100, 120, 140, 160)) %>%
  left_join(regional_coefs, by = "region_num") %>%
  filter(!is.na(a_reg)) %>%
  mutate(
    lon         = ifelse(lon < 0, lon + 360, lon),
    regional_wt = a_reg * length^b_reg,
    pct_diff    = 100 * (pred_wt - regional_wt) / regional_wt
  )

save_ggfig("Fig13_spatial_vs_regional", make_map(pred_compare_reg),
           width = 10, height = 6)


# ==============================================================================
# FIGURE 14: Predicted weight at length across locations and months
# 4 rows × 4 columns:
#   Rows a, b: Base model (mod10rgtbx_ml_scat) — natural (a) and log (b) scale
#   Rows c, d: Interaction model (mod13crgtbxy_ml) — natural (c) and log (d) scale
#   Col 1: Lat 20, Lon 160, months 2/5/8/11
#   Col 2: Lats 20/0/-15/-30, Lon 160, month 2
#   Col 3: Lats 30/15/0/-30, Lon 190, month 8
#   Col 4: Lons 140/170/200/230, Lat 5, month 11
# HBF, year, and observer effects excluded from predictions.
# ==============================================================================

pred_fig14 <- function(mod, dat, xlat, xlon, xmon) {
  new_data <- with(dat,
                   expand.grid(
                     hbf    = median(hbf),
                     loglen = seq(min(loglen), max(loglen), length.out = 200),
                     logwt  = median(logwt),
                     obs_name = factor(obs_name[1], levels = levels(obs_name)),
                     mon    = xmon,
                     lat    = xlat,
                     lon    = xlon,
                     yrf    = yrf[1],
                     year   = median(year)
                   ))
  excl <- c("s(obs_name)", "s(hbf)", "yrf")
  pred <- predict(mod, new_data, exclude = excl,
                  type = "link", se.fit = TRUE, unconditional = TRUE)
  pred <- cbind(pred, new_data)
  pred <- transform(pred, fitted = fit)
  return(pred)
}

# --- Generate all predictions for both models ---
# Column 1: by month (Lat 20, Lon 160)
c1_specs <- list(
  list(lat = 20, lon = 160, mon = 2),
  list(lat = 20, lon = 160, mon = 5),
  list(lat = 20, lon = 160, mon = 8),
  list(lat = 20, lon = 160, mon = 11)
)
c1_labs <- paste0("Mon ", c(2, 5, 8, 11))

# Column 2: by latitude (Lon 160, month 2)
c2_specs <- list(
  list(lat = 20,  lon = 160, mon = 2),
  list(lat = 0,   lon = 160, mon = 2),
  list(lat = -15, lon = 160, mon = 2),
  list(lat = -30, lon = 160, mon = 2)
)
c2_labs <- paste0("Lat ", c(20, 0, -15, -30))

# Column 3: by latitude (Lon 190, month 8)
c3_specs <- list(
  list(lat = 30,  lon = 190, mon = 8),
  list(lat = 15,  lon = 190, mon = 8),
  list(lat = 0,   lon = 190, mon = 8),
  list(lat = -30, lon = 190, mon = 8)
)
c3_labs <- paste0("Lat ", c(30, 15, 0, -30))

# Column 4: by longitude (Lat 5, month 11)
c4_specs <- list(
  list(lat = 5, lon = 140, mon = 11),
  list(lat = 5, lon = 170, mon = 11),
  list(lat = 5, lon = 200, mon = 11),
  list(lat = 5, lon = 230, mon = 11)
)
c4_labs <- paste0("Lon ", c(140, 170, 200, 230))

col_specs <- list(c1_specs, c2_specs, c3_specs, c4_specs)
col_labs  <- list(c1_labs, c2_labs, c3_labs, c4_labs)
col_titles <- c("Lat 20, Lon 160", "Lon 160, Mon 2", "Lon 190, Mon 8", "Lat 5, Mon 11")

# Predict for base and interaction models
pred_base <- pred_int <- vector("list", 4)
for (j in 1:4) {
  pred_base[[j]] <- lapply(col_specs[[j]], function(s)
    pred_fig14(mod10rgtbx_ml_scat, drgtb_ro, s$lat, s$lon, s$mon))
  pred_int[[j]]  <- lapply(col_specs[[j]], function(s)
    pred_fig14(mod13crgtbxy_ml,    drgtb_ro, s$lat, s$lon, s$mon))
}

# --- Plotting helper ---
plot_col <- function(preds, labs, log_scale = FALSE, show_ylab = FALSE,
                     show_xlab = FALSE, title = "") {
  cols <- c(1, 2, 2, 1)
  ltys <- c(1, 1, 2, 2)
  
  if (log_scale) {
    yl <- range(sapply(preds, function(p) range(p$fitted)))
    plot(preds[[1]]$loglen, preds[[1]]$fitted, type = "l", lwd = 2,
         col = cols[1], lty = ltys[1], ylim = yl,
         xlab = "", ylab = "", main = title, cex.main = 1.35, cex.axis = 1.1)
    for (k in 2:4) lines(preds[[k]]$loglen, preds[[k]]$fitted,
                         col = cols[k], lty = ltys[k], lwd = 2)
  } else {
    yl <- c(0, 130)
    plot(exp(preds[[1]]$loglen), exp(preds[[1]]$fitted), type = "l", lwd = 2,
         col = cols[1], lty = ltys[1], ylim = yl,
         xlab = "", ylab = "", main = title, cex.main = 1.35, cex.axis = 1.1)
    for (k in 2:4) lines(exp(preds[[k]]$loglen), exp(preds[[k]]$fitted),
                         col = cols[k], lty = ltys[k], lwd = 2)
  }
  legend("topleft", legend = labs, col = cols, lty = ltys, lwd = 2,
         cex = 1.0, bty = "n", seg.len = 4)
}

save_fig("Fig14_predicted_wt_at_length", width = 12, height = 14.4, quote({
  par(mfrow = c(4, 4), mar = c(2, 2, 2.5, 0.5), oma = c(3, 6, 2, 0))
  
  row_labels <- c("(a) Base model", "(b) Base model\n(log scale)", 
                  "(c) Interaction\nmodel", "(d) Interaction\nmodel (log scale)")
  
  # Row a: base model, natural scale
  for (j in 1:4) {
    plot_col(pred_base[[j]], col_labs[[j]], log_scale = FALSE, title = col_titles[j])
    if (j == 1) mtext(row_labels[1], side = 2, line = 3.5, cex = 1.05, las = 0)
  }
  
  # Row b: base model, log scale
  for (j in 1:4) {
    plot_col(pred_base[[j]], col_labs[[j]], log_scale = TRUE, title = "")
    if (j == 1) mtext(row_labels[2], side = 2, line = 3.5, cex = 1.05, las = 0)
  }
  
  # Row c: interaction model, natural scale
  for (j in 1:4) {
    plot_col(pred_int[[j]], col_labs[[j]], log_scale = FALSE, title = "")
    if (j == 1) mtext(row_labels[3], side = 2, line = 3.5, cex = 1.05, las = 0)
  }
  
  # Row d: interaction model, log scale
  for (j in 1:4) {
    plot_col(pred_int[[j]], col_labs[[j]], log_scale = TRUE, title = "")
    if (j == 1) mtext(row_labels[4], side = 2, line = 3.5, cex = 1.05, las = 0)
  }
  
  # Outer axis labels
  mtext("Length (cm)", side = 1, line = 1.5, outer = TRUE, cex = 1.2)
}))


# ==============================================================================
# SUPPLEMENTARY: Distribution comparison (Gaussian vs scaled-t diagnostics)
# Uses compare_distributions() from 03b_distribution_comparison.R
# ==============================================================================

compare_distributions(
  mod_gauss  = mod10rgtfx_ml_gaus,
  mod_scat   = mod10rgtfx_ml_scat,
  output_dir = fig_dir
)
# Note: compare_distributions() creates its own PNG at 300 dpi.
# Add TIFF version:
tiff(file.path(fig_dir, "FigS_distribution_comparison.tiff"),
     width = 10, height = 8, units = "in", res = 600, compression = "lzw")
# Re-run the plotting portion from compare_distributions manually, or
# convert the PNG to TIFF:
img <- image_read(file.path(fig_dir, "Fig_distribution_comparison.png"))
image_write(img, file.path(fig_dir, "FigS_distribution_comparison.tiff"),
            format = "tiff", compression = "LZW", density = "600x600")
cat("Saved: FigS_distribution_comparison\n")


# ==============================================================================
# SUPPLEMENTARY: Male RGT comprehensive diagnostics
# Model: mod10rgtmx_ml_scat
# ==============================================================================

doplots(mod = mod10rgtmx_ml_scat, dat = drgtm_ro,
        direc = fig_dir, modn = "FigS_mod10rgtmx_ml_scat",
        diag = TRUE, loglen = TRUE, maps = TRUE, hbf = TRUE,
        year = FALSE, yrf = TRUE, wv = FALSE, obs = TRUE,
        comprehensive = TRUE)
cat("Saved: FigS male comprehensive (via doplots)\n")


cat("\n=== All figures generated in", fig_dir, "===\n")