# ==============================================================================
# 75_figure1_and_supplementary_s1_s2.R -- Figure 1 and Supplementary Figures S1 and S2
#
# Figure 1 is drawn from data/Supplementary_material_1_Table_S1.xlsx.
#
# ENGINE AND TIMING: no model fitting; under a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
suppressMessages(library(readxl))

fig_dir <- file.path(FIG, "manuscript")
save_fig <- function(name, width, height, expr, res_png = 300, res_tiff = 600) {
  png(file.path(fig_dir, paste0(name, ".png")), width = width, height = height, units = "in", res = res_png)
  eval(expr); dev.off()
  tiff(file.path(fig_dir, paste0(name, ".tiff")), width = width, height = height, units = "in",
       res = res_tiff, compression = "lzw")
  eval(expr); dev.off()
  cat("Saved:", name, "\n")
}

# ---- Figure 1 -----------------------------------------------------------------
S1_FILE <- file.path(ROOT, "data", "Supplementary_material_1_Table_S1.xlsx")
lwr <- data.frame(read_excel(S1_FILE, n_max = 57, .name_repair = "unique_quiet"))
names(lwr) <- tolower(gsub("\\.+", ".", names(lwr)))
code_col <- grep("^ref", names(lwr), value = TRUE)[1]
lwr <- lwr[lwr$use.not %in% "O", ]
rng <- strsplit(as.character(lwr$fl.range), "-")
lwr$flmin <- sapply(rng, function(x) as.numeric(x[1]))
lwr$flmax <- sapply(rng, function(x) as.numeric(x[2]))
lwr$ocean <- factor(lwr$ocean, levels = c("ATL", "IND", "PAC"))
# Ronquillo (1963) is omitted: its two relationships give 16 kg (female) and 122 kg (male)
# at 150 cm, against 65-70 kg for every other study, which points to a transcription or
# unit error that could not be checked against the original report.
drawn <- !is.na(lwr$flmin) & !grepl("^Ronquillo", lwr[[code_col]])
cat("Table S1 relationships marked for use:", nrow(lwr), "| drawn (length range reported):", sum(drawn), "\n")
cat("  not drawn:", paste(lwr[[code_col]][!drawn], collapse = ", "), "\n")

save_fig("Fig01_published_LWRs", width = 9, height = 9, quote({
  par(mar = c(5, 5, 1, 1))
  plot(c(20, max(lwr$flmax, na.rm = TRUE)), c(0, 290), type = "n",
       xlab = "Fork length (cm)", ylab = "Weight (kg)", cex.lab = 1.2, las = 1)
  for (i in which(drawn)) {
    L <- seq(lwr$flmin[i], lwr$flmax[i], by = 1)
    lines(L, lwr$a[i] * L^lwr$b[i], col = i, lty = as.numeric(lwr$ocean[i]))
  }
  legend("topleft", legend = lwr[[code_col]][drawn], col = which(drawn),
         lty = as.numeric(lwr$ocean)[drawn], ncol = 2, cex = 0.9, bty = "n")
  legend("bottomright", legend = c("Atlantic", "Indian", "Pacific"), lty = 1:3,
         title = "Ocean", bty = "n", cex = 0.95)
}))

# ---- Figure S1: analysis dataset --------------------------------------------
d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
lab <- list(
  weather   = c(sunny = "Sunny", cloudy = "Cloudy", rain = "Rain", OTH = "Other"),
  wavescale = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5+" = "5+"),
  baitType  = c(SAP = "Saury", MAX = "Mackerel", PIL = "Pilchard", OTH = "Other"),
  Cluster   = c("1" = "1", "2" = "2", "3" = "3"))
title <- c(weather = "Weather", wavescale = "Wave scale", baitType = "Bait type",
           Cluster = "Targeting cluster")
prop_by_year <- function(v) {
  x <- d[!is.na(d[[v]]), ]
  f <- factor(as.character(x[[v]]), levels = names(lab[[v]]), labels = lab[[v]])
  tab <- table(f, x$year)
  sweep(tab, 2, colSums(tab), "/")
}
save_fig("FigS1_proportions_by_year", width = 10, height = 10, quote({
  par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), xpd = TRUE)
  for (k in seq_along(title)) {
    v <- names(title)[k]
    par(mar = c(ifelse(k > 2, 5, 3), ifelse(k %% 2 == 1, 5, 3), 3, 7))
    p <- prop_by_year(v)
    barplot(p, col = hcl.colors(nrow(p), "Set 2"), border = NA, las = 1,
            xlab = if (k > 2) "Year" else "", ylab = if (k %% 2 == 1) "Proportion of samples" else "",
            main = title[k], cex.lab = 1.2)
    legend("topright", inset = c(-0.28, 0), legend = rev(rownames(p)),
           fill = rev(hcl.colors(nrow(p), "Set 2")), border = NA, bty = "n")
  }
}))

# ---- Figure S2: retained pool with wave scale recorded -----------------------
DROP <- readRDS(file.path(OUT, "final_dataset.rds"))$DROP
r <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10, !obs_name %in% DROP,
         !is.na(wavescale), !is.na(weather)) %>%
  mutate(wave = factor(as.character(wavescale), levels = c("1", "2", "3", "4", "5+")))
cat("Figure S2 records:", nrow(r), "\n")
rem_prop <- function(x) { t <- table(r$wave, factor(x %% 5, levels = 0:4)); sweep(t, 1, rowSums(t), "/") }
pw <- rem_prop(r$weight); pl <- rem_prop(r$length)
cols <- hcl.colors(5, "Viridis", rev = TRUE)
save_fig("FigS2_rounding_by_wavescale", width = 9, height = 5, quote({
  par(mfrow = c(1, 2), mar = c(5, 4.5, 3, 1))
  for (k in 1:2) {
    p <- if (k == 1) pw else pl
    matplot(0:4, t(p), type = "b", pch = 16, lty = 1, col = cols, ylim = c(0, 0.4), las = 1,
            xlab = "Remainder after division by 5", ylab = if (k == 1) "Proportion of records" else "",
            main = c("Weight", "Fork length")[k], cex.lab = 1.1)
    abline(h = 0.2, lty = 3)
    if (k == 2) legend("topright", legend = rownames(p), col = cols, lty = 1, pch = 16,
                       title = "Wave scale", bty = "n")
  }
}))
cat("Figure S2: records per wave scale:", paste(names(table(r$wave)), table(r$wave), collapse = "; "), "\n")
