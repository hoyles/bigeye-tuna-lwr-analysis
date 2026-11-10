# ==============================================================================
# 40_manuscript_figures.R -- Manuscript figures 2-9
#
# Figures 2-9.
#
# ENGINE AND TIMING: no model fitting; 3-6 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(maps)
set.seed(20260828)

fig_dir <- file.path(FIG, "manuscript")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

MIN_SETS <- 10     # cells with fewer sets than this are not drawn
GC       <- 5      # grid cell size, degrees

fin  <- readRDS(file.path(OUT, "final_model.rds"))
dat  <- readRDS(file.path(OUT, "final_dataset.rds"))
gaus <- readRDS(file.path(OUT, "final_model_gaussian.rds"))
m <- fin$m; ml <- fin$ml; mg <- gaus$mg
d <- dat$d
d$tripf <- factor(d$tripf); d$obs_name <- factor(d$obs_name); d$yrf <- factor(d$yrf)

# --- the pool for the rounding figures ----------------------------------------
# Same filters as 32_final_model.R, but WITHOUT the observer exclusions, for this reason.
#
# Rule (3) excludes observers for recording more than 80% of weights or lengths
# on multiples of 5 -- that is, FOR ROUNDING. Figures 6 and 7 exist to show the
# range of rounding behaviour in the programme. Excluding the observers who round
# most removes the clearest cases from the very figure that documents the
# phenomenon, and understates it: the reader would see the distribution that
# survives a screen built on the quantity being plotted. These figures therefore
# use every fish passing the data filters, before any observer is excluded.
#
# This is deliberately the opposite convention from the model, which is fitted on
# post-exclusion data. The two answer different questions: the model needs
# observers whose measurements can be trusted; the rounding figures need all of
# them, including the ones the screen removes.
pool <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10)
pool_excl <- pool %>% filter(!obs_name %in% dat$DROP)
cat(sprintf("analysis dataset   : %d fish (one per set, observers excluded)\n", nrow(d)))
cat(sprintf("rounding pool      : %d fish, %d observers (BEFORE exclusion, Figs 6-7)\n",
            nrow(pool), dplyr::n_distinct(pool$obs_name)))
cat(sprintf("  excluded from it : %d fish, %d observers (%.1f%% of the pool)\n",
            nrow(pool) - nrow(pool_excl),
            dplyr::n_distinct(pool$obs_name) - dplyr::n_distinct(pool_excl$obs_name),
            100*(1 - nrow(pool_excl)/nrow(pool))))

save_fig <- function(name, width, height, expr, res_png = 300, res_tiff = 600) {
  png(file.path(fig_dir, paste0(name, ".png")), width = width, height = height,
      units = "in", res = res_png); eval(expr); dev.off()
  tiff(file.path(fig_dir, paste0(name, ".tiff")), width = width, height = height,
       units = "in", res = res_tiff, compression = "lzw"); eval(expr); dev.off()
  cat("Saved:", name, "\n")
}
save_ggfig <- function(name, p, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 300)
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), p, width = width, height = height,
         dpi = 600, compression = "lzw")
  cat("Saved:", name, "\n")
}

# --- region polygons, for overlaying on maps 
regions_json <- '{"fishery_regions":[
 {"id":"R1","nodes":[[120,50],[170,50],[170,10],[140,10],[140,20],[120,20],[120,50]]},
 {"id":"R2","nodes":[[170,50],[-150,50],[-150,10],[170,10],[170,50]]},
 {"id":"R3","nodes":[[140,10],[170,10],[170,-10],[160,-10],[160,-5],[155,-5],[155,0],[140,0],[140,10]]},
 {"id":"R4","nodes":[[170,10],[-150,10],[-150,-10],[170,-10],[170,10]]},
 {"id":"R5","nodes":[[140,-10],[170,-10],[170,-40],[140,-40],[140,-20],[150,-20],[150,-15],[140,-15],[140,-10]]},
 {"id":"R6","nodes":[[170,-10],[-150,-10],[-150,-40],[170,-40],[170,-10]]},
 {"id":"R7","nodes":[[110,20],[140,20],[140,-10],[110,-10],[110,20]]},
 {"id":"R8","nodes":[[140,0],[155,0],[155,-5],[160,-5],[160,-10],[140,-10],[140,0]]},
 {"id":"R9","nodes":[[140,-15],[150,-15],[150,-20],[140,-20],[140,-15]]},
 {"id":"R10","nodes":[[-150,10],[-90,10],[-90,-40],[-150,-40],[-150,10]]},
 {"id":"R11","nodes":[[-150,50],[-110,50],[-110,10],[-150,10],[-150,50]]}]}'
regions_list <- fromJSON(regions_json)$fishery_regions
region_df <- bind_rows(lapply(seq_len(nrow(regions_list)), function(i) {
  nd <- regions_list$nodes[[i]]
  tibble(id = regions_list$id[i],
         lon = ifelse(nd[, 1] < 0, nd[, 1] + 360, nd[, 1]), lat = nd[, 2])
}))

# REGION LABELS. Tables 9 and 10 and section 4.3 are keyed by region number.
# The label sits at the polygon's area centroid, clamped inside the plot window
# because R7, R10 and R11 extend past it, with a white halo so that it stays
# legible over the colour ramp.
region_centroid <- function(z) {
  x <- z$lon; y <- z$lat; n <- length(x)
  if (x[n] == x[1] && y[n] == y[1]) { x <- x[-n]; y <- y[-n]; n <- n - 1 }
  j  <- c(2:n, 1)
  cr <- x * y[j] - x[j] * y
  A  <- sum(cr) / 2
  if (abs(A) < 1e-8) return(c(mean(x), mean(y)))
  c(sum((x + x[j]) * cr) / (6 * A), sum((y + y[j]) * cr) / (6 * A))
}
halo_text <- function(x, y, lab, cex, col, bg = "white") {
  rx <- 0.09 * strwidth(lab, cex = cex); ry <- 0.09 * strheight(lab, cex = cex)
  for (a in seq(0, 2*pi, length.out = 17)[-17])
    text(x + rx*cos(a), y + ry*sin(a), lab, cex = cex, col = bg, font = 2)
  text(x, y, lab, cex = cex, col = col, font = 2)
}
draw_regions <- function(lwd = 1.2, col = "grey20", labels = TRUE, cex = 0.75) {
  for (rid in unique(region_df$id)) {
    z <- region_df[region_df$id == rid, ]
    polygon(z$lon, z$lat, border = col, lwd = lwd)
    if (labels) {
      p <- region_centroid(z); u <- par("usr")
      p[1] <- min(max(p[1], u[1] + 5), u[2] - 5)
      p[2] <- min(max(p[2], u[3] + 3), u[4] - 3)
      halo_text(p[1], p[2], rid, cex = cex, col = col)
    }
  }
}

# ==============================================================================
# FIGURE 2 -- sampling coverage, ANALYSIS DATA, with colour ramp and regions
# ==============================================================================
cellof <- function(x) floor(x/GC)*GC + GC/2
dens_q <- d %>% mutate(clon = cellof(lon), clat = cellof(lat), qtr = ceiling(mon/3)) %>%
  count(clon, clat, qtr, name = "fish")
save_fig("Fig02_coverage_by_quarter", width = 11, height = 9, quote({
  layout(matrix(c(1,2,3,4,5,5), nrow = 3, byrow = TRUE), heights = c(1,1,0.28))
  par(mar = c(4, 4, 3, 1))
  brk <- c(1, 2, 5, 10, 25, 50, 100, 250, 1000)
  pal <- hcl.colors(length(brk) - 1, "YlGnBu", rev = TRUE)
  for (q in 1:4) {
    z <- dens_q[dens_q$qtr == q, ]
    plot(NA, xlim = c(120, 250), ylim = c(-40, 45), xlab = "Longitude (deg E)",
         ylab = "Latitude (deg)", main = paste0("Quarter ", q), las = 1)
    for (i in seq_len(nrow(z))) {
      k <- findInterval(z$fish[i], brk, rightmost.closed = TRUE)
      rect(z$clon[i]-GC/2, z$clat[i]-GC/2, z$clon[i]+GC/2, z$clat[i]+GC/2,
           col = pal[k], border = NA)
    }
    map("world2", add = TRUE, fill = TRUE, col = "grey85", border = "grey55")
    draw_regions()
    box()
  }
  par(mar = c(3, 4, 1, 1))
  plot(NA, xlim = c(0, length(pal)), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(length(pal)-1), 0.45, 1:length(pal), 0.95, col = pal, border = "grey40")
  axis(1, at = 0:length(pal), labels = brk, tick = FALSE, line = -1.4, cex.axis = 0.9)
  mtext("Fish in the analysis dataset per 5-degree cell", side = 1, line = 1.3, cex = 0.95)
}))

# ==============================================================================
# FIGURE 3 -- effort summaries. Trips are shown, as no vessel identifier exists
# ==============================================================================
save_fig("Fig03_summary_plots", width = 9, height = 7, quote({
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 1))
  barplot(table(d$year), xlab = "Year", ylab = "Fish in analysis dataset", las = 1)
  barplot(tapply(d$obs_name, d$year, n_distinct), xlab = "Year",
          ylab = "Observers per year", las = 1)
  barplot(tapply(d$tripf, d$year, n_distinct), xlab = "Year",
          ylab = "Trips per year", las = 1)
  barplot(tapply(d$setid, d$year, n_distinct), xlab = "Year",
          ylab = "Sets per year", las = 1)
}))



# ==============================================================================
# FIGURE 4 -- weight and length frequency: rounding made visible
# ALL fish passing the data filters, BEFORE observer exclusion (see header)
# Uses the full pre-exclusion pool, not the analysis subsample -- see header note
# ==============================================================================
save_fig("Fig04_weight_length_freq", width = 9, height = 10, quote({
  par(mfrow = c(2, 1), mar = c(4.5, 4.5, 3, 1))
  hist(pool$weight, breaks = seq(0, max(pool$weight, na.rm = TRUE) + 1, by = 1),
       xlim = c(0, 120), axes = FALSE, col = "grey70", border = "grey40",
       main = sprintf("Recorded weights (%s fish, before observer exclusion)",
                      format(nrow(pool), big.mark = ",")),
       xlab = "Weight (kg)")
  for (xx in seq(0, 120, 5)) abline(v = xx, lty = 2, col = "firebrick", lwd = 0.7)
  axis(1, at = seq(0, 120, 10)); axis(2, las = 1); box()
  hist(pool$length, breaks = seq(69.5, 200.5, by = 1), xlim = c(70, 200),
       axes = FALSE, col = "grey70", border = "grey40",
       main = "Recorded lengths", xlab = "Fork length (cm)")
  for (xx in seq(70, 200, 5)) abline(v = xx, lty = 2, col = "firebrick", lwd = 0.7)
  axis(1, at = seq(70, 200, 10)); axis(2, las = 1); box()
}))

# ==============================================================================
# FIGURE 5 -- rounding by observer. PRE-EXCLUSION POOL, so that the observers
# the precision rule removes are visible here.
# ==============================================================================
save_fig("Fig05_rounding_by_observer", width = 10, height = 5.5, quote({
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  panel <- function(v, ttl) {
    a <- table(pool$obs_name, v %% 5)
    a <- a[rowSums(a) > 200, , drop = FALSE]
    pr <- a / rowSums(a)
    plot(NA, xlim = c(-0.1, 4.1), ylim = c(0, 1), xlab = "Remainder (value mod 5)",
         ylab = "Proportion of an observer's records", main = ttl, las = 1)
    abline(h = 0.2, lty = 3, col = "grey50")
    for (i in seq_len(nrow(pr)))
      lines(0:4, pr[i, ], col = adjustcolor("steelblue", 0.35), lwd = 1)
    ov <- table(v %% 5) / length(v)
    lines(0:4, ov, col = "firebrick", lwd = 3, type = "b", pch = 19)
    legend("topright", bty = "n", lwd = c(1, 3), pch = c(NA, 19),
           col = c(adjustcolor("steelblue", 0.6), "firebrick"),
           legend = c(sprintf("individual observers (n = %d)", nrow(pr)), "all fish pooled"))
  }
  panel(pool$weight, "Weights")
  panel(pool$length, "Lengths")
}))

# ==============================================================================
# FIGURE 6 -- scaled-t vs Gaussian
# ==============================================================================
LENS <- seq(70, 200, by = 2.5)
nd <- data.frame(loglen = log(LENS), loglen_c = log(LENS) - mean(d$loglen),
                 lat = median(d$lat), lon = median(d$lon), mon = 6,
                 sexf = factor("F", levels = levels(d$sexf)),
                 yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                 obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                 tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
p_scat <- exp(as.numeric(predict(m,  nd, exclude = EX)))
p_gaus <- exp(as.numeric(predict(mg, nd, exclude = EX)))
save_fig("Fig06_gaussian_vs_scaled_t", width = 10, height = 5, quote({
  par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
  plot(LENS, p_scat, type = "l", lwd = 2, col = "#2E7D5B", las = 1,
       xlab = "Fork length (cm)", ylab = "Predicted weight (kg)",
       main = "Predicted weight at length")
  lines(LENS, p_gaus, lwd = 2, col = "#A03A3A", lty = 2)
  legend("topleft", bty = "n", lwd = 2, lty = c(1, 2),
         col = c("#2E7D5B", "#A03A3A"), legend = c("scaled t", "Gaussian"))
  pct <- 100*(p_gaus - p_scat)/p_scat
  plot(LENS, pct, type = "l", lwd = 2, col = "#A03A3A", las = 1,
       xlab = "Fork length (cm)", ylab = "Gaussian minus scaled t (%)",
       main = "Difference, and where the data are")
  abline(h = 0, col = "grey50")
  rug(sample(d$length, 2000), col = "grey60", lwd = 0.5)
  q <- quantile(d$length, c(0.025, 0.975))
  abline(v = q, lty = 3, col = "grey40")
  text(mean(q), max(pct)*0.9, "95% of fish", col = "grey30", cex = 0.9)
}))

# ==============================================================================
# FIGURE 7 -- the final model, term by term (replaces the doplots panel set)
# HBF panel removed: the term is dropped. Trip random effect panel added.
# ==============================================================================
save_fig("Fig07_final_model_terms", width = 11, height = 8, quote({
  par(mfrow = c(2, 3), mar = c(4.5, 4.5, 3, 1))
  plot(m, select = 1, shade = TRUE, las = 1, main = "s(log length)",
       xlab = "log(fork length)", ylab = "Partial effect on log weight")
  ro <- coef(m)[m$smooth[[2]]$first.para:m$smooth[[2]]$last.para]
  hist(100*(exp(ro) - 1), breaks = 25, col = "grey75", border = "grey40", las = 1,
       main = sprintf("Observer effects (SD %.2f%%)", 100*(exp(sd(ro)) - 1)),
       xlab = "Effect on weight at length (%)")
  abline(v = 0, col = "firebrick", lwd = 2)
  rt <- coef(m)[m$smooth[[3]]$first.para:m$smooth[[3]]$last.para]
  hist(100*(exp(rt) - 1), breaks = 25, col = "grey75", border = "grey40", las = 1,
       main = sprintf("Trip effects (SD %.2f%%)", 100*(exp(sd(rt)) - 1)),
       xlab = "Effect on weight at length (%)")
  abline(v = 0, col = "firebrick", lwd = 2)
  yy <- summary(m)$p.table
  yi <- grep("^yrf", rownames(yy))
  yr <- c(0, yy[yi, 1]); yse <- c(0, yy[yi, 2]); ylab_ <- levels(d$yrf)
  plot(seq_along(yr), 100*(exp(yr) - 1), pch = 19, las = 1, xaxt = "n", ylim = 100*(exp(range(c(yr-2*yse, yr+2*yse)))-1),
       xlab = "Year", ylab = "Effect on weight (%)", main = "Year effects")
  ok <- yse > 0                       # the reference level has no interval
  arrows(seq_along(yr)[ok], 100*(exp(yr-2*yse)[ok]-1),
         seq_along(yr)[ok], 100*(exp(yr+2*yse)[ok]-1),
         angle = 90, code = 3, length = 0.03)
  points(1, 0, pch = 21, bg = "white", cex = 1.3)   # reference year
  axis(1, at = seq_along(yr), labels = ylab_, cex.axis = 0.8); abline(h = 0, col = "grey50")
  qq <- qqnorm(residuals(m, type = "deviance"), main = "Residual Q-Q", las = 1,
               pch = 16, col = adjustcolor("grey30", 0.3)); qqline(residuals(m, type = "deviance"), col = "firebrick")
  plot(fitted(m), residuals(m, type = "deviance"), pch = 16, las = 1,
       col = adjustcolor("grey30", 0.25), xlab = "Fitted (log weight)",
       ylab = "Deviance residual", main = "Residuals vs fitted")
  abline(h = 0, col = "firebrick")
}))

# ==============================================================================
# FIGURE 8 -- departure from the power law, as a percentage.
#
# BASELINE. This must be the SAME baseline as Table 11, or the figure and the
# text disagree about the maturation dip. Table 11 (32_) measures departure from
# a log-linear regression fitted through the smooth's OWN predictions at the ten
# tabulated lengths, weighted by the number of fish within 7.5 cm, so the
# reference power law is anchored where the fish are, and the curve passes
# through the tabulated values.
#
# BAND. Drawn from the same posterior draws as the curve, so that the two fits
# are not treated as independent.
# ==============================================================================
LEN2 <- seq(70, 200, by = 1)
LTAB <- c(80, 90, 100, 110, 120, 130, 140, 150, 160, 175)   # as in 32_
nd2 <- nd[rep(1, length(LEN2)), ]; nd2$loglen <- log(LEN2)
nd2$loglen_c <- nd2$loglen - mean(d$loglen)
ndT <- nd[rep(1, length(LTAB)), ]; ndT$loglen <- log(LTAB)
ndT$loglen_c <- ndT$loglen - mean(d$loglen)

X2 <- predict(m, nd2, type = "lpmatrix", exclude = EX)
XT <- predict(m, ndT, type = "lpmatrix", exclude = EX)
wts <- sapply(LTAB, function(L) sum(abs(d$length - L) <= 7.5)); wts <- wts / sum(wts)
xT  <- log(LTAB)
set.seed(20260910)
B   <- MASS::mvrnorm(4000, coef(m), vcov(m))
lp2 <- B %*% t(X2); lpT <- B %*% t(XT)
devs <- t(vapply(seq_len(nrow(B)), function(i) {
  cf <- coef(lm(lpT[i, ] ~ xT, weights = wts))
  100 * (exp(lp2[i, ] - (cf[1] + cf[2] * log(LEN2))) - 1)
}, numeric(length(LEN2))))
dev_pct <- apply(devs, 2, median)
band    <- t(apply(devs, 2, quantile, c(0.025, 0.975)))
cat(sprintf("
Fig08 against the Table 11 baseline: %+.2f%% at 110 cm (32_ reports -1.27%%)
",
            dev_pct[which.min(abs(LEN2 - 110))]))
save_fig("Fig08_departure_from_power_law", width = 9, height = 6, quote({
  par(mar = c(4.5, 4.5, 3, 1))
  plot(LEN2, dev_pct, type = "n", las = 1, ylim = range(band),
       xlab = "Fork length (cm)", ylab = "Departure from the power law (%)",
       main = "Weight at length relative to a simple power law")
  polygon(c(LEN2, rev(LEN2)), c(band[, 1], rev(band[, 2])),
          col = adjustcolor("steelblue", 0.25), border = NA)
  lines(LEN2, dev_pct, lwd = 2, col = "#1F4E79")
  abline(h = 0, col = "grey40")
  rug(sample(d$length, 2000), col = "grey60", lwd = 0.5)
  q <- quantile(d$length, c(0.025, 0.975)); abline(v = q, lty = 3, col = "grey40")
  i110 <- which.min(abs(LEN2 - 110))
  points(110, dev_pct[i110], pch = 19, col = "firebrick", cex = 1.2)
  text(110, dev_pct[i110], sprintf("  %.2f%% at 110 cm", dev_pct[i110]),
       pos = 4, col = "firebrick", cex = 0.9)
}))

# ==============================================================================
# FIGURE 9 -- the spatial surface, MASKED to cells that hold data
# ==============================================================================
ann <- d %>% mutate(clon = cellof(lon), clat = cellof(lat)) %>%
  group_by(clon, clat) %>% summarise(n_sets = n_distinct(setid), .groups = "drop") %>%
  filter(n_sets >= MIN_SETS)
gsp <- ann %>% select(clon, clat) %>% mutate(k = 1) %>%
  inner_join(tibble(mon = 1:12, k = 1), by = "k", relationship = "many-to-many") %>% select(-k)
nds <- data.frame(loglen = log(120), loglen_c = log(120) - mean(d$loglen),
                  lon = gsp$clon, lat = gsp$clat, mon = gsp$mon,
                  sexf = factor("F", levels = levels(d$sexf)),
                  yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                  obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                  tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
gsp$fit <- as.numeric(predict(m, nds, exclude = EX))
surf <- gsp %>% group_by(clon, clat) %>% summarise(fit = mean(fit), .groups = "drop") %>%
  mutate(pct = 100*(exp(fit - mean(fit)) - 1))
cat(sprintf("\nspatial surface, cells with >= %d sets: %d cells, range %.1f%%\n",
            MIN_SETS, nrow(surf), diff(range(surf$pct))))
p12 <- ggplot(surf, aes(clon, clat)) +
  geom_tile(aes(fill = pct), width = GC, height = GC) +
  geom_polygon(data = map_data("world2"), aes(long, lat, group = group),
               fill = "grey85", colour = "grey55", linewidth = 0.2) +
  geom_path(data = region_df, aes(lon, lat, group = id), colour = "grey20", linewidth = 0.4) +
  scale_fill_gradient2(name = "Weight at\nlength (%)", low = "#2166AC",
                       mid = "grey95", high = "#B2182B", midpoint = 0) +
  coord_quickmap(xlim = c(120, 250), ylim = c(-40, 45)) +
  labs(x = "Longitude (deg E)", y = "Latitude (deg)",
       title = sprintf("Spatial variation in weight at length, cells with >= %d sets", MIN_SETS),
       subtitle = sprintf("Range %.1f%%. Cells without data are not drawn.", diff(range(surf$pct)))) +
  theme_bw(base_size = 11)
save_ggfig("Fig09_spatial_surface", p12, width = 9, height = 6)

message("40_manuscript_figures.R complete")
