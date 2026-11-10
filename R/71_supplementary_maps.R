# ==============================================================================
# 71_supplementary_maps.R -- Supplementary seasonal maps
#
# Supplementary Figures S3 and S4.
#
# ENGINE AND TIMING: prediction only; 1-2 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(maps)

fig_dir <- file.path(FIG, "manuscript")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

MIN_SETS_Q <- 5    # sets required IN A QUARTER before a cell is drawn
GC         <- 5
REF_LEN    <- 120
MONTHS     <- c(2, 5, 8, 11)   # representative month per quarter, as in S9-S11
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
cellof <- function(x) floor(x/GC)*GC + GC/2

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
rl <- fromJSON(regions_json)$fishery_regions
region_df <- bind_rows(lapply(seq_len(nrow(rl)), function(i) {
  nd <- rl$nodes[[i]]
  tibble(id = rl$id[i], lon = ifelse(nd[,1] < 0, nd[,1] + 360, nd[,1]), lat = nd[,2])
}))

save_ggfig <- function(name, p, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 300)
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), p, width = width, height = height,
         dpi = 600, compression = "lzw")
  cat("Saved:", name, "\n")
}

# ------------------------------------------------------------------------------
# one masked seasonal map, for whichever period is passed in
# ------------------------------------------------------------------------------
seasonal_map <- function(m, d, label, period) {
  d <- d %>% mutate(clon = cellof(lon360(lon)), clat = cellof(lat),
                    qtr = match(3*ceiling(mon/3) - 1, MONTHS))
  dens <- d %>% filter(!is.na(qtr)) %>% group_by(clon, clat, qtr) %>%
    summarise(sets = n_distinct(setf), .groups = "drop") %>%
    filter(sets >= MIN_SETS_Q)
  if (!nrow(dens)) { cat("no cells survive the mask for", label, "\n"); return(NULL) }

  nd <- dens %>% mutate(mon = MONTHS[qtr], lon = clon, lat = clat,
                        loglen = log(REF_LEN),
                        loglen_c = log(REF_LEN) - mean(d$loglen),
                        sexf = factor("F", levels = levels(factor(d$sexf))),
                        yrf  = factor(levels(factor(d$yrf))[1], levels = levels(factor(d$yrf))),
                        obs_name = factor(levels(factor(d$obs_name))[1],
                                          levels = levels(factor(d$obs_name))),
                        tripf = factor(levels(factor(d$tripf))[1],
                                       levels = levels(factor(d$tripf)))) %>%
    as.data.frame()
  nd$fit <- as.numeric(predict(m, nd, exclude = intersect(EX,
              vapply(m$smooth, function(z) z$label, character(1)))))
  surf <- nd %>% mutate(pct = 100*(exp(fit - mean(fit)) - 1),
                        panel = factor(qtr, levels = 1:4,
                                       labels = paste0("Q", 1:4, " (month ", MONTHS, ")")))

  cat(sprintf("\n%s: cells drawn per quarter %s | range %.1f%% | %d cell-quarters\n",
              label, paste(as.integer(table(surf$panel)), collapse = "/"),
              diff(range(surf$pct)), nrow(surf)))

  p <- ggplot(surf, aes(clon, clat)) +
    geom_tile(aes(fill = pct), width = GC, height = GC) +
    geom_polygon(data = map_data("world2"), aes(long, lat, group = group),
                 fill = "grey85", colour = "grey55", linewidth = 0.2) +
    geom_path(data = region_df, aes(lon, lat, group = id), colour = "grey20",
              linewidth = 0.3) +
    scale_fill_gradient2(name = "Weight at\nlength (%)", low = "#2166AC",
                         mid = "grey95", high = "#B2182B", midpoint = 0) +
    coord_quickmap(xlim = c(120, 250), ylim = c(-40, 45)) +
    facet_wrap(~ panel, ncol = 2) +
    labs(x = "Longitude (deg E)", y = "Latitude (deg)",
         title = sprintf("Seasonal variation in weight at length, %s", period),
         subtitle = sprintf(
           "Cells with at least %d sets in the quarter. Range %.1f%%. Cells without data are not drawn.",
           MIN_SETS_Q, diff(range(surf$pct)))) +
    theme_bw(base_size = 10)

  q <- ggplot(surf, aes(clon, clat)) +
    geom_tile(aes(fill = sets), width = GC, height = GC) +
    geom_polygon(data = map_data("world2"), aes(long, lat, group = group),
                 fill = "grey85", colour = "grey55", linewidth = 0.2) +
    scale_fill_viridis_c(name = "Sets", trans = "log10") +
    coord_quickmap(xlim = c(120, 250), ylim = c(-40, 45)) +
    facet_wrap(~ panel, ncol = 2) +
    labs(x = "Longitude (deg E)", y = "Latitude (deg)",
         title = sprintf("Sampling behind each cell, %s", period)) +
    theme_bw(base_size = 10)
  list(effect = p, density = q, surf = surf)
}

# ------------------------------------------------------------------------------
# RGT 2015-2022 -- replaces S9, S10 and S11
# ------------------------------------------------------------------------------
fin <- readRDS(file.path(OUT, "final_model.rds"))
dat <- readRDS(file.path(OUT, "final_dataset.rds"))$d
dat <- dat %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                      sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
rgt <- seasonal_map(fin$m, dat, "RGT", "RGT, 2015-2022")
if (!is.null(rgt)) {
  save_ggfig("FigS9_seasonal_surface_RGT", rgt$effect, width = 10, height = 7.5)
  save_ggfig("FigS10_sampling_by_quarter_RGT", rgt$density, width = 10, height = 7.5)
  write_csv(rgt$surf %>% select(clon, clat, qtr, sets, pct),
            file.path(OUT, "TableS_seasonal_surface_RGT.csv"))
}

# ------------------------------------------------------------------------------
# RGG 2009-2014 -- accompanies Supplementary Text S2
# ------------------------------------------------------------------------------
f_rgg <- file.path(OUT, "rgg_supplementary.rds")
f_dgg <- file.path(OUT, "rgg_dataset.rds")
if (file.exists(f_rgg) && file.exists(f_dgg)) {
  rggm <- readRDS(f_rgg)$m
  dgg  <- readRDS(f_dgg)$d
  dgg  <- dgg %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                         sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
  rgg <- seasonal_map(rggm, dgg, "RGG", "RGG, 2009-2014")
  if (!is.null(rgg)) {
    save_ggfig("FigS11_seasonal_surface_RGG", rgg$effect, width = 10, height = 7.5)
    write_csv(rgg$surf %>% select(clon, clat, qtr, sets, pct),
              file.path(OUT, "TableS_seasonal_surface_RGG.csv"))
  }
} else {
  cat("\nRGG model or dataset not found; run 45_rgg_supplementary.R first.\n")
}

cat("\nThe seasonal mask is stricter than the annual mask used for Figure 12, so\n")
cat("fewer cells are drawn here than there. That difference is the point: it is\n")
cat("what the seasonal coverage of this programme actually supports.\n")
message("71_supplementary_maps.R complete")
