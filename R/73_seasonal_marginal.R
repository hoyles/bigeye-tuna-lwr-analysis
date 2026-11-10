# ==============================================================================
# 73_seasonal_marginal.R -- Seasonal cycle by cell
#
# Section 4.3 (seasonal variation). Diagnostic figure only.
#
# ENGINE AND TIMING: prediction only; 1-2 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(maps)

MIN_SETS <- 10      # as section 3.2 states for every map in the paper
GC       <- 5       # degree cell
REF_LEN  <- 120     # reference length, as in 71_
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
cellof <- function(x) floor(x/GC)*GC + GC/2
# The figure is a diagnostic, not a manuscript figure: the seasonal result is
# reported in the text only. It is kept out of figures/manuscript so it is not
# mistaken for one.
fig_dir <- file.path(FIG, "diagnostic")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

fin <- readRDS(file.path(OUT, "final_model.rds"))
m   <- fin$m
d   <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d   <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                    sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()

# ---- cells that carry data, on the annual mask -------------------------------
dens <- d %>% mutate(clon = cellof(lon360(lon)), clat = cellof(lat)) %>%
  group_by(clon, clat) %>% summarise(sets = n_distinct(setf), .groups = "drop") %>%
  filter(sets >= MIN_SETS)
cat(sprintf("cells with at least %d sets: %d, holding %.0f%% of fish\n",
            MIN_SETS, nrow(dens),
            100*mean(paste(cellof(lon360(d$lon)), cellof(d$lat)) %in%
                     paste(dens$clon, dens$clat))))

# ---- the fitted cycle in each cell, months 1 to 12 ---------------------------
grid <- dens %>% tidyr::crossing(mon = 1:12) %>%
  mutate(lon = clon, lat = clat, loglen = log(REF_LEN),
         loglen_c = log(REF_LEN) - mean(d$loglen),
         sexf = factor("F", levels = levels(d$sexf)),
         yrf  = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
         obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
         tripf    = factor(levels(d$tripf)[1],    levels = levels(d$tripf))) %>%
  as.data.frame()
keep <- intersect(EX, vapply(m$smooth, function(z) z$label, character(1)))
grid$fit <- as.numeric(predict(m, grid, exclude = keep))

# each cell's cycle, expressed as % departure from that cell's own annual mean,
# so this is SEASONALITY and not the spatial pattern re-expressed
cyc <- grid %>% group_by(clon, clat) %>%
  mutate(pct = 100*(exp(fit - mean(fit)) - 1)) %>% ungroup()

per_cell <- cyc %>% group_by(clon, clat, sets) %>%
  summarise(range_pct = diff(range(pct)), peak = mon[which.max(pct)],
            trough = mon[which.min(pct)], .groups = "drop")
cat(sprintf("\nwithin-cell seasonal range: median %.2f%%, IQR %.2f to %.2f%%, max %.2f%%\n",
            median(per_cell$range_pct), quantile(per_cell$range_pct, .25),
            quantile(per_cell$range_pct, .75), max(per_cell$range_pct)))

# ---- the average cycle, and how consistent it is across cells ----------------
avg <- cyc %>% group_by(mon) %>% summarise(pct = mean(pct), .groups = "drop")
cat(sprintf("mean cycle: amplitude %.2f%% | heaviest in month %d, lightest in month %d\n",
            diff(range(avg$pct)), avg$mon[which.max(avg$pct)], avg$mon[which.min(avg$pct)]))
cat("\n  mean cycle by month (%% from the annual mean):\n")
print(as.data.frame(avg %>% mutate(pct = round(pct, 2))), row.names = FALSE)

# phase agreement: is the same month the heaviest everywhere?
tb <- sort(table(per_cell$peak), decreasing = TRUE)
cat(sprintf("\n  cells peaking in the modal month: %d of %d (%.0f%%); modal month %s\n",
            tb[1], nrow(per_cell), 100*tb[1]/nrow(per_cell), names(tb)[1]))
cat("  A cycle that were an artefact would peak in scattered months; one that is\n")
cat("  real should agree on phase across cells that share no observers.\n")

# does the amplitude depend on latitude?
band <- per_cell %>% mutate(b = cut(clat, c(-40,-20,-10,0,10,20,50))) %>%
  group_by(b) %>% summarise(cells = dplyr::n(), amp = median(range_pct), .groups="drop")
cat("\n  seasonal amplitude by latitude band:\n")
print(as.data.frame(band %>% mutate(amp = round(amp, 2))), row.names = FALSE)

# ---- figure: the cycle, with each cell drawn faintly behind the mean ---------
p <- ggplot(cyc, aes(mon, pct, group = interaction(clon, clat))) +
  geom_line(colour = "grey70", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
  geom_line(data = avg, aes(mon, pct), inherit.aes = FALSE,
            colour = "#2E7D5B", linewidth = 1.2) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Weight at length, % from the cell's annual mean",
       title = sprintf("Seasonal cycle in weight at %d cm, by 5-degree cell", REF_LEN),
       subtitle = sprintf("%d cells with at least %d sets; grey lines are cells, green the mean cycle",
                          nrow(dens), MIN_SETS)) +
  theme_bw(base_size = 10)
ggsave(file.path(fig_dir, "seasonal_cycle.png"), p, width = 7, height = 4.5, dpi = 300)
ggsave(file.path(fig_dir, "seasonal_cycle.tiff"), p, width = 7, height = 4.5,
       dpi = 600, compression = "lzw")
cat("\nSaved: seasonal_cycle\n")

write_csv(per_cell, file.path(OUT, "TableR107_seasonal_by_cell.csv"))
write_csv(avg,      file.path(OUT, "TableR108_seasonal_mean_cycle.csv"))
saveRDS(list(per_cell = per_cell, avg = avg, band = band, min_sets = MIN_SETS,
             ref_len = REF_LEN), file.path(OUT, "seasonal_marginal.rds"))
message("73_seasonal_marginal.R complete")
