# ==============================================================================
# 63_spatial_description.R -- Description of the fitted spatial surface over well-sampled cells
#
# Section 4.3 (latitudinal and longitudinal pattern, seasonal cycle).
#
# ENGINE AND TIMING: prediction only; under a minute.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
CELL <- 5; MIN_SETS <- 10; REF_LEN <- 120

fm <- readRDS(file.path(OUT, "final_model.rds")); m <- fm$m
d  <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d$clon <- floor(lon360(d$lon)/CELL)*CELL + CELL/2
d$clat <- floor(d$lat/CELL)*CELL + CELL/2

dens <- d %>% group_by(clon, clat) %>%
  summarise(sets = n_distinct(setf), fish = dplyr::n(), .groups = "drop") %>%
  filter(sets >= MIN_SETS)
cat(sprintf("%d cells with >= %d sets (%.0f%% of fish)\n", nrow(dens), MIN_SETS,
            100*sum(dens$fish)/nrow(d)))

g <- expand.grid(clon = dens$clon, mon = 1:12) %>% distinct()
g <- dens %>% select(clon, clat) %>% tidyr::crossing(mon = 1:12)
nd <- g %>% mutate(lon = clon, lat = clat,
                   loglen = log(REF_LEN), loglen_c = log(REF_LEN) - mean(d$loglen),
                   sexf = factor("F", levels = levels(factor(d$sexf))),
                   yrf = factor(levels(factor(d$yrf))[5], levels = levels(factor(d$yrf))),
                   obs_name = factor(levels(factor(d$obs_name))[1],
                                     levels = levels(factor(d$obs_name))),
                   tripf = factor(levels(factor(d$tripf))[1],
                                  levels = levels(factor(d$tripf)))) %>% as.data.frame()
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
nd$fit <- as.numeric(predict(m, nd, exclude = EX))

cellmean <- nd %>% group_by(clon, clat) %>%
  summarise(mean_fit = mean(fit), seas_range = diff(range(fit)), .groups = "drop") %>%
  mutate(pct = 100*(exp(mean_fit - mean(mean_fit)) - 1)) %>%
  left_join(dens, by = c("clon","clat"))

cat(sprintf("\nsurface range across masked cells: %.2f%% (SD %.2f%%)\n",
            diff(range(cellmean$pct)), sd(cellmean$pct)))
cat(sprintf("median seasonal range within a cell: %.2f%% -- %s\n",
            100*(exp(median(cellmean$seas_range))-1),
            ifelse(median(cellmean$seas_range) < 0.02,
                   "small, so an annual description is fair",
                   "appreciable, so season should be mentioned")))

cat("\n=== by latitude band ===\n")
lat_b <- cellmean %>%
  mutate(band = cut(clat, breaks = c(-40,-20,-10,0,10,20,30,45),
                    labels = c("20-40S","10-20S","0-10S","0-10N","10-20N","20-30N","30-45N"))) %>%
  group_by(band) %>%
  summarise(cells = dplyr::n(), sets = sum(sets), pct = mean(pct), .groups = "drop")
print(as.data.frame(lat_b %>% mutate(pct = round(pct,2))), row.names = FALSE)

cat("\n=== by longitude band (degrees east) ===\n")
lon_b <- cellmean %>%
  mutate(band = cut(clon, breaks = c(120,150,180,210,240,290),
                    labels = c("120-150E","150-180E","180-210","210-240","240-290"))) %>%
  group_by(band) %>%
  summarise(cells = dplyr::n(), sets = sum(sets), pct = mean(pct), .groups = "drop")
print(as.data.frame(lon_b %>% mutate(pct = round(pct,2))), row.names = FALSE)

cat("\n=== the five highest and five lowest cells ===\n")
ext <- bind_rows(cellmean %>% arrange(desc(pct)) %>% head(5) %>% mutate(end = "high"),
                 cellmean %>% arrange(pct) %>% head(5) %>% mutate(end = "low"))
print(as.data.frame(ext %>% select(end, clon, clat, pct, sets, fish) %>%
        mutate(pct = round(pct,2))), row.names = FALSE)

cat("\n=== checks on the description ===\n")
trop <- cellmean %>% filter(abs(clat) <= 10)
farn <- cellmean %>% filter(clat >= 25)
fars <- cellmean %>% filter(clat <= -15)
fare <- cellmean %>% filter(clon >= 220)
c20n <- cellmean %>% filter(abs(clat - 20) <= 5, abs(clon - 170) <= 10)
chk <- function(lab, z, claim) cat(sprintf("  %-34s %+6.2f%% (%2d cells) -- claim '%s' %s\n",
  lab, mean(z$pct), nrow(z), claim,
  ifelse(nrow(z)==0, "NOT TESTABLE",
         ifelse(sign(mean(z$pct)) == ifelse(grepl("higher", claim), 1, -1),
                "SUPPORTED", "NOT supported"))))
chk("tropics (10S-10N)",        trop, "higher in tropical areas")
chk("far north (>=25N)",        farn, "higher in the far north")
chk("far south (<=15S)",        fars, "lower in the far south")
chk("far east (>=220E)",        fare, "lower in the far east")
chk("around 20N 170E",          c20n, "lower there")

write_csv(cellmean, file.path(OUT, "TableR96_spatial_cell_means.csv"))
saveRDS(list(cells = cellmean, lat = lat_b, lon = lon_b),
        file.path(OUT, "spatial_description.rds"))
message("63_spatial_description.R complete")
