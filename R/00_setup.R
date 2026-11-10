# ==============================================================================
# 00_setup.R -- shared setup, sourced by every script in R/
#
# Run all scripts from the repository root, for example by opening
# bigeye-tuna-lwr-analysis.Rproj in RStudio, or with setwd() to the root.
#
# The observer data are not included (see README.md and data/README.md). By
# default they are read from data/dat.Rdata, which must contain the data frame
# `da2`. Set the environment variable LWR_DATA to read them from elsewhere.
#
# The GAM fits are dominated by dense linear algebra. An optimised BLAS
# (OpenBLAS or MKL) makes them several times faster; with one, set the BLAS to a
# single thread (for example OPENBLAS_NUM_THREADS=1) when running the parallel
# scripts, so that workers do not compete for cores.
# ==============================================================================

suppressMessages({
  library(tidyverse)
  library(mgcv)
  library(sf)
  library(jsonlite)
  library(rnaturalearth)
})

# --- Paths --------------------------------------------------------------------
ROOT <- normalizePath(".", winslash = "/")
if (!file.exists(file.path(ROOT, "R", "00_setup.R")))
  stop("run the scripts from the repository root (the folder that contains R/)")
DATA_FILE <- Sys.getenv("LWR_DATA", file.path(ROOT, "data", "dat.Rdata"))
OUT <- file.path(ROOT, "output")
FIG <- file.path(ROOT, "figures")
for (d in c(OUT, FIG)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
LIB <- .libPaths()          # passed to parallel workers

# --- Data ---------------------------------------------------------------------
if (!file.exists(DATA_FILE))
  stop("observer data not found at ", DATA_FILE, "; see data/README.md")
load(DATA_FILE)             # provides da2: one row per fish, all processing types
stopifnot(exists("da2"))

# --- Helpers ------------------------------------------------------------------
# Draw one fish per group (set). Shuffling once and taking the first row per
# group is much faster than grouped sampling when repeated many times.
draw_one_per_group <- function(d, group) {
  g <- d[[group]]
  o <- sample.int(nrow(d))
  d[o, ][!duplicated(g[o]), ]
}

# Predicted random effects for a bs = "re" term, with their standard errors.
extract_ranef <- function(mod, term = NULL) {
  lab <- sapply(mod$smooth, function(s) s$label)
  isre <- sapply(mod$smooth, function(s) inherits(s, "random.effect"))
  if (!any(isre)) stop("no random effect (bs='re') smooth found")
  idx <- if (is.null(term)) which(isre)[1] else which(lab == term)
  sm  <- mod$smooth[[idx]]
  ii  <- sm$first.para:sm$last.para
  grp <- levels(mod$model[[sm$term]])
  se  <- sqrt(diag(mod$Vp)[ii])
  tibble(effect = "re", group_var = sm$term, group = grp,
         value = unname(coef(mod)[ii]), se = unname(se))
}

lon360 <- function(x) ifelse(x < 0, x + 360, x)

# --- WCPFC bigeye assessment regions (Day et al. 2023), plus R10 and R11 -------
regions_json <- '{
  "fishery_regions": [
    {"id": "R1",  "nodes": [[120, 50], [170, 50], [170, 10], [140, 10], [140, 20], [120, 20], [120, 50]]},
    {"id": "R2",  "nodes": [[170, 50], [-150, 50], [-150, 10], [170, 10], [170, 50]]},
    {"id": "R3",  "nodes": [[140, 10], [170, 10], [170, -10], [160, -10], [160, -5], [155, -5], [155, 0], [140, 0], [140, 10]]},
    {"id": "R4",  "nodes": [[170, 10], [-150, 10], [-150, -10], [170, -10], [170, 10]]},
    {"id": "R5",  "nodes": [[140, -10], [170, -10], [170, -40], [140, -40], [140, -20], [150, -20], [150, -15], [140, -15], [140, -10]]},
    {"id": "R6",  "nodes": [[170, -10], [-150, -10], [-150, -40], [170, -40], [170, -10]]},
    {"id": "R7",  "nodes": [[110, 20], [140, 20], [140, -10], [110, -10], [110, 20]]},
    {"id": "R10", "nodes": [[-150, 10], [-90, 10], [-90, -40], [-150, -40], [-150, 10]]},
    {"id": "R11", "nodes": [[-150, 50], [-110, 50], [-110, 10], [-150, 10], [-150, 50]]}
  ]
}'
regions_list <- fromJSON(regions_json)$fishery_regions
regions_df <- map_dfr(seq_len(nrow(regions_list)), function(i) {
  nd <- regions_list$nodes[[i]]
  tibble(region = regions_list$id[i],
         lon = ifelse(nd[, 1] < 0, nd[, 1] + 360, nd[, 1]),
         lat = nd[, 2])
})

# Pacific-centred coastline
world_pac <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_break_antimeridian(lon_0 = 180) |>
  st_shift_longitude()

# --- Figure helper ------------------------------------------------------------
save_fig <- function(name, plot = NULL, width = 10, height = 7, expr = NULL,
                     res_png = 300, tiff = TRUE) {
  png(file.path(FIG, paste0(name, ".png")), width = width, height = height,
      units = "in", res = res_png)
  if (!is.null(plot)) print(plot) else eval(expr)
  dev.off()
  if (tiff) {
    tiff(file.path(FIG, paste0(name, ".tiff")), width = width, height = height,
         units = "in", res = 600, compression = "lzw")
    if (!is.null(plot)) print(plot) else eval(expr)
    dev.off()
  }
  message("saved: ", name)
}

message("setup complete | da2: ", nrow(da2), " records")
