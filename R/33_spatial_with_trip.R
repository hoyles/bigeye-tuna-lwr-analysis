# ==============================================================================
# 33_spatial_with_trip.R -- Spatial magnitude and split-half reproducibility of the spatial pattern
#
# Section 4.3.
#
# ENGINE AND TIMING: about 50 bam fits in parallel, plus prediction from the final model; 2-5 min.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
set.seed(20260826)

CELL <- 10; MIN_FISH <- 30; MIN_OBS <- 2; NSPLIT <- 12; NWORKER <- 16

fin <- readRDS(file.path(OUT, "final_model.rds"))
dat <- readRDS(file.path(OUT, "final_dataset.rds"))
m <- fin$m; d <- dat$d
d$cellf <- factor(paste0(floor(d$lon/CELL)*CELL, "_", floor(d$lat/CELL)*CELL))
# trip random SLOPE: the split-half fits below carried a trip
# intercept but not a slope, so they were testing spatial consistency under a
# specification the paper no longer uses.
d$loglen_c <- d$loglen - mean(d$loglen)
cat(sprintf("final dataset: %d fish | %d trips | %d observers | %d cells\n\n",
            nrow(d), nlevels(d$tripf), nlevels(d$obs_name), nlevels(d$cellf)))

# ---------------------------------------------------------------------------
# PART A: magnitude of spatial variation under the final model
# ---------------------------------------------------------------------------
cat("=== PART A: spatial magnitude, final gam(ML) model ===\n")
GC <- 5
cellof <- function(x) floor(x/GC)*GC + GC/2
dens <- d %>% mutate(clon = cellof(lon), clat = cellof(lat)) %>%
  count(clon, clat, name = "n_sets")
g <- expand.grid(clon = sort(unique(dens$clon)), clat = sort(unique(dens$clat)), mon = 1:12)
g$lon <- g$clon; g$lat <- g$clat
EX <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
for (L in c(100, 120, 140)) {
  gg <- g; gg$loglen <- log(L); gg$loglen_c <- log(L) - mean(d$loglen)
  gg$yrf <- factor(levels(d$yrf)[5], levels = levels(d$yrf))
  gg$sexf <- factor("F", levels = levels(d$sexf))
  gg$obs_name <- factor(levels(d$obs_name)[1], levels = levels(d$obs_name))
  gg$tripf <- factor(levels(d$tripf)[1], levels = levels(d$tripf))
  gg$fit <- as.numeric(predict(m, gg, exclude = EX))
  ann <- gg %>% group_by(clon, clat) %>% summarise(fit = mean(fit), .groups = "drop") %>%
    left_join(dens, by = c("clon","clat")) %>% mutate(n_sets = replace_na(n_sets, 0))
  cat(sprintf("\n  reference length %d cm:\n", L))
  for (mn in c(0, 1, 10, 20, 50)) {
    s <- ann %>% filter(n_sets >= mn); if (nrow(s) < 5) next
    p <- 100*(exp(s$fit - mean(s$fit)) - 1)
    cat(sprintf("    cells n>=%2d (%3d): range %5.1f%%, SD %4.2f%%\n",
                mn, nrow(s), diff(range(p)), sd(p)))
  }
  rmap <- d %>% mutate(clon = cellof(lon), clat = cellof(lat)) %>%
    count(clon, clat, region) %>% group_by(clon, clat) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(clon, clat, region)
  reg <- ann %>% filter(n_sets >= 10) %>% inner_join(rmap, by = c("clon","clat")) %>%
    group_by(region) %>% summarise(cells = dplyr::n(),
      fit = weighted.mean(fit, n_sets), .groups = "drop") %>% filter(cells >= 3)
  rp <- 100*(exp(reg$fit - weighted.mean(reg$fit, reg$cells)) - 1)
  cat(sprintf("    BETWEEN REGIONS (%d regions, >=3 well-sampled cells): range %.1f%%\n",
              nrow(reg), diff(range(rp))))
}
cat("  02d (no trip RE) gave 5.9%% across cells with n>=10 and 2.2%% between regions\n")

# ---------------------------------------------------------------------------
# PART B: split-half by observer, with and without the trip random effect
# ---------------------------------------------------------------------------
cat("\n=== PART B: split-half consistency, with vs without trip RE ===\n")
run_split <- function(job) {
  .libPaths(LIB); suppressMessages({library(mgcv); library(dplyr); library(tibble)})
  set.seed(2000 + job$split)
  obs <- levels(d$obs_name)
  h1 <- sample(obs, floor(length(obs)/2))
  form <- if (job$trip)
    logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + s(tripf, bs = "re") +
            s(loglen_c, tripf, bs = "re") + sexf + cellf
  else
    logwt ~ s(loglen, k = 20) + s(obs_name, bs = "re") + sexf + cellf
  eff <- function(dd) {
    ok <- dd %>% group_by(cellf) %>%
      summarise(f = dplyr::n(), o = n_distinct(obs_name), .groups = "drop") %>%
      filter(f >= MIN_FISH, o >= MIN_OBS) %>% pull(cellf)
    dd <- droplevels(dd[dd$cellf %in% ok, ])
    if (nlevels(dd$cellf) < 6) return(NULL)
    mm <- try(bam(form, data = dd, family = scat, method = "fREML",
                  discrete = TRUE, gamma = 1.4), silent = TRUE)
    if (inherits(mm, "try-error")) return(NULL)
    nd <- data.frame(cellf = levels(dd$cellf), loglen = mean(d$loglen),
                     loglen_c = 0,
                     sexf = factor("F", levels = levels(dd$sexf)),
                     obs_name = factor(levels(dd$obs_name)[1], levels = levels(dd$obs_name)),
                     tripf = factor(levels(dd$tripf)[1], levels = levels(dd$tripf)))
    ex <- intersect(c("s(obs_name)","s(tripf)","s(loglen_c,tripf)"), sapply(mm$smooth, function(z) z$label))
    p <- as.numeric(predict(mm, nd, exclude = ex))
    tibble(cellf = nd$cellf, est = p - mean(p))
  }
  A <- eff(d[d$obs_name %in% h1, ]); B <- eff(d[!d$obs_name %in% h1, ])
  if (is.null(A) || is.null(B)) return(NULL)
  j <- inner_join(A, B, by = "cellf", suffix = c("_a","_b"))
  if (nrow(j) < 6) return(NULL)
  tibble(trip = job$trip, split = job$split, cells = nrow(j),
         r = cor(j$est_a, j$est_b), cov = cov(j$est_a, j$est_b))
}
jobs <- c(lapply(1:NSPLIT, function(i) list(split = i, trip = TRUE)),
          lapply(1:NSPLIT, function(i) list(split = i, trip = FALSE)))
cl <- makeCluster(NWORKER)
clusterExport(cl, c("d","LIB","MIN_FISH","MIN_OBS"), envir = environment())
t0 <- Sys.time()
sp <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs, run_split)))
stopCluster(cl)
cat(sprintf("elapsed %.1f min for %d fits\n\n",
            as.numeric(difftime(Sys.time(), t0, units="mins")), 2*nrow(sp)))

for (tr in c(FALSE, TRUE)) {
  s <- sp %>% filter(trip == tr)
  if (!nrow(s)) next
  mc <- mean(s$cov); tt <- t.test(s$cov)
  cat(sprintf("  %-18s splits %2d | mean cells %4.1f | mean r %+.3f | positive %d/%d\n",
              if (tr) "WITH trip RE" else "WITHOUT trip RE", nrow(s), mean(s$cells),
              mean(s$r), sum(s$r > 0), nrow(s)))
  cat(sprintf("  %-18s mean cov %.6f | t = %.2f, p = %s | consistent spatial SD = %s\n\n",
              "", mc, tt$statistic, format.pval(tt$p.value, digits = 3),
              if (mc > 0) sprintf("%.4f (%.2f%% in weight)", sqrt(mc), 100*(exp(sqrt(mc))-1))
              else "not distinguishable from zero"))
}
a <- sp %>% filter(!trip) %>% pull(cov); b <- sp %>% filter(trip) %>% pull(cov)
if (length(a) && length(b)) {
  cat(sprintf("  paired difference (with - without trip): %+.6f, p = %s\n",
              mean(b) - mean(a), format.pval(t.test(b, a, paired = TRUE)$p.value, digits = 3)))
  cat(sprintf("  => %.0f%% of the spatial signal found without trip survives with it\n",
              100 * max(0, mean(b)) / max(mean(a), 1e-12)))
}

write_csv(sp, file.path(OUT, "TableR53_splithalf_with_trip.csv"))
saveRDS(list(sp = sp), file.path(OUT, "spatial_with_trip.rds"))
message("33_spatial_with_trip.R complete")
