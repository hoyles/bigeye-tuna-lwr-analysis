# ==============================================================================
# 85_gamma_gam.R -- Model A by gam(ML) at each value of gamma
#
# Supplementary Text S1: total, length and spatial edf and the departure from log-linearity at 110 cm
# (defined as in Table 11) at gamma = 1, 2, 2.65, 4 and 6 (1.4 is Model A), and where the gamma = 1 and
# 1.4 spatial surfaces differ. Reads 31_'s cross-validated scores.
# ENGINE AND TIMING: five gam(ML) fits in parallel, 45-75 min.
# ==============================================================================

source("R/00_setup.R")
library(parallel)
JOBDIR <- file.path(OUT, "gamma_gam_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
GAMMAS <- c(1.0, 1.4, 2, 2.65, 4, 6)

fm <- readRDS(file.path(OUT, "final_model.rds")); m14 <- fm$m
d  <- readRDS(file.path(OUT, "final_dataset.rds"))$d
FORM <- formula(m14)
stopifnot(m14$method == "ML", isTRUE(all.equal(eval(m14$call$gamma), 1.4)), nrow(m14$model) == nrow(d))
cat(sprintf("Model A: %d fish | %s\n", nrow(d), paste(deparse(FORM, width.cutoff = 500), collapse = "")))

key <- function(g) sprintf("modelA_%s_g%.2f_ML_n%d.rds",
                           substr(digest::digest(paste(deparse(FORM), collapse = "")), 1, 8), g, nrow(d))
fit_one <- function(g) {
  .libPaths(LIB); suppressMessages(library(mgcv))
  outf <- file.path(JOBDIR, key(g))
  if (file.exists(outf)) return(readRDS(outf))
  t0 <- Sys.time()
  m <- try(gam(FORM, data = d, family = scat, method = "ML", gamma = g), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  attr(m, "mins") <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  saveRDS(m, outf); m
}
todo <- setdiff(GAMMAS, 1.4)
cat(sprintf("fitting %d gam(ML) models on %d workers ...\n", length(todo), length(todo)))
cl <- makeCluster(length(todo))
clusterExport(cl, c("LIB", "JOBDIR", "FORM", "d", "key"), envir = environment())
clusterEvalQ(cl, Sys.setenv(OPENBLAS_NUM_THREADS = "1"))
t0 <- Sys.time()
fits <- parLapplyLB(cl, todo, fit_one)
stopCluster(cl)
cat(sprintf("elapsed %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
names(fits) <- sprintf("%.2f", todo)
if (any(vapply(fits, is.null, logical(1)))) stop("failed fits: ", paste(names(fits)[vapply(fits, is.null, logical(1))], collapse = ", "))
fits[["1.40"]] <- m14
fits <- fits[sprintf("%.2f", GAMMAS)]

# --- (1) edf and the dip, 32_ definition --------------------------------------
LENS <- c(80, 90, 100, 110, 120, 130, 140, 150, 160, 175)
nd <- data.frame(loglen = log(LENS), loglen_c = log(LENS) - mean(d$loglen),
                 lat = median(d$lat), lon = median(d$lon), mon = 6,
                 yrf = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
                 sexf = factor("F", levels = levels(d$sexf)),
                 obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
                 tripf = factor(levels(d$tripf)[1], levels = levels(d$tripf)))
EX  <- c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)")
wts <- sapply(LENS, function(L) sum(abs(d$length - L) <= 7.5)); wts <- wts / sum(wts)
x   <- log(LENS); i110 <- which(LENS == 110)
devf <- function(y) 100 * (exp(y - fitted(lm(y ~ x, weights = wts))) - 1)
edf_of <- function(mod, lab) { i <- grep(lab, sapply(mod$smooth, `[[`, "label"), fixed = TRUE)[1]
  sum(mod$edf[mod$smooth[[i]]$first.para:mod$smooth[[i]]$last.para]) }
cvs <- read.csv(file.path(OUT, "TableR50_gamma_cv_summary.csv"))
cvs <- cvs[cvs$spatial, ]
tab <- do.call(rbind, lapply(names(fits), function(nm) {
  mod <- fits[[nm]]; g <- as.numeric(nm)
  Xp  <- predict(mod, nd, type = "lpmatrix", exclude = EX)
  set.seed(20261111)
  lp  <- MASS::mvrnorm(4000, coef(mod), vcov(mod)) %*% t(Xp)
  data.frame(gamma = g,
             logscore_cv = cvs$logscore_full[abs(cvs$gamma - g) < 1e-9],
             edf_tot = sum(mod$edf), edf_len = edf_of(mod, "s(loglen)"), edf_sp = edf_of(mod, "te(lon,lat,mon)"),
             dip110_point = devf(as.numeric(Xp %*% coef(mod)))[i110],
             dip110_median = median(apply(lp, 1, devf)[i110, ]),
             aic = AIC(mod), mins = if (is.null(attr(mod, "mins"))) NA else attr(mod, "mins"))
}))
cat("\n=== gamma, gam(ML) on the analysis dataset ===\n")
print(tab, digits = 5, row.names = FALSE)
cat(sprintf("  gamma = 1.4 row is Model A: dip %.2f%% (main text -1.27%%)\n", tab$dip110_median[tab$gamma == 1.4]))

# --- (2) where the gamma = 1 and 1.4 surfaces differ (66_ part 2) -------------
CELL <- 5; MIN_SETS <- 10; REF_LEN <- 120
m1 <- fits[["1.00"]]
d$clon <- floor(lon360(d$lon)/CELL)*CELL + CELL/2
d$clat <- floor(d$lat/CELL)*CELL + CELL/2
dens <- d %>% group_by(clon, clat) %>% summarise(sets = n_distinct(setf), .groups = "drop") %>% filter(sets >= MIN_SETS)
ng <- dens %>% select(clon, clat) %>% tidyr::crossing(mon = 1:12) %>%
  mutate(lon = clon, lat = clat, loglen = log(REF_LEN), loglen_c = log(REF_LEN) - mean(d$loglen),
         sexf = factor("F", levels = levels(d$sexf)),
         yrf  = factor(levels(d$yrf)[5], levels = levels(d$yrf)),
         obs_name = factor(levels(d$obs_name)[1], levels = levels(d$obs_name)),
         tripf    = factor(levels(d$tripf)[1],    levels = levels(d$tripf))) %>% as.data.frame()
ng$f1 <- as.numeric(predict(m1,  ng, exclude = EX))
ng$f2 <- as.numeric(predict(m14, ng, exclude = EX))
cellw <- ng %>% group_by(clon, clat) %>% summarise(d1 = mean(f1), d2 = mean(f2), .groups = "drop") %>%
  mutate(d1 = d1 - mean(d1), d2 = d2 - mean(d2), absdiff = 100*abs(exp(d1 - d2) - 1)) %>%
  left_join(dens, by = c("clon", "clat"))
sl <- suppressWarnings(stats::cor.test(log(cellw$sets), cellw$absdiff, method = "spearman"))
q  <- cellw %>% mutate(tert = dplyr::ntile(sets, 3)) %>% group_by(tert) %>%
  summarise(cells = dplyr::n(), median_sets = median(sets), mean_absdiff = mean(absdiff), .groups = "drop")
cat(sprintf("\n=== gamma = 1 against 1.4 surfaces, gam(ML) ===\n  %d cells | mean |difference| %.3f%% | Spearman(log sets, |difference|) %+.3f, p = %.2g\n",
            nrow(cellw), mean(cellw$absdiff), as.numeric(sl$estimate), sl$p.value))
print(as.data.frame(q), row.names = FALSE)

write.csv(tab, file.path(OUT, "TableR133_gamma_gam.csv"), row.names = FALSE)
write.csv(cellw, file.path(OUT, "TableR134_gamma_gam_surface.csv"), row.names = FALSE)
saveRDS(list(tab = tab, cells = cellw, terciles = q,
             spearman = c(rho = as.numeric(sl$estimate), p = sl$p.value)),
        file.path(OUT, "gamma_gam.rds"))
message("85_gamma_gam.R complete")
