# ==============================================================================
# 80_gam_refits.R -- within-set ICC, the trip effect and HBF, and in-sample support for the Table 5 terms
#
# Sections 2.4, 3.1, 4.1, 4.2, 4.4; Tables 3 and 5. All fits gam(ML) with Model A's terms; 'without trip
# effects' is Model A with the trip intercept and slope removed and nothing else changed.
#   ICC, design effect, set-level SD, spatial edf: Model A on every fish of the pre-exclusion pool, and on
#     one fish per set from it (section 3.1, Table 3)
#   trip effect and HBF: without trip effects with s(hbf), then the trip intercept, then the slope (4.1, 4.2)
#   length x year (4.4); length x space and observer slopes in sample (Table 5)
#   sensitivity to the excluded observers: equation (3) by REML with and without them (2.4)
# Writes trip_re_hbf.rds, which 64_ reads.
# ENGINE AND TIMING: 13 gam fits on 6 workers; the all-fish fit dominates, 40-100 min.
# ==============================================================================

source("R/00_setup.R")
suppressMessages({ library(parallel); library(digest) })
set.seed(20261111)
JOBDIR <- file.path(OUT, "gam_refit_jobs"); if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)

fd <- readRDS(file.path(OUT, "final_dataset.rds")); d <- fd$d; DROP <- fd$DROP
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf), sexf = factor(sexf), yrf = factor(yrf),
                  yr = as.numeric(as.character(yrf))) %>% as.data.frame()
pool <- da2 %>%
  filter(processtype == "RGT", year >= 2015, year <= 2022, sex %in% c("F", "M"),
         length >= 70, length <= 200, hbf > 10) %>%
  mutate(lon = lon360(lon), obs_name = factor(obs_name), yrf = factor(year), setf = factor(setid),
         tripf = factor(tripID), sexf = factor(sex)) %>%
  filter(complete.cases(logwt, loglen, lon, lat, mon, obs_name, yrf)) %>% droplevels() %>% as.data.frame()
pool$loglen_c <- pool$loglen - mean(d$loglen)        # the same centring as Model A
pool1 <- droplevels(draw_one_per_group(pool, "setf"))
# B9: the analysis dataset plus one fish per set from the excluded observers
excl1 <- draw_one_per_group(pool[pool$obs_name %in% DROP, ], "setf")
excl1 <- excl1[, intersect(names(d), names(excl1))]
dplus <- bind_rows(mutate(d[, names(excl1)], across(where(is.factor), as.character)),
                   mutate(excl1, across(where(is.factor), as.character))) %>%
  mutate(obs_name = factor(obs_name), tripf = factor(tripf), sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
cat(sprintf("pool %d fish | pool one per set %d | analysis %d | analysis + excluded %d\n",
            nrow(pool), nrow(pool1), nrow(d), nrow(dplus)))

SP    <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
NOTR  <- paste("logwt ~ s(loglen, k = 20) + s(obs_name, bs = 're') + sexf +", SP, "+ yrf")
TRIPI <- "s(tripf, bs = 're')"
SLOPE <- "s(loglen_c, tripf, bs = 're')"
MODA  <- paste(NOTR, "+", TRIPI, "+", SLOPE)
HBF   <- "s(hbf, k = 15)"
LXS   <- "ti(lat, lon, loglen, k = c(8, 8, 6))"
OSL   <- "s(loglen_c, obs_name, bs = 're')"
EQ3   <- "logwt ~ loglen + s(obs_name, bs = 're') + s(tripf, bs = 're') + s(loglen_c, tripf, bs = 're') + sexf"
J <- list(
  list(id = "B1_pool_all",  data = "pool",  f = MODA, method = "ML"),
  list(id = "B1_pool_one",  data = "pool1", f = MODA, method = "ML"),
  list(id = "notrip",       data = "d",     f = NOTR, method = "ML"),
  list(id = "notrip_hbf",   data = "d",     f = paste(NOTR, "+", HBF), method = "ML"),
  list(id = "tripi_hbf",    data = "d",     f = paste(NOTR, "+", TRIPI, "+", HBF), method = "ML"),
  list(id = "modA_hbf",     data = "d",     f = paste(MODA, "+", HBF), method = "ML"),
  list(id = "notrip_lxs",   data = "d",     f = paste(NOTR, "+", LXS), method = "ML"),
  list(id = "modA_lxs",     data = "d",     f = paste(MODA, "+", LXS), method = "ML"),
  list(id = "notrip_osl",   data = "d",     f = paste(NOTR, "+", OSL), method = "ML"),
  list(id = "modA_osl",     data = "d",     f = paste(MODA, "+", OSL), method = "ML"),
  list(id = "modA_ti_yr",   data = "d",     f = paste(MODA, "+ ti(loglen, yr, k = c(10, 5))"), method = "ML"),
  list(id = "B9_eq3_d",     data = "d",     f = EQ3,  method = "REML"),
  list(id = "B9_eq3_dplus", data = "dplus", f = EQ3,  method = "REML"))
DATA <- list(pool = pool, pool1 = pool1, d = d, dplus = dplus)

run_fit <- function(j) {
  .libPaths(LIB); suppressMessages(library(mgcv))
  dat <- DATA[[j$data]]
  key <- substr(digest::digest(list(j$f, j$method, nrow(dat), sum(dat$logwt))), 1, 10)
  outf <- file.path(JOBDIR, sprintf("%s_%s.rds", j$id, key))
  if (file.exists(outf)) return(readRDS(outf))
  t0 <- Sys.time()
  m <- try(gam(as.formula(j$f), data = dat, family = scat, method = j$method, gamma = 1.4), silent = TRUE)
  if (inherits(m, "try-error")) return(list(id = j$id, error = as.character(m)))
  m$fit_minutes <- as.numeric(difftime(Sys.time(), t0, units = "mins")); m$job_id <- j$id
  saveRDS(m, outf); m
}
cat("fitting", length(J), "gam models on 6 workers ...\n")
cl <- makeCluster(6)
clusterExport(cl, c("DATA", "LIB", "JOBDIR")); clusterEvalQ(cl, library(digest))
t0 <- Sys.time()
fits <- parLapplyLB(cl, J, run_fit); stopCluster(cl)
names(fits) <- sapply(J, `[[`, "id")
cat(sprintf("elapsed %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
bad <- names(fits)[!sapply(fits, inherits, "gam")]
if (length(bad)) cat("FAILED:", paste(bad, collapse = ", "), "\n")

# --- summaries --------------------------------------------------------------------
lab_edf <- function(m, pat) { lab <- sapply(m$smooth, `[[`, "label"); i <- grep(pat, lab, fixed = TRUE)
  if (!length(i)) return(NA_real_); sum(sapply(i, function(k) sum(m$edf[m$smooth[[k]]$first.para:m$smooth[[k]]$last.para]))) }
icc_mom <- function(x, g) { s <- split(x, g); s <- s[lengths(s) >= 2]; n <- lengths(s); mn <- sapply(s, mean)
  gm <- sum(n*mn)/sum(n); msb <- sum(n*(mn-gm)^2)/(length(s)-1)
  msw <- sum(sapply(s, function(z) sum((z-mean(z))^2)))/(sum(n)-length(s)); n0 <- (sum(n)-sum(n^2)/sum(n))/(length(s)-1)
  c(icc = (msb-msw)/(msb+(n0-1)*msw), n0 = n0) }
hbf_range <- function(m) {                     # as 29_: s(hbf) across HBF 11-32, at any length
  if (!"s(hbf)" %in% sapply(m$smooth, `[[`, "label")) return(NA_real_)
  nd <- m$model[rep(1, 22), ]; nd$hbf <- 11:32
  p <- predict(m, nd, type = "terms")[, "s(hbf)"]; 100 * (exp(diff(range(p))) - 1) }
ok <- function(id) inherits(fits[[id]], "gam")
res <- list()
if (ok("B1_pool_all") && ok("B1_pool_one")) {
  ma <- fits$B1_pool_all; r <- as.numeric(ma$y - ma$fitted.values)
  is <- icc_mom(r, pool$setf); it <- icc_mom(r, pool$tripf)
  deff <- 1 + (is[["n0"]] - 1) * is[["icc"]]
  res$B1 <- c(icc_set = is[["icc"]], n0_set = is[["n0"]], icc_trip = it[["icc"]], deff = deff,
              eff_n = nrow(pool) / deff, set_sd_pct = 100 * (exp(sqrt(max(0, is[["icc"]])) * sd(r)) - 1),
              edf_sp_all = lab_edf(ma, "te("), edf_sp_one = lab_edf(fits$B1_pool_one, "te("),
              edf_sp_analysis = lab_edf(readRDS(file.path(OUT, "final_model.rds"))$m, "te("))
}
aic <- function(id) if (ok(id)) AIC(fits[[id]]) else NA_real_
res$B2 <- c(daic_hbf_notrip = aic("notrip_hbf") - aic("notrip"),
            daic_tripi = aic("tripi_hbf") - aic("notrip_hbf"), daic_tripi_slope = aic("modA_hbf") - aic("notrip_hbf"),
            edf_hbf_notrip = if (ok("notrip_hbf")) lab_edf(fits$notrip_hbf, "s(hbf)") else NA,
            edf_hbf_tripi = if (ok("tripi_hbf")) lab_edf(fits$tripi_hbf, "s(hbf)") else NA,
            edf_hbf_modA = if (ok("modA_hbf")) lab_edf(fits$modA_hbf, "s(hbf)") else NA,
            hbf_range_notrip = if (ok("notrip_hbf")) hbf_range(fits$notrip_hbf) else NA,
            hbf_range_tripi = if (ok("tripi_hbf")) hbf_range(fits$tripi_hbf) else NA,
            hbf_range_modA = if (ok("modA_hbf")) hbf_range(fits$modA_hbf) else NA)
mA <- readRDS(file.path(OUT, "final_model.rds"))$m
res$T5 <- c(daic_lxs_notrip = aic("notrip_lxs") - aic("notrip"), daic_lxs_modA = aic("modA_lxs") - AIC(mA),
            daic_osl_notrip = aic("notrip_osl") - aic("notrip"), daic_osl_modA = aic("modA_osl") - AIC(mA))
res$B5 <- c(daic_ti_yr = aic("modA_ti_yr") - AIC(mA),
            edf_ti_yr = if (ok("modA_ti_yr")) lab_edf(fits$modA_ti_yr, "ti(loglen,yr)") else NA)
if (ok("B9_eq3_d") && ok("B9_eq3_dplus")) {
  L <- 70:200; mk <- function(m) { nd <- data.frame(loglen = log(L), loglen_c = log(L) - mean(d$loglen),
    obs_name = levels(m$model$obs_name)[1], tripf = levels(m$model$tripf)[1], sexf = factor("F", levels = c("F", "M")))
    as.numeric(predict(m, nd, exclude = c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)"))) }
  dd <- 100 * (exp(mk(fits$B9_eq3_dplus) - mk(fits$B9_eq3_d)) - 1)
  res$B9 <- c(max_abs_pct = max(abs(dd)), b_d = unname(coef(fits$B9_eq3_d)["loglen"]),
              b_dplus = unname(coef(fits$B9_eq3_dplus)["loglen"]), n_dplus = nrow(dplus))
}
res$minutes <- sapply(fits, function(m) if (inherits(m, "gam")) round(m$fit_minutes, 1) else NA)
str(res)
saveRDS(res, file.path(OUT, "gam_refits.rds"))
saveRDS(list(hbf_range_notrip = res$B2[["hbf_range_notrip"]]), file.path(OUT, "trip_re_hbf.rds"))
message("80_gam_refits.R complete")
