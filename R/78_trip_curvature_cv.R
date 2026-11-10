# ==============================================================================
# 78_trip_curvature_cv.R -- Trip slope and trip-level curvature, by trip-blocked cross-validation
#
# Section 4.6. Compares Model A without the trip slope, Model A, and Model A with a random quadratic per trip,
# on the same ten folds.
#
# ENGINE AND TIMING: 30 bam fits, serial; a few minutes.
# Run from the repository root after R/00_setup.R; see README.md for the order.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260831)
JOBDIR <- file.path(OUT, "random_smooth_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
K <- 10

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()
d$loglen_q <- d$loglen_c^2 - mean(d$loglen_c^2)

COMMON <- "logwt ~ s(loglen, k = 20) + s(obs_name, bs = 're') + sexf +
  te(lon, lat, mon, k = c(6, 6, 4), bs = c('cr', 'cr', 'cc')) + yrf + s(tripf, bs = 're') + s(loglen_c, tripf, bs = 're')"
SPECS <- list(A0 = sub(" + s(loglen_c, tripf, bs = 're')", "", COMMON, fixed = TRUE),
              A = COMMON, Q = paste(COMMON, "+ s(loglen_q, tripf, bs = 're')"))

trips <- levels(d$tripf)
fold  <- setNames(sample(rep(1:K, length.out = length(trips))), trips)
d$fold <- fold[as.character(d$tripf)]

CVF <- file.path(JOBDIR, "curvature_cv_3models.rds")
if (file.exists(CVF)) cv <- readRDS(CVF) else {
  rows <- list(); t0 <- Sys.time()
  for (k in 1:K) {
    tr <- droplevels(d[d$fold != k, ]); te <- d[d$fold == k, ]
    te <- te[te$obs_name %in% levels(tr$obs_name) & te$yrf %in% levels(tr$yrf), ]
    te$obs_name <- factor(te$obs_name, levels = levels(tr$obs_name))
    te$yrf  <- factor(te$yrf, levels = levels(tr$yrf))
    te$sexf <- factor(te$sexf, levels = levels(tr$sexf))
    te$tripf <- factor(levels(tr$tripf)[1], levels = levels(tr$tripf))
    for (nm in names(SPECS)) {
      m <- try(bam(as.formula(SPECS[[nm]]), data = tr, family = scat, method = "fREML",
                   discrete = TRUE, gamma = 1.4), silent = TRUE)
      if (inherits(m, "try-error")) { cat("fit failed", k, nm, "\n"); next }
      labs <- vapply(m$smooth, function(z) z$label, character(1))
      ex <- intersect(c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)", "s(loglen_q,tripf)"), labs)
      p <- as.numeric(predict(m, newdata = te, type = "link", exclude = ex, discrete = FALSE))
      th <- m$family$getTheta(TRUE); nu <- th[1]; sig <- th[2]
      gv <- function(lab) { i <- match(lab, labs); if (is.na(i)) 0 else
        var(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]) }
      v_tot <- sig^2 * nu/(nu - 2) + gv("s(obs_name)") + gv("s(tripf)") +
        gv("s(loglen_c,tripf)") * te$loglen_c^2 + gv("s(loglen_q,tripf)") * te$loglen_q^2
      rows[[length(rows) + 1]] <- data.frame(fold = k, model = nm, n = nrow(te),
        logscore_full = mean(dnorm(te$logwt, p, sqrt(v_tot), log = TRUE)),
        rmse = sqrt(mean((te$logwt - p)^2)))
    }
    cat(sprintf("  fold %d done (%.1f min)\n", k, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  cv <- bind_rows(rows); saveRDS(cv, CVF)
}
w <- tidyr::pivot_wider(cv[, c("fold", "model", "logscore_full")], names_from = model, values_from = logscore_full)
w <- w[complete.cases(w), ]
d0 <- w$A - w$A0
cat(sprintf("\n=== trip slope (A against A0), same folds: mean %+.5f | improved %d of %d | paired t p = %.3f ===\n",
            mean(d0), sum(d0 > 0), nrow(w), t.test(d0)$p.value))
diff <- w$Q - w$A
cat(sprintf("\n=== random quadratic per trip, trip-blocked %d-fold CV, full predictive log score ===\n", nrow(w)))
cat(sprintf("  mean change %+.5f | folds improved %d of %d | paired t p = %.3f | Wilcoxon p = %.3f\n",
            mean(diff), sum(diff > 0), nrow(w), t.test(diff)$p.value,
            suppressWarnings(wilcox.test(diff))$p.value))
wr <- tidyr::pivot_wider(cv[, c("fold", "model", "rmse")], names_from = model, values_from = rmse)
cat(sprintf("  RMSE (log scale): Q better in %d of %d folds, mean change %+.6f\n",
            sum(wr$Q < wr$A, na.rm = TRUE), nrow(wr), mean(wr$Q - wr$A, na.rm = TRUE)))
print(as.data.frame(w), row.names = FALSE)
write_csv(cv, file.path(OUT, "TableR122_trip_curvature_cv.csv"))
message("78_trip_curvature_cv.R complete")

