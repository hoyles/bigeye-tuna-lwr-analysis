# ==============================================================================
# 61_interaction_cv.R -- the trip slope and the region x length interaction on withheld trips
#
# Sections 4.3 and 4.6; Table 5. Trip-blocked ten-fold cross-validation, scored by the full predictive log
# score (random-effect variance added back), of Model A with and without the trip slope, each with and
# without a region x length interaction.
# ENGINE AND TIMING: bam(discrete = TRUE), fold differences only; 40 fits, a few minutes.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260831)

JOBDIR <- file.path(OUT, "interaction_cv_jobs")
if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
K <- 10

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), region = factor(region),
                  loglen_c = loglen - mean(loglen)) %>% as.data.frame()
cat(sprintf("%d fish | %d trips | %d regions\n",
            nrow(d), nlevels(d$tripf), nlevels(d$region)))

RE    <- "s(obs_name, bs='re') + s(tripf, bs='re')"
SLOPE <- "s(loglen_c, tripf, bs='re')"
SPECS <- list(
  base_noslope  = paste("logwt ~ loglen + region +", RE, "+ sexf"),
  inter_noslope = paste("logwt ~ loglen * region +", RE, "+ sexf"),
  base_slope    = paste("logwt ~ loglen + region +", RE, "+", SLOPE, "+ sexf"),
  inter_slope   = paste("logwt ~ loglen * region +", RE, "+", SLOPE, "+ sexf"))

trips <- levels(d$tripf)
fold  <- setNames(sample(rep(1:K, length.out = length(trips))), trips)
d$fold <- fold[as.character(d$tripf)]

CVF <- file.path(JOBDIR, "cv.rds")
if (file.exists(CVF)) { cv <- readRDS(CVF); cat("  (cached)\n") } else {
  rows <- list(); t0 <- Sys.time()
  for (k in 1:K) {
    tr <- droplevels(d[d$fold != k, ]); te <- d[d$fold == k, ]
    # an unseen observer cannot be scored either; keep only observers the fold saw
    te <- te[te$obs_name %in% levels(tr$obs_name) &
             te$region   %in% levels(tr$region), ]
    if (!nrow(te)) next
    te$obs_name <- factor(te$obs_name, levels = levels(tr$obs_name))
    te$region   <- factor(te$region,   levels = levels(tr$region))
    te$sexf     <- factor(te$sexf,     levels = levels(tr$sexf))
    te$tripf    <- factor(levels(tr$tripf)[1], levels = levels(tr$tripf))
    for (nm in names(SPECS)) {
      m <- try(bam(as.formula(SPECS[[nm]]), data = tr, family = scat,
                   method = "fREML", discrete = TRUE, gamma = 1.4), silent = TRUE)
      if (inherits(m, "try-error")) next
      ex <- c("s(obs_name)", "s(tripf)")
      if (grepl("slope", nm)) ex <- c(ex, "s(loglen_c,tripf)")
      p <- try(predict(m, newdata = te, type = "link", exclude = ex,
                       discrete = FALSE), silent = TRUE)
      if (inherits(p, "try-error")) next
      th  <- m$family$getTheta(TRUE)         # scaled-t: (nu, sigma)
      nu  <- th[1]; sig <- th[2]
      # RESIDUAL-ONLY score: the naive version. Fair BETWEEN models with the same
      # random-effect structure, but not across them -- adding a random effect
      # moves variance out of the residual, so scoring an unseen trip against the
      # shrunken residual scale penalises the richer model for variance it has
      # correctly identified. 37_ hit the same trap from the other direction.
      ls  <- dt((te$logwt - as.numeric(p))/sig, df = nu, log = TRUE) - log(sig)
      # FULL PREDICTIVE score: an unseen trip carries the observer, trip and (if
      # fitted) trip-slope variance too, the last of which grows with distance
      # from the centring point. Gaussian approximation on the total, which is
      # consistent across models and is what a user predicting a new trip faces.
      labs <- vapply(m$smooth, function(z) z$label, character(1))
      gv <- function(lab) { i <- match(lab, labs); if (is.na(i)) 0 else
        var(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]) }
      v_res <- sig^2 * nu/(nu - 2)
      v_tot <- v_res + gv("s(obs_name)") + gv("s(tripf)") +
               gv("s(loglen_c,tripf)") * te$loglen_c^2
      ls_full <- dnorm(te$logwt, mean = as.numeric(p), sd = sqrt(v_tot), log = TRUE)
      rows[[length(rows)+1]] <- data.frame(
        fold = k, model = nm, n = nrow(te),
        logscore = mean(ls), logscore_full = mean(ls_full),
        rmse = sqrt(mean((te$logwt - as.numeric(p))^2)))
    }
    cat(sprintf("  fold %d done (%.1f min elapsed)\n", k,
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  cv <- bind_rows(rows); saveRDS(cv, CVF)
}

sm <- cv %>% group_by(model) %>%
  summarise(folds = dplyr::n(), logscore = mean(logscore),
            logscore_full = mean(logscore_full), rmse = mean(rmse),
            .groups = "drop") %>% arrange(desc(logscore_full))
cat("\n=== trip-blocked 10-fold CV, higher log score is better ===\n")
cat("    logscore      = residual scale only; comparable only WITHIN a slope condition\n")
cat("    logscore_full = predictive variance including the random-effect components\n")
print(as.data.frame(sm %>% mutate(across(where(is.numeric), ~round(.x, 5)))), row.names = FALSE)

pair <- function(a, b, lab, col = "logscore") {
  w <- cv %>% filter(model %in% c(a, b)) %>%
    select(fold, model, all_of(col)) %>%
    tidyr::pivot_wider(names_from = model, values_from = all_of(col))
  w <- w[stats::complete.cases(w), ]
  if (!nrow(w)) return(invisible())
  diff <- w[[b]] - w[[a]]
  tt <- try(t.test(diff), silent = TRUE)
  cat(sprintf("\n  %s\n", lab))
  cat(sprintf("    mean change in log score %+.5f | folds improved %d of %d",
              mean(diff), sum(diff > 0), nrow(w)))
  if (!inherits(tt, "try-error")) cat(sprintf(" | paired p = %.3f", tt$p.value))
  cat("\n")
}
cat("\n=== does adding the region-by-length interaction help out of sample? ===")
pair("base_noslope", "inter_noslope", "WITHOUT a trip slope, residual score")
pair("base_slope",   "inter_slope",   "WITH a trip slope, residual score")
pair("base_slope",   "inter_slope",   "WITH a trip slope, full predictive score",
     "logscore_full")
cat("\n=== and does the trip slope itself help predict unseen trips? ===")
cat("\n  Only the full predictive score can answer this. The residual-only version is")
cat("\n  shown so the trap is visible: adding a random effect moves variance out of")
cat("\n  the residual, so scoring an unseen trip against the shrunken residual scale")
cat("\n  penalises the richer model for variance it has correctly identified.")
pair("base_noslope", "base_slope", "residual scale only -- NOT a fair comparison")
pair("base_noslope", "base_slope", "full predictive variance -- the fair one",
     "logscore_full")

cat("\n=== READING THIS ===\n")
w <- cv %>% filter(model %in% c("base_slope", "inter_slope")) %>%
  select(fold, model, logscore_full) %>%
  tidyr::pivot_wider(names_from = model, values_from = logscore_full)
w <- w[stats::complete.cases(w), ]
dd <- w$inter_slope - w$base_slope
cat(sprintf("  With the trip slope modelled, the region-by-length interaction changes the\n"))
cat(sprintf("  log score by %+.5f and improves %d of %d folds.\n",
            mean(dd), sum(dd > 0), length(dd)))
if (mean(dd) > 0 && sum(dd > 0) >= 8) {
  cat("  It earns its place out of sample. Regional variation in the exponent is real\n")
  cat("  and should be reported, with the caveat that no single region can be singled\n")
  cat("  out once standard errors are honest (57_).\n")
} else {
  cat("  It does NOT earn its place out of sample, and the in-sample dAIC of -9.0 is\n")
  cat("  the sixth instance in this paper of a term supported by fit and not by\n")
  cat("  prediction. The honest conclusion is that these data do not support regional\n")
  cat("  variation in the exponent -- neither region by region nor jointly.\n")
}
cat("\n  Note the contrast between the two rows above: whether the interaction looks\n")
cat("  worth having depends on whether trip-level slope variation is in the model.\n")

write_csv(cv, file.path(OUT, "TableR93_interaction_cv.csv"))
saveRDS(list(cv = cv, summary = sm), file.path(OUT, "interaction_cv.rds"))
message("61_interaction_cv.R complete")
