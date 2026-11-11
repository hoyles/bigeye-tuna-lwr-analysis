# ==============================================================================
# 81_table5_cv.R -- Table 5 terms and the spatial basis judged on withheld trips
#
# Sections 3.2, 4.1; Table 5. Trip-blocked ten-fold cross-validation (the folds of 61_), scored by the full
# predictive log score: a withheld trip's observer, trip and slope effects are excluded from the predicted
# mean and their variance is added to the predictive distribution. Models: without trip effects and Model A,
# each with s(hbf, k = 15), length x space or observer slopes added; and Model A with spatial basis
# k = (10, 10, 4) against (6, 6, 4).
# ENGINE AND TIMING: bam(discrete = TRUE), only fold differences reported; 90 fits on 8 workers, 20-30 min.
# ==============================================================================

source("R/00_setup.R")
suppressMessages(library(parallel))
JOBDIR <- file.path(OUT, "table5_cv_jobs"); if (!dir.exists(JOBDIR)) dir.create(JOBDIR, recursive = TRUE)
fd <- readRDS(file.path(OUT, "final_dataset.rds")); d <- fd$d; DROP <- fd$DROP
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf), sexf = factor(sexf), yrf = factor(yrf)) %>% as.data.frame()

SP    <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
NOTR  <- paste("logwt ~ s(loglen, k = 20) + s(obs_name, bs = 're') + sexf +", SP, "+ yrf")
MODA  <- paste(NOTR, "+ s(tripf, bs = 're') + s(loglen_c, tripf, bs = 're')")
ADD   <- c(hbf = "s(hbf, k = 15)", lxs = "ti(lat, lon, loglen, k = c(8, 8, 6))", osl = "s(loglen_c, obs_name, bs = 're')")
SPECS <- c(list(notrip = NOTR, modA = MODA,
                modA_k10 = sub("k = c(6,6,4)", "k = c(10,10,4)", MODA, fixed = TRUE)),
           setNames(lapply(ADD, function(a) paste(NOTR, "+", a)), paste0("notrip_", names(ADD))),
           setNames(lapply(ADD, function(a) paste(MODA, "+", a)), paste0("modA_", names(ADD))))

set.seed(20260831)                                    # the fold assignment of 61_
K <- 10; trips <- levels(d$tripf)
fold <- setNames(sample(rep(1:K, length.out = length(trips))), trips)
d$fold <- fold[as.character(d$tripf)]

cv_job <- function(j) {
  .libPaths(LIB); suppressMessages(library(mgcv))
  outf <- file.path(JOBDIR, sprintf("cv_%s_%d.rds", j$spec, j$k))
  if (file.exists(outf)) return(readRDS(outf))
  tr <- droplevels(D[D$fold != j$k, ]); te <- D[D$fold == j$k, ]
  te <- te[te$obs_name %in% levels(tr$obs_name) & te$yrf %in% levels(tr$yrf), ]
  for (v in c("obs_name", "yrf", "sexf")) te[[v]] <- factor(te[[v]], levels = levels(tr[[v]]))
  te$tripf <- factor(levels(tr$tripf)[1], levels = levels(tr$tripf))
  m <- try(bam(as.formula(S[[j$spec]]), data = tr, family = scat, method = "fREML", discrete = TRUE, gamma = 1.4), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  labs <- vapply(m$smooth, function(z) z$label, character(1))
  ex <- intersect(c("s(obs_name)", "s(tripf)", "s(loglen_c,tripf)", "s(loglen_c,obs_name)"), labs)
  p <- as.numeric(predict(m, newdata = te, exclude = ex, discrete = FALSE))
  th <- m$family$getTheta(TRUE); nu <- th[1]; sig <- th[2]
  gv <- function(lab) { i <- match(lab, labs); if (is.na(i)) 0 else var(coef(m)[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para]) }
  vt <- sig^2 * nu / (nu - 2) + gv("s(obs_name)") + gv("s(tripf)") +
        (gv("s(loglen_c,tripf)") + gv("s(loglen_c,obs_name)")) * te$loglen_c^2
  r <- data.frame(spec = j$spec, fold = j$k, n = nrow(te),
                  logscore_full = mean(dnorm(te$logwt, p, sqrt(vt), log = TRUE)), rmse = sqrt(mean((te$logwt - p)^2)))
  saveRDS(r, outf); r
}

jobs_cv <- unlist(lapply(names(SPECS), function(s) lapply(1:K, function(k) list(spec = s, k = k))), recursive = FALSE)
cat(sprintf("%d CV fits on 8 workers ...\n", length(jobs_cv)))
cl <- makeCluster(8)
clusterExport(cl, c("LIB", "JOBDIR"))
clusterExport(cl, c("D", "S"), envir = list2env(list(D = d, S = SPECS)))
t0 <- Sys.time()
cv <- bind_rows(Filter(Negate(is.null), parLapplyLB(cl, jobs_cv, cv_job)))
stopCluster(cl)
cat(sprintf("elapsed %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

cmp <- function(a, b) { w <- reshape(cv[cv$spec %in% c(a, b), c("fold", "spec", "logscore_full")], idvar = "fold",
                                     timevar = "spec", direction = "wide"); w <- w[complete.cases(w), ]
  x <- w[[paste0("logscore_full.", b)]] - w[[paste0("logscore_full.", a)]]
  data.frame(base = a, candidate = b, folds = length(x), improved = sum(x > 0), mean_gain = mean(x), p_paired_t = t.test(x)$p.value) }
t5 <- bind_rows(lapply(names(ADD), function(a) rbind(cmp("notrip", paste0("notrip_", a)), cmp("modA", paste0("modA_", a)))),
                cmp("notrip", "modA"), cmp("modA_k10", "modA"))
print(t5, row.names = FALSE, digits = 4)
write_csv(t5, file.path(OUT, "TableR130_table5_cv.csv"))
saveRDS(list(cv = cv, t5 = t5), file.path(OUT, "table5_cv.rds"))
message("81_table5_cv.R complete")
