# ==============================================================================
# 70_hbf_select.R -- the linear HBF term that remains under Model A, and a null-space penalty
#
# Section 4.2. Model A plus s(hbf) by gam(ML): the effect, its p value and AIC change; then select = TRUE,
# which can shrink a term out completely, and its effect on the other terms.
# ENGINE AND TIMING: gam(ML) for the reported fits (bam screen first); 30-90 min.
# ==============================================================================

source("R/00_setup.R")
set.seed(20260904)

d <- readRDS(file.path(OUT, "final_dataset.rds"))$d
d <- d %>% mutate(obs_name = factor(obs_name), tripf = factor(tripf),
                  sexf = factor(sexf), yrf = factor(yrf),
                  loglen_c = loglen - mean(loglen)) %>% as.data.frame()
cat(sprintf("n = %d | HBF range %.0f to %.0f, median %.0f\n",
            nrow(d), min(d$hbf), max(d$hbf), median(d$hbf)))

SP <- "te(lon, lat, mon, k = c(6,6,4), bs = c('cr','cr','cc'))"
RE <- "s(obs_name, bs='re') + s(tripf, bs='re') + s(loglen_c, tripf, bs='re')"
base  <- paste("logwt ~ s(loglen, k=20) +", RE, "+ sexf +", SP, "+ yrf")
wHBF  <- paste(base, "+ s(hbf, k=15)")

info <- function(m, lab) {
  L <- vapply(m$smooth, function(z) z$label, character(1))
  i <- match(lab, L); if (is.na(i)) return(NULL)
  st <- summary(m)$s.table
  e <- sum(m$edf[m$smooth[[i]]$first.para:m$smooth[[i]]$last.para])
  p <- if (lab %in% rownames(st)) st[lab, "p-value"] else NA_real_
  c(edf = e, p = p)
}
# effect of HBF across its observed range at a reference length, other terms held
rng <- function(m) {
  L <- vapply(m$smooth, function(z) z$label, character(1))
  if (!"s(hbf)" %in% L) return(NA_real_)
  nd <- data.frame(hbf = seq(min(d$hbf), max(d$hbf), length.out = 50))
  nd$loglen <- log(120); nd$loglen_c <- log(120) - mean(d$loglen)
  nd$sexf <- factor("F", levels = levels(d$sexf))
  nd$yrf  <- factor(levels(d$yrf)[5], levels = levels(d$yrf))
  nd$obs_name <- factor(levels(d$obs_name)[1], levels = levels(d$obs_name))
  nd$tripf    <- factor(levels(d$tripf)[1],    levels = levels(d$tripf))
  nd$lon <- median(d$lon); nd$lat <- median(d$lat); nd$mon <- 6
  pr <- predict(m, nd, type = "terms", terms = "s(hbf)")
  100 * (exp(diff(range(as.numeric(pr)))) - 1)
}

cat("\n=== HBF with the trip terms present ===\n")
m0 <- bam(as.formula(base),  data = d, family = scat, method = "fREML", discrete = TRUE, gamma = 1.4)
m1 <- bam(as.formula(wHBF),  data = d, family = scat, method = "fREML", discrete = TRUE, gamma = 1.4)
m2 <- bam(as.formula(wHBF),  data = d, family = scat, method = "fREML", discrete = TRUE,
          select = TRUE, gamma = 1.4)
i1 <- info(m1, "s(hbf)"); i2 <- info(m2, "s(hbf)")
cat(sprintf("  select = FALSE : edf %.3f | p = %.3f | range across HBF %.2f%% | dAIC vs no HBF %+.1f\n",
            i1["edf"], i1["p"], rng(m1), AIC(m1) - AIC(m0)))
cat(sprintf("  select = TRUE  : edf %.3f | p = %.3f | range across HBF %.2f%% | dAIC vs no HBF %+.1f\n",
            i2["edf"], i2["p"], rng(m2), AIC(m2) - AIC(m0)))
cat("  edf ~1 under select=FALSE means a LINEAR effect survives: the penalty\n")
cat("  shrinks toward its null space, which is a straight line, not toward zero.\n")
cat("  select=TRUE adds a null-space penalty, so a term CAN be removed entirely.\n")

cat("\n=== does select = TRUE remove anything else? ===\n")
m3 <- bam(as.formula(base), data = d, family = scat, method = "fREML", discrete = TRUE,
          select = TRUE, gamma = 1.4)
L0 <- vapply(m0$smooth, function(z) z$label, character(1))
L3 <- vapply(m3$smooth, function(z) z$label, character(1))
tab <- do.call(rbind, lapply(seq_along(L0), function(i) {
  e0 <- sum(m0$edf[m0$smooth[[i]]$first.para:m0$smooth[[i]]$last.para])
  j <- match(L0[i], L3)
  e3 <- sum(m3$edf[m3$smooth[[j]]$first.para:m3$smooth[[j]]$last.para])
  data.frame(term = L0[i], edf_normal = round(e0, 2), edf_select = round(e3, 2))
}))
print(tab, row.names = FALSE)
cat(sprintf("  total edf %.1f -> %.1f | dAIC %+.1f\n",
            sum(m0$edf), sum(m3$edf), AIC(m3) - AIC(m0)))
cat("  select=TRUE is a diagnostic here, not a proposed specification: it changes\n")
cat("  the penalty on every smooth at once, including the length relationship.\n")

saveRDS(list(hbf_normal = i1, hbf_select = i2,
             range_normal = rng(m1), range_select = rng(m2),
             daic_normal = AIC(m1)-AIC(m0), daic_select = AIC(m2)-AIC(m0),
             terms = tab, daic_select_all = AIC(m3)-AIC(m0)),
        file.path(OUT, "hbf_select.rds"))
write_csv(tab, file.path(OUT, "TableR104_select_true_edf.csv"))
# ---------------------------------------------------------------------------
# EXACT PASS -- gam(method = "ML"). Cached on the formula AND on select, since
# two fits share a formula and differ only in the penalty; a key that ignored
# select would silently return the wrong one.
# ---------------------------------------------------------------------------
cat("
=== exact refit: gam(method = \"ML\") ===
")
JOBDIR <- file.path(OUT, "hbf_select_jobs"); dir.create(JOBDIR, showWarnings = FALSE)

fitg <- function(f, id, select = FALSE) {
  outf <- file.path(JOBDIR, sprintf("%s_%s_sel%d_ML.rds", id,
                    substr(digest::digest(deparse(f)), 1, 8), as.integer(select)))
  if (file.exists(outf)) { cat("  (cached)", id, "
"); return(readRDS(outf)) }
  t0 <- Sys.time()
  m <- gam(as.formula(f), data = d, family = scat, method = "ML", select = select, gamma = 1.4)
  cat(sprintf("  fitted %-12s in %5.1f min
", id,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  saveRDS(m, outf); m
}

g0 <- fitg(base, "no_hbf")
g1 <- fitg(wHBF, "hbf")
g2 <- fitg(wHBF, "hbf_select", select = TRUE)

j1 <- info(g1, "s(hbf)"); j2 <- info(g2, "s(hbf)")
cat(sprintf("  select = FALSE : edf %.3f | p = %.3f | range across HBF %.2f%% | dAIC vs no HBF %+.1f
",
            j1["edf"], j1["p"], rng(g1), AIC(g1) - AIC(g0)))
cat(sprintf("  select = TRUE  : edf %.4f | p = %.3f | range across HBF %.2f%% | dAIC vs no HBF %+.1f
",
            j2["edf"], j2["p"], rng(g2), AIC(g2) - AIC(g0)))

Lg0 <- vapply(g0$smooth, function(z) z$label, character(1))
g3  <- fitg(base, "no_hbf_select", select = TRUE)
Lg3 <- vapply(g3$smooth, function(z) z$label, character(1))
tabg <- do.call(rbind, lapply(seq_along(Lg0), function(i) {
  e0 <- sum(g0$edf[g0$smooth[[i]]$first.para:g0$smooth[[i]]$last.para])
  j  <- match(Lg0[i], Lg3)
  e3 <- sum(g3$edf[g3$smooth[[j]]$first.para:g3$smooth[[j]]$last.para])
  data.frame(term = Lg0[i], edf_normal = round(e0, 2), edf_select = round(e3, 2))
}))
cat("
  does the null-space penalty remove anything else? (exact fits)
")
print(tabg, row.names = FALSE)
cat(sprintf("  total edf %.1f -> %.1f | dAIC %+.1f
",
            sum(g0$edf), sum(g3$edf), AIC(g3) - AIC(g0)))

saveRDS(list(engine = "gam(ML)",
             hbf_normal = j1, hbf_select = j2,
             range_normal = rng(g1), range_select = rng(g2),
             daic_normal = AIC(g1) - AIC(g0), daic_select = AIC(g2) - AIC(g0),
             terms = tabg, daic_select_all = AIC(g3) - AIC(g0)),
        file.path(OUT, "hbf_select_exact.rds"))
write_csv(tabg, file.path(OUT, "TableR107_select_true_edf_exact.csv"))
message("70_hbf_select.R complete")
