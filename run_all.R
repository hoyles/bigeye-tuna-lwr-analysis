# Run every script in order, from the repository root. Each script sources
# R/00_setup.R itself. The full set takes several hours; see README.md.

scripts <- c(
  "32_final_model",                      # analysis dataset, Model A and Model C: run first
  "24_set_icc", "30_specification_cv", "50_bait_and_operational", "64_hbf_within_trip",
  "37_scat_vs_gaussian_final", "38_gaussian_counterpart",
  "78_trip_curvature_cv", "54_trip_slope_replication", "77_observer_trip_slope_reml",
  "48_observer_vs_trip_lw", "56_trip_slope_delivered",
  "33_spatial_with_trip", "63_spatial_description", "34_regional",
  "15_uniform_baseline", "16_rounding_bias_by_size", "17_rounding_bias_by_weight",
  "10_rounding_consequences",
  "39_manuscript_tables",                # before 45_
  "45_rgg_supplementary",                # before 46_
  "46_period_bootstrap",
  "40_manuscript_figures",               # after 38_
  "75_figure1_and_supplementary_s1_s2",
  "71_supplementary_maps", "72_distribution_figure", "73_seasonal_marginal")

for (s in scripts) {
  message("\n==== ", s, " ====")
  t0 <- Sys.time()
  source(file.path("R", paste0(s, ".R")), local = new.env())
  message(sprintf("==== %s done in %.1f min", s, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
