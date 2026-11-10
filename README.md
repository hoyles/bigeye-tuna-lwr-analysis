# Length–weight relationships from fisheries observer data: Pacific bigeye tuna

R code for

> Hoyle, S.D., Macdonald, J.I. and Chang, S.-K. Good practices in estimating length–weight relationships from fisheries observer data: a case study using Pacific bigeye tuna (*Thunnus obesus*). *Fisheries Research* (in revision, manuscript FISH15752).

The analysis fits generalized additive mixed models (mgcv) to length and weight measurements of bigeye tuna recorded by the Taiwanese longline observer programme (TOP), 2009–2022. The scripts here reproduce the paper's tables and figures and its main results. They also show the procedures the paper recommends:

- measuring clustering before choosing model complexity;
- cross-validating on withheld trips, with the random-effect variance added back to the predictive distribution;
- testing covariates within trips;
- checking that observer and trip effects replicate;
- estimating rounding and its effect on the LWR.

## Data

The observer data are subject to legal restrictions and are not included. Data in compiled form are available from the corresponding author on reasonable request. [data/README.md](data/README.md) describes the input the scripts expect. Only the published length–weight relationships of Supplementary Table S1 are included, as `data/Supplementary_material_1_Table_S1.xlsx`.

## What is included

The scripts that build the analysis dataset, fit the reported models, and produce every table and figure, together with the main supporting analyses. Scripts keep the numbers they had during the analysis, so gaps in the numbering are expected.

Further sensitivity tests and diagnostics mentioned in the paper are not included, but are available from the authors.

| Script | Produces | Manuscript |
|---|---|---|
| `00_setup.R` | Packages, paths, data, shared helpers | — |
| `32_final_model.R` | Analysis dataset, observer exclusions, Model A and Model C | §2.4, §3.1, §3.4, §4.2, §4.4; Tables 2, 11 |
| `24_set_icc.R` | Within-set ICC, design effect, effective sample size | §3.1; Table 3 |
| `30_specification_cv.R` | Specification choices by trip-blocked cross-validation; ICC of covariates within trip | §3.2, §3.5, §4.1; Table 4 |
| `50_bait_and_operational.R` | Bait and operational covariates with and without trip effects | §4.1, §4.6; Tables 4, 5 |
| `64_hbf_within_trip.R` | Hooks between floats split into trip mean and within-trip departure | §4.2 |
| `37_scat_vs_gaussian_final.R` | Scaled-t against Gaussian errors, by cross-validation | §4.5 |
| `38_gaussian_counterpart.R` | Gaussian version of Model A | §4.5; Figure 6 |
| `78_trip_curvature_cv.R` | Trip slope and trip-level curvature, by cross-validation | §4.6 |
| `54_trip_slope_replication.R` | Split-half replication of trip slopes | §4.6 |
| `77_observer_trip_slope_reml.R` | Division of slope variation between trips and observers | §4.6 |
| `48_observer_vs_trip_lw.R` | Split-half replication of observer effects; bootstrap of the observer and trip SDs | §4.6 |
| `56_trip_slope_delivered.R` | Power-law *a* and *b* with and without the trip slope | §4.6 |
| `33_spatial_with_trip.R` | Spatial magnitude and split-half reproducibility | §4.3 |
| `63_spatial_description.R` | Description of the spatial surface and seasonal cycle | §4.3 |
| `34_regional.R` | Regional power laws against the spatial model | §4.3 |
| `15_uniform_baseline.R` | Rounding propensity and direction | §3.1, §4.6; Table 12 |
| `16_rounding_bias_by_size.R`, `17_rounding_bias_by_weight.R` | Size dependence of rounding and the implied change in *b* | §4.6 |
| `10_rounding_consequences.R` | Simulated effect of rounding on *b* | §4.6; Table 13 |
| `39_manuscript_tables.R` | Model tables, including Model B (equation 3) overall and by region | Tables 6–11 |
| `45_rgg_supplementary.R` | The 2009–2014 period under the same approach | Supplementary Text S2; §4.9 |
| `46_period_bootstrap.R` | Uncertainty in *b* by resampling trips within each period | §3.6, §4.9 |
| `40_manuscript_figures.R` | Figures 2–9 | Figures 2–9 |
| `75_figure1_and_supplementary_s1_s2.R` | Figure 1 and Supplementary Figures S1, S2 | Figure 1; Figures S1, S2 |
| `71_supplementary_maps.R`, `72_distribution_figure.R`, `73_seasonal_marginal.R` | Supplementary figures | Supplementary material |

Table 1 (variables), Table 2 (exclusion rules) and Table 5 (a summary of results from several analyses) are not computed by a single script.

## How to run

1. Install R (4.5.2 was used) and the packages below.
2. Put the observer data where `R/00_setup.R` expects them (see [data/README.md](data/README.md)).
3. Open `bigeye-tuna-lwr-analysis.Rproj`, or set the working directory to the repository root.
4. Run `run_all.R`, or run the scripts individually in the order it lists.

`32_final_model.R` must run first: it writes `output/final_dataset.rds` and `output/final_model.rds`, which most other scripts read. `38_` must run before `40_` and `72_`, `39_` before `45_`, and `45_` before `46_`.

Results go to `output/` (CSV tables and saved models) and `figures/`. Long-running scripts save each fit as it completes, under `output/*_jobs/`, and resume from there if rerun.

The full set takes several hours. The longest are the cross-validation of the error distribution (`37_`, 30–90 minutes) and the 2009–2014 analysis (`45_`, 35–60 minutes). Several scripts run fits in parallel, with the number of workers set near the top of each. Reduce it to suit the machine.

## Notes on the methods

- Anything reported in the paper is fitted with `gam()`. The faster approximation `bam(discrete = TRUE)` is used only where a model is refitted many times (cross-validation, resampling, permutation), and only differences or correlations from those fits are reported.
- Model A is reported from its maximum likelihood (ML) fit, so that the same fit serves the AIC comparisons. A restricted maximum likelihood (REML) fit gives the same estimates to within 1%.
- In cross-validation, withheld trips and their observers are new to the model. Their random effects are therefore excluded from the predicted mean, and their variance is added to the predictive distribution. Omitting that variance can reverse the ranking of models that differ in random-effect structure.

## Software

R 4.5.2 with mgcv 1.9.4, tidyverse 2.0.0 (dplyr 1.2.1, ggplot2 4.0.3), sf 1.1.2, rnaturalearth 1.2.0 (with rnaturalearthdata), jsonlite 2.0.0, scoringRules 1.1.3, digest 0.6.39, readxl 1.5.0, maps and MASS. The parallel package is part of base R.

## Licence

MIT; see [LICENSE](LICENSE).
