# Input data

The observer data are subject to legal restrictions and are not included in this repository. Data in compiled form are available from the corresponding author on reasonable request.

`R/00_setup.R` loads `data/dat.Rdata`, or the file named by the environment variable `LWR_DATA`. The file must contain a data frame `da2` with one row per bigeye tuna measured by an observer, for all processing types and both periods (2009–2022). The scripts apply every filter themselves (section 2.4 of the paper). They use these columns:

| Column | Type | Description |
|---|---|---|
| `tripID` | character | Trip identifier |
| `setid` | factor | Set identifier, unique across trips |
| `workdate` | Date | Date of the set |
| `year` | numeric | Year of the set |
| `mon` | numeric | Month of the set, 1–12 |
| `qtr` | factor | Quarter, 1–4 |
| `yrf` | factor | Year, as a factor |
| `obs_name` | factor | Anonymised observer identifier |
| `processtype` | character | Recorded processing type: `RGG` (gilled and gutted), `RGT` (gutted and tailed), or other |
| `sex` | character | `F`, `M`, or other codes (excluded) |
| `length` | numeric | Fork length, cm |
| `weight` | numeric | Processed weight, kg (whole kg) |
| `loglen`, `logwt` | numeric | log(length) and log(weight) |
| `lon`, `lat` | numeric | Set position, to one degree; longitude on a 0–360 scale |
| `region` | factor | Assessment region (see `regions_json` in `R/00_setup.R`) |
| `hbf` | numeric | Hooks between floats |
| `baitType` | character | Bait: `SAP` (saury), `MAX` (mackerel), `PIL` (pilchard), `OTH` |
| `lightstick` | factor | Light sticks used, 0 or 1 |
| `weather` | factor | `sunny`, `cloudy`, `rain`, `OTH` |
| `wavescale` | factor | Recorded sea state, `1`–`4` and `5+` |
| `lunar_illumination` | numeric | Fraction of the moon illuminated, 0–1 |
| `phase` | numeric | Lunar phase angle, radians |
| `Cluster` | numeric | Targeting cluster from set-level species composition, 1–3 |

`Supplementary_material_1_Table_S1.xlsx` is Supplementary Table S1 of the paper, the published bigeye length–weight relationships drawn in Figure 1.
