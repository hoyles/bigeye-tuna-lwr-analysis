# Bigeye Tuna Length-Weight Relationship Analysis

Analysis of length-weight relationships for Pacific bigeye tuna (*Thunnus obesus*) using Taiwanese observer program data (2008-2022).

## Project Overview

This repository contains code and documentation for estimating bigeye tuna length-weight relationships using generalized additive mixed models (GAMMs) to address spatial, temporal, and methodological biases in fisheries observer data.

## Repository Structure
├── R/                     # Analysis scripts
├── data/                  # Data files (raw and processed)
├── output/                # Results (figures, tables, models)
├── docs/                  # Documentation and manuscripts
└── reports/               # Generated reports

## Key Features

- Bias correction for measurement rounding
- Observer effect modeling using random effects
- Spatial-temporal variation via tensor product smooths
- Process type transition analysis (RGG to RGT)
- Model validation and comparison

## Getting Started

1. Clone this repository
2. Install required packages: `source("R/00_setup.R")`
3. Run analysis scripts in numerical order

## Requirements

- R (≥ 4.0.0)
- Key packages: tidyverse, mgcv, lubridate, lunar

## Authors

Simon D. Hoyle, Jed I. Macdonald, Shui-Kai Chang

## License

MIT License - see LICENSE file for details