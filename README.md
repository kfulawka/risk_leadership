# Human and Chimpanzee Leaders are More Risk-Loving

This repository contains the R code and supporting files used for the analyses reported in:

**Human and Chimpanzee Leaders Are More Risk-Loving**  
Lou M. Haux, Kamil Fulawka, Gert G. Wagner, Ralph Hertwig, and Esther Herrmann
*The Leadership Quarterly*

The repository is organized to reproduce the data processing, statistical models, figures, and model-based results reported in the paper.

## Reproducing the analyses

The scripts should be run in the following order.

### 1. Data preparation and model estimation

First, run the scripts beginning with `00`:

- `00a_chimps_dat_mod.R`
- `00b_human_dat_mod.R`
- `00c_within_human_var.R`

These scripts prepare the analysis data, estimate the statistical models, and save the resulting processed data and posterior model objects to the `data/` and `posteriors/` directories, respectively.

The model-estimation step must be completed before running the scripts used to reproduce the reported model-based results.

### 2. Main results and sensitivity analyses

After the required data objects and posterior model objects have been generated, run:

- `01_main_results.R`
- `02_chimps_rater_sensitivity.R`

`01_main_results.R` reproduces the main figures, descriptive summaries, regression estimates, and other model-based results reported in the manuscript.

`02_chimps_rater_sensitivity.R` reproduces the chimpanzee rater-sensitivity analysis using non-overlapping subsets of caregivers for hierarchy-rank and risk-preference ratings.

Additional helper scripts used for plotting and extracting model estimates are also included in the repository.

## Data availability

### Chimpanzee data

The chimpanzee data required to reproduce the chimpanzee analyses are included in this repository.

### Human data

The human analyses use data from the German Socio-Economic Panel (SOEP), provided by the German Institute for Economic Research (DIW Berlin).

The SOEP data are **not included in this repository** and must be obtained separately from DIW Berlin under the applicable SOEP data-access conditions.

Accordingly, the following scripts cannot be run without access to the required SOEP data:

- `00b_human_dat_mod.R`
- `00c_within_human_var.R`

Once the required SOEP data have been obtained and placed in the expected location, these scripts can be run as described above.

## Model diagnostics

The `brms_diagnostics/` directory contains diagnostic output for the Bayesian models underlying the main results reported in the paper.

For the relevant models, the directory contains:

- raw model summaries in plain-text (`.txt`) format;
- posterior-distribution plots;
- MCMC trace plots; and
- autocorrelation diagnostics.

These files allow inspection of model estimates, convergence, and sampler behavior without having to re-estimate the models.

## Repository structure

- `00a_chimps_dat_mod.R` — chimpanzee data preparation and model estimation
- `00b_human_dat_mod.R` — human data preparation and model estimation
- `00c_within_human_var.R` — within-person human analysis and model estimation
- `01_main_results.R` — main figures, descriptive summaries, and model-based results
- `02_chimps_rater_sensitivity.R` — chimpanzee rater-sensitivity analysis
- `99_ce_dat_plt.R` — helper functions for plotting conditional effects and observed data
- `99_extract_coefs.R` — helper function for extracting and saving posterior coefficient summaries
- `data/` — raw and processed analysis data
- `posteriors/` — saved Bayesian model objects
- `results/` — generated figures and model summaries
- `brms_diagnostics/` — model summaries and MCMC diagnostics

## Data and Code Availability

The materials in this repository are released under separate licenses for data and code:

- **Data license:** [PDDL (Open Data Commons Public Domain Dedication and License)](https://opendatacommons.org/licenses/pddl/)
- **Code license:** [MIT License](https://opensource.org/license/MIT)

The chimpanzee data distributed with this repository may be used and shared under the PDDL license.

The R code is released under the MIT License to facilitate transparency, reuse, and reproducibility.

The SOEP human data are not distributed with this repository and are therefore **not covered by the repository's data license**. Their use remains subject to the data-access and licensing conditions of DIW Berlin / SOEP.

## Contact

For questions specifically related to the statistical analyses, please contact:

**Kamil Fulawka**  
kamil.fulawka@tu-dresden.de

**Lou M. Haux**  
haux@mpib-berlin.mpg.de
