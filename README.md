# Net-Zero Targets under Dual Emissions Accounting and Global Effort Sharing

Replication code for:

> **"Net-Zero Targets under Dual Emissions Accounting and Global Effort Sharing"**  
> *[Authors, Journal, Year — to be updated upon acceptance]*

---

## Overview

This repository contains the R code used to derive national carbon budgets
and implied net-zero years under a dual (territorial + consumption-based)
accounting framework, and to produce all figures in the paper and supplementary
material.

The key finding is that assessments of national mitigation ambition are
sensitive not only to the carbon budget allocation principle applied, but also
to the weight assigned to producing versus consuming countries in bearing
responsibility for emissions.

---

## Repository structure

```
main.R                        # Entry point — sources all modules in order
R/
  00_setup.R                  # Pre-flight checks and package loading
  01_parameters.R             # Global parameters and plot labels
  02_load_data.R              # Download and read raw data
  03_prepare_data.R           # Cleaning, reshaping, Rest-of-world aggregation
  04_allocation_functions.R   # Allocation-principle helper functions
  05_calculate_budgets.R      # National carbon budget calculations (parallelised)
  06_plot_sample_countries.R  # Sample-country figures (Figures 1–3)
  07_plot_all_countries.R     # All-countries figures (Figures 4–5, Extended Data)
  08_export.R                 # Write Excel output tables
  09_text_data.R              # In-text statistics (sample countries + all countries)
  10_sankey_transitions.R     # Sankey diagram of budget-category transitions (Figure 6)
python/
  fetch_ssp_data.py           # Fetch SSP projections from IIASA (run once)
data/
  ssp_data.csv                # SSP output from fetch_ssp_data.py (not in repo)
output/
  Graphs/                     # PNG figures (created on first run)
  NationalCarbonBudgets.xlsx  # Full results table
  DataForSupplementary.xlsx   # Data underlying supplementary figures
```

---

## Quick start

### 1. Install R packages

```r
install.packages(c(
  "tidyverse", "readxl", "openxlsx", "wbstats",
  "doParallel", "foreach", "ggrepel", "ggforce", "ggh4x", "scico", "legendry"
))
```

Minimum requirement: **dplyr ≥ 1.1.0** (released 2023-01).

> **Note on ggplot2 version:** `legendry`'s nested axis guide is incompatible
> with ggplot2 ≥ 4.0.0. The code is tested on ggplot2 3.5.2. To restore a
> known-working set of packages:
> ```r
> remotes::install_version("ggplot2",   "3.5.2")
> remotes::install_version("legendry",  "0.2.2")
> remotes::install_version("ggh4x",     "0.2.8")
> remotes::install_version("ggrepel",   "0.9.5")
> ```

### 2. Fetch SSP scenario data (once)

```bash
pip install pyam-iamc pandas
python python/fetch_ssp_data.py
```

This downloads GDP|PPP and Population projections from the
[IIASA SSP Scenario Database 3.2](https://data.ece.iiasa.ac.at) and saves
them to `data/ssp_data.csv`.  A free IIASA account is required; credentials
are cached after the first login.

After downloading, open `data/ssp_data.csv`, note the model names printed
by the script, and pin them in `R/01_parameters.R`:

```r
SSPModelGDP        <- "OECD ENV-Growth 2023"   # example — check your output
SSPModelPopulation <- "IIASA-WiC POP 2023"     # example — check your output
```

**If you cannot access the IIASA database**, place `iamc_db_GDP.xlsx` and
`iamc_db_POP.xlsx` (IAMC wide format, downloaded manually from the SSP portal)
in the project root.  `R/02_load_data.R` will detect the xlsx files and use
them instead.

### 3. Provide remaining input files

| File | Description | Source |
|------|-------------|--------|
| `data/CountryAssumptions.xlsx` | Country list, development classification, EU membership | Compiled by authors |
| `data/National_Fossil_Carbon_Emissions_2025_v0.3.xlsx` | GCP national emission data | [Global Carbon Project](https://globalcarbonbudget.org/datahub/) |
| `data/Global_Carbon_Budget_2025_v0.6.xlsx` | GCP global carbon budget | [Global Carbon Project](https://globalcarbonbudget.org/datahub/) |

**UN Population data** are fetched automatically from the
[UN Population Data Portal API](https://population.un.org/dataportalapi/) on
the first run and cached to `data/un_population.csv`. A free Bearer token is
required for the first run:

```r
Sys.setenv(UN_POP_TOKEN = "your_token_here")
source("main.R")
```

Or add `UN_POP_TOKEN=your_token_here` to your `.Renviron` file
(`usethis::edit_r_environ()`). The token is not written to any project file.

**Historical GDP data** are downloaded automatically from the
[World Bank API](https://data.worldbank.org/) at run time.

### 4. Run the analysis

```r
# In R or RStudio
source("main.R")
```

```bash
# From the terminal
Rscript main.R
```

All figures are saved to `output/Graphs/` and tables to `output/`.

---

## Carbon budget allocation principles

Five allocation principles are implemented (see Supplementary Methods):

| Principle | Description |
|-----------|-------------|
| Equal Cumulative per Capita | Equal per-capita share of the cumulative budget from a historic base year |
| Annual Equal per Capita | Equal per-capita share of the annual global emission pathway |
| Grandfathering | Proportional to current (2021) emission share |
| Contraction and Convergence | Convergence to equal per capita by 2050 |
| Capability | Inverse weighting by GDP per unit of population² |

The `AccountingFramework` parameter (*f*) ranges from 0 (full territorial
responsibility) to 1 (full consumption-based responsibility), in steps of 0.05.
For the Equal Cumulative per Capita principle, historic base years from 1990 to
2021 (in 5-year steps) are used.

---

## Key parameters (`R/01_parameters.R`)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `NoCores` | `detectCores() - 1` | CPU cores for parallel budget calculation |
| `CarbonBudget` | 500 GtCO₂ (1.5 °C), 900 GtCO₂ (2 °C) | AR6 WG1 budgets (adjusted for 2020–2021) |
| `SSPScenario` | `"SSP2"` | SSP scenario used for Capability and C&C |
| `SSPModelGDP` | `NULL` (auto) | Model for GDP|PPP projections |
| `SSPModelPopulation` | `NULL` (auto) | Model for population projections |
| `YearEnd` | `2021` | Last year with observed GCP emissions |

---

## License

[To be specified upon publication]
