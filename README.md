# PrepareUPD
Prepare UPD files for Spectrum using World Population Prospects outputs

# Background
World Population Prospects (WPP) outputs are provided by the United Nations Population Division for use in Spectrum in support of the UNAIDS annual HIV estimates process. The inputs required by Spectrum are stored in `.upd` files. This package automates much of the process of producing these `.upd` files.

# How to use this package
## Setup
1. Place required WPP outputs in the `data` folder. These should be placed in a subfolder, for example, `data/2024` for WPP 2024. See the "WPP outputs" section below for a description of the required outputs. 
2. In R, run `prepare_rds(wpp_revision)` to reformat the WPP products as RDS files. `wpp_revision` is the name of the folder (e.g., `2024`) where WPP outputs are stored.

## Producing UPD files
To be completed

# WPP outputs
The required WPP outputs used to produce Spectrum UPD files are not included in this repository because they are quite large, and may be shared in advance of the public release of WPP revisions. We document the required outputs here.

The required WPP outputs consist of the following files

| File names | Description |
|-----------|-------------|-------------|
| `e0M.txt`, `e0F.txt`   | Life expectancy at birth by sex (M=male, F=female). This is a tab-delimited table with columns `country_code`, `country`, then all years 1950 from 2100. |
| `mxM.txt`, `mxF.txt`   | Central death rates by sex. This is a tab-delimited table with columns `country_code`, `country`, then all years 1950 from 2100. |
| `popM.txt`, `popF.txt` | Population at January 1. This is a tab-delimited table with columns `country_code`, `country`, `age`, then all years 1950 from 2100. |

Central death rates and life expectancies at birth are not quite enough to construct the life tables Spectrum needs. For WPP 2024 the UN Population Division also provided numbers of survivors to certain ages from abridged life tables. The number of survivors to age 1 (life table function $l_1$) suffice to complete the life tables Spectrum needs.

- `WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Male.xlsx`
- `WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Female.xlsx`
