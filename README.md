# PrepareUPD
Prepare UPD files for Spectrum using World Population Prospects outputs

# Background
World Population Prospects (WPP) outputs are provided by the United Nations Population Division for use in Spectrum in support of the UNAIDS annual HIV estimates process. The inputs required by Spectrum are stored in `.upd` files. This package automates much of the process of producing these `.upd` files.

# How to use this package
## Setup
1. Place required WPP outputs in the `data` folder. These should be placed in a subfolder, for example, `data/2024` for WPP 2024. See the "WPP outputs" section below for a description of the required outputs. 
2. In R, run `prepare_rds(wpp_revision)` to reformat the WPP products as RDS files. `wpp_revision` is the name of the folder (e.g., `2024`) where WPP outputs are stored.

## Producing UPD files
After preparing RDS resources as described above, run the script `R/genupd.R` to prepare UPD files. You must first install Spectrum, as the script uses Spectrum's database of countries and locations. The script will scan this database and prepare three UPD files for each country and location:

1. `<Country>_<ISOCode>_acm_15_49.upd`: A UPD file with a compact life table based on all-cause mortality ("acm"). Age-specific fertility is modeled from age 15 to 49 for use in Spectrum.
2. `<Country>_<ISOCode>_acm_10_54.upd`: A UPD file with a compact life table based on all-cause mortality ("acm"). Age-specific fertility is modeled from age 10 to 54, as in WPP 2022 and later revisions. Spectrum currently cannot use UPD files with fertility at ages below 15 and above 49. These UPD files can be used with the [`DemProjR` package](https://github.com/rlglaubius/DemProjR) to compare outcomes to the 15-49 UPD files prepared for Spectrum.
3. `<Country>_<ISOCode>_acm_full.upd`: A UPD file with a detailed life table based on all-cause mortality ("acm"). Age-specific fertility is modeled from age 15 to 49 for use in Spectrum. The life tables in these files contain details needed to deduct HIV-related mortality when producing cause-deducted UPD files for countries with high HIV burden. Cause-deduction is performed using a purpose-built tool in Spectrum.

Output files are produced in the `output/upd` folder.

As `genupd` runs, it uses the [`DemProjR` package](https://github.com/rlglaubius/DemProjR) to perform demographic projections with `<Country>_<ISOCode>_acm_15_49.upd` and `<Country>_<ISOCode>_acm_10_54.upd` UPD files, then compares population estimates from these projections to WPP estimates using plots. These plots are stored in the `output/figures` subfolder. Four plots are produced per country: `pop_<ISOCode>_[m,f]_size.tiff` (e.g., `Afghanistan_4_m_size.tiff`) shows trends in population sizes for males (`m`) or females (`f`), with one panel per single age, while `pop_<ISOCode>_[m,f]_diff.tiff` shows the difference in population size from DemProjR projections in comparison to WPP estimates.

# WPP outputs
The required WPP outputs used to produce Spectrum UPD files are not included in this repository because they are quite large, and may be shared in advance of the public release of WPP revisions. We document the required outputs here.

The required WPP outputs consist of the following files

| File names | Description |
|---|---|
| `e0M.txt`, `e0F.txt`   | Life expectancy at birth by sex (M=male, F=female). Files contain tab-delimited tables with columns `country_code`, `country`, then all years 1950 from 2100. |
|`migrationM.txt`, `migrationF.txt` | Net migrants by sex, in 1000s. Files contain tab-delimited tables with columns `country_code`, `country`, `age`, then all years 1950 from 2100. |
| `mxM.txt`, `mxF.txt`   | Central death rates by sex. Files contain tab-delimited tables with columns `country_code`, `country`, `age`, then all years 1950 from 2100. |
| `percentASFR.txt` | Proportionate age-specific fertility rates. File contains a tab-delimited table with columns `country_code`, `country`, `age`, then all years 1950 from 2100. |
| `popM.txt`, `popF.txt` | Population at January 1 by sex, in 1000s. Files contain tab-delimited tables with columns `country_code`, `country`, `age`, then all years 1950 from 2100. |
| `sexRatio.txt` | Sex ratio at birth, expressed as male births per female birth. File contains a tab-delimited table with columns `country_code`, `country`, then all years 1950 from 2100. |
| `tfr.txt` | Total fertility rate. File contains a tab-delimited table with columns `country_code`, `country`, then all years 1950 from 2100. |

Central death rates and life expectancies at birth are not quite enough to construct the life tables Spectrum needs. For WPP 2024 the UN Population Division also provided numbers of survivors to certain ages from abridged life tables. The number of survivors to age 1 (life table function $l_1$) suffice to complete the life tables Spectrum needs.

- `WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Male.xlsx`
- `WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Female.xlsx`
