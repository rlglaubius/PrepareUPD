library(dplyr)
library(tidyr)

## For WPP 2024, some mx values were clamped to be <= 1. This can led to
## inconsistent estimates of mx and e0 in 14 of 258 countries and regions. To
## work around this, we used published estimates of qx to recalculate mx.
wpp2024_generate_mx = function() {
  wpp2024_lt_f_est = read.csv("data/2024/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv")
  wpp2024_lt_f_prj = read.csv("data/2024/WPP2024_Life_Table_Complete_Medium_Female_2024-2100.csv")
  wpp2024_lt_m_est = read.csv("data/2024/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv")
  wpp2024_lt_m_prj = read.csv("data/2024/WPP2024_Life_Table_Complete_Medium_Male_2024-2100.csv")
  
  dat = dplyr::bind_rows(list(wpp2024_lt_f_est, wpp2024_lt_f_prj, wpp2024_lt_m_est, wpp2024_lt_m_prj)) %>%
    mutate(mx_calc = round(ifelse(AgeGrpStart<100, qx / (1 - (1 - ax) * qx), mx), 8))
  
  mxF = dat %>%
    filter(Sex=="Female") %>%
    select("LocID", "Location", "Time", "AgeGrpStart", "mx_calc") %>%
    rename(country_code="LocID", country="Location", year="Time", age="AgeGrpStart") %>%
    pivot_wider(names_from=year, values_from=mx_calc)
  
  mxM = dat %>%
    filter(Sex=="Male") %>%
    select("LocID", "Location", "Time", "AgeGrpStart", "mx_calc") %>%
    rename(country_code="LocID", country="Location", year="Time", age="AgeGrpStart") %>%
    pivot_wider(names_from=year, values_from=mx_calc)
  
  write.table(mxF, "data/2024/mxF_alt.txt", sep="\t", row.names=FALSE)
  write.table(mxM, "data/2024/mxM_alt.txt", sep="\t", row.names=FALSE)
}
