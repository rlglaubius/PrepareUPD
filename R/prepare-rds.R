## Helper function used to read text files provided by UN Population Division
## for indicators that are stratified by age and year.
read_indicator_by_age_year = function(filename) {
  col_type = c("integer", "factor", "integer", rep("numeric", 2100 - 1950 + 1))
  read.table(filename, sep="\t", header=TRUE, check.names=FALSE, quote="\"", colClasses=col_type)
}

## Helper function used to read text files provided by UN Population Division
## for indicators that are stratified by year.
read_indicator_by_year = function(filename) {
  col_type = c("integer", "factor", rep("numeric", 2100 - 1950 + 1))
  read.table(filename, sep="\t", header=TRUE, check.names=FALSE, quote="\"", colClasses=col_type)
}

prepare_population = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing population data..."))
  data_path = sprintf("data/%s", wpp_revision)
  pop_list = list(Male   = read_indicator_by_age_year(sprintf("%s/popM.txt", data_path)),
                  Female = read_indicator_by_age_year(sprintf("%s/popF.txt", data_path)))
  pop_data = dplyr::bind_rows(pop_list, .id="sex") 
  pop_data$sex = factor(pop_data$sex, levels=c("Male", "Female")) # Force ordering of levels so that Male=1, Female=2 as Spectrum expects
  saveRDS(pop_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_life_table = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing life table data..."))
  data_path = sprintf("data/%s", wpp_revision)
  
  col_type = c("numeric", "text", "text",    "text", "text",
               "text",    "text", "numeric", "text",	"numeric",
               "numeric", "text",	"text",	   "numeric",
               rep("numeric", 22))
  
  if (wpp_revision == "2024") {
    ## lx was provided in abridged format in Excel for WPP 2024. Since we have mx
    ## and e0, we really just need l1 to work out the whole life table, but we
    ## retain the remaining lx values to verify our calculations.
    lx_list = list(
      m_est = readxl::read_excel("data/2024/WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Male.xlsx",   sheet="Estimates", col_types=col_type),
      m_med = readxl::read_excel("data/2024/WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Male.xlsx",   sheet="Medium",    col_types=col_type),
      f_est = readxl::read_excel("data/2024/WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Female.xlsx", sheet="Estimates", col_types=col_type),
      f_med = readxl::read_excel("data/2024/WPP2024_Survivors_to_exact_age_(lx)_per_100,000_live_births_Abridged_Ages_Female.xlsx", sheet="Medium",    col_types=col_type))
    
    ## Reformat lx for consistency with e0 and mx
    lx_long = reshape2::melt(dplyr::bind_rows(lx_list),
                             id.vars=c("LocationID", "LocationName", "Sex", "Year"),
                             measure.vars=c("0", "1", seq(5, 95, 5), "100+"),
                             variable.name="age",
                             value.name="value")
    lx_long$age = as.numeric(gsub("100\\+", "100", as.character(lx_long$age)))
    lx_wide = reshape2::dcast(dplyr::rename(lx_long, country_code=LocationID, country=LocationName, sex=Sex, year=Year),
                              country_code+country+age+sex~year)
  } else {
    error("No source for lx")
  }
  
  e0_wide = dplyr::bind_rows(list(Male   = read_indicator_by_year(sprintf("%s/e0M.txt", data_path)),
                                  Female = read_indicator_by_year(sprintf("%s/e0F.txt", data_path))), .id="sex")
  mx_wide = dplyr::bind_rows(list(Male   = read_indicator_by_age_year(sprintf("%s/mxM.txt", data_path)),
                                  Female = read_indicator_by_age_year(sprintf("%s/mxF.txt", data_path))), .id="sex")
  
  lx_wide$sex = factor(lx_wide$sex, levels=c("Male", "Female"))
  e0_wide$sex = factor(e0_wide$sex, levels=c("Male", "Female"))
  mx_wide$sex = factor(mx_wide$sex, levels=c("Male", "Female"))
  
  ltab_data = list(mx = mx_wide, e0 = e0_wide, lx = lx_wide)
  saveRDS(ltab_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_tfr = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing total fertility rate data..."))
  data_path = sprintf("data/%s", wpp_revision)
  tfr_data = read_indicator_by_year(sprintf("%s/tfr.txt", data_path))
  saveRDS(tfr_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_srb = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing sex ratio at birth data..."))
  data_path = sprintf("data/%s", wpp_revision)
  srb_data = read_indicator_by_year(sprintf("%s/sexRatio.txt", data_path))
  saveRDS(srb_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_fertility_by_age = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing proportionate age-specific fertility data..."))
  data_path = sprintf("data/%s", wpp_revision)
  pasfr_data = read_indicator_by_age_year(sprintf("%s/percentASFR.txt", data_path))
  saveRDS(pasfr_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_net_migration = function(wpp_revision, rds_name) {
  cat(sprintf("- preparing net migration data..."))
  data_path = sprintf("data/%s", wpp_revision)
  migr_list = list(Male   = read_indicator_by_age_year(sprintf("%s/migrationM.txt", data_path)),
                   Female = read_indicator_by_age_year(sprintf("%s/migrationF.txt", data_path)))
  migr_data = dplyr::bind_rows(migr_list, .id="sex") 
  migr_data$sex = factor(migr_data$sex, levels=c("Male", "Female")) # Force ordering of levels so that Male=1, Female=2 as Spectrum expects
  saveRDS(migr_data, sprintf("%s/%s", data_path, rds_name))
  cat(sprintf("done\n"))
}

prepare_rds = function(wpp_revision="2024") {
  cat(sprintf("processing revision %s\n", wpp_revision))
  # prepare_metadata("data/WPP2022_metadata.rds")
  prepare_population(wpp_revision, "population.rds")
  prepare_life_table(wpp_revision, "life-table.rds")
  prepare_tfr(wpp_revision, "tfr.rds")
  prepare_srb(wpp_revision, "srb.rds")
  prepare_fertility_by_age(wpp_revision, "pasfr.rds")
  prepare_net_migration(wpp_revision, "migration.rds")
}


