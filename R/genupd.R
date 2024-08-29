library(DemProjR)
library(ggplot2)

source("R/wpp2upd.R")

spec_file = "C:/Program Files (x86)/Spectrum6/ModData/GB/GBModData.xlsx"
spec_countries = data.frame(readxl::read_excel(spec_file, sheet="CountryMaster"))
spec_countries = spec_countries[spec_countries$Subnat.Code==0,] # filter out subnational regions
spec_countries = spec_countries[,c("Country.Code", "Country")]  # retain just the ISO numeric code and country name used by Spectrum
colnames(spec_countries) = c("country_code", "country")         # standardize column names'

spec_regions = data.frame(readxl::read_excel(spec_file, sheet="WPP"))
spec_regions = spec_regions[,c("Region.Code", "Region")]
colnames(spec_regions) = c("country_code", "country")

spec_locations = dplyr::bind_rows(list(spec_countries, spec_regions))

wpp_data = wpp_init()

plot_size = function(pop, tiffname) {
  ggplot(pop, aes(x=Year, y=Value, color=Source)) +
    geom_line() +
    facet_wrap(~Age) +
    theme_bw()
  ggsave(tiffname, dpi=300, compression="lzw", width=16, height=9)
}

plot_diff = function(pop, tiffname) {
  ggplot(pop, aes(x=Year, y=Value, color=Source)) +
    geom_line() +
    facet_wrap(~Age) +
    theme_bw()
  ggsave(tiffname, dpi=300, compression="lzw", width=16, height=9)
}

calc_projection = function(upd_file) {
  pars = read_upd(upd_file)
  proj = demproj(pars, ccmpp_migr_end, spec.fert=FALSE)
  pop = data.table::as.data.table(as.data.frame.table(proj$pop, responseName="Value"))
  pop$Year = as.numeric(as.character(pop$Year))
  pop$Age  = as.numeric(gsub("80\\+", "80", pop$Age))
  return(pop)
}

genupd = function(country_code, spec_locations, wpp_data = NULL, tag="24") {
  if (is.null(wpp_data)) {
    wpp_data = wpp_init()
  }
  
  loc_name = spec_locations$country[spec_locations$country_code==country_code]
  if (length(loc_name) == 0) {
    warning(sprintf("Location ID %d not found in spec_locations, skipped", country_code))
  } else {
    ## "acm" stands for all-cause mortality in the UPD filenames. This is
    ## because the UPD files produced have life tables without any specific
    ## causes (e.g., HIV-related mortality) deducted
    upd_name = sprintf("output/upd/%s_%d_%s_acm.upd", loc_name, country_code, tag)
    
    upd_spec = gsub("\\.upd", "_15_49.upd", upd_name)
    upd_test = gsub("\\.upd", "_10_54.upd", upd_name)
    upd_full = gsub("\\.upd", "_full.upd",  upd_name)
    
    ## We generate three UPD files per country, using Kenya filenames as an example
    ## 1. Kenya_404_22_acm_15_49.upd
    ## 2. Kenya_404_22_acm_10_54.upd
    ## 3. Kenya_404_22_acm_full.upd
    ##
    ## Kenya_404_22_acm_15_49.upd is intended for use in Spectrum (except in
    ## places where we need to deduct HIV-related mortality from life tables).
    ## Fertility is truncated to ages 15-49, and the life table is compact.
    ## Kenya_404_22_acm_10_54.upd assumes fertility at ages 10-54 for
    ## consistency with WPP2022, and is intended for verification against
    ## WPP2022 population estimates via demographic projections done in
    ## DemProjR. Kenya_404_22_acm_full.upd truncates fertility to ages 15-49,
    ## and includes a full life table, which is needed for HIV-related mortality
    ## deduction
    wpp2upd(country_code, upd_spec, wpp_data=wpp_data, compact=TRUE,  fert10_54=FALSE, final_year=2100)
    wpp2upd(country_code, upd_test, wpp_data=wpp_data, compact=TRUE,  fert10_54=TRUE,  final_year=2100)
    wpp2upd(country_code, upd_full, wpp_data=wpp_data, compact=FALSE, fert10_54=FALSE, final_year=2100)
    
    ## Calculate population projections for verification
    dpr_pop_15_49 = calc_projection(upd_spec)
    dpr_pop_10_54 = calc_projection(upd_test)
    
    wpp_pop = reshape2::melt(wpp_data$population[wpp_data$population$country_code==country_code,],
                             id.vars = c("country_code", "country", "sex", "age"),
                             measure.vars = sprintf("%d", 1971:2100),
                             variable.name="year",
                             value.name="value")
    colnames(wpp_pop) = c("country_code", "country", "Sex", "Age", "Year", "Value")
    wpp_pop$Value = 1000 * wpp_pop$Value
    wpp_pop$Age[wpp_pop$Age>=80] = 80
    wpp_pop$Year = as.numeric(as.character(wpp_pop$Year)) - 1 # Align with DemProjR (WPP 1971 = January 1, 1971 ~ December 31, 1970 = DemProjR 1970)
    wpp_pop = data.table::as.data.table(wpp_pop)[,.(Value=sum(Value)),by=.(Year,Sex,Age)]

    pop_long = dplyr::bind_rows(list(WPP=wpp_pop, DP15_49=dpr_pop_15_49, DP10_54=dpr_pop_10_54), .id="Source")
    pop_wide = reshape2::dcast(pop_long, Year+Sex+Age~Source, value.var="Value")
    pop_wide$Diff_DP10_54 = pop_wide$DP10_54 - pop_wide$WPP
    pop_wide$Diff_DP15_49 = pop_wide$DP15_49 - pop_wide$WPP
    pop_diff = reshape2::melt(pop_wide, id.vars=c("Year", "Sex", "Age"), measure.vars=c("Diff_DP10_54", "Diff_DP15_49"), variable.name="Source", value.name="Value")
    pop_diff$Source = gsub("Diff_", "", pop_diff$Source)

    plot_size(subset(pop_long, Sex=="Female"),          sprintf("output/figures/pop_%d_f_size.tiff", country_code))
    plot_diff(subset(pop_diff, Sex=="Female" & Age<80), sprintf("output/figures/pop_%d_f_diff.tiff", country_code))

    plot_size(subset(pop_long, Sex=="Male"),            sprintf("output/figures/pop_%d_m_size.tiff", country_code))
    plot_diff(subset(pop_diff, Sex=="Male" & Age<80),   sprintf("output/figures/pop_%d_m_diff.tiff", country_code))
  }
}

# lapply(spec_locations$country_code, function(country_code) {
lapply(c(646), function(country_code) {
  t0 = Sys.time()
  loc_name = spec_locations$country[spec_locations$country_code==country_code]
  cat(sprintf("Processing %0.0f (%s)...", country_code, loc_name))
  if (country_code %in% wpp_data$tfr$country_code) {
    genupd(country_code, spec_locations, wpp_data=wpp_data)
    t1 = Sys.time()
    cat(sprintf("done (%0.0f seconds)\n", t1-t0))
  } else {
    cat(sprintf("skipped: no WPP data!\n"))
  }
})
