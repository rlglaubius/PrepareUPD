#' Load WPP data needed to prepare UPD files
#' @return a named list
#' @export
wpp_init = function(wpp_revision="2024") {
  data_path = sprintf("data/%s", wpp_revision)
  return(list(
    # metadata  = readRDS("data/WPP2022_metadata.rds"),
    population  = readRDS(sprintf("%s/population.rds", data_path)),
    life_table  = readRDS(sprintf("%s/life-table.rds", data_path))
    # ind_data  = readRDS("data/WPP2022_Demographic_Indicators_Medium.rds"),
    # asfr_data = readRDS("data/WPP2022_Fertility_by_Age1.rds"),
    # migr_data = readRDS("data/migration.rds")
  ))
}

#' Create a UPD file for a specified country
#'
#' @param country_code numeric country or region code
#' @param upd_name File name for the UPD file produced
#' @param wpp_data WPP data. If this is NULL, it will be loaded by
#'   \code{wpp_init}. \code{wpp_init} is slow, you may wish to call this
#'   yourself and reuse the result when preparing multiple UPD files
#' @param compact Indicates whether a compact or complete life table should be
#'   output. See Details.
#' @param fert10_54 Indicates whether age-specific fertilty should be output for
#'   ages 10-54 (\code{fert10_54=TRUE) or 15-49 (\code{fert10_54=FALSE}). See
#'   Details.
#' @section Details:
#'
#'   UPD files include a life table for each one-year period between 1970 and
#'   2050. Since many life table indicators can be derived from one another, UPD
#'   files used in Spectrum include a compact life table to reduce UPD file
#'   sizes. More complete life tables can be produced if needed.
#'
#'   If \code{compact=FALSE}, the output life table includes the following by
#'   year, age, and sex:
#'   \itemize{
#'   \item{mx  central death rates between age x and x+1}
#'   \item{ax  average number of years lived among those who die between age x and x+1}
#'   \item{qx  probability of dying between age x and x+1}
#'   \item{lx  number of survivors to age x}
#'   \item{dx  number of deaths between age x and x+1}
#'   \item{Lx  number of person-years lived between age x and x+1}
#'   \item{Tx  person-years lived after age x}
#'   \item{ex  expectation of life after age x}
#'   \item{Sx  survival ratio between age x and x+1}
#'   }
#'   If \code{compact=TRUE}, the output life table includes just \code{ex} and \code{Sx} for all
#'   ages and \code{lx} for ages \code{x=1} and \code{x=5}. Note that UPD files generated with
#'   \code{compact=FALSE} cannot be used in Spectrum.
#'
#'   Note that \code{Sx} values listed on rows for age 0 are survival from birth to age 0,
#'   age 1 pertains to survival from age 0 to age 1, etc.
#'
#'   If \code{fert10_54=TRUE}, then the <pasfrs> block of the UPD file will include
#'   proportionate age-specific fertility at ages 10-54. If \code{fert10_54=FALSE},
#'   this block will only report fertility at ages 15-49. In this case, WPP2022 estimates
#'   of fertility at ages 10-14 are redistributed at ages 15-19, and fertility at ages 50-54
#'   is redistributed to ages 45-49, so that 100% of fertility is accounted for each year.
#'   Note that UPD files generated with \code{fert10_54=TRUE} cannot be used in Spectrum.
#'
#' @export
wpp2upd = function(country_code, upd_name, wpp_data=NULL, compact=TRUE, fert10_54=FALSE) {
  if (is.null(wpp_data)) {
    wpp_data = wpp_init()
  }
  
  if (compact) {
    num_cols = 6
  } else {
    num_cols = 12
  }
  
  ## Note that we are passing wpp_data by value, which could be expensive
  upd = list(
    # header    = generate_header(country_code, wpp_data, compact),
    basepop   = generate_basepop(country_code, wpp_data),
    lfts      = generate_lfts(country_code, wpp_data, compact)
    # tfr       = generate_tfr(country_code, wpp_data),
    # srb       = generate_srb(country_code, wpp_data),
    # pasfrs    = generate_pasfrs(country_code, wpp_data, fert10_54),
    # migration = generate_migration(country_code, wpp_data)
  )
  
  upd_handle = file(upd_name, open="w", encoding="UTF-8")
  # writeChar(iconv("\ufeff", to="UTF-8"), upd_handle, eos=NULL) # write byte-order mark (BOM) to the UPD file
  # write_upd_list(      upd$header,    upd_handle, num_cols, tag="header")
  write_upd_data_frame(upd$basepop,   upd_handle, num_cols, tag="basepop")
  write_upd_data_frame(upd$lfts,      upd_handle, num_cols, tag="lfts")
  # write_upd_data_frame(upd$tfr,       upd_handle, num_cols, tag="tfr")
  # write_upd_data_frame(upd$srb,       upd_handle, num_cols, tag="srb")
  # write_upd_data_frame(upd$pasfrs,    upd_handle, num_cols, tag="pasfrs")
  # write_upd_data_frame(upd$migration, upd_handle, num_cols, tag="migration")
  close(upd_handle)
}

generate_basepop = function(country_code, wpp_data) {
  ## As of WPP2022, WPP estimates pertain to January 1. As of the 2023 round of
  ## UNAIDS HIV estimates, Spectrum population sizes pertain to December 31 to
  ## align with the way ART and PMTCT program data historically were entered
  ## into Spectrum. Therefore, we use (e.g.) the January 1, 1971 population from
  ## WPP as the Spectrum base-year population for (December 31) 1970
  pop_long = reshape2::melt(wpp_data$population[wpp_data$population$country_code==country_code,],
                            id.vars = c("country_code", "country", "sex", "age"),
                            measure.vars = c("1971", "1976", "1981", "1986"),
                            variable.name="year",
                            value.name="value")
  pop_long$year = as.numeric(as.character(pop_long$year))
  
  ## Aggregate ages 80+
  pop_long$age[pop_long$age > 80] = 80
  pop_aggr = data.table::as.data.table(pop_long)[,.(value=sum(value)),by=.(year,sex,age)]
  
  ## Standardize data encoding and column names
  pop_aggr$sex = as.integer(pop_aggr$sex) # convert from factor to integers
  pop_aggr$value = 1000 * pop_aggr$value  # convert from thousands to numbers
  pop_aggr$year = pop_aggr$year - 1 # shift from January 1 of year t to December 31 of year t-1
  
  return(pop_aggr[order(pop_aggr$year, pop_aggr$sex, pop_aggr$age),])
}

#' Write out a portion of the UPD file that is stored as a data frame
#' @param df the data to write out
#' @param upd_handle an open file handle to the UPD file
#' @param num_cols the number of columns in the UPD file
#' @param tag specifies the tags that delimit the block (e.g., \code{<basepop>...</basepop>})
write_upd_data_frame = function(df, upd_handle, num_cols, tag) {
  cat(sprintf("<%s>%s\n", tag, stringi::stri_dup(",", num_cols - 1)), file=upd_handle, append=TRUE)
  pad = stringi::stri_dup(",", num_cols - ncol(df))
  cat(sprintf("%s%s\n", paste(colnames(df), collapse=","), pad), file=upd_handle, append=TRUE)
  for (k in 1:nrow(df)) {
    outstr = sprintf("%s%s\n", paste(df[k,], collapse=","), pad)
    cat(gsub(",NA,", ",,", outstr), file=upd_handle, append=TRUE) # Some lx values in compact life tables are NA. gsub replaces these with blank cells.
  }
  cat(sprintf("</%s>%s\n", tag, stringi::stri_dup(",", num_cols - 1)), file=upd_handle, append=TRUE)
}

generate_lfts = function(country_code, wpp_data, compact=TRUE, year_final=2049) {
  n_sex = 2
  n_age = 101
  n_yrs = 2100 - 1950 + 1
  
  dim = c(n_age, n_sex, n_yrs)
  dimnames = list(age=0:100, sex=c("Male", "Female"), year=1950:2100)
  
  lt_fn = c("mx", "ax", "qx", "lx", "dx", "Lx", "Tx", "ex", "Sx")
  lt = lapply(1:length(lt_fn), function(k) {array(NA, dim=dim, dimnames=dimnames)})
  names(lt) = lt_fn
  
  ## We reconstruct the life table for ages 0, 1, ..., 99, 100+ from mx, e0, and l1
  mx_df = wpp_data$life_table$mx[wpp_data$life_table$mx$country_code == country_code,]
  e0_df = wpp_data$life_table$e0[wpp_data$life_table$e0$country_code == country_code,]
  l1_df = wpp_data$life_table$lx[wpp_data$life_table$lx$country_code == country_code & wpp_data$life_table$lx$age == 1,]
  
  mx_df = mx_df[order(mx_df$sex, mx_df$age),]
  e0_df = e0_df[order(e0_df$sex),]
  l1_df = l1_df[order(l1_df$sex),]
  
  lt$mx = array(data.matrix(mx_df[,5:ncol(mx_df)]), dim=dim, dimnames=dimnames)
  lt$ex[1,,] = data.matrix(e0_df[,4:ncol(e0_df)])
  lt$lx[1,,] = 1e5
  lt$lx[2,,] = data.matrix(l1_df[,5:ncol(l1_df)])
  lt$ax[1,,] = 1.0 - (lt$lx[1,,] * lt$mx[1,,] + lt$lx[2,,] - lt$lx[1,,]) / (lt$mx[1,,] * (lt$lx[1,,] - lt$lx[2,,]))
  lt$ax[2:100,,] = 0.5
  lt$qx[1    ,,] = 1.0 - lt$lx[2,,] / lt$lx[1,,]
  lt$qx[2:100,,] = lt$mx[2:100,,] / (1.0 + (1.0 - lt$ax[2:100,,]) * lt$mx[2:100,,])
  lt$qx[101  ,,] = 1.0
  
  for (r in 3:101) {
    lt$lx[r,,] = (1.0 - lt$qx[r-1,,]) * lt$lx[r-1,,]
  }
  
  lt$dx[1:100,,] = lt$lx[1:100,,] - lt$lx[2:101,,]
  lt$dx[101  ,,] = lt$lx[101,,]
  lt$Lx[1:100,,] = lt$lx[2:101,,] + lt$dx[1:100,,] * lt$ax[1:100,,]
  lt$Tx[1  ,,] = lt$ex[1,,] * lt$lx[1,,]
  lt$Lx[101,,] = lt$Tx[1,,] - colSums(lt$Lx[1:100,,], dims=1)
  lt$ax[101,,] = lt$Lx[101,,]
  lt$Tx[101,,] = lt$Lx[101,,]
  
  for (r in 100:2) {
    lt$Tx[r,,] = lt$Lx[r,,] + lt$Tx[r+1,,]
  }
  
  lt$ex[2:101,,] = lt$Tx[2:101,,] / lt$lx[2:101,,]
  lt$Sx[1    ,,] = lt$Lx[1,,] / lt$lx[1,,]
  lt$Sx[2:100,,] = lt$Lx[2:100,,] / lt$Lx[1:99,,]
  
  lt_list = lapply(lt, function(ind) {as.data.frame.table(ind, responseName="value")})
  lt_long = dplyr::bind_rows(lt_list, .id="fn")
  lt_long$fn = factor(lt_long$fn, levels=lt_fn)
  lt_wide = dplyr::mutate(reshape2::dcast(lt_long, year+sex+age~fn),
                          year = as.numeric(as.character(year)),
                          age  = as.numeric(as.character(age)))
  
  ## BGN verification check
  tol = 0.1
  lx_src_wide = wpp_data$life_table$lx[wpp_data$life_table$lx$country_code == country_code,]
  lx_src_long = reshape2::melt(lx_src_wide,
                               id.vars=c("sex", "age"),
                               measure.vars=sprintf("%d", 1950:2100),
                               variable.name="year",
                               value.name="value")
  lx_src_long = dplyr::mutate(lx_src_long,
                              year = as.numeric(as.character(year)),
                              age  = as.numeric(as.character(age)))
  
  lx_comp = dplyr::left_join(lx_src_long, lt_wide, by=c("year", "sex", "age"))
  lx_comp$difference = abs(lx_comp$value - lx_comp$lx)
  if (any(lx_comp$difference > 0.1)) {
    warning("Calculated number of survivors differs from UNPD estimate by more than %f people", tol)
  }
  ## END verification check
  
  ## Given a life table for one year and sex covering ages 0-99 and 100+, return
  ## a life table covering ages 0-80 and 81+
  truncate_lfts = function(lfts) {
    lfts_0_80 = lfts[lfts$age <= 80,]
    lfts_80   = lfts[lfts$age == 80,]
    lfts_81   = lfts[lfts$age == 81,]
    
    lfts_81up = data.frame(
      year = lfts$year[1],
      sex  = lfts$sex[1],
      age  = 81,
      mx   = lfts_81$lx / lfts_81$Tx, # obtained by solving Eqn. 13.30 of siegel2004book for m(x,inf)
      ax   = lfts_81$ex,
      qx   = 1.0,
      lx   = lfts_81$lx,
      dx   = lfts_81$lx,
      Lx   = lfts_81$Tx,
      Tx   = lfts_81$Tx,
      ex   = lfts_81$ex,
      Sx   = lfts_81$Tx / lfts_80$Tx)
    
    return(rbind(lfts_0_80, lfts_81up))
  }
  
  lfts_list = plyr::ddply(lt_wide, .variables=c("year", "sex"), truncate_lfts)
  lfts_flat = dplyr::bind_rows(lfts_list)
  lfts_flat$sex = as.numeric(lfts_flat$sex)
  lfts_flat = lfts_flat[lfts_flat$year >= 1970 & lfts_flat$year <= year_final,]

  if (compact) {
    keep = c("year", "sex", "age", "lx", "ex", "Sx")
    lfts_flat$lx[!(lfts_flat$age %in% c(1,5))] = NA
  } else {
    keep = c("year", "sex", "age", lt_fn)
  }
  
  return(lfts_flat[order(lfts_flat$year, lfts_flat$sex, lfts_flat$age),keep])
}
