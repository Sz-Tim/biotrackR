
#' Download weekly Scottish sea lice counts
#'
#' Accessed through aquaculture.scotland.gov.uk. Downloads a json file from the
#' ArcGIS online map viewer, then processes that into a csv. Note that only data
#' from March 2021 are available through this function. Older data must be
#' downloaded manually from the website. Data are the weekly average number of
#' adult female Lepeophtheirus salmonis reported by the salmon companies.
#'
#' @param begin_ymd Beginning date in YYYY-MM-DD format
#' @param end_ymd Ending date in YYYY-MM-DD format
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
#'
download_lice_counts <- function(begin_ymd, end_ymd, out_file) {

  lice_url <- paste0("https://utility.arcgis.com/usrsvcs/servers/",
                     "8999c2e86c7246cb93b420e810458293/rest/services/Secure/",
                     "Sealice/MapServer/2/query?f=json&where=1%3D1&",
                     "returnGeometry=false&spatialRel=esriSpatialRelIntersects&",
                     "outFields=*&orderByFields=WEEK_BEGINNING%20DESC&outSR=4326&",
                     "resultOffset=0&resultRecordCount=100000")
  download.file(lice_url, "lice_counts.json")
  data_df <- fromJSON("lice_counts.json")$features$attributes |>
    clean_names(case="small_camel") |>
    mutate(day=day(ymd(weekBeginning)),
           month=month(weekBeginning)) |>
    filter(between(date, ymd(begin_ymd), ymd(end_ymd)))
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}




#' Download monthly Scottish fish farm biomass
#'
#' Accessed through aquaculture.scotland.gov.uk. Downloads a csv of monthly fish
#' biomass on each farm.
#'
#' @param begin_ymd Beginning date in YYYY-MM-DD format
#' @param end_ymd Ending date in YYYY-MM-DD format
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
#'
download_fish_biomass <- function(begin_ymd, end_ymd, out_file) {
  library(tidyverse); library(janitor)
  fish_url <- "http://aquaculture.scotland.gov.uk/csvexport/se_monthly_reports.csv"
  download.file(fish_url, out_file)
  data_df <- read_csv(out_file) |>
    clean_names(case="small_camel") |>
    mutate(date=dmy(year)) |>
    filter(between(date, ymd(begin_ymd), ymd(end_ymd))) |>
    filter(actualBiomassOnSiteTonnes > 0)
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}

