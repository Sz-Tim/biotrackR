




download_global_site_id <- function(out_file) {
  url <- "https://aquaculture.scotland.gov.uk/xlsx/GlobalSiteIDScotlandsAquaculture.xlsx"
}


#' Download weekly Scottish sea lice counts
#'
#' Accessed through aquaculture.scotland.gov.uk. Downloads a json file from the
#' ArcGIS online map viewer, then processes that into a csv.
#'
#' Data from before 2021-March-29 must be downloaded manually from the website.
#' Data are the weekly average number of adult female Lepeophtheirus salmonis
#' reported by the salmon companies.
#'
#' http://aquaculture.scotland.gov.uk/data/data.aspx
#' https://scottishepa.maps.arcgis.com/apps/webappviewer/index.html?id=2218824350e5470e8026076d4138da58
#'
#' @param begin_ymd Beginning date in YYYY-MM-DD format
#' @param end_ymd Ending date in YYYY-MM-DD format
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
#'
download_lice_counts <- function(begin_ymd, end_ymd, out_file) {
  download_yrs <- unique(year(seq(ymd(begin_ymd), ymd(end_ymd), by=1)))

  data_ls <- vector("list", 2)
  if(any(download_yrs %in% year(today()))) {
    data_ls[[1]] <- url("https://aquaculture.scotland.gov.uk/csv/ms_sea_lice_current.csv") |>
      read_csv(show_col_types=FALSE) |>
      clean_names(case="small_camel")

  }
  if(any(! download_yrs %in% year(today()))) {
    download.file("https://map.sepa.org.uk/sealice/ms_sea_lice.zip", "lice.zip")
    untar("lice.zip")
    file.remove("lice.zip")
    data_ls[[2]] <- read_csv("ms_sea_lice.csv", show_col_types=FALSE) |>
      clean_names(case="small_camel")
    file.remove("ms_sea_lice.csv")
  }

  data_df <- data_ls |>
    reduce(bind_rows) |>
    mutate(weekBeginning=ymd(weekBeginning),
           day=day(weekBeginning),
           month=month(weekBeginning),
           year=year(weekBeginning),
           weeklyAverageAf=as.numeric(weeklyAverageAf)) |>
    filter(between(weekBeginning, ymd(begin_ymd), ymd(end_ymd))) |>
    select(-easting, -northing, -nationalGridReference) |>
    arrange(weekBeginning, siteNo)
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}




#' Download monthly Scottish fish farm biomass
#'
#' Accessed through aquaculture.scotland.gov.uk. Downloads a csv of monthly fish
#' biomass on each farm.
#'
#' http://aquaculture.scotland.gov.uk/data/data.aspx
#'
#' @param begin_ymd Beginning date in YYYY-MM-DD format
#' @param end_ymd Ending date in YYYY-MM-DD format
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
#'
download_fish_biomass <- function(begin_ymd, end_ymd, out_file) {
  fish_url <- "https://aquaculture.scotland.gov.uk/csv/se_monthly_reports.csv"
  download.file(fish_url, out_file)
  data_df <- read.csv(out_file, stringsAsFactors=FALSE) |>
    clean_names(case="small_camel") |>
    mutate(date=dmy(year)) |>
    filter(between(date, ymd(begin_ymd), ymd(end_ymd))) |>
    filter(actualBiomassOnSiteTonnes > 0)
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}





#' Download Marine Scotland site details
#'
#' http://aquaculture.scotland.gov.uk/data/data.aspx
#' http://aquaculture.scotland.gov.uk/data/site_details.aspx
#'
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
download_ms_site_details <- function(out_file) {
  ms_url <- "https://aquaculture.scotland.gov.uk/csv/ms_site_details.csv"
  download.file(ms_url, out_file)
  data_df <- read_csv(out_file, show_col_types=FALSE) |>
    clean_names(case="small_camel")
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}





#' Download SEPA farm license details
#'
#' http://aquaculture.scotland.gov.uk/data/data.aspx
#' http://aquaculture.scotland.gov.uk/data/lease_details.aspx
#'
#' @param out_file Filename for the csv output
#'
#' @return Success message if csv is correctly saved
#' @export
download_sepa_licenses <- function(out_file) {
  sepa_url <- "https://aquaculture.scotland.gov.uk/csv/se_permit_conditions.csv"
  download.file(sepa_url, out_file)
  data_df <- read.csv(out_file) |>
    clean_names(case="small_camel")
  data_df |>
    write_csv(out_file)
  cat(nrow(data_df), "records saved to", out_file, "\n")
}
