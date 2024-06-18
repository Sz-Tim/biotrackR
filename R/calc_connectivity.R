#' Load pairwise connectivity matrix
#'
#' @param f Filename of connectivity matrix output by biotracker
#' @param source_names Vector of site names; order corresponds to integers in connectivity csvs
#' @param dest_names Vector of site names; order corresponds to integers in connectivity csvs
#' @param liceScale Multiplier for original particle density values
#'
#' @return Dataframe with a column for source, destination, particle density transferred, and an identifier column with the date
#' @export
#'
load_connectivity <- function(f, source_names, dest_names=NULL, liceScale=28.2*240) {
  library(tidyverse)
  if(is.null(dest_names)) {
    dest_names <- source_names
  }
  read_csv(f, col_types="iid") |>
    mutate(source=factor(source, levels=seq_along(source_names)-1, labels=source_names),
           destination=factor(destination, levels=seq_along(dest_names)-1, labels=dest_names),
           date=ymd(str_split_fixed(basename(f), "_", 3)[,2]),
           value=value*liceScale)
}




#' Calculate influx
#'
#' @param data Dataframe in long format
#' @param dest_col Destination site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{influx} giving the sum, and
#'   \code{N_influx} giving the number of farms contributing infection pressure
#'   to each destination. Note that this includes self infection.
#' @export
#'
calc_influx <- function(data, dest_col, N_col, ...) {
  library(tidyverse)
  data |>
    group_by({{dest_col}}, ...) |>
    summarise(influx=sum({{N_col}}),
              N_influx=n()) |>
    ungroup()
}




#' Calculate self infection
#'
#' @param data Dataframe in long format
#' @param src_col Source site (unquoted column name)
#' @param dest_col Destination site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{self} giving the sum
#' @export
#'
calc_self_infection <- function(data, src_col, dest_col, N_col, ...) {
  library(tidyverse)
  data |>
    filter({{src_col}} == {{dest_col}}) |>
    group_by({{src_col}}, ...) |>
    summarise(self=sum({{N_col}})) |>
    ungroup()
}




#' Calculate outflux
#'
#' @param data Dataframe in long format
#' @param src_col Source site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param dest_areas Dataframe with columns for site names and catchment areas
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{outflux} giving the sum and
#'   \code{N_outflux} giving the number of farms receiving infection pressure
#'   from each source. Note that this includes self infection. If
#'   \code{dest_areas} is provided, additionally includes a column with outflux
#'   scaled by the square meter area of each destination.
#' @export
#'
calc_outflux <- function(data, src_col, N_col, dest_areas=NULL, ...) {
  library(tidyverse)
  if(is.null(dest_areas)) {
    data |>
      group_by({{src_col}}, ...) |>
      summarise(outflux=sum({{N_col}}),
                N_outflux=n()) |>
      ungroup()
  } else {
    data |>
      left_join(dest_areas) |>
      group_by({{src_col}}, ...) |>
      summarise(outflux=sum({{N_col}}),
                outflux_m2=sum({{N_col}}/area),
                N_outflux=n()) |>
      ungroup()
  }

}
