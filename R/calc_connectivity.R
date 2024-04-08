#' Load pairwise connectivity matrix
#'
#' @param f Filename of connectivity matrix output by biotracker
#' @param site_names Vector of site names; order corresponds to integers in connectivity csvs
#' @param liceScale Multiplier for original particle density values
#'
#' @return Dataframe with a column for source, destination, particle density transferred, and an identifier column with the date
#' @export
#'
load_connectivity2 <- function(f, site_names, liceScale=28.2*240) {
  library(tidyverse)
  read_csv(f, col_types="iid") |>
    mutate(source=factor(source, levels=seq_along(site_names)-1, labels=site_names),
           destination=factor(destination, levels=seq_along(site_names)-1, labels=site_names),
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
#' @return Summarised dataframe with column \code{influx} giving the sum. Note
#' that this includes self infection.
#' @export
#'
calc_influx <- function(data, dest_col, N_col, ...) {
  library(tidyverse)
  data |>
    group_by({{dest_col}}, ...) |>
    summarise(influx=sum({{N_col}})) |>
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
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{outflux} giving the sum. Note
#' that this includes self infection.
#' @export
#'
calc_outflux <- function(data, src_col, N_col, ...) {
  library(tidyverse)
  data |>
    group_by({{src_col}}, ...) |>
    summarise(outflux=sum({{N_col}})) |>
    ungroup()
}
