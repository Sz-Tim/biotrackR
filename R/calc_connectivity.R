#' Load pairwise connectivity matrix
#'
#' @param f Filename of connectivity matrix output by biotracker
#' @param site_names Vector of site names; length(site_names) = ncol(connect_mx)
#'
#' @return Dataframe with a column for each site (destinations), and rows for each site (source), with an identifier column with the date
#' @export
#'
load_connectivity <- function(f, site_names) {
  library(tidyverse)
  read_delim(f, delim=" ", col_names=site_names, col_types="d") |>
    mutate(source=factor(site_names, levels=site_names),
           date=ymd(str_split_fixed(basename(f), "_", 3)[,2]))
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
