#' Calculate self infection
#'
#' @param data Dataframe in long format
#' @param src_col Source site (unquoted column name)
#' @param dest_col Destination site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{total} giving the sum
#' @export
#'
calc_self_infection <- function(data, src_col, dest_col, N_col, ...) {
  library(tidyverse)
  data |>
    filter({{src_col}} == {{dest_col}}) |>
    group_by(...) |>
    summarise(total=sum({{N_col}})) |>
    ungroup()
}
