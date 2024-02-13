#' Calculate influx
#'
#' @param data Dataframe in long format
#' @param dest_col Destination site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{total} giving the sum. Note
#' that this includes self infection.
#' @export
#'
calc_influx <- function(data, dest_col, N_col, ...) {
  library(tidyverse)
  data |>
    group_by(dest_col, ...) |>
    summarise(total=sum({{N_col}})) |>
    ungroup()
}
