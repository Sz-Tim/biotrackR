#' Calculate outflux
#'
#' @param data Dataframe in long format
#' @param src_col Source site (unquoted column name)
#' @param N_col Lice density (unquoted column name)
#' @param ... Grouping columns (unquoted)
#'
#' @return Summarised dataframe with column \code{total} giving the sum. Note
#' that this includes self infection.
#' @export
#'
calc_outflux <- function(data, src_col, N_col, ...) {
  library(tidyverse)
  data |>
    group_by(src_col, ...) |>
    summarise(total=sum({{N_col}})) |>
    ungroup()
}
