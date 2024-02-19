
#' Load psteps (particle densities per mesh element)
#'
#' If biotracker is run with \code{splitPsteps='true'}, output is a dataframe with
#' column 'i' for mesh element and a column for each release site giving the
#' density within that file (e.g., weekly totals). In this case,
#' \code{site_names} must be provided. If \code{splitPsteps='false'}, biotracker
#' sums all particle densities within each element regardless of release site,
#' and \code{site_names} should be left \code{NULL}.
#'
#' @param f Filename of psteps file output by biotracker
#' @param site_names Vector of site names; length(site_names) = (ncol(pstepsFile)-1)
#'
#' @return Dataframe with column \code{i} for mesh element, and colmns for particle densities named with either 'wk_' or the sitename plus the YYYYMMDD date (e.g., \code{wk_20190407}).
#' @export
#'
load_psteps <- function(f, site_names=NULL) {
  library(tidyverse)
  if(is.null(site_names)) {
    # Densities already summed in column 2 by biotracker
    read_delim(f, delim=" ", col_select=1:2, col_types="d",
               col_names=c("i", glue("wk_{str_sub(basename(.x), 14, 21)}")))
  } else {
    # Column for each release site
    read_delim(f, delim=" ", col_types="d",
               col_names=c("i", glue("{site_names}_{str_sub(basename(.x), 14, 21)}")))
  }
}
