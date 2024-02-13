#' Load pairwise connectivity matrices
#'
#' @param f Vector of filenames
#' @param site_names Vector of site names; length(site_names) = ncol(connect_mx)
#'
#' @return Dataframe with a column for each site (destinations), and rows for each site (source) x date combination
#' @export
#'
load_connectivity <- function(f, site_names) {
  library(tidyverse)
  f_base <- basename(f)
  map_dfr(seq_along(f),
          ~read_delim(f[.x], delim=" ", col_names=site_names, col_types="d") |>
            mutate(source=factor(site_names, levels=site_names),
                   date=ymd(str_split_fixed(f_base[.x], "_", 3)[,2])))
}
