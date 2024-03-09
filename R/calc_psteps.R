
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
#' @return Dataframe with column \code{i} for mesh element, and colmns for particle densities named with either 't_' or the sitename plus the YYYYMMDD date (e.g., \code{t_20190407}).
#' @export
#'
load_psteps <- function(f, site_names=NULL) {
  library(tidyverse)
  timestep <- str_sub(str_split_fixed(basename(f), "_", 3)[,3], 1, -5)
  if(is.null(site_names)) {
    # Densities already summed in column 2 by biotracker
    read_delim(f, delim=" ", col_select=1:2, col_types="d",
               col_names=c("i",
                           paste0("t_", timestep))) |>
      mutate(i=i+1) # Java uses 0-based indexing, R uses 1-based indexing
  } else {
    # Column for each release site
    read_delim(f, delim=" ", col_types="d",
               col_names=c("i",
                           paste(site_names, "_", timestep))) |>
      mutate(i=i+1) # Java uses 0-based indexing, R uses 1-based indexing
  }
}







#' Load particle steps for a set of simulations
#'
#' Particle steps from simulations organized into subdirectories within
#' \code{out_dir} are loaded in wide format, with a column for each week and a
#' row for each mesh element x simulation (sparse: only elements with particles).
#'
#' @param out_dir Main output directory containing simulation-specific subdirectories
#' @param mesh_i Dataframe with mesh information including columns \code{i}=element and (optionally) \code{area}=element area
#' @param sim_i Simulation metadata with column \code{sim} naming each simulation. These should also be the subdirectory names
#' @param ncores Number of cores to use for parallel processing
#' @param stage Life stage ("Mature" or "Immature")
#' @param liceScale Multiplier for original pstep values
#' @param per_m2 Logical: Scale values by element area?
#' @param log Logical: ln(values)? Performed after area scaling
#'
#' @return Wide format dataframe
#' @export
#'
load_psteps_simSets <- function(out_dir, mesh_i, sim_i, ncores=8,
                                stage="Mature", liceScale=28.2*240,
                                per_m2=TRUE, log=TRUE) {
  library(tidyverse); library(glue); library(furrr)
  plan(multisession, workers=ncores)
  ps_wide <- map_dfr(sim_i$sim,
                     ~dir(glue("{out_dir}/{.x}"), glue("psteps{stage}.*dat"),
                          recursive=T, full.names=T) |>
                       future_map(~load_psteps(.x)) |>
                       reduce(full_join, by="i") |>
                       mutate(sim=.x)) |>
    arrange(sim, i) |>
    select(sim, i, starts_with("t_")) |>
    mutate(across(starts_with("t_"), ~.x*liceScale)) |>
    left_join(mesh_i, by="i")
  if(per_m2) {
    ps_wide <- ps_wide |>
      mutate(across(starts_with("t_"), ~.x/area))
  }
  if(log) {
    ps_wide <- ps_wide |>
      mutate(across(starts_with("t_"), ~log(.x)))
  }
  plan(sequential)
  return(ps_wide)
}
