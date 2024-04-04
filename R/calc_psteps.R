
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
    read_csv(f, col_select=1:2, col_types="d",
             col_names=c("i",
                         paste0("t_", timestep))) |>
      mutate(i=i+1) # Java uses 0-based indexing, R uses 1-based indexing
  } else {
    # Column for each release site
    read_csv(f, col_types="d",
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
load_psteps_simSets <- function(out_dir, mesh_i, sim_i, ncores=4,
                                stage="Mature", liceScale=28.2*240,
                                per_m2=TRUE, log=TRUE) {
  library(tidyverse); library(glue); library(furrr)
  plan(multisession, workers=ncores)
  ps_wide <- map_dfr(sim_i$sim,
                     ~dir(glue("{out_dir}/{.x}"), glue("psteps{stage}.*csv"),
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








#' Calculate difference in particle densities between to simulations
#'
#' @param ps_wide Output from \code{load_psteps_simSets()} or similar, with
#'   columns \code{i} with element indexes, \code{sim} with simulation
#'   identifier, and columns \code{t_*} with particle densities per timestep
#'   \code{t}
#' @param sims_comp Vector of length two indicating which simulations to
#'   compare, corresponding to values in column \code{sim}
#' @param ncores Number of cores for parallel processing using the \code{furrr}
#'   package
#'
#' @return Dataframe with one row per element in column \code{i} and columns
#'   \code{t_*} with differences. Note that the difference is \code{last() -
#'   first()}, with order by R default (i.e., \code{sort(sims_comp)}) and _NOT_
#'   the order given in \code{sims_comp}. Elements where \code{is.na(first())}
#'   return \code{9999}, elements where \code{is.na(last())} return
#'   \code{-9999}.
#' @export
#'
calc_psteps_diff <- function(ps_wide, sims_comp, ncores=4) {
  library(tidyverse); library(furrr); library(carrier)

  crate_diff <- crate(
    function(x) {
      na_i <- is.na(x)
      d <- x[2,] - x[1,]
      d[na_i[1,] & !na_i[2,]] <- 9999
      d[!na_i[1,] & na_i[2,]] <- -9999
      d[na_i[1,] & na_i[2,]] <- NA_real_
      return(d)
    }
  )
  opts <- furrr_options(globals = FALSE)
  plan(multisession, gc=T, workers=ncores)

  ps_wide <- ps_wide |>
    select(i, sim, starts_with("t_")) |>
    filter(sim %in% sims_comp)
  gc()

  psdiff_wide <- ps_wide |>
    full_join(expand_grid(i=unique(ps_wide$i), sim=sims_comp)) |>
    arrange(i, sim) |>
    nest(data=starts_with("t_"), .by=i) |>
    mutate(diff=future_map(data, crate_diff, .options=opts)) |>
    select(-data) |>
    unnest(diff)

  plan(sequential)

  return(psdiff_wide)
}










#' Calculate average particle densities
#'
#' @param ps_long Long-format dataframe
#' @param y_col Quoted column name with particle densities
#' @param grp_col \code{NULL} or quoted column name with time to average over (e.g., years or months)
#' @param ncores Number of cores for parallelization with furrr
#' @param mesh_sf \code{NULL} or mesh sf object
#'
#' @return Dataframe with averaged particle densities according to the \code{grp_col} columns.
#' @export
calc_psteps_avg <- function(ps_long, y_col, grp_col=NULL, ncores=4, mesh_sf=NULL) {
  library(tidyverse); library(furrr); library(carrier)

  crate_summary <- crate(~data.frame(pOcc=mean(!is.na(.x)),
                                     mean_N=mean(.x, na.rm=T),
                                     median_N=stats::median(.x, na.rm=T),
                                     sd_N=stats::sd(.x, na.rm=T),
                                     max_N=max(.x, na.rm=T)))
  opts <- furrr_options(globals = FALSE)
  plan(multisession, workers=ncores)

  if(is.null(grp_col)) {
    ps_avg <- ps_long |>
      select("sim", "i", all_of(y_col)) |>
      group_by(sim, i) |>
      nest(data=.data[[y_col]]) |>
      ungroup() |>
      mutate(data=map(data, ~.x[[y_col]]),
             summary=future_map(data, crate_summary, .options=opts)) |>
      select(-data) |>
      unnest(summary) |>
      mutate(CV_N=sd_N/mean_N)
  } else {
    ps_avg <- ps_long |>
      select("sim", "i", all_of(c(y_col, grp_col))) |>
      group_by(sim, i, .data[[grp_col]]) |>
      nest(data=.data[[y_col]]) |>
      ungroup() |>
      mutate(data=map(data, ~.x[[y_col]]),
             summary=future_map(data, crate_summary, .options=opts)) |>
      select(-data) |>
      unnest(summary) |>
      mutate(CV_N=sd_N/mean_N)
  }

  if(!is.null(mesh_sf)) {
    ps_avg <- ps_avg |>
      left_join(mesh_sf |> select(i, geom), y=_, by="i")
  }

  if(is.null(grp_col)) {
    ps_avg <- ps_avg |>
      filter(!is.na(sim)) # elements with no particles ever
  } else {
    ps_avg <- ps_avg |>
      filter(!is.na(sim) & !is.na(.data[[grp_col]])) # elements with no particles ever
  }

  plan(sequential)
  return(ps_avg)
}
