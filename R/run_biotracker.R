#' Run biotracker
#'
#' @param jdk_path Path to java jdk
#' @param javaLib_path Path to commons-io-2.6.jar and netcdfAll-4.6.11.jar
#' @param jar_path Path to biotracker.jar
#' @param f_properties Properties file
#' @param sim_dir Simulation directory
#' @param clean_output Move files to appropriate subdirectories?
#'
#' @return Reporting only
#' @export
#'
run_biotracker <- function(jdk_path="/usr/local/java/jre1.8.0_211/bin/java",
                           javaLib_path="jar/lib/",
                           jar_path="/home/sa04ts/biotracker/jar/biotracker.jar",
                           f_properties="sim_01.properties",
                           sim_dir="sim_01/",
                           clean_output=TRUE) {

  now <- Sys.time()
  cat("Starting biotracker:", format(now, "%F %T"), "\n")
  cat("  Properties:", f_properties, "\n")
  cat("  Output dir:", sim_dir, "\n")

  proj_dir <- getwd()
  file.copy(javaLib_path, sim_dir, recursive=T, overwrite=T)
  file.copy(f_properties, sim_dir)
  setwd(sim_dir)

  system2(jdk_path,
          c("-Xmx8192m -Xms4096m -jar", jar_path, f_properties),
          stdout="stdout.txt")

  later <- Sys.time()
  cat("Finished:", format(later, "%F %T"), "\n")
  cat("  Run time:", round(later - now, 2), "minutes", "\n")

  if(clean_output) {
    cat("Cleaning directory\n")
    clean_out_dir("arrivals_", "arrivals")
    clean_out_dir("connectivity_", "connectivity")
    clean_out_dir("pstepsMature_", "pstepsMature")
    clean_out_dir("pstepsImmature_", "pstepsImmature")
    clean_out_dir("locations_", "locations")
  }

  setwd(proj_dir)
}
