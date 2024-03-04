#' Run biotracker
#'
#' @param jdk_path Path to java jdk
#' @param javaLib_path Path to commons-io-2.6.jar and netcdfAll-4.6.11.jar
#' @param jar_path Path to biotracker.jar
#' @param f_properties Properties file
#' @param sim_dir Simulation directory
#' @param clean_output Move files to appropriate subdirectories?
#' @param tar_output Create tar.gz of all output?
#'
#' @return Reporting only
#' @export
#'
run_biotracker <- function(jdk_path="/usr/local/java/jre1.8.0_211/bin/java",
                           javaLib_path="jar/lib/",
                           jar_path="/home/sa04ts/biotracker/jar/biotracker.jar",
                           f_properties="sim_01.properties",
                           sim_dir="sim_01/",
                           clean_output=TRUE,
                           tar_output=TRUE) {

  begin <- Sys.time()
  cat("\n--- Starting biotracker:", format(begin, "%F %T"), "\n")
  cat("---   Properties:", f_properties, "\n")
  cat("---   Output dir:", sim_dir, "\n")

  proj_dir <- getwd()
  file.copy(javaLib_path, sim_dir, recursive=T, overwrite=T)
  file.copy(f_properties, sim_dir)
  setwd(sim_dir)

  system2(jdk_path,
          c("-Xmx8192m -Xms4096m -jar", jar_path, f_properties),
          stdout="stdout.txt")

  later <- Sys.time()
  begin_later <- round(later - begin, 2)
  cat("--- Finished simulations:", format(later, "%F %T"), "\n")
  cat("---   Run time:", begin_later, attr(begin_later, "units"), "\n")

  if(clean_output) {
    begin_clean <- Sys.time()
    cat("--- Cleaning directory:", format(begin_clean, "%F %T"), "\n")
    clean_out_dir("arrivals_", "arrivals")
    clean_out_dir("connectivity_", "connectivity")
    clean_out_dir("pstepsMature_", "pstepsMature")
    clean_out_dir("pstepsImmature_", "pstepsImmature")
    clean_out_dir("locations_", "locations")
    later_clean <- Sys.time()
    cat("---   All files moved:", format(later_clean, "%F %T"), "\n")
  }
  if(tar_output) {
    begin_tar <- Sys.time()
    cat("--- Making a tarball:", format(begin_tar, "%F %T"), "\n")
    tar(paste0("../", basename(sim_dir), ".tar.gz"), dir(), compression="gzip")
    cat("---   Created", paste0("../", basename(sim_dir), ".tar.gz"), "\n")
  }

  later_final <- Sys.time()
  begin_final <- round(later_final - begin, 2)
  cat("--- Complete:", format(later_final, "%F %T"), "\n")
  cat("---   Total time:", begin_final, attr(begin_final, "units"), "\n")

  setwd(proj_dir)
}
