#' Clean output directory
#'
#' Simulations put all files in the default output directory. This function moves file types to corresponding folders.
#'
#' @param pattern File name pattern
#' @param newdir New directory name
#'
#' @return No return
#' @export
clean_out_dir <- function(pattern, newdir) {
  if(length(dir(pattern=pattern)) > 0) {
    dir.create(newdir)
    f <- dir(pattern=pattern)
    file.rename(f, paste0(newdir, "/", f))
  }
}
