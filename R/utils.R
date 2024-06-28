#' Load packages
#' 
#' A wrapper for `library()` that can load multiple packages at once. If the 
#' package is not installed, the function will install it first.
#' 
#' @param pkgs A character vector of package names
#' @param install_missing A boolean indicating whether to install missing
#' packages. Default is TRUE
#' 
#' @returns The function does not return anything
#' 
#' @examples
#' \dontrun{
#'    load_packages(c("dplyr", "ggplot2"))
#'  }
#'  
#' @export
load_packages <- function(pkgs, install_missing = TRUE) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (install_missing) {
        install.packages(pkg)
        
      } else {
        cli_warn(paste0("Package '", pkg, "' is not installed."))
        
      }
    }
    
    library(pkg, character.only = TRUE)
  }
}

#' Repeat rows
#'
#' This function repeats the rows of a [matrix][base::matrix], 
#' [data.frame][base::data.frame] or [tibble][tibble::tibble-package] by a given 
#' number of times.
#' 
#' In case of a data.frame, new row names are preserved.
#'
#' @param x A [matrix][base::matrix], [data.frame][base::data.frame] or 
#' [tibble][tibble::tibble-package]
#' @param n The number of times to repeat each row 
#' 
#' @returns A [matrix][base::matrix], [data.frame][base::data.frame] or 
#' [tibble][tibble::tibble-package] depending on the type of the input
#' 
#' @examples
#' repeat_rows(matrix(1:4, nrow = 2), 2)
#' repeat_rows(tibble::tibble(a = 1:2, b = 3:4), 2)
#' repeat_rows(data.frame(a = 1:2, b = 3:4), 2)
#' 
#' @export
repeat_rows <- function(x, n) {
  # Check the type of input
  if (!(is.matrix(x) || is.data.frame(x) || is_tibble(x))) {
    cli_abort("'x' must be a matrix, data.frame or tibble")
    
  }
  
  return(
    x[rep(seq_len(nrow(x)), each = n), , drop = FALSE]
  )
}
