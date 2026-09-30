#' Function to suggest parallel settings
#'
#' Suggests the number of cores and cluster type to use for parallel
#' processing based on the operating system and the number of cores available.
#'
#' @return A list with suggested settings
#'
#' @export
suggest_parallel <- function() {
  return(
    list(
      suggested_cores = max(1L, detectCores() - 1L, na.rm = TRUE),
      suggested_cluster_type = if (.Platform$OS.type == "windows") {
        "PSOCK"
      } else {
        "FORK"
      }
    )
  )
}
