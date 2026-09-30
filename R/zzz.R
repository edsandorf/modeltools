#' Print package startup message
#'
#' The function is called when the package is loaded through the `library` or
#' `require` functions. It prints a message to the console. The latest version
#' is only looked up on GitHub in interactive sessions, with a short timeout,
#' to avoid slowing down or blocking non-interactive and offline sessions.
#'
#' @param libname The library name
#' @param pkgname The package name
#'
#' @return Nothing
#'
#' @noRd
.onAttach <- function(libname, pkgname) {
  installed_version <- utils::packageDescription(pkgname, fields = "Version")
  remote_version <- "NA"

  if (interactive()) {
    old <- options(timeout = 2)
    on.exit(options(old), add = TRUE)

    description <- tryCatch({
      readLines(
        "https://raw.githubusercontent.com/edsandorf/modeltools/main/DESCRIPTION",
        warn = FALSE
      )

    }, warning = function(w) {
      return(character(0))

    }, error = function(e) {
      return(character(0))

    })

    version_line <- grep("^Version:", description, value = TRUE)
    if (length(version_line) == 1) {
      remote_version <- gsub("Version:\\s*", "", version_line)

    }
  }

  packageStartupMessage(
    "You are currently using modeltools version: ",
    installed_version, "\n\n",
    "The latest version is: ", remote_version, "\n\n",
    "To access the latest version, please run \n",
    "devtools::install_github('edsandorf/modeltools') \n\n",
    "To cite this package: \n",
    "utils::citation('modeltools')"
  )
}
