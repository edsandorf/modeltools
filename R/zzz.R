#' Print package startup message
#' 
#' The function is called when the package is loaded through the `library` or
#' `require` functions. It prints a message to the console.
#' 
#' @param libname The library name
#' @param pkgname The package name
#' 
#' @return Nothing
.onAttach <- function(libname, pkgname) {
  installed_version <- utils::packageDescription("modeltools", fields = "Version")
  
  description <- tryCatch({
    readLines("https://raw.githubusercontent.com/edsandorf/modeltools/master/DESCRIPTION")
    
  }, warning = function(w) {
    return("NA")
    
  }, error = function(e) {
    return("NA")
    
  })
  
  if (length(description) == 1) {
    remote_version <- description
    
  } else {
    remote_version <- gsub("Version:\\s*", "", description[grep('Version:', description)])    
    
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
