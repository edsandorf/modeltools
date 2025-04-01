#' Create stars
#' 
#' Creates stars based on the p-value of a test.
#' 
#' @param p A double or numeric vector of p-values.
#' 
#' @return A character vector of stars.
#' 
#' @examples
#' stars(0.05)
#' 
#' @export
stars <- function(p) {
  return(
    case_when(
      p <= 0.001 ~ "***",
      p <= 0.01 ~ "**",
      p <= 0.05 ~ "*",
      p <= 0.1 ~ ".",
      .default = ""
    )
  )
}

#' Prepare model output for `gt`
#' 
#' Takes a model object as an input and prepares a table with estimated 
#' coefficients, standard errors, and stars indicating significance. Names in
#' `term` will be converted to lower case to ensure seamless use of left join.
#' 
#' The table can quickly chained to other tables using left_join(). 
#' 
#' @param x A model object
#' @param ... Additional arguments passed to [broom::tidy()] and 
#' [broom::glance()]
#' 
#' @return A tibble with estimated coefficients, standard errors, and stars.
#' 
#' @export
prep_for_gt <- function(x, ...) {
  return(
    tidy(x, ...) |> 
      bind_rows(
        glance(x, ...) |> 
          pivot_longer(cols = everything(), names_to = "term", values_to = "estimate")
      ) |> 
      mutate(
        term = tolower(term),
        stars = stars(.data$p.value)
      ) 
  )
}
