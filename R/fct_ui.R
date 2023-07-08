#' ui
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sidebarPanel2 <- function (..., out = NULL, width = 4)
{

  if (is.null(out)) {
    out <- p("")
  }

  div(class = paste0("col-sm-", width),
      tags$form(class = "well", ...),
      out
  )
}
