#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd



#' @title Clean Numeric Input
#' @description Sanitise a string input by removing symbols (except decimal point) and checking numeric range
#' @param txt String to clean
#' @param min Minimum acceptable value, Default: -Inf
#' @param max Maximum acceptable value, Default: Inf
#' @return Santised string to clean
#' @details
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  clean_numeric_input("£100")
#'
#'  #EXAMPLE2
#'  clean_numeric_input("5.5%")
#'  }
#' }
#' @rdname clean_numeric_input
#' @export
clean_numeric_input <- function (txt, error_alternative = NA_real_, min = -Inf, max = Inf) {
  # TODO error alternative not working
  # test using empty value in tracker rates input
  val = tryCatch({
    txt %>%
      stringr::str_replace_all("[^[:alnum:]\\.]", "") %>%
      as.numeric
  }, error = function (e) {
    if (!identical(error_alternative, NA_real_)) {
      return(error_alternative)
    } else {
      stop(glue::glue("Error trying to clean numeric input: {txt} within the range {min}-{max} -- {e}"))
    }
  })

  if (any(is.na(val))) {
    if (!identical(error_alternative, NA_real_)) {
      val = error_alternative
    } else {
      stop(glue::glue("Error trying to clean numeric input: {txt} is not within range {min}-{max} "))
    }
  }

  if (any(val < min) | any(val > max)) {
    if (!identical(error_alternative, NA_real_)) {
      val = error_alternative
    } else {
      stop(glue::glue("Error trying to clean numeric input: {txt} is not within range {min}-{max}"))
    }
  }

  val
}

#' @title Every nth
#' @description Return a function that returns every nth value from a vector, starting with the first. Used in printing axes in ggplot2
#' @param n Value to return every n of
#' @return Function
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname every_nth
#' @export
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#' @title Count Decimals
#' @description Get the number of decimal places from a numeric input
#' @param x numeric vector
#' @return Vector with number of decimal places of each value in x
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname count_decimals
#' @export
count_decimals = function(x) {
  #length zero input
  if (length(x) == 0) return(numeric())

  #count decimals
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0

  x_nchr
}

