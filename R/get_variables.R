#' Get NZ census variables
#'
#' @return A table of available variables, which can be queried with get_data()
#' @export
#'
#' @examples
#' get_variables()
get_variables = function() {
  result = db.censusnz::available_variables

  return(result)
}
