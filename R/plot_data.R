#' Plot New Zealand census data
#'
#' @return A ggplot object containing a plot of each variable, for each geographic
#' region name, coloured by variable group, from 2018 NZ census data.
#'
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or character vector of the variables to be selected. Can use get_variables() to examine available variables
#' @param year The year of data requested. Currently the only available year is 2018, which is the default
#'
#' @export
#'
#' @examples
#' plot_data("RC", c("maori_descent", "smoking_status"))

plot_data = function(geography=NULL, variables=NULL, year = 2018) {
  variable=NULL
  count=NULL
  variable_group=NULL

  data = censusnz::get_data(geography, variables, year)
  result = data %>% ggplot2::ggplot(ggplot2::aes(x=variable, y=count, fill=variable_group)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~name)
  return(result)
}
