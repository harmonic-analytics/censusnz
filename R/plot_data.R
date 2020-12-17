#' Plot New Zealand census data
#'
#' @return A ggplot object containing a plot of each variable, for each geographic
#' region name, coloured by variable group, from 2018 NZ census data.
#'
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or character vector of the variables to be selected. Can use get_variables() to examine available variables
#' @param year The year of data requested. Currently the only available year is 2018, which is the default
#' @param n The number of areas to use as faceted plots
#' @param exclude_total Whether or not to exclude any data with name label containing the substring "Total"
#' @param exclude_other Whether or not to exclude the 'Other' column after data has been lumped
#'
#' @export
#'
#' @examples
#' plot_data("RC", c("maori_descent", "smoking_status"))

plot_data = function(geography=NULL, variables=NULL, year = 2018, n = 6, exclude_total=TRUE, exclude_other=TRUE) {
  variable=NULL
  count=NULL
  variable_group=NULL
  name=NULL

  data = censusnz::get_data(geography, variables, year) %>% dplyr::filter(!is.na(count))

  if(exclude_total){data=data %>% dplyr::filter(!grepl("Total", name, fixed=TRUE))}

  # remove total curp
  data = data %>% dplyr::filter(!grepl("total", variable_group, fixed = TRUE))

  data$name = as.factor(data$name)

  # based on https://stackoverflow.com/questions/41963053/r-stacked-bar-charts-including-other-using-ggplot2
  tidy_df = data[rep(rownames(data), times = data$count), 3] # slow
  top_n = tidy_df$name %>% forcats::fct_lump_n(n-1+exclude_other) %>% table()
  data$name = as.character(data$name)
  data$name[!(data$name %in% names(top_n))] = "Other"

  if(exclude_other){data=data %>% dplyr::filter(!grepl("Other", name, fixed=TRUE))}

  result = data %>% ggplot2::ggplot(ggplot2::aes(x=variable,
                                                 y=count,
                                                 fill=stringr::str_replace_all(variable_group, paste0("(.{20})"), "\\1\n"))) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~forcats::fct_reorder(name, count)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3)) +
    viridis::scale_fill_viridis(discrete = TRUE)
  return(result)
}
