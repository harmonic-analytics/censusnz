#' Plot relative levels for New Zealand census data
#'
#' This function will plot the relative levels of certain variables for several
#'   named regions. By default this will take the top 6 regions, excluding total
#'   NZ or other summary groups. Several parameters can be configured including
#'   the position of the bars and the number of regions to plot.
#'
#' Plotting proportions relative to other levels often provides much more
#'   intuitive or useful information as it allows quick comparison of a region
#'   to other regions or to the average, while avoiding scale issues created by
#'   few regions having large numbers of observations.
#'
#' @param geography A string of the geographic area to be selected. Must be one
#'   of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or vector of the variables to be selected. Can use
#'   get_variables() to examine available variables
#' @param regions A string or vector of the regions to be selected
#' @param year The year of data requested. Currently the only available year is
#'   2018, which is the default
#' @param n The number of areas to use as faceted plots
#' @param position The ggplot2 bar plot position, i.e. "fill" or "dodge"
#' @param exclude_total Whether or not to exclude any data with name label
#'   containing the substring "Total"
#' @param exclude_other Whether or not to exclude the 'Other' column after data
#'   has been lumped
#'
#' @return A ggplot object containing a plot of each variable, for each
#'   geographic region name, coloured by variable group, from 2018 NZ census
#'   data.
#' @export
#'
#' @examples
#' plot_data("RC", c("maori_descent", "smoking_status"))
#' # Customising the layout and exclusions
#' plot_relative("RC",
#'           "maori_descent",
#'           n = 3,
#'           position = "dodge",
#'           exclude_total = FALSE,
#'           exclude_other = FALSE)

plot_relative = function(geography = NULL,
                         variables = NULL,
                         regions = NULL,
                         year = 2018,
                         n = 6,
                         position = "fill",
                         exclude_total = TRUE,
                         exclude_other = TRUE) {

  count = NULL
  name = NULL
  variable_group = NULL
  total = NULL
  variable = NULL
  prop = NULL


  # fetch data and remove NAs
  data = censusnz::get_data(geography, variables, year) %>%
    dplyr::filter(!is.na(count))

  if(is.null(regions)) {
    # isolate total_curp
    total_curps = data %>% dplyr::filter(grepl("total_curp", variable_group) & variable == variables[1])
    total_curps = total_curps[order(-total_curps$count),]

    # get the n largest names
    n_largest_names = total_curps$name[1:n]

    # if desired, remove Total NZ names at this point
    if(exclude_total == TRUE) {
      data = dplyr::filter(data, !grepl("Total", name, fixed = TRUE))
    }

    # remove our 'other' category if desired, for scale reasons
    if(exclude_other){
      data = dplyr::filter(data, !grepl("Other", name, fixed=TRUE))
    }

    # lump all but top n names into "Other"
    data$name[!(data$name %in% n_largest_names)] = "Other"
  }
  else {
    data = dplyr::filter(data, name %in% regions)
  }

  # remove 'total * curp' names and groups
  data = data %>%
    dplyr::filter(!grepl("total", variable_group, fixed = TRUE))

  # produce summary of total count per name (region)
  plot_summaries = data %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(total = sum(count), .groups = "drop_last")

  # join the total counts per name to plot_data
  data_props = data %>%
    dplyr::left_join(plot_summaries, by = "name") %>%
    dplyr::mutate(prop = count / total)
  result = ggplot2::ggplot(data_props,
                           ggplot2::aes(fill = stringr::str_replace_all(
                             variable_group, paste0("(.{20})"),"\\1\n"),
                             x = variable, y = prop)) +
    ggplot2::geom_bar(stat = "identity", position=position) +
    ggplot2::facet_wrap(~name) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3)) +
    ggplot2::scale_fill_brewer(palette="Set3")

  return(result)
}
