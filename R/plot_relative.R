#' Plot relative levels for New Zealand census data
#'
#' This function will plot the relative levels of certain variables for several
#'   named regions. By default this will take the top 6 regions, excluding total
#'   NZ or other summary groups. It can also be configured
#'
#' @return A ggplot object containing a plot of each variable, for each
#'   geographic region name, coloured by variable group, from 2018 NZ census
#'   data.
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
#' @export
#'
#' @examples
#' plot_data("RC", c("maori_descent", "smoking_status"))

plot_relative = function(geography = NULL,
                         variables = NULL,
                         regions = NULL,
                         year = 2018,
                         n = 2,
                         position = "fill",
                         exclude_total = TRUE,
                         exclude_other = TRUE) {

  # fetch data and remove NAs
  data = censusnz::get_data(geography, variables, year) %>%
    dplyr::filter(!is.na(count))

  # first find global avg proportions
  total_data = data %>%
    dplyr::filter(grepl("Total", name, fixed = TRUE)) %>%
    dplyr::filter(!grepl("total", variable_group, fixed = TRUE)) %>%
    dplyr::mutate(prop = count / sum(count)) %>%
    dplyr::select(c("variable_group", "prop"))

  plot_data = data
  if(exclude_total == TRUE) {
    plot_data = dplyr::filter(plot_data, !grepl("Total", name, fixed = TRUE))
  }

  if(!is.null(regions)) {
    # select only desired regions
    plot_data = dplyr::filter(plot_data, name %in% regions)
  } else {
    tidy_df = plot_data[rep(rownames(plot_data), times = plot_data$count), 3] # slow
    top_n = tidy_df$name %>% forcats::fct_lump_n(n-1+exclude_other) %>% table()
    plot_data$name = as.character(plot_data$name)
    plot_data$name[!(plot_data$name %in% names(top_n))] = "Other"

    # remove our 'other' category if desired, for scale reasons
    if(exclude_other){plot_data=plot_data %>% dplyr::filter(!grepl("Other", name, fixed=TRUE))}
  }

  # remove 'total * curp' names and groups
  plot_data = plot_data %>%
    dplyr::filter(!grepl("total", variable_group, fixed = TRUE))

  # produce summary of total count per name (region)
  plot_summaries = plot_data %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(total = sum(count), .groups = "drop_last")

  plot_data_props = plot_data %>%
    dplyr::left_join(plot_summaries, by = "name") %>%
    dplyr::mutate(prop = count / total)
  result = ggplot2::ggplot(plot_data_props,
                           ggplot2::aes(fill = stringr::str_replace_all(
                             variable_group,
                             paste0("(.{20})"),
                             "\\1\n"),
                             x = variable,
                             y = prop)) +
    ggplot2::geom_bar(stat = "identity", position=position) +
    ggplot2::facet_wrap(~name) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3)) +
    viridis::scale_fill_viridis(discrete = TRUE)

  return(result)
}
