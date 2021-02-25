#' Plot New Zealand census data
#'
#' This function provides a drop-in replacement for `censusnz::get_data` that
#'   allows helpful graphs of the census data to be conveniently plotted. By
#'   default the function plots the 6 regions with the highest number of
#'   observations, excluding 'NZ Total' and 'Other' categories. Configuration of
#'   the number of regions as well as whether to exclude the total and other
#'   categories is possible.
#'
#' @return A ggplot object containing a plot of each variable, for each geographic
#' region name, coloured by variable group, from 2018 NZ census data.
#'
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or character vector of the variables to be selected. Can use get_variables() to examine available variables
#' @param regions A string or vector of the regions to be selected
#' @param year The year of data requested. Currently the only available year is 2018, which is the default
#' @param n The number of areas to use as faceted plots
#' @param position The position to plot the bars in, "dodge", "stacked" or "fill"
#' @param exclude_total Whether or not to exclude any data with name label containing the substring "Total"
#' @param exclude_other Whether or not to exclude the 'Other' column after data has been lumped
#'
#' @export
#'
#' @examples
#' # Multiple variables with defaults
#' plot_data("RC", c("maori_descent", "smoking_status"))
#' # Customising the layout and exclusions
#' plot_data("RC",
#'           "maori_descent",
#'           n = 3,
#'           exclude_total = FALSE,
#'           exclude_other = FALSE)
#' # Plot by proportion
#' plot_data("RC",
#'           "maori_descent",
#'           position = "fill")
#' # Compare selected regions
#' plot_data("RC",
#'           "maori_descent",
#'           regions = c("Auckland Region", "Wellington Region"),
#'           position = "fill")
plot_data = function(geography = NULL,
                     variables = NULL,
                     regions = NULL,
                     year = 2018,
                     n = 6,
                     position = "dodge",
                     exclude_total = TRUE,
                     exclude_other = TRUE) {
  # Initialise ----

  variable = NULL
  count = NULL
  variable_group = NULL
  name = NULL

  # Fetch data and remove NAs
  data = censusnz::get_data(geography, variables, year) %>% dplyr::filter(!is.na(count))

  # If desired, remove Total NZ names (helpful for scale reasons)
  if(exclude_total){data = dplyr::filter(data, !grepl("Total", name, fixed = TRUE))}

  if(is.null(regions)) {
    # Find largest regions to plot ----

    # Isolate total_curp
    total_curps = data %>% dplyr::filter(grepl("total_curp", variable_group) & variable == variables[1])
    total_curps = total_curps[order(-total_curps$count),]

    # Get the n largest names
    n_largest_names = total_curps$name[1:n-1+exclude_other]

    # Remove total_curp and total_stated_curp as they are redundant and unhelpful
    data = dplyr::filter(data, !grepl("total", variable_group, fixed = TRUE))

    # Lump all but top n names into "Other" and sum
    data$name[!(data$name %in% n_largest_names)] = "Other"
    data = data %>% dplyr::group_by(name, variable, variable_group) %>% dplyr::summarise(count = sum(count)) %>% dplyr::ungroup()

    # Remove our 'other' category if desired, likely for scale reasons
    if(exclude_other){data=data %>% dplyr::filter(!grepl("Other", name, fixed=TRUE))}
  }
  else {
    data = dplyr::filter(data, name %in% regions)

    # Remove 'total * curp' names and groups
    data = dplyr::filter(data, !grepl("total", variable_group, fixed = TRUE))
  }

  # Produce Graph ----

  # Replace underscores with spaces in variable_group for readability
  data$variable_group = gsub("_", " ", data$variable_group)

  # Generate extended colour palette in case there are many variables
  my_colours_function = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

  num_unique_curp = length(unique(data$variable_group))
  my_colours = my_colours_function(num_unique_curp)

  # Reorder variable_group factor for legend labels
  vars_and_groups = data %>% dplyr::select(variable, variable_group)
  vars_and_groups = unique(vars_and_groups[order(vars_and_groups$variable, vars_and_groups$variable_group), ])

  data$variable_group = factor(data$variable_group, levels = vars_and_groups$variable_group)

  # Plot graph, wrap legend labels and put legend below for visibility
  result = data %>%
    ggplot2::ggplot(ggplot2::aes(x = variable,
                                 y = count,
                                 fill = variable_group)) +
    ggplot2::geom_col(position = position) +
    ggplot2::facet_wrap(~forcats::fct_reorder(name, count)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = my_colours, labels = function(x) stringr::str_wrap(x, width=20))

  # Use sensible label format for absolute scale graphs
  if(tolower(position) == "dodge" || tolower(position) == "stack") {
    result = result + ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
  } else {
    result = result + ggplot2::labs(y = 'proportion')
  }


  return(result)
}
