#' Get New Zealand census data
#'
#' @param geography A string of the geographic area to be selected. Must be one
#'   of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or character vector of the variables to be selected
#'   . Can use get_variables() to examine available variables
#' @param year The year of data requested. Currently the only available year is
#'   2018, which is the default
#'
#' @return The resulting dataframe for the requested geography and variable(s)
#' @export
#'
#' @examples
#' # Get data for a single variable
#' get_data("RC", c("maori_descent"))
#' # Get data for multiple variables
#' get_data("RC", c("maori_descent", "smoking_status"))

get_data = function(geography=NULL, variables=NULL, year = 2018) {
  geoid = NULL
  LANDWATER_NAME = NULL
  variable = NULL
  n_landtype = NULL

  # Make sure a geography(s) is provided
  assertthat::assert_that(!is.null(geography),
                            msg = "Must provide a geography")

  # Make sure a variable(s) is provided
  assertthat::assert_that(!is.null(variables),
                            msg = "Must provide a variable(s)")

  # Setup
  result <- tibble::tribble(~geoid,
                            ~land_type,
                            ~name,
                            ~variable,
                            ~variable_group,
                            ~count)
  geography = toupper(geography)

  # Check provided variables are valid
  avail_vars = db.censusnz::available_variables$variable
  assertthat::assert_that(all(tolower(variables) %in% avail_vars),
                            msg = "At least one of the provided variables is not
                                   valid, see censusnz::get_variables()")

  if(!is.null(variables)){
    variables = match.arg(tolower(variables),
                            unique(db.censusnz::available_variables$variable),
                            several.ok = TRUE)
  }

  geography_df = get_geography(geography)

  # Filter Data
  relevant_hierarchies = c(
    "LANDWATER_NAME",
    colnames(geography_df %>%
               dplyr::select(dplyr::ends_with("_CODE") | dplyr::ends_with("_NAME")))
  )

  # If there is no land type info available, set land_type column to NA
  if(sum(relevant_hierarchies %in% colnames(db.censusnz::area_hierarchy))==1){
    suppressMessages((
      result = geography_df %>%
        dplyr::filter(variable %in% variables) %>%
        dplyr::mutate(land_type=NA)
    ))
  }
  else{
    # Process data to provide useful land_type column
    land_type = db.censusnz::area_hierarchy %>%
      dplyr::select(tidyselect::any_of(relevant_hierarchies)) %>%
      dplyr::rename(land_type = LANDWATER_NAME) %>%
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.factor), as.character)) %>%
      dplyr::distinct() %>%
      # if multiple land types for same name, change type to mixture
      dplyr::group_by(dplyr::across(-tidyselect::starts_with("land_type"))) %>%
      dplyr::summarise(land_type = land_type[1], n_landtype = dplyr::n()) %>%
      dplyr::mutate(land_type = dplyr::if_else(n_landtype > 1, "Mixture", land_type)) %>%
      dplyr::select(-n_landtype) %>%
      dplyr::ungroup()

    suppressMessages((
      result = geography_df %>%
        dplyr::filter(variable %in% variables) %>%
        dplyr::left_join(land_type)
    ))
  }

  # Complete output by renaming some cols
  result = result %>%
    dplyr::rename(geoid = dplyr::ends_with("_CODE")) %>%
    dplyr::rename(name = dplyr::ends_with("_NAME")) %>%
    dplyr::select(geoid, land_type, dplyr::everything())

  return (result)
}

#' Get geography dataframe given input string
#'
#' @return The resulting dataframe for the requested geography, unedited from db.censusnz
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @noRd
get_geography = function(geography) {

  # Make sure area type is one of the accepted types
  types = c("SA1", "SA2", "LBA", "DHB", "TA", "RC", "WARD")
  assertthat::assert_that(geography %in% types, msg = paste("geography must be one of:", paste(types, collapse = ', ')))

  result = switch(geography,
                  "SA1" = db.censusnz::SA1,
                  "SA2" = db.censusnz::SA2,
                  "LBA" = db.censusnz::LBA,
                  "DHB" = db.censusnz::DHB,
                  "TA" = db.censusnz::TA,
                  "RC" = db.censusnz::RC,
                  "WARD" = db.censusnz::WARD
                  )
  return(result)
}
