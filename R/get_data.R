#' Get New Zealand census data
#'
#' @return The resulting dataframe for the requested geography, variables
#'
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param variables A string or character vector of the variables to be selected. Can use get_variables() to examine available variables
#' @param year The year of data requested. Currently the only available year is 2018, which is the default
#'
#' @export
#'
#' @examples
#' get_data("RC", c("maori_descent", "smoking_status"))

get_data = function(geography=NULL, variables=NULL, year = 2018) {
  geoid = NULL
  LANDWATER_NAME = NULL
  variable = NULL
  n_landtype = NULL

  # Make sure a geography(s) is provided
  assertthat::assert_that(!is.null(geography), msg = "Must provide a geography")

  # Make sure a variable(s) is provided
  assertthat::assert_that(!is.null(variables), msg = "Must provide a variable(s)")

  # Setup
  result <- tibble::tribble(~geoid, ~land_type, ~name, ~variable, ~variable_group, ~count)

  # Cast area_type to uppercase
  geography = toupper(geography)

  # Check provided variables are valid
  avail_vars = db.censusnz::available_variables$variable
  assertthat::assert_that(all(tolower(variables) %in% avail_vars), msg = "At least one of the provided variables is not valid, see censusnz::get_variables()")

  # Cast variables to lower
  if(!is.null(variables)){
    variables = match.arg(tolower(variables), unique(db.censusnz::available_variables$variable), several.ok = TRUE)
  }


  # Gather the data and return it
  geography_df = get_geography(geography)

  # Filter Data
  relevant_hierarchies = c(
    "LANDWATER_NAME",
    colnames(geography_df %>% dplyr::select(dplyr::ends_with("_CODE") | dplyr::ends_with("_NAME")))
  )

  # Process data to provide useful land_type column
  land_type = db.censusnz::area_hierarchy %>%
    dplyr::select(tidyselect::any_of(relevant_hierarchies)) %>% # renames
    dplyr::rename(land_type = LANDWATER_NAME) %>% # renames LANDWATER_NAME column to land_type
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.factor), as.character)) %>% # converts factor cols to character (note where is unexported)
    dplyr::distinct() %>% # carried over from previous code
    dplyr::group_by(dplyr::across(-tidyselect::starts_with("land_type"))) %>% # group by anything that's not land type
    dplyr::summarise(land_type = land_type[1], n_landtype = dplyr::n()) %>%
    dplyr::mutate(land_type = dplyr::if_else(n_landtype > 1, "Mixture", land_type)) %>% # if multiple land types for same name, change type to mixture
    dplyr::select(-n_landtype) %>%
    dplyr::ungroup()

  # if geography has no relevant hierarchies, our land_type is not useful, mutate to keep shape and col names consistent
  if(nrow(land_type)==1){
    suppressMessages((
      result = geography_df %>%
        dplyr::filter(variable %in% variables) %>%
        dplyr::mutate(land_type=land_type[[1]]) %>%
        dplyr::rename(geoid = dplyr::ends_with("_CODE")) %>%
        dplyr::rename(name = dplyr::ends_with("_NAME")) %>%
        dplyr::select(geoid, land_type, dplyr::everything())
    ))
  }
  # else we use a left join to get useful land_type values
  else{
    suppressMessages((
      result = geography_df %>%
        dplyr::filter(variable %in% variables) %>%
        dplyr::left_join(land_type) %>%
        dplyr::rename(geoid = dplyr::ends_with("_CODE")) %>%
        dplyr::rename(name = dplyr::ends_with("_NAME")) %>%
        dplyr::select(geoid, land_type, dplyr::everything())
    ))
  }

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
