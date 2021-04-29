#' Get New Zealand census data
#'
#' @param geography A string of the geographic area to be selected. Must be one
#'   of SA1, SA2, LBA, DHB, TA, RC, WARD.
#' @param variables A string or character vector of the variables to be selected
#'   . Can use get_variables() to examine available variables.
#' @param year The year of data requested, by default 2018. Must be one of 2006, 2013 or 2018.
#'
#' @return The resulting dataframe for the requested geography, year and and variable(s)
#' @export
#'
#' @examples
#' # Get data for a single variable
#' get_data("RC", c("maori_descent"), 2018)
#' # Get data for multiple variables
#' get_data("RC", c("maori_descent", "smoking_status"), 2006)

get_data = function(geography = NULL, variables = NULL, year = 2018) {
  geoid = NULL
  LANDWATER_NAME = NULL
  variable = NULL
  n_landtype = NULL

  # Make sure a geography is provided
  assert_geography(geography)

  # Make sure a year is provided
  assert_year(year)

  # Make sure a variable(s) is provided
  assert_variables(variables)

  # Setup
  result = tibble::tribble(~geoid,
                            ~land_type,
                            ~name,
                            ~variable,
                            ~variable_group,
                            ~count)
  geography = toupper(geography)

  # Get data set for specified geography and year
  geography_df = get_geog_year(geography, year)

  # Filter Data
  relevant_hierarchies = c(
    "LANDWATER_NAME",
    colnames(geography_df %>%
               dplyr::select(dplyr::ends_with("_CODE") | dplyr::ends_with("_NAME")))
  )

  # If there is no land type info available, set land_type column to NA
  if(sum(relevant_hierarchies %in% colnames(db.censusnz::area_hierarchy_2018))==1){
    suppressMessages((
      result = geography_df %>%
        dplyr::filter(variable %in% variables) %>%
        dplyr::mutate(land_type=NA)
    ))
  }
  else{
    # Process data to provide useful land_type column
    land_type = db.censusnz::area_hierarchy_2018 %>%
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


assert_geography = function(geography){
  # Make sure there is one area type specified, from the accepted types
  geography = toupper(geography)
  areas = c("SA1", "SA2", "LBA", "DHB", "TA", "RC", "WARD")
  assertthat::assert_that(!is.null(geography), msg = "Must provide a geography")
  assertthat::assert_that(geography %in% areas, msg = paste("geography must be one of:", paste(areas, collapse = ', ')))
  assertthat::assert_that(length(geography) == 1, msg = "only one geography can be specified")
  return(invisible(geography))
}

assert_year = function(year){
  # Make sure one year is specified, from the accepted years
  yrs = c(2006, 2013, 2018)
  assertthat::assert_that(year %in% yrs, msg = paste("year must be one of:", paste(yrs, collapse = ', ')))
  assertthat::assert_that(length(year) == 1, msg = "only one year can be specified")
  return(invisible(year))
}

assert_variables = function(variable){
  # Make sure variable(s) is/are one of the accepted variables
  assertthat::assert_that(!is.null(variable), msg = "Must provide at least one variable")
  assertthat::assert_that(all(tolower(variable) %in% available_variables()),
                          msg = "At least one of the provided variables is not valid, see censusnz::get_variables()")
  return(invisible(variable))
}

available_variables = function(){
  unique(db.censusnz::available_variables$variable)
}


#' Get geography dataframe given input string
#'
#' @return The resulting dataframe for the requested geography and year, unedited from db.censusnz
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param year The year of data requested. Must be one of 2006, 2013 or 2018.
#' @noRd
get_geog_year = function(geography, year) {

  assert_geography(geography)
  assert_year(year)

  result = switch(EXPR = paste0(geography, year),
                  "SA12006"  = db.censusnz::SA1_2006,
                  "SA12013"  = db.censusnz::SA1_2013,
                  "SA12018"  = db.censusnz::SA1_2018,
                  "SA22006"  = db.censusnz::SA2_2006,
                  "SA22013"  = db.censusnz::SA2_2013,
                  "SA22018"  = db.censusnz::SA2_2018,
                  "LBA2006"  = db.censusnz::LBA_2006,
                  "LBA2013"  = db.censusnz::LBA_2013,
                  "LBA2018"  = db.censusnz::LBA_2018,
                  "DHB2006"  = db.censusnz::DHB_2006,
                  "DHB2013"  = db.censusnz::DHB_2013,
                  "DHB2018"  = db.censusnz::DHB_2018,
                  "TA2006"   = db.censusnz::TA_2006,
                  "TA2013"   = db.censusnz::TA_2013,
                  "TA2018"   = db.censusnz::TA_2018,
                  "RC2006"   = db.censusnz::RC_2006,
                  "RC2013"   = db.censusnz::RC_2013,
                  "RC2018"   = db.censusnz::RC_2018,
                  "WARD2006" = db.censusnz::WARD_2006,
                  "WARD2013" = db.censusnz::WARD_2013,
                  "WARD2018" = db.censusnz::WARD_2018
                  )

  return(result)
}
