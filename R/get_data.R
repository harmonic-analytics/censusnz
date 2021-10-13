#' Get New Zealand census data
#'
#' @param geography A string of the geographic area to be selected. Must be one
#'   of SA1, SA2, LBA, DHB, TA, RC, WARD.
#' @param variables A string or character vector of the variables to be selected
#'   . Can use get_variables() to examine available variables.
#' @param year The year(s) of data requested, by default 2018. Must be one or more of 2006, 2013 or 2018.
#'
#' @return The resulting dataframe for the requested geography, year(s) and and variable(s)
#' @export
#'
#' @examples
#' # Get data for a single variable
#' get_data("RC", c("maori_descent"), 2018)
#' # Get data for multiple variables
#' get_data("RC", c("maori_descent", "smoking_status"), 2006)
#' # Get data for multiple years
#' get_data("RC", c("maori_descent"), c(2006, 2013))

get_data = function(geography = NULL, variables = NULL, year = 2018) {
  geoid = NULL
  LANDWATER_NAME = NULL
  variable = NULL
  category = NULL
  n_landtype = NULL

  # Make sure a geography is provided
  assert_geography(geography)

  # Make sure a year is provided
  assert_year(year)

  # Make sure a variable(s) is provided
  assert_variables(variables)

  # Warn against missing variable combinations
  assert_variables_available_at_all_levels(variables, geography, year)

  # Setup
  result = tibble::tribble(~geoid,
                            ~land_type,
                            ~name,
                            ~variable,
                            ~variable_group,
                            ~count)

  geography = toupper(geography)
  variables = tolower(variables)
  geography_copy = geography

  # Determine categories required
  variable_category_mapping =
    db.censusnz::available_variables %>%
    dplyr::filter(variable %in% variables, geography == geography_copy) %>%
    dplyr::select(variable, category) %>%
    dplyr::distinct()

  all_results = dplyr::tribble(~category, ~year, ~geography_type, ~geoid, ~land_type, ~name, ~variable, ~variable_group, ~count)
  # Get data set for specified geography and year from required categories
  for(y in year){
    # Get all data for necessary geography and year across necessary categories
    geography_df =
      lapply(
        unique(variable_category_mapping$category),
        function(c){
          data_returned = get_geog_year_category(geography, y, c) %>%  dplyr::mutate(year = y)

          colnames(data_returned) =
            colnames(data_returned) %>%
            stringr::str_remove_all(paste(c("INDIVIDUAL_", "DWELLING_", "HOUSEHOLD_"), collapse = "|"))

          return(data_returned)
        }
      ) %>%  dplyr::bind_rows()

    # Filter Data
    relevant_hierarchies = c(
      "LANDWATER_NAME",
      colnames(geography_df %>%
                 dplyr::select(dplyr::ends_with("_CODE") | dplyr::ends_with("_NAME")))
    )

    geography_type = relevant_hierarchies[stringr::str_detect(relevant_hierarchies,"_CODE")]  %>%
      stringr::str_remove("_CODE")

    # If there is no land type info available, set land_type column to NA
    if(sum(relevant_hierarchies %in% colnames(db.censusnz::area_hierarchy_2018))==1){
      suppressMessages((
        result = geography_df %>%
          dplyr::filter(variable %in% variables) %>%
          dplyr::mutate(land_type=NA)
      ))
    }  else {
      # Process data to provide useful land_type column
      suppressMessages((
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
      ))

      suppressMessages((
        result = geography_df %>%
          dplyr::filter(variable %in% variables) %>%
          dplyr::left_join(land_type)
      ))
    }

    # Complete output by renaming some cols
    suppressMessages((
      result = result %>%
        dplyr::rename(geoid = dplyr::ends_with("_CODE")) %>%
        dplyr::rename(name = dplyr::ends_with("_NAME")) %>%
        dplyr::left_join(variable_category_mapping) %>%
        dplyr::mutate(geography_type = geography_type) %>%
        dplyr::select(category, year, geography_type, geoid, land_type, dplyr::everything())
    ))

    all_results <- all_results %>% dplyr::bind_rows(result)
  }

  return (all_results)
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
  assertthat::assert_that(all(year %in% yrs), msg = paste("year must be one or more of:", paste(yrs, collapse = ', ')))
  return(invisible(year))
}

assert_variables = function(variable){
  # Make sure variable(s) is/are one of the accepted variables
  assertthat::assert_that(!is.null(variable), msg = "Must provide at least one variable")
  assertthat::assert_that(all(tolower(variable) %in% available_variables()),
                          msg = "At least one of the provided variables is not valid, see censusnz::get_variables()")
  return(invisible(variable))
}

assert_category = function(category){
  # Make sure only one category is specified, from the accepted values
  category = toupper(category)
  categories = c("INDIVIDUAL", "DWELLING", "HOUSEHOLD")
  assertthat::assert_that(!is.null(category), msg = "Must provide a category")
  assertthat::assert_that(category %in% categories, msg = paste("category must be one of:", paste(categories, collapse = ', ')))
  assertthat::assert_that(length(category) == 1, msg = "only one category can be specified")
  return(invisible(category))
}

assert_variables_available_at_all_levels = function(variables, geography, year){
  variable = NULL
  variables = tolower(variables)
  geography = toupper(geography)
  all_combinations =
    expand.grid(list("geography" = geography, "year" = year, "variable" = variables)) %>%
    dplyr::as_tibble()

  missing_combinations =
    all_combinations %>%
    dplyr::anti_join(db.censusnz::available_variables %>%  dplyr::mutate(year = as.numeric(year)),
              by = c("geography", "year", "variable")) %>%
    dplyr::mutate(warning_text = paste0(variable, " for ", geography, " in ", year))

  if(nrow(missing_combinations) > 0){
    warning(
      paste(c("These variables are not available at the following levels:", missing_combinations$warning_text), collapse = '\n')
    )
  }
  return(invisible(variables))
}

available_variables = function(){
  unique(db.censusnz::available_variables$variable)
}


#' Get geography dataframe given input string
#'
#' @return The resulting dataframe for the requested geography and year and category, unedited from db.censusnz
#' @param geography A string of the geographic area to be selected. Must be one of SA1, SA2, LBA, DHB, TA, RC, WARD
#' @param year The year of data requested. Must be one of 2006, 2013 or 2018.
#' @param category The data category requested. Must be one of INDIVIDUAL, DWELLING, HOUSEHOLD
#' @noRd
get_geog_year_category = function(geography, year, category) {

  assert_geography(geography)
  assert_year(year)
  assert_category(category)

  geography = toupper(geography)
  category = toupper(category)

  result = switch(EXPR = paste0(category, geography, year),
                  'INDIVIDUALSA12006'  = db.censusnz::INDIVIDUAL_SA1_2006,
                  'INDIVIDUALSA12013'  = db.censusnz::INDIVIDUAL_SA1_2013,
                  'INDIVIDUALSA12018'  = db.censusnz::INDIVIDUAL_SA1_2018,
                  'INDIVIDUALSA22006'  = db.censusnz::INDIVIDUAL_SA2_2006,
                  'INDIVIDUALSA22013'  = db.censusnz::INDIVIDUAL_SA2_2013,
                  'INDIVIDUALSA22018'  = db.censusnz::INDIVIDUAL_SA2_2018,
                  'INDIVIDUALLBA2006'  = db.censusnz::INDIVIDUAL_LBA_2006,
                  'INDIVIDUALLBA2013'  = db.censusnz::INDIVIDUAL_LBA_2013,
                  'INDIVIDUALLBA2018'  = db.censusnz::INDIVIDUAL_LBA_2018,
                  'INDIVIDUALDHB2006'  = db.censusnz::INDIVIDUAL_DHB_2006,
                  'INDIVIDUALDHB2013'  = db.censusnz::INDIVIDUAL_DHB_2013,
                  'INDIVIDUALDHB2018'  = db.censusnz::INDIVIDUAL_DHB_2018,
                  'INDIVIDUALTA2006'  = db.censusnz::INDIVIDUAL_TA_2006,
                  'INDIVIDUALTA2013'  = db.censusnz::INDIVIDUAL_TA_2013,
                  'INDIVIDUALTA2018'  = db.censusnz::INDIVIDUAL_TA_2018,
                  'INDIVIDUALRC2006'  = db.censusnz::INDIVIDUAL_RC_2006,
                  'INDIVIDUALRC2013'  = db.censusnz::INDIVIDUAL_RC_2013,
                  'INDIVIDUALRC2018'  = db.censusnz::INDIVIDUAL_RC_2018,
                  'INDIVIDUALWARD2006'  = db.censusnz::INDIVIDUAL_WARD_2006,
                  'INDIVIDUALWARD2013'  = db.censusnz::INDIVIDUAL_WARD_2013,
                  'INDIVIDUALWARD2018'  = db.censusnz::INDIVIDUAL_WARD_2018,
                  'DWELLINGSA12006'  = db.censusnz::DWELLING_SA1_2006,
                  'DWELLINGSA12013'  = db.censusnz::DWELLING_SA1_2013,
                  'DWELLINGSA12018'  = db.censusnz::DWELLING_SA1_2018,
                  'DWELLINGSA22006'  = db.censusnz::DWELLING_SA2_2006,
                  'DWELLINGSA22013'  = db.censusnz::DWELLING_SA2_2013,
                  'DWELLINGSA22018'  = db.censusnz::DWELLING_SA2_2018,
                  'DWELLINGLBA2006'  = db.censusnz::DWELLING_LBA_2006,
                  'DWELLINGLBA2013'  = db.censusnz::DWELLING_LBA_2013,
                  'DWELLINGLBA2018'  = db.censusnz::DWELLING_LBA_2018,
                  'DWELLINGDHB2006'  = db.censusnz::DWELLING_DHB_2006,
                  'DWELLINGDHB2013'  = db.censusnz::DWELLING_DHB_2013,
                  'DWELLINGDHB2018'  = db.censusnz::DWELLING_DHB_2018,
                  'DWELLINGTA2006'  = db.censusnz::DWELLING_TA_2006,
                  'DWELLINGTA2013'  = db.censusnz::DWELLING_TA_2013,
                  'DWELLINGTA2018'  = db.censusnz::DWELLING_TA_2018,
                  'DWELLINGRC2006'  = db.censusnz::DWELLING_RC_2006,
                  'DWELLINGRC2013'  = db.censusnz::DWELLING_RC_2013,
                  'DWELLINGRC2018'  = db.censusnz::DWELLING_RC_2018,
                  'DWELLINGWARD2006'  = db.censusnz::DWELLING_WARD_2006,
                  'DWELLINGWARD2013'  = db.censusnz::DWELLING_WARD_2013,
                  'DWELLINGWARD2018'  = db.censusnz::DWELLING_WARD_2018,
                  'HOUSEHOLDSA12006'  = db.censusnz::HOUSEHOLD_SA1_2006,
                  'HOUSEHOLDSA12013'  = db.censusnz::HOUSEHOLD_SA1_2013,
                  'HOUSEHOLDSA12018'  = db.censusnz::HOUSEHOLD_SA1_2018,
                  'HOUSEHOLDSA22006'  = db.censusnz::HOUSEHOLD_SA2_2006,
                  'HOUSEHOLDSA22013'  = db.censusnz::HOUSEHOLD_SA2_2013,
                  'HOUSEHOLDSA22018'  = db.censusnz::HOUSEHOLD_SA2_2018,
                  'HOUSEHOLDLBA2006'  = db.censusnz::HOUSEHOLD_LBA_2006,
                  'HOUSEHOLDLBA2013'  = db.censusnz::HOUSEHOLD_LBA_2013,
                  'HOUSEHOLDLBA2018'  = db.censusnz::HOUSEHOLD_LBA_2018,
                  'HOUSEHOLDDHB2006'  = db.censusnz::HOUSEHOLD_DHB_2006,
                  'HOUSEHOLDDHB2013'  = db.censusnz::HOUSEHOLD_DHB_2013,
                  'HOUSEHOLDDHB2018'  = db.censusnz::HOUSEHOLD_DHB_2018,
                  'HOUSEHOLDTA2006'  = db.censusnz::HOUSEHOLD_TA_2006,
                  'HOUSEHOLDTA2013'  = db.censusnz::HOUSEHOLD_TA_2013,
                  'HOUSEHOLDTA2018'  = db.censusnz::HOUSEHOLD_TA_2018,
                  'HOUSEHOLDRC2006'  = db.censusnz::HOUSEHOLD_RC_2006,
                  'HOUSEHOLDRC2013'  = db.censusnz::HOUSEHOLD_RC_2013,
                  'HOUSEHOLDRC2018'  = db.censusnz::HOUSEHOLD_RC_2018,
                  'HOUSEHOLDWARD2006'  = db.censusnz::HOUSEHOLD_WARD_2006,
                  'HOUSEHOLDWARD2013'  = db.censusnz::HOUSEHOLD_WARD_2013,
                  'HOUSEHOLDWARD2018'  = db.censusnz::HOUSEHOLD_WARD_2018
                  )

  return(result)
}
