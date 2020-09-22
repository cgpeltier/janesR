#' @title get_janes_json
#' @description Pulls Janes data as a JSON file for all endpoints.
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Depends on endpoint
#' @param endpoint One of 6 options currently
#' @param query Search term
#' @param environment Of search, i.e. "Air"
#' @param operator_force Operator force
#'
#' @return Janes data in JSON format.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom jsonlite flatten
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
#' @importFrom dplyr starts_with
#' @importFrom dplyr any_of
#' @importFrom jsonlite write_json


get_janes_json <- function(country = NULL, branch = NULL, type = NULL,
                           operator_force = NULL, query = NULL, environment = NULL,
                           endpoint = c("inventories", "equipment", "orbats",
                                        "bases", "airports", "countryrisks",
                                        "companies", "events", "equipmentrelationships",
                                        "references", "samsites", "ewsites")){

    page_range <- get_page_range(country = country, endpoint = endpoint, branch = branch,
                                 type = type, operator_force = operator_force,
                                 environment = environment, query = query)

    temp <- map(page_range, ~ get_janes_info(x = .x, country = country, branch = branch,
                                             type = type, operator_force = operator_force,
                                             environment = environment, endpoint = endpoint,
                                             query = query)) %>%
        bind_rows()

    map(temp$url, ~ get_janes_data_json(.x) %>%
          write_json(., path = paste0(getwd(), "/", endpoint, "_", str_extract(.x, "(?<=\\/)[^\\/]+$"), ".json")))

}

#' @export

