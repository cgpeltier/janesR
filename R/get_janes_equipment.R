#' @title get_janes_equipment
#' @description Pulls Janes equipment.
#'
#' @param country Country filter for news
#'
#' @return Janes equipment data.
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_replace_all
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @importFrom jsonlite flatten
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom tibble tibble
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr unnest_auto
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
#' @importFrom dplyr starts_with
#' @importFrom dplyr any_of
#' @export



get_janes_inventories <- function(country = NULL){
  page_range <- get_page_range(country = country, endpoint = "inventories")
  equipment <- map(page_range, ~ get_janes_info(.x, country = country,
                                                  endpoint = "equipment")) %>%
      bind_rows()
  equipment_data <- map(equipment$url, get_janes_data)

  equipment_data %>%
      tibble() %>%
      rename(equipment = ".") %>%
      unnest_auto(equipment) %>%
      rename(equipment = ".") %>%
      unnest_auto(equipment) %>%
      unnest_auto(equipment) %>%
      unnest_wider(types) %>%
      unnest_wider(operatorCountries) %>%
      select(-any_of("...1")) %>%
      unnest_wider(operatorCountry) %>%
      select(-any_of("...1")) %>%
      unnest_wider(operatorCountryName) %>%
      rename_with(.fn = ~ gsub("...", "operator_country", .x, fixed = TRUE),
                  .cols = starts_with("...")) %>%
      unnest_wider(type) %>%
      rename_with(.fn = ~ gsub("...", "type", .x, fixed = TRUE),
                  .cols = starts_with("...")) %>%
      unnest_wider(roles) %>%
      select(-any_of("...1")) %>%
      unnest_wider(role) %>%
      rename_with(.fn = ~ gsub("...", "role", .x, fixed = TRUE),
                  .cols = starts_with("...")) %>%
      unnest_wider(manufacturers) %>%
      select(-any_of("...1")) %>%
      unnest_wider(manufacturer) %>%
      rename_with(.fn = ~ gsub("...", "manufacturer", .x, fixed = TRUE),
                  .cols = starts_with("...")) %>%
      unnest_wider(manufacturerName) %>%
      rename_with(.fn = ~ gsub("...", "manufacturer_name", .x, fixed = TRUE),
                  .cols = starts_with("..."))
}


#' @export
