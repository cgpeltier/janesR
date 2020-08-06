#' @title get_janes_equipment_relationships
#' @description Pulls Janes equipment relationships (hierarchical links).
#'
#' @param country Country filter for equipment
#' @param query Query filter for equipment
#'
#' @return Janes equipment hierarchical links.
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



get_janes_equipment_relationships <- function(country = NULL, query = NULL){
  page_range <- get_page_range(country = country, endpoint = "equipmentrelationships",
                               query = str_replace_all(query, " ", "%20"))
  equipment <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                endpoint = "equipmentrelationships",
                                                query = str_replace_all(query, " ", "%20"))) %>%
    bind_rows()

  equipment_data <- map(equipment$url, get_janes_data)

  equipment_data %>%
    tibble() %>%
    rename(equipment = ".") %>%
    unnest_wider(equipment) %>%
    rename(equipment = ".") %>%
    unnest_wider(equipment) %>%
    unnest_wider(itemLink) %>%
    conditional_unnest_wider("countries") %>%
    conditional_unnest_wider("country") %>%
    clean_names() %>%
    remove_empty()
}







#' @export
