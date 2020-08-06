#' @title get_janes_inventories
#' @description Pulls Janes equipment inventories.
#'
#' @param country Country filter
#' @param operator_force Operator force
#'
#' @return Janes equipment inventories.
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
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
#' @export



get_janes_inventories <- function(country = NULL, operator_force = NULL){
  page_range <- get_page_range(country = country, operator_force = operator_force,
                               endpoint = "inventories")
  inventories <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                  operator_force = operator_force,
                                                  endpoint = "inventories")) %>%
    bind_rows()
  inventories_data <- map(inventories$url, get_janes_data)

  inventories_data %>%
    tibble() %>%
    rename(inventory = ".") %>%
    unnest_wider(inventory) %>%
    rename(inventory = ".") %>%
    unnest_wider(inventory) %>%
    unnest_wider(inventory) %>%
    select(-"id", -"title") %>%
    unnest_wider(equipment) %>%
    unnest_wider(family) %>%
    unnest_wider(types) %>%
    unnest_wider(type) %>%
    rename_with(.fn = ~ gsub("...", "type_", .x, fixed = TRUE),
                .cols = contains("...")) %>%
    unnest_wider(roles) %>%
    unnest_wider(role) %>%
    rename_with(.fn = ~ gsub("...", "role_", .x, fixed = TRUE),
                .cols = contains("...")) %>%
    unnest_wider(operator) %>%
    janitor::clean_names() %>%
    janitor::remove_empty()

}


#' @export
