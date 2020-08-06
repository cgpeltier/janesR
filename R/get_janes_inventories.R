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
    rename(inventory_id = id, inventory_title = title) %>%
    select(1:15) %>%
    unnest_wider(equipment) %>%
    #rename(any_of(roles_type = type)) %>%
    unnest_wider(family, names_repair = ~gsub('...', 'family', ., fixed = TRUE)) %>%
    unnest_wider(types, names_repair = ~gsub('...', 'types', ., fixed = TRUE)) %>%
    unnest_wider(type, names_repair = ~gsub('...', 'type', ., fixed = TRUE)) %>%
    unnest_wider(roles, names_repair = ~gsub('...', 'roles', ., fixed = TRUE)) %>%
    unnest_wider(role, names_repair = ~gsub('...', 'role', ., fixed = TRUE)) %>%
    unnest_wider(operator, names_repair = ~gsub('...', 'operator', ., fixed = TRUE)) %>%
    unnest_wider(documents, names_repair = ~gsub('...', 'documents', ., fixed = TRUE)) %>%
    unnest_wider(document,  names_repair = ~gsub('...', 'document', ., fixed = TRUE)) %>%
    unnest_wider(documentId, names_repair = ~gsub('...', 'document_id', ., fixed = TRUE)) %>%
    unnest_wider(documentTitle, names_repair = ~gsub('...', 'document_title', ., fixed = TRUE)) %>%
    clean_names() %>%
    remove_empty()

}

## old code
# inventories_data %>%
#     tibble() %>%
#     rename(inventory = ".") %>%
#     unnest_wider(inventory) %>%
#     rename(inventory = ".") %>%
#     unnest_wider(inventory) %>%
#     unnest_wider(inventory) %>%
#     select(-"id", -"title") %>%
#     unnest_wider(equipment) %>%
#     unnest_wider(family, names_repair = ~gsub('...', 'family', ., fixed = TRUE)) %>%
#     unnest_wider(types, names_repair = ~gsub('...', 'types', ., fixed = TRUE)) %>%
#     unnest_wider(type, names_repair = ~gsub('...', 'type', ., fixed = TRUE)) %>%
#     unnest_wider(roles, names_repair = ~gsub('...', 'roles', ., fixed = TRUE)) %>%
#     unnest_wider(role, names_repair = ~gsub('...', 'role', ., fixed = TRUE)) %>%
#     unnest_wider(operator, names_repair = ~gsub('...', 'operator', ., fixed = TRUE)) %>%
#     unnest_wider(documents, names_repair = ~gsub('...', 'documents', ., fixed = TRUE)) %>%
#     unnest_wider(document,  names_repair = ~gsub('...', 'document', ., fixed = TRUE)) %>%
#     unnest_wider(documentId, names_repair = ~gsub('...', 'document_id', ., fixed = TRUE)) %>%
#     unnest_wider(documentTitle, names_repair = ~gsub('...', 'document_title', ., fixed = TRUE)) %>%
#     clean_names() %>%
#     remove_empty()



#' @export
