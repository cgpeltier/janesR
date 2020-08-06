#' @title get_janes_equipment
#' @description Pulls Janes equipment.
#'
#' @param country Country filter for equipment
#' @param query Query filter for equipment
#' @param type Filter for type of equipment (i.e. "Platforms")
#' @param environment Filter for environment of equipment (i.e. "Air")
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



get_janes_equipment <- function(country = NULL, query = NULL,
                                environment = NULL, type = NULL){
    page_range <- get_page_range(country = country, endpoint = "equipment",
                                 query = str_replace_all(query, " ", "%20"),
                                 environment = environment,
                                 type = type)

    equipment <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                  endpoint = "equipment",
                                                  query = str_replace_all(query, " ", "%20"),
                                                  environment = environment,
                                                  type = type)) %>%
        bind_rows()

    equipment_data <- map(equipment$url, get_janes_data)

    equipment_data %>%
        tibble() %>%
        rename(equipment = ".") %>%
        unnest_wider(equipment) %>%
        rename(equipment = ".") %>%
        unnest_wider(equipment) %>%
        unnest_wider(equipment) %>%
        select(1:18) %>%
        unnest_wider(types, names_repair = ~gsub('...', 'types', ., fixed = TRUE)) %>%
        unnest_wider(type, names_repair = ~gsub('...', 'type', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountries, names_repair = ~gsub('...', 'operator_countries', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountry, names_repair = ~gsub('...', 'operator_country', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountryName, names_repair = ~gsub('...', 'operator_country_name', ., fixed = TRUE)) %>%
        unnest_wider(roles, names_repair = ~gsub('...', 'roles', ., fixed = TRUE)) %>%
        unnest_wider(role, names_repair = ~gsub('...', 'role', ., fixed = TRUE)) %>%
        unnest_wider(manufacturers, names_repair = ~gsub('...', 'manufacturers', ., fixed = TRUE)) %>%
        unnest_wider(manufacturer, names_repair = ~gsub('...', 'manufacturer', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerName, names_repair = ~gsub('...', 'manufacturer_name', ., fixed = TRUE)) %>%
        unnest_wider(overallFamily, names_repair = ~gsub('...', 'overall_family', ., fixed = TRUE)) %>%
        unnest_wider(family, names_repair = ~gsub('...', 'family', ., fixed = TRUE)) %>%
        unnest_wider(primayParent, names_repair = ~gsub('...', 'primary_parent', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerCountries, names_repair = ~gsub('...', 'manufacturer_countries', ., fixed = TRUE)) %>%
        unnest_wider(documents, names_repair = ~gsub('...', 'documents', ., fixed = TRUE)) %>%
        unnest_wider(document, names_repair = ~gsub('...', 'document', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerId, names_repair = ~gsub('...', 'manufacturer_id', ., fixed = TRUE)) %>%
        unnest_wider(documentId, names_repair = ~gsub('...', 'document_id', ., fixed = TRUE)) %>%
        unnest_wider(documentTitle, names_repair = ~gsub('...', 'document_title', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerCountry, names_repair = ~gsub('...', 'manufacturer_country', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerCountryIso, names_repair = ~gsub('...', 'manufacturer_country_iso', ., fixed = TRUE)) %>%
        unnest_wider(manufacturerCountryName, names_repair = ~gsub('...', 'manufacturer_country_name', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountryIso, names_repair = ~gsub('...', 'operator_country_iso', ., fixed = TRUE)) %>%
        unnest_wider(synonyms, names_repair = ~gsub('...', 'synonyms', ., fixed = TRUE)) %>%
        unnest_wider(synonym, names_repair = ~gsub('...', 'synonym', ., fixed = TRUE)) %>%
        unnest_wider(users, names_repair = ~gsub('...', 'users', ., fixed = TRUE)) %>%
        unnest_wider(user, names_repair = ~gsub('...', 'user', ., fixed = TRUE)) %>%
        unnest_wider(environments, names_repair = ~gsub('...', 'environments', ., fixed = TRUE)) %>%
        unnest_wider(environment, names_repair = ~gsub('...', 'environment', ., fixed = TRUE)) %>%
        conditional_unnest_wider("mobility") %>%
        conditional_unnest_wider("operations") %>%
        conditional_unnest_wider("operation") %>%
        #unnest_wider(mobilities, names_repair = ~gsub('...', 'mobilities', ., fixed = TRUE)) %>%
        #unnest_wider(operations, names_repair = ~gsub('...', 'operations', ., fixed = TRUE)) %>%
        #unnest_wider(operation, names_repair = ~gsub('...', 'operation', ., fixed = TRUE)) %>%
        clean_names() %>%
        remove_empty()

}


# the commented-out lines above may need to be active to fully unnest all data in big pulls

## old code
# equipment_data %>%
#     tibble() %>%
#     rename(equipment = ".") %>%
#     unnest_wider(equipment) %>%
#     rename(equipment = ".") %>%
#     unnest_wider(equipment) %>%
#     unnest_wider(equipment) %>%
#     select(1:20) %>%
#     unnest_wider(types) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(type) %>%
#     rename_with(.fn = ~ gsub("...", "type", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(operatorCountries) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(operatorCountry) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(operatorCountryName) %>%
#     rename_with(.fn = ~ gsub("...", "operator_country", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(roles) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(role) %>%
#     rename_with(.fn = ~ gsub("...", "role", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(manufacturers) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(manufacturer) %>%
#     rename_with(.fn = ~ gsub("...", "manufacturer", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(manufacturerName) %>%
#     rename_with(.fn = ~ gsub("...", "manufacturer_name", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(overallFamily) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(family) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(primayParent) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(manufacturerCountries) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(documents) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(document) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(manufacturerId) %>%
#     rename_with(.fn = ~ gsub("...", "manufactureId", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(documentId) %>%
#     rename_with(.fn = ~ gsub("...", "document_id", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(documentTitle) %>%
#     rename_with(.fn = ~ gsub("...", "document_title", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(manufacturerCountry) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(manufacturerCountryIso) %>%
#     rename_with(.fn = ~ gsub("...", "manufacturer_country_iso", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(manufacturerCountryName) %>%
#     rename_with(.fn = ~ gsub("...", "manufacturer_country_name", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(operatorCountryIso) %>%
#     rename_with(.fn = ~ gsub("...", "operator_country_iso", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(synonyms) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(synonym) %>%
#     rename_with(.fn = ~ gsub("...", "synonym", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(users) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(user) %>%
#     rename_with(.fn = ~ gsub("...", "user", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(environments) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(environment) %>%
#     rename_with(.fn = ~ gsub("...", "environment", .x, fixed = TRUE),
#                 .cols = starts_with("...")) %>%
#     unnest_wider(mobilities) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(operations) %>%
#     select(-any_of("...1")) %>%
#     unnest_wider(operation) %>%
#     clean_names()

#' @export
