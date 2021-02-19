#' @title get_janes_equipment
#' @description Pulls Janes equipment.
#'
#' @param country Country filter for equipment
#' @param query Query filter for equipment
#' @param type Filter for type of equipment (i.e. "Platforms")
#' @param environment Filter for environment of equipment (i.e. "Air")
#' @param overall_family Overall equipment family
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
#' @importFrom purrr pluck
#' @importFrom dplyr pull
#' @importFrom dplyr bind_cols
#' @export


#
# get_janes_equipment <- function(country = NULL, query = NULL,
#                                 environment = NULL, type = NULL,
#                                 overall_family = NULL){
#
#     page_range <- get_page_range(country = country, endpoint = "equipment",
#                                  query = str_replace_all(query, " ", "%20"),
#                                  environment = environment,
#                                  type = type,
#                                  overall_family = overall_family)
#
#     equipment <- map(page_range, ~ get_janes_info(x = .x, country = country,
#                                                   endpoint = "equipment",
#                                                   query = str_replace_all(query, " ", "%20"),
#                                                   environment = environment,
#                                                   type = type,
#                                                   overall_family = overall_family)) %>%
#         bind_rows()
#
#
#     equipment_data <- map(equipment$url, get_janes_data2)
#
#     equipment_data %>%
#         tibble() %>%
#         conditional_unnest_wider(".") %>%
#         conditional_unnest_wider(".") %>%
#         conditional_unnest_wider("equipment") %>%
#         unnest_all("equipment") %>%
#         unnest_all("equipment") %>%
#         unnest_all("equipment") %>%
#         unnest_all("equipment") %>%
#         rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
#         rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))


get_janes_equipment <- function(country = NULL ) {

    GET(paste0("https://developer.janes.com/api/v1/data/equipment?f=countryiso(", country, ")&num=100000"),
        add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
        content(as = "text") %>%
        fromJSON() %>%
        pluck(2) %>%
        tibble() %>%
        dplyr::pull(url) %>%
        map_dfr(~ GET(.x,  add_headers("Authorization" = Sys.getenv("JANES_KEY"))) %>%
                    content(as = "text") %>%
                    fromJSON() %>%
                    tibble()) %>%
        conditional_unnest_wider(".") %>%
        unnest_all2() %>%
        unnest_all2() %>%
        unnest_all2() %>%
        unnest_all2() %>%
        rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
        rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))


}



#' @export
