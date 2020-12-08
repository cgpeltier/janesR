#' @title get_janes_airports
#' @description Pulls Janes airports data.
#'
#' @param country Country in which airport is located
#'
#' @return Janes airports data.
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
#' @importFrom tidyr unite
#' @export



get_janes_airports <- function(country = NULL){
    page_range <- get_page_range(country = country, endpoint = "airports")

    airports <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                              endpoint = "airports")) %>%
        bind_rows()

    airports_data <- map(airports$url, get_janes_data)

    airports_data %>%
        tibble() %>%
        unnest_wider(".") %>%
        unnest_wider(".") %>%
        unnest_wider(installation) %>%
        unnest_all("airport") %>%
        unnest_all("airport") %>%
        unnest_all("airport") %>%
        unnest_all("airport") %>%
        rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
        rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))
}


#' @export
