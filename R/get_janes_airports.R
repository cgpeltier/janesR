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
  names_sep_vector <- paste0("_", seq(1:20))

  airports_data %>%
      tibble() %>%
      rename(airport = ".") %>%
      unnest_wider(airport) %>%
      rename(airport = ".") %>%
      unnest_wider(airport) %>%
      unnest_wider(installation) %>%
      unnest_wider(location) %>%
      unnest_wider(synonyms) %>%
      unnest_wider(synonym, names_repair = "unique", names_sep = names_sep_vector) %>%
      unnest_wider(operators) %>%
      unnest_wider(operator, names_repair = "unique", names_sep = "_") %>%
      janitor::clean_names() %>%
      janitor::remove_empty()
}


#' @export
