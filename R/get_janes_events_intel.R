#' @title get_janes_events_intel
#' @description Pulls Janes intel events data
#'
#' @param country Event country
#' @param query Search term for events
#' @param post_date Event post date
#' @param start_date Event start date
#'
#' @return Janes events data
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
#' @importFrom dplyr mutate
#' @importFrom naniar replace_with_na_all
#' @export


get_janes_events_intel <- function(country = NULL, query = NULL, post_date = NULL,
                                   start_date = NULL){



  page_range <- get_page_range(country = country, endpoint = "events",
                               query = str_replace_all(query, " ", "%2B"),
                               post_date = post_date,
                               start_date = start_date,
                               event_type = "Intelligence Events")



  events <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                             endpoint = "events",
                                             query = str_replace_all(query, " ", "%2B"),
                                             post_date = post_date,
                                             start_date = start_date,
                                             event_type = "Intelligence Events")) %>%
    bind_rows()


  events_data <- map(events$url, get_janes_data)

  events_data

      events_data %>%
          tibble() %>%
          conditional_unnest_wider(".") %>%
          conditional_unnest_wider(".") %>%
          unnest_all("event") %>%
          unnest_all("event") %>%
          unnest_all("event") %>%
          unnest_all("event") %>%
          rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
          rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)")) %>%
          naniar::replace_with_na_all(condition = ~.x == "")


}





#' @export
