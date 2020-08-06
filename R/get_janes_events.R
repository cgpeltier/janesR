#' @title get_janes_events
#' @description Pulls Janes events data
#'
#' @param country Event country
#' @param query Search term for events
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
#' @export



get_janes_events <- function(country = NULL, query = NULL){
    page_range <- get_page_range(country = country, endpoint = "events",
                                 query = str_replace_all(query, " ", "%20"))
    events <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                  endpoint = "events",
                                                  query = str_replace_all(query, " ", "%20"))) %>%
        bind_rows()
    events_data <- map(events$url, get_janes_data)

    events_data %>%
        tibble() %>%
        rename(events = ".") %>%
        unnest_wider(events) %>%
        rename(events = ".") %>%
        unnest_wider(events) %>%
        unnest_wider(eventLocation, names_repair = ~gsub('...', 'event_location', ., fixed = TRUE)) %>%
        unnest_wider(date, names_repair = ~gsub('...', 'date', ., fixed = TRUE)) %>%
        unnest_wider(eventSources, names_repair = ~gsub('...', 'event_sources', ., fixed = TRUE)) %>%
        unnest_wider(place, names_repair = ~gsub('...', 'place', ., fixed = TRUE)) %>%
        conditional_unnest_wider("name") %>%
        unnest_wider(casualties, names_repair = ~gsub('...', 'casualties', ., fixed = TRUE)) %>%
        unnest_wider(intelligence, names_repair = ~gsub('...', 'intelligence', ., fixed = TRUE)) %>%
        unnest_wider(actors, names_repair = ~gsub('...', 'actors', ., fixed = TRUE)) %>%
        conditional_unnest_wider("ct_statement") %>%
        conditional_unnest_wider("nsag_attack") %>%
        unnest_wider(offset,names_repair = ~gsub('...', 'offset', ., fixed = TRUE)) %>%
        conditional_unnest_wider("direction") %>%
        conditional_unnest_wider("distance") %>%
        conditional_unnest_wider("direction") %>%
        conditional_unnest_wider("unitOfMeasure") %>%
        conditional_unnest_wider("locationQuality") %>%
        conditional_unnest_wider("militants") %>%
        conditional_unnest_wider("civilians") %>%
        conditional_unnest_wider("securityForces") %>%
        conditional_unnest_wider("civilianSecurityForces") %>%
        conditional_unnest_wider("unidentified") %>%
        conditional_unnest_wider("nonMilitantTotals") %>%
        conditional_unnest_wider("totals") %>%
        #conditional_unnest_wider("killed") %>%
        #unnest_wider(number, names_repair = ~gsub('...', 'killed_number', ., fixed = TRUE)) %>%
        #unnest_wider(precision, names_repair = ~gsub('...', 'killed_precision', ., fixed = TRUE)) %>%
        #unnest_wider(wounded, names_repair = ~gsub('...', 'wounded', ., fixed = TRUE)) %>%
        #unnest_wider(number, names_repair = ~gsub('...', 'wounded_number', ., fixed = TRUE)) %>%
        #unnest_wider(precision, names_repair = ~gsub('...', 'wounded_precision', ., fixed = TRUE)) %>%
        #conditional_unnest_wider("captured") %>%
        conditional_unnest_wider("role") %>%
        conditional_unnest_wider("name") %>%
        conditional_unnest_wider("family") %>%
        conditional_unnest_wider("nation") %>%
        conditional_unnest_wider("scope") %>%
        conditional_unnest_wider("region_2") %>%
        conditional_unnest_wider("orientation") %>%
        conditional_unnest_wider("type_2") %>%
        conditional_unnest_wider("ct_statement") %>%
        conditional_unnest_wider("nsag_attack") %>%
        clean_names() %>%
        remove_empty()

}


#' @export
