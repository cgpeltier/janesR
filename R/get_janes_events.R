#' @title get_janes_events
#' @description Pulls Janes events data
#'
#' @param country Event country
#' @param query Search term for events
#' @param post_date
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



get_janes_events <- function(country = NULL, query = NULL, post_date = NULL){
    page_range <- get_page_range(country = country, endpoint = "events",
                                 query = str_replace_all(query, " ", "%20"),
                                 post_date = post_date)
    events <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "events",
                                               query = str_replace_all(query, " ", "%20"),
                                               post_date = post_date)) %>%
        bind_rows()
    events_data <- map(events$url, get_janes_data)

    events_data %>%
        tibble()

    # %>%
    #     rename(events = ".") %>%
    #     unnest_wider(events) %>%
    #     rename(events = ".") %>%
    #     unnest_wider(events) %>%
    #     conditional_unnest_wider("eventLocation") %>%
    #     conditional_unnest_wider("date") %>%
    #     conditional_unnest_wider("eventSources") %>%
    #     conditional_unnest_wider("place") %>%
    #     conditional_unnest_wider("name") %>%
    #     conditional_unnest_wider("casualties") %>%
    #     conditional_unnest_wider("intelligence") %>%
    #     conditional_unnest_wider("actors") %>%
    #     conditional_unnest_wider("ct_statement") %>%
    #     conditional_unnest_wider("nsag_attack") %>%
    #     conditional_unnest_wider("offset") %>%
    #     conditional_unnest_wider("direction") %>%
    #     conditional_unnest_wider("distance") %>%
    #     conditional_unnest_wider("direction") %>%
    #     conditional_unnest_wider("unitOfMeasure") %>%
    #     conditional_unnest_wider("locationQuality") %>%
    #     conditional_unnest_wider("militants") %>%
    #     conditional_unnest_wider("civilians") %>%
    #     conditional_unnest_wider("securityForces") %>%
    #     conditional_unnest_wider("civilianSecurityForces") %>%
    #     conditional_unnest_wider("unidentified") %>%
    #     conditional_unnest_wider("nonMilitantTotals") %>%
    #     conditional_unnest_wider("totals") %>%
    #     conditional_unnest_wider("killed") %>%
    #     conditional_unnest_wider("killedNumber") %>%
    #     conditional_unnest_wider("killedPrecision") %>%
    #     conditional_unnest_wider("wounded") %>%
    #     conditional_unnest_wider("woundedNumber") %>%
    #     conditional_unnest_wider("woundedPrecision") %>%
    #     conditional_unnest_wider("capture") %>%
    #     conditional_unnest_wider("role") %>%
    #     conditional_unnest_wider("name") %>%
    #     conditional_unnest_wider("family") %>%
    #     conditional_unnest_wider("nation") %>%
    #     conditional_unnest_wider("scope") %>%
    #     conditional_unnest_wider("region_2") %>%
    #     conditional_unnest_wider("orientation") %>%
    #     conditional_unnest_wider("type_2") %>%
    #     conditional_unnest_wider("ct_statement") %>%
    #     conditional_unnest_wider("nsag_attack") %>%
    #     clean_names()
}



#' @export
