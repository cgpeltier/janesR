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
        conditional_unnest_wider("location") %>%
        conditional_unnest_wider("synonyms") %>%
        conditional_unnest_wider("synonym") %>%
        conditional_unnest_wider("operators") %>%
        conditional_unnest_wider2("operator") %>%
        conditional_unnest_wider("runways") %>%
        conditional_unnest_wider("runway") %>%
        conditional_unnest_wider("runwayLengthMetres") %>%
        conditional_unnest_wider("runwayOrientationOpposing") %>%
        conditional_unnest_wider("runwayOrientation") %>%
        conditional_unnest_wider("runwaySurface") %>%
        conditional_unnest_wider("runwayName") %>%
        conditional_unnest_wider("runwayDirection1Name") %>%
        conditional_unnest_wider("runwayDirection2Name") %>%
        conditional_unnest_wider("runwayStaus") %>%
        conditional_unnest_wider("runwayLengthFeet") %>%
        conditional_unnest_wider("runwayWidthMetres") %>%
        conditional_unnest_wider("runwayWidthFeet") %>%
        conditional_unnest_wider("runwayCenterline") %>%
        conditional_unnest_wider("operator_operatorCountryISO") %>%
        conditional_unnest_wider("operator_operatorId") %>%
        conditional_unnest_wider("operator_installationId") %>%
        conditional_unnest_wider("operator_operatorServiceType") %>%
        conditional_unnest_wider("operator_operatorCountry") %>%
        conditional_unnest_wider("operator_operatorRegion") %>%
        conditional_unnest_wider("runwayLightingIntensity") %>%
        conditional_unnest_wider("runwayPCN") %>%
        select(-(starts_with("operator_installationId"))) %>%
        rename_with(.cols = starts_with("operator_"), ~ str_remove(., "operator_")) %>%
        janitor::clean_names()
}


#' @export
