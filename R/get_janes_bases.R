#' @title get_janes_bases
#' @description Pulls Janes bases data.
#'
#' @param country Country in which base is located
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
#' @importFrom tidyr unite
#' @export



get_janes_bases <- function(country = NULL){
    page_range <- get_page_range(country = country, endpoint = "bases")
    bases <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                             endpoint = "bases")) %>%
        bind_rows()
    bases_data <- map(bases$url, get_janes_data)

    bases_data %>%
        tibble() %>%
        rename(base = ".") %>%
        unnest_wider(base) %>%
        rename(base = ".") %>%
        unnest_wider(base) %>%
        select(-any_of("...1")) %>%
        conditional_unnest_wider("installation") %>%
        conditional_unnest_wider("operators") %>%
        conditional_unnest_wider("operator") %>%
        select(-"installationId") %>%
        conditional_unnest_wider("operatorCountry") %>%
        conditional_unnest_wider("runways") %>%
        conditional_unnest_wider("runway") %>%
        conditional_unnest_wider("runwayOrientation") %>%
        conditional_unnest_wider("runwayOreientationOpposed") %>%
        conditional_unnest_wider("runwaySurface") %>%
        conditional_unnest_wider("runwayName") %>%
        conditional_unnest_wider("runwayStatus") %>%
        conditional_unnest_wider("runwayCenterline") %>%
        conditional_unnest_wider("synonyms") %>%
        conditional_unnest_wider("synonym") %>%
        conditional_unnest_wider("operatorId") %>%
        conditional_unnest_wider("installationId") %>%
        conditional_unnest_wider("operatorServiceType") %>%
        conditional_unnest_wider("operatorRegion") %>%
        conditional_unnest_wider("operatorCountryISO") %>%
        conditional_unnest_wider("location") %>%
        conditional_unnest_wider("runwayLengthMetres") %>%
        conditional_unnest_wider("runwayLengthFeet") %>%
        conditional_unnest_wider("runwayOrientationOpposing") %>%
        conditional_unnest_wider("runwayDirection1Name") %>%
        conditional_unnest_wider("runwayDirection2Name") %>%
        conditional_unnest_wider("runwayStaus") %>%
        conditional_unnest_wider("runwayWidthMetres") %>%
        conditional_unnest_wider("runwayWidthFeet") %>%
        conditional_unnest_wider("runwayPCN") %>%
        janitor::clean_names()


}


#' @export
