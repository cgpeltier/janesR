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
    names_sep_vector <- paste0("_", seq(1:20))

    bases_data %>%
        tibble() %>%
        rename(base = ".") %>%
        unnest_wider(base) %>%
        rename(base = ".") %>%
        unnest_wider(base) %>%
        select(-any_of("...1")) %>%
        unnest_wider(installation, names_repair = ~gsub('...', 'installation', ., fixed = TRUE)) %>%
        unnest_wider(operators, names_repair = ~gsub('...', 'operators', ., fixed = TRUE)) %>%
        select(-installationId) %>%
        unnest_wider(operator, names_repair = ~gsub('...', 'operator', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountry, names_repair = ~gsub('...', 'operator_country', ., fixed = TRUE)) %>%
        unnest_wider(location, names_repair = ~gsub('...', 'location', ., fixed = TRUE)) %>%
        unnest_wider(runways, names_repair = ~gsub('...', 'runways', ., fixed = TRUE)) %>%
        unnest_wider(runway, names_repair = ~gsub('...', 'runway', ., fixed = TRUE)) %>%
        unnest_wider(runwayLengthMetres, names_repair = ~gsub('...', 'runway_length_m', ., fixed = TRUE)) %>%
        unnest_wider(runwayOrientationOpposing, names_repair = ~gsub('...', 'runway_orientation_opp', ., fixed = TRUE)) %>%
        unnest_wider(runwayOrientation, names_repair = ~gsub('...', 'runway_orientation', ., fixed = TRUE)) %>%
        unnest_wider(runwaySurface, names_repair = ~gsub('...', 'runway_surface', ., fixed = TRUE)) %>%
        unnest_wider(runwayName, names_repair = ~gsub('...', 'runway_name', ., fixed = TRUE)) %>%
        unnest_wider(runwayDirection1Name, names_repair = ~gsub('...', 'runway_direction1_name', ., fixed = TRUE)) %>%
        unnest_wider(runwayDirection2Name, names_repair = ~gsub('...', 'runway_direction1_name', ., fixed = TRUE)) %>%
        unnest_wider(runwayStaus, names_repair = ~gsub('...', 'runway_status', ., fixed = TRUE)) %>%
        unnest_wider(runwayLengthFeet, names_repair = ~gsub('...', 'runway_length_ft', ., fixed = TRUE)) %>%
        unnest_wider(runwayWidthMetres, names_repair = ~gsub('...', 'runway_length_m', ., fixed = TRUE)) %>%
        unnest_wider(runwayWidthFeet, names_repair = ~gsub('...', 'runway_width_ft', ., fixed = TRUE)) %>%
        unnest_wider(runwayCenterline, names_repair = ~gsub('...', 'runway_centerline', ., fixed = TRUE)) %>%
        unnest_wider(synonyms, names_repair = ~gsub('...', 'synonyms', ., fixed = TRUE)) %>%
        unnest_wider(operatorId, names_repair = ~gsub('...', 'operator_id', ., fixed = TRUE)) %>%
        unnest_wider(installationId, names_repair = ~gsub('...', 'installation_id', ., fixed = TRUE)) %>%
        unnest_wider(operatorServiceType, names_repair = ~gsub('...', 'operator_service_type', ., fixed = TRUE)) %>%
        unnest_wider(operatorRegion, names_repair = ~gsub('...', 'operator_region', ., fixed = TRUE)) %>%
        unnest_wider(operatorCountryISO, names_repair = ~gsub('...', 'operator_country_iso', ., fixed = TRUE)) %>%
        janitor::clean_names() %>%
        janitor::remove_empty()

}


#' @export
