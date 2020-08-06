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
        rename(airport = ".") %>%
        unnest_wider(airport) %>%
        rename(airport = ".") %>%
        unnest_wider(airport) %>%
        unnest_wider(installation) %>%
        unnest_wider(location, names_repair = ~gsub('...', 'location', ., fixed = TRUE)) %>%
        unnest_wider(synonyms, names_repair = ~gsub('...', 'synonyms', ., fixed = TRUE)) %>%
        unnest_wider(synonym, names_repair = ~gsub('...', 'synonym', ., fixed = TRUE)) %>%
        unnest_wider(operators) %>%
        unnest_wider(operator, names_repair = ~gsub('...', 'operator', ., fixed = TRUE)) %>%
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
        janitor::clean_names() %>%
        janitor::remove_empty()
}


#' @export
