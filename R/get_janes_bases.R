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
        unnest_wider(installation) %>%
        select(-any_of("...1")) %>%
        unnest_wider(operators) %>%
        select(-installationId) %>%
        select(-any_of("...1")) %>%
        unnest_wider(operator) %>%
        select(-any_of("...1")) %>%
        unnest_wider(operatorCountry, names_repair = "unique", names_sep = names_sep_vector) %>%
        unnest_wider(location) %>%
        select(-any_of("...1")) %>%
        unnest_wider(runways) %>%
        select(-any_of("...1")) %>%
        unnest_wider(runway) %>%
        select(-any_of("...1")) %>%
        unnest_wider(runwayLengthMetres) %>%
        rename_with(.fn = ~ gsub("...", "runway_length_m", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayOrientationOpposing) %>%
        rename_with(.fn = ~ gsub("...", "runway_length_opposing", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayOrientation) %>%
        rename_with(.fn = ~ gsub("...", "runway_orientation", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwaySurface) %>%
        rename_with(.fn = ~ gsub("...", "runway_surface", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayName) %>%
        rename_with(.fn = ~ gsub("...", "runway_name", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayDirection1Name) %>%
        rename_with(.fn = ~ gsub("...", "runway_direction1_name", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayDirection2Name) %>%
        rename_with(.fn = ~ gsub("...", "runway_direction2_name", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayStaus) %>%
        rename_with(.fn = ~ gsub("...", "runway_status", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayLengthFeet) %>%
        rename_with(.fn = ~ gsub("...", "runway_length_ft", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayWidthMetres) %>%
        rename_with(.fn = ~ gsub("...", "runway_width_m", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayWidthFeet) %>%
        rename_with(.fn = ~ gsub("...", "runway_width_ft", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(runwayCenterline) %>%
        rename_with(.fn = ~ gsub("...", "runway_centerline", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(synonyms) %>%
        select(-any_of("...1")) %>%
        unnest_wider(operatorId) %>%
        rename_with(.fn = ~ gsub("...", "operator_id", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(installationId) %>%
        rename_with(.fn = ~ gsub("...", "installation_id", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(operatorServiceType) %>%
        rename_with(.fn = ~ gsub("...", "operator_service_type", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(operatorRegion) %>%
        rename_with(.fn = ~ gsub("...", "operator_region", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(operatorCountryISO) %>%
        rename_with(.fn = ~ gsub("...", "operator_country_iso", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        janitor::clean_names() %>%
        janitor::remove_empty()

}


#' @export
