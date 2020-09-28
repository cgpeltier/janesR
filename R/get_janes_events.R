#' @title get_janes_events
#' @description Pulls Janes events data
#'
#' @param country Event country
#' @param query Search term for events
#' @param post_date Event start date
#' @param event_type Either "Terorism and Insurgency" or "Intelligence Events"
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



get_janes_events <- function(country = NULL, query = NULL, post_date = NULL,
                             event_type = c("Terrorism and Insurgency", "Intelligence Events")){

    page_range <- get_page_range(country = country, endpoint = "events",
                                 query = str_replace_all(query, " ", "%20"),
                                 post_date = post_date,
                                 event_type = event_type)

    events <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "events",
                                               query = str_replace_all(query, " ", "%20"),
                                               post_date = post_date,
                                               event_type = event_type)) %>%
        bind_rows()

    events_data <- map(events$url, get_janes_data)

    # if(event_type == "Terrorism and Insurgency"){
    #     events_data %>%
    #         tibble() %>%
    #
    #
    # }
    #
    # if(event_type == "Intelligence Events"){
    #     events_data %>%
    #         tibble() %>%
    #
    #
    # }



    events_data %>%
        tibble() %>%
        unnest_wider(".") %>%
        unnest_wider(".") %>%
        rename(event_type = type) %>%
        conditional_unnest_wider("eventLocation") %>%
        rename(event_region= region) %>%
        conditional_unnest_wider("date") %>%
        conditional_unnest_wider("eventSources") %>%
        conditional_unnest_wider("place") %>%
        conditional_unnest_wider("name") %>%
        conditional_unnest_wider("casualties") %>%
        conditional_unnest_wider("intelligence") %>%
        conditional_unnest_wider("actors") %>%
        conditional_unnest_wider("eventSource") %>%
        conditional_unnest_wider("ctStatement") %>%
        conditional_unnest_wider("nsag_attack") %>%
        conditional_unnest_wider("offset") %>%
        conditional_unnest_wider("direction") %>%
        conditional_unnest_wider("distance") %>%
        conditional_unnest_wider("direction") %>%
        conditional_unnest_wider("unitOfMeasure") %>%
        conditional_unnest_wider("locationQuality") %>%
        conditional_unnest_wider("militants") %>%
        conditional_unnest_wider("killed") %>%
        rename(militants_killed_num = number, militants_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(militants_wounded_num = number, militants_wounded_prec = precision) %>%
        conditional_unnest_wider("captured") %>%
        rename(militants_captured_num = number, militants_captured_prec = precision) %>%
        conditional_unnest_wider("civilians") %>%
        conditional_unnest_wider("killed") %>%
        rename(civilians_killed_num = number, civilians_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(civilians_wounded_num = number, civilians_wounded_prec = precision) %>%
        conditional_unnest_wider("captured") %>%
        rename(civilians_captured_num = number, civilians_captured_prec = precision) %>%
        conditional_unnest_wider("securityForces") %>%
        conditional_unnest_wider("killed") %>%
        rename(sec_forces_killed_num = number, sec_forces_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(sec_forces_wounded_num = number, sec_forces_wounded_prec = precision) %>%
        conditional_unnest_wider("captured") %>%
        rename(sec_forces_captured_num = number, sec_forces_captured_prec = precision) %>%
        conditional_unnest_wider("civilianSecurityForces") %>%
        conditional_unnest_wider("killed") %>%
        rename(civ_sec_forces_killed_num = number, civ_sec_forces_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(civ_sec_forces_wounded_num = number, civ_sec_forces_wounded_prec = precision) %>%
        conditional_unnest_wider("captured") %>%
        rename(civ_sec_forces_captured_num = number, civ_sec_forces_captured_prec = precision) %>%
        conditional_unnest_wider("unidentified") %>%
        conditional_unnest_wider("killed") %>%
        rename(unidentified_killed_num = number, unidentified_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(unidentified_wounded_num = number, unidentified_wounded_prec = precision) %>%
        conditional_unnest_wider("captured") %>%
        rename(unidentified_captured_num = number, unidentified_captured_prec = precision) %>%
        conditional_unnest_wider("nonMilitantTotals") %>%
        rename(event_location_name = name1) %>%
        conditional_unnest_wider("killed") %>%
        rename(nonmilitant_killed_num = number, nonmilitant_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(nonmilitant_wounded_num = number, nonmilitant_wounded_prec = precision) %>%
        # conditional_unnest_wider("captured") %>%
        # rename(nonmilitant_captured_num = number, nonmilitant_captured_prec = precision) %>%
        conditional_unnest_wider("totals") %>%
        conditional_unnest_wider("killed") %>%
        rename(total_killed_num = number, total_killed_prec = precision) %>%
        conditional_unnest_wider("wounded") %>%
        rename(total_wounded_num = number, total_wounded_prec = precision) %>%
        # conditional_unnest_wider("captured") %>%
        # rename(total_captured_num = number, total_captured_prec = precision) %>%
        conditional_unnest_wider("role") %>%
        conditional_unnest_wider("name") %>%
        conditional_unnest_wider("family") %>%
        conditional_unnest_wider("nation") %>%
        conditional_unnest_wider("scope") %>%
        conditional_unnest_wider("region") %>% # works to here
        conditional_unnest_wider("orientation") %>%
        conditional_unnest_wider("type") %>%
        conditional_unnest_wider("ctOperation") %>%
        conditional_unnest_wider("nsag_attack") %>%
        conditional_unnest_wider("nsagStatement") %>%
        conditional_unnest_wider("ctJuditial") %>%
        conditional_unnest_wider("forces") %>%
        conditional_unnest_wider("accests") %>%
        conditional_unnest_wider("armsSeizedDestroyed") %>%
        rename(attack_environment = environment) %>%
        conditional_unnest_wider("nsagAttack") %>%
        conditional_unnest_wider("attackTargets") %>%
        conditional_unnest_wider("attack_targets") %>% # remove
        rename(attack_sector = sector, attack_subsector = subSector,
               attack_target_objects = targetObjects, attack_target_nationality = targetNationality) %>%
        conditional_unnest_wider("attack_sector") %>%
        conditional_unnest_wider("attack_subsector") %>%
        conditional_unnest_wider("attack_target_objects") %>%
        conditional_unnest_wider("attack_target_nationality") %>%
        conditional_unnest_wider("attackModes") %>%
        conditional_unnest_wider("attack_modes") %>%
        clean_names()
}





#' @export
