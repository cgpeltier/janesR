#' @title get_janes_programs
#' @description Pulls Janes defense programs data
#'
#' @param operator_country Country to pull programs from
#' @param histories Pull only program revision history data. Logical.
#'
#' @return Janes defense programs data.
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
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom dplyr if_else
#' @importFrom tidyr fill
#' @importFrom tidyr pivot_wider
#' @export



get_janes_programs <- function(operator_country = NULL, histories = FALSE){
    page_range <- get_page_range_programs(operator_country = operator_country)

    programs <- map(page_range, ~ get_janes_info_programs(x = .x, operator_country = operator_country)) %>%
        bind_rows()

    programs_data <- map(programs$url, get_janes_data)



    program_data2 <- programs_data %>%
        tibble() %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider2("programme") %>%
        #rename(program_id = id, program_country = operatingCountry) %>%
        conditional_unnest_wider2("platform") %>%
        #rename(platform_name = name, program_updated_date = updatedDate) %>%
        conditional_unnest_wider2("operator") %>%
        #select(-id, -updatedDate, -operatingCountry) %>%
        conditional_unnest_wider2("programmeAnalysis") %>%
        conditional_unnest_wider2("platformVersions") %>%
        #unnest_longer(revisionHistories) %>%
        conditional_unnest_wider2("platformVersions_platformVersion") %>%
        conditional_unnest_wider2("platformVersions_inventory") %>%
        conditional_unnest_wider2("platformVersions_platformVersionManufacturer") %>%
        conditional_unnest_wider2("subsystems") %>%
        conditional_unnest_wider2("configurations") %>%
        conditional_unnest_wider2("subsystems_system") %>%
        conditional_unnest_wider2("subsystems_subsystem") %>%
        conditional_unnest_wider2("subsystems_programmeDescription") %>%
        conditional_unnest_wider2("subsystems_programmeStatus") %>%
        conditional_unnest_wider2("subsystems_upgrader") %>%
        conditional_unnest_wider2("subsystems_manufacturer") %>%
        conditional_unnest_wider2("configurations_programmeName") %>%
        conditional_unnest_wider2("configurations_configuration") %>%
        conditional_unnest_wider2("configurations_vehicleName") %>%
        conditional_unnest_wider2("configurations_platformType") %>%
        conditional_unnest_wider2("configurations_platformRole") %>%
        conditional_unnest_wider2("configurations_platformId") %>%
        conditional_unnest_wider2("configurations_updatedDate")


    if(histories == TRUE){
      programs_data %>%
          tibble() %>%
          conditional_unnest_wider(".") %>%
          conditional_unnest_wider(".") %>%
          conditional_unnest_wider2("programme") %>%
          select(programme_id, revisionHistories) %>%
          unnest_longer(revisionHistories) %>%
          janitor::clean_names()


    } else {

        program_data2 %>%
            select(-where(is.list)) %>%
            janitor::clean_names()

      }
}


#' @export
