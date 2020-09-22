#' @title get_janes_sam
#' @description Pulls Janes SAM sites data
#'
#' @param country Country in which SAM site is located
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
#' @importFrom dplyr mutate
#' @export



get_janes_sam <- function(country = NULL){
    page_range <- get_page_range(country = country, endpoint = "samsites")
    samsites <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "samsites")) %>%
        bind_rows()
    samsites_data <- map(samsites$url, get_janes_data)

    samsites_data %>%
        tibble() %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider("installation") %>%
        conditional_unnest_wider("system") %>%
        conditional_unnest_wider("equipmentType") %>%
        conditional_unnest_wider("associatedEquipments") %>%
        rename(primary_equipment_id = equipmentId, primary_equipment_name = equipmentName,
               primary_family_root_id = familyRootId, primary_equipment_type = equipmentType1) %>%
        conditional_unnest_wider("associatedEquipment") %>%
        conditional_unnest_wider("equipmentId") %>%
        conditional_unnest_wider("familyRootId") %>%
        conditional_unnest_wider("equipmentName") %>%
        conditional_unnest_wider("numberOfItems") %>%
        conditional_unnest_wider("rangeInMeters") %>%
        conditional_unnest_wider("associatedEquipments") %>%
        conditional_unnest_wider("equipmentType") %>%
        conditional_unnest_wider("location") %>%
        conditional_unnest_wider("latitude") %>%
        conditional_unnest_wider("longitude") %>%
        janitor::clean_names()


}


#' @export
