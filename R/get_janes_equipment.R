#' @title get_janes_equipment
#' @description Pulls Janes equipment.
#'
#' @param country Country filter for equipment
#' @param query Query filter for equipment
#' @param type Filter for type of equipment (i.e. "Platforms")
#' @param environment Filter for environment of equipment (i.e. "Air")
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
#' @export



get_janes_equipment <- function(country = NULL, query = NULL,
                                environment = NULL, type = NULL){

    page_range <- get_page_range(country = country, endpoint = "equipment",
                                 query = str_replace_all(query, " ", "%20"),
                                 environment = environment,
                                 type = type)

    equipment <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                  endpoint = "equipment",
                                                  query = str_replace_all(query, " ", "%20"),
                                                  environment = environment,
                                                  type = type)) %>%
        bind_rows()



    equipment_data <- map(equipment$url, get_janes_data2)

    equipment_data2 <- equipment_data %>%
        tibble() %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider("equipment") %>%
        conditional_unnest_wider("manufacturerCountries") %>%
        conditional_unnest_wider("manufacturerCountry") %>%
        conditional_unnest_wider("manufacturerCountryIso") %>%
        conditional_unnest_wider("manufacturerCountryName")


    if("landPlatformProtection" %in% colnames(equipment_data2)){


        equipment_data_weird <- equipment_data2 %>%
            filter(!is.na(landPlatformProtection), !is.na(path)) %>%
            # select(1:18) %>%
            conditional_unnest_wider("manufacturers") %>%
            conditional_unnest_wider("manufacturer") %>%
            conditional_unnest_wider("manufacturerName") %>%
            conditional_unnest_wider("overallFamily") %>%
            conditional_unnest_wider("documents") %>%
            conditional_unnest_wider("document") %>%
            conditional_unnest_wider("documentId") %>%
            conditional_unnest_wider("documentTitle") %>%
            conditional_unnest_wider("manufacturerId") %>%
            conditional_unnest_wider("manufacturerTitle") %>%
            conditional_unnest_wider("synonyms") %>%
            conditional_unnest_wider("synonym") %>%
            select(!where(is.list)) %>%
            janitor::clean_names()


        equipment_data2 %>% # delete the created obj!
            filter(is.na(landPlatformProtection), is.na(path))  %>%
            janitor::remove_empty() %>%
            conditional_unnest_wider("types") %>%
            conditional_unnest_wider("type") %>%
            conditional_unnest_wider("operatorCountries") %>%
            conditional_unnest_wider("operatorCountry") %>%
            conditional_unnest_wider("operatorCountryName") %>%
            conditional_unnest_wider("operatorCountryIso") %>%
            conditional_unnest_wider("roles") %>%
            conditional_unnest_wider("role") %>%
            conditional_unnest_wider("manufacturers") %>%
            conditional_unnest_wider("manufacturer") %>%
            conditional_unnest_wider("manufacturerName") %>%
            conditional_unnest_wider("overallFamily") %>%
            conditional_unnest_wider("family") %>%
            conditional_unnest_wider("primayParent") %>%
            conditional_unnest_wider("documents") %>%
            conditional_unnest_wider("document") %>%
            conditional_unnest_wider("documentId") %>%
            conditional_unnest_wider("documentTitle") %>%
            conditional_unnest_wider("manufacturerId") %>%
            conditional_unnest_wider("manufacturerTitle") %>%
            conditional_unnest_wider("synonyms") %>%
            conditional_unnest_wider("synonym") %>%
            conditional_unnest_wider("users") %>%
            conditional_unnest_wider("user") %>%
            conditional_unnest_wider("environments") %>%
            conditional_unnest_wider("environment") %>%
            conditional_unnest_wider("mobility") %>%
            conditional_unnest_wider("operations") %>%
            conditional_unnest_wider("operation") %>%
            conditional_unnest_wider("mobilities") %>%
            conditional_unnest_wider("mobility") %>%
            select(!where(is.list)) %>%
            janitor::clean_names() %>%
            bind_rows(equipment_data_weird)

    } else {
        equipment_data2 %>%
            conditional_unnest_wider("types") %>%
            conditional_unnest_wider("type") %>%
            conditional_unnest_wider("operatorCountries") %>%
            conditional_unnest_wider("operatorCountry") %>%
            conditional_unnest_wider("operatorCountryName") %>%
            conditional_unnest_wider("operatorCountryIso") %>%
            conditional_unnest_wider("roles") %>%
            conditional_unnest_wider("role") %>%
            conditional_unnest_wider("manufacturers") %>%
            conditional_unnest_wider("manufacturer") %>%
            conditional_unnest_wider("manufacturerName") %>%
            conditional_unnest_wider("overallFamily") %>%
            conditional_unnest_wider("family") %>%
            conditional_unnest_wider("primayParent") %>%
            conditional_unnest_wider("documents") %>%
            conditional_unnest_wider("document") %>%
            conditional_unnest_wider("documentId") %>%
            conditional_unnest_wider("documentTitle") %>%
            conditional_unnest_wider("manufacturerId") %>%
            conditional_unnest_wider("manufacturerTitle") %>%
            conditional_unnest_wider("synonyms") %>%
            conditional_unnest_wider("synonym") %>%
            conditional_unnest_wider("users") %>%
            conditional_unnest_wider("user") %>%
            conditional_unnest_wider("environments") %>%
            conditional_unnest_wider("environment") %>%
            conditional_unnest_wider("mobility") %>%
            conditional_unnest_wider("operations") %>%
            conditional_unnest_wider("operation") %>%
            conditional_unnest_wider("mobilities") %>%
            conditional_unnest_wider("mobility") %>%
            janitor::clean_names()




    }





}



#' @export
