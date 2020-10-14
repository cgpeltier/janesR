#' @title get_janes_companies
#' @description Pulls Janes company data
#'
#' @param country Country in which base is located
#' @param query Search term for companies (i.e. company name)
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



get_janes_companies <- function(country = NULL, query = NULL){
    page_range <- get_page_range(country = country, endpoint = "companies",
                                 query = str_replace_all(query, " ", "%20"))
    companies <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "companies",
                                               query = str_replace_all(query, " ", "%20"))) %>%
        bind_rows()

    companies_data <- map(companies$url, get_janes_data)

    companies_data %>%
        tibble() %>%
        unnest_wider(".") %>%
        unnest_wider(".")  %>%
        conditional_unnest_wider("organisation") %>%
        conditional_unnest_wider("equipmentFamilies") %>%
        conditional_unnest_wider("equipmentFamily") %>%
        unite(col = "all_equipment_families",
              starts_with("equipmentFamily"),
              sep = ", ", na.rm = TRUE)  %>%
        rename(company_country = country) %>%
        conditional_unnest_wider("contactDetails") %>%
        conditional_unnest_wider("address") %>%
        conditional_unnest_wider("synonyms") %>%
        conditional_unnest_wider("synonym") %>%
        conditional_unnest_wider("telephones") %>%
        conditional_unnest_wider2("telephone") %>%
        conditional_unnest_wider("telephone_number") %>%
        select(-any_of("telephone_qualifier")) %>%
        conditional_unnest_wider("faxes") %>%
        conditional_unnest_wider2("fax") %>%
        select(-any_of("fax_qualifier")) %>%
        conditional_unnest_wider("fax_number") %>%
        conditional_unnest_wider("documents") %>%
        conditional_unnest_wider("document") %>%
        conditional_unnest_wider("emails") %>%
        conditional_unnest_wider("email") %>%
        select(-any_of("email_qualifier")) %>%
        conditional_unnest_wider("socialMedias") %>%
        conditional_unnest_wider2("socialMedia") %>%
        conditional_unnest_wider("socialMedia_media") %>%
        conditional_unnest_wider("socialMedia_type") %>%
        conditional_unnest_wider("jointVentureOwners") %>%
        conditional_unnest_wider("owner") %>%
        conditional_unnest_wider("name") %>%
        conditional_unnest_wider("ownershipPercentage") %>%
        conditional_unnest_wider("documentId") %>%
        conditional_unnest_wider("documentTitle") %>%
        clean_names()

}


#' @export
