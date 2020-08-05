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
        rename(companies = ".") %>%
        unnest_wider(companies) %>%
        rename(companies = ".") %>%
        unnest_wider(companies) %>%
        unnest_wider(organisation) %>%
        unnest_wider(equipmentFamilies) %>%
        select(-any_of("...1")) %>%
        unnest_wider(equipmentFamily) %>%
        rename_with(.fn = ~ gsub("...", "equipment_family", .x, fixed = TRUE),
                  .cols = starts_with("...")) %>%
        unite(col = "equipment_families",
              starts_with("equipment_family"),
              sep = ", ", na.rm = TRUE) %>%
        unnest_wider(contactDetails) %>%
        select(-any_of("country")) %>%
        unnest_wider(address) %>%
        select(-any_of("...1")) %>%
        unnest_wider(synonyms) %>%
        select(-any_of("...1")) %>%
        unnest_wider(synonym) %>%
        rename_with(.fn = ~ gsub("...", "synonym", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(telephones) %>%
        select(-any_of("...1")) %>%
        unnest_wider(telephone) %>%
        select(-any_of("...1")) %>%
        rename(telephone_number = number) %>%
        unnest_wider(telephone_number) %>%
        rename_with(.fn = ~ gsub("...", "telephone_number", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        select(-any_of("qualifier")) %>%
        unnest_wider(faxes) %>%
        select(-any_of("...1")) %>%
        unnest_wider(fax) %>%
        rename(fax_number = number) %>%
        select(-any_of("...1")) %>%
        select(-any_of("qualifier")) %>%
        unnest_wider(fax_number) %>%
        select(-any_of("qualifier")) %>%
        rename_with(.fn = ~ gsub("...", "fax_number", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(documents) %>%
        select(-any_of("...1")) %>%
        unnest_wider(document) %>%
        select(-any_of("...1")) %>%
        unnest_wider(emails) %>%
        select(-any_of("...1")) %>%
        unnest_wider(email) %>%
        rename_with(.fn = ~ gsub("...", "email", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        select(-any_of("qualifier")) %>%
        select(-any_of("socialMedias")) %>%
        unnest_wider(jointVentureOwners) %>%
        select(-any_of("...1")) %>%
        unnest_wider(owner) %>%
        select(-any_of("...1")) %>%
        unnest_wider(name) %>%
        rename_with(.fn = ~ gsub("...", "owner_name", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        unnest_wider(ownershipPercentage) %>%
        rename_with(.fn = ~ gsub("...", "ownership_percentage", .x, fixed = TRUE),
                    .cols = starts_with("...")) %>%
        clean_names()

}


#' @export
