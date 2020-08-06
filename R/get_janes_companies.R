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
        unnest_wider(organisation, names_repair = ~gsub('...', 'organization', ., fixed = TRUE)) %>%
        unnest_wider(equipmentFamilies, names_repair = ~gsub('...', 'equipment_families', ., fixed = TRUE)) %>%
        unnest_wider(equipmentFamily, names_repair = ~gsub('...', 'equipment_family', ., fixed = TRUE)) %>%
        unite(col = "all_equipment_families",
              starts_with("equipment_family"),
              sep = ", ", na.rm = TRUE) %>%
        unnest_wider(contactDetails, names_repair = ~gsub('...', 'contact_details', ., fixed = TRUE)) %>%
        select(-any_of("country")) %>%
        unnest_wider(address, names_repair = ~gsub('...', 'address', ., fixed = TRUE)) %>%
        unnest_wider(synonyms, names_repair = ~gsub('...', 'synonyms', ., fixed = TRUE)) %>%
        unnest_wider(synonym, names_repair = ~gsub('...', 'synonym', ., fixed = TRUE)) %>%
        unnest_wider(telephones, names_repair = ~gsub('...', 'telephones', ., fixed = TRUE)) %>%
        unnest_wider(telephone, names_repair = ~gsub('...', 'telephone', ., fixed = TRUE)) %>%
        #rename(telephone_number = number) %>%
        unnest_wider(number, names_repair = ~gsub('...', 'telephone_number', ., fixed = TRUE)) %>%
        select(-any_of("qualifier")) %>%
        unnest_wider(faxes, names_repair = ~gsub('...', 'faxes', ., fixed = TRUE)) %>%
        unnest_wider(fax, names_repair = ~gsub('...', 'fax', ., fixed = TRUE)) %>%
        #rename(fax_number = number) %>%
        select(-any_of("qualifier")) %>%
        unnest_wider(number, names_repair = ~gsub('...', 'fax_number', ., fixed = TRUE)) %>%
        select(-any_of("qualifier")) %>%
        unnest_wider(documents, names_repair = ~gsub('...', 'documents', ., fixed = TRUE)) %>%
        unnest_wider(document, names_repair = ~gsub('...', 'document', ., fixed = TRUE)) %>%
        unnest_wider(emails, names_repair = ~gsub('...', 'emails', ., fixed = TRUE)) %>%
        unnest_wider(email,names_repair = ~gsub('...', 'email', ., fixed = TRUE)) %>%
        select(-any_of("qualifier")) %>%
        select(-any_of("socialMedias")) %>%
        unnest_wider(jointVentureOwners, names_repair = ~gsub('...', 'joint_venture_owners', ., fixed = TRUE)) %>%
        unnest_wider(owner, names_repair = ~gsub('...', 'owner', ., fixed = TRUE)) %>%
        unnest_wider(name, names_repair = ~gsub('...', 'owner_name', ., fixed = TRUE)) %>%
        unnest_wider(ownershipPercentage, names_repair = ~gsub('...', 'ownership_perc', ., fixed = TRUE)) %>%
        clean_names() %>%
        remove_empty()

}


#' @export
