#' @title get_janes_country_risks
#' @description Pulls Janes country risk data
#'
#' @param country Country in which base is located
#'
#' @return Janes country risk data
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
#' @importFrom tibble enframe
#' @importFrom tidyr unnest
#' @importFrom dplyr if_else
#' @importFrom tidyr fill
#' @importFrom tidyr pivot_wider
#' @export



get_janes_country_risks <- function(country = NULL){
    page_range <- get_page_range(country = country, endpoint = "countryrisks")
    cr <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                               endpoint = "countryrisks")) %>%
        bind_rows()

    cr_data <- map(cr$id, ~ GET(url = paste0("https://developer.janes.com/api/v1/data/countryrisks/",
                                                        .), add_headers(Authorization = janes_key)) %>%
                     content(as = "text") %>%
                     fromJSON() %>%
                     tibble())

    cr_data %>%
        tibble() %>%
        rename(cr = ".") %>%
        unnest_wider(cr) %>%
        rename(cr = ".") %>%
        unnest_wider(cr) %>%
        unnest_wider(country) %>%
        clean_names()
}


#' @export
