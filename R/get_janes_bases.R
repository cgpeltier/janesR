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

    bases_data %>%
        tibble() %>%
        conditional_unnest_wider(".") %>%
        conditional_unnest_wider(".") %>%
        select(-any_of("...1")) %>%
        conditional_unnest_wider("installation") %>%
        unnest_all("base") %>%
        unnest_all("base") %>%
        unnest_all("base") %>%
        unnest_all("base") %>%
        rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
        rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))


}


