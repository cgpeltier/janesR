#' @title get_janes_inventories
#' @description Pulls Janes equipment inventories.
#'
#' @param country Country filter
#' @param operator_force Operator force
#'
#' @return Janes equipment inventories.
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
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
#' @export



get_janes_inventories <- function(country = NULL, operator_force = NULL){

    page_range <- get_page_range(country = country, operator_force = operator_force,
                                 endpoint = "inventories")

    inventories <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                    operator_force = operator_force,
                                                    endpoint = "inventories")) %>%
        bind_rows()

    inventories_data <- map(inventories$url, get_janes_data) %>%
        bind_rows()

    inventories_data
    #
    # %>%
    #     tibble() %>%
    #     conditional_unnest_wider(".") %>%
    #     conditional_unnest_wider(".") %>%
    #     conditional_unnest_wider("inventories") %>%
    #     unnest_all("inventories") %>%
    #     unnest_all("inventories") %>%
    #     unnest_all("inventories") %>%
    #     unnest_all("inventories") %>%
    #     rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
    #     rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))

}





