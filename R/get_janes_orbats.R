#' @title get_janes_orbats
#' @description Pulls Janes order of battle (ORBAT) data.
#'
#' @param country Country in which base is located
#' @param branch Military branch (Air Force, Army, Navy)
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



get_janes_orbats <- function(country = NULL, branch = NULL){
  page_range <- get_page_range(country = country, endpoint = "orbats",
                               branch = str_replace_all(branch, " ", "%20"))
  orbats <- map(page_range, ~ get_janes_info(x = .x, country = country,
                                                endpoint = "orbats",
                                                branch = str_replace_all(branch, " ", "%20"))) %>%
      bind_rows()
  orbats_data <- map(orbats$url, get_janes_data)

  orbats_data %>%
      tibble() %>%
      conditional_unnest_wider(".") %>%
      conditional_unnest_wider(".") %>%
      conditional_unnest_wider(".") %>%
      unnest_all("equipment") %>%
      unnest_all("equipment") %>%
      unnest_all("equipment") %>%
      unnest_all("equipment") %>%
      rename_with(~ str_remove(., "^[^_]+_[^_]+_")) %>%
      rename_with(~ str_remove(., "(?<=[a-z])_(?=\\d+)"))
}


