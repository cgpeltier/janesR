#' @title unnest_all2
#' @description Helper function to unnest all list columns. If no list_cols, cleans names
#'
#' @param data Country filter for equipment
#' @param variable Query filter for equipment
#'
#' @return Unnests_wider if variable is in dataset, doesn't if not.
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr rename_with
#' @importFrom janitor clean_names
#' @importFrom stringr str_remove
#'



unnest_all2 <- function(data){

  list_cols <- names(select(data, where(is.list)))

  data_non_list <- data %>%
    select(!where(is.list))


  if(length(list_cols) != 0){

    map_dfc(list_cols, ~
              data %>%
              select(.x) %>%
              unnest_wider(c(!!.x), names_sep= "_", names_repair = 'unique')) %>%
      bind_cols(data_non_list, .)

  } else {

    data

  }

}


#' @export


