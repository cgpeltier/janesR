#' @title get_janes_markets_forecast
#' @description Pulls Janes Markets Forecast data.
#'
#' @param country Country in which base is located
#' @param query Search term
#' @param market JMF market to query
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



get_janes_markets_forecast <- function(end_user_country = NULL, query = NULL, market = NULL){

  page_range <- get_page_range(end_user_country = end_user_country, endpoint = "marketforecasts",
                               query = str_replace_all(query, " ", "%20"),
                               market = market)

  jmf <- map(page_range, ~ get_janes_info(x = .x, end_user_country = end_user_country,
                                          endpoint = "marketforecasts",
                                          query = str_replace_all(query, " ", "%20"),
                                          market = market)) %>%
    bind_rows()

  jmf_data <- map(jmf$url, get_janes_data)

  jmf_data %>%
      tibble() %>%
      conditional_unnest_wider(".")
#
#
#   test_jmf2 <- test_jmf %>%
#     tibble() %>%
#     conditional_unnest_wider(".") %>%
#     conditional_unnest_wider(".") %>%
#     conditional_unnest_wider("platform") %>%
#     conditional_unnest_wider("supplier") %>%
#     conditional_unnest_wider("endUser") %>%
#     conditional_unnest_wider("lifeCycle") %>%
#     conditional_unnest_wider("marketsSpecificAttributes)
#     conditional_unnest_wider2("finalAssembly") %>%
#     conditional_unnest_wider2("country") %>%
#     conditional_unnest_wider("slsInformation") %>%
#     conditional_unnest_wider("documentation") %>%
#     conditional_unnest_wider("finalAssembly_country") %>%
#     conditional_unnest_wider2("subsystemSuppliers") %>%
#     conditional_unnest_wider2("name") %>%
#     conditional_unnest_wider2("subSystemType") %>%
#     conditional_unnest_wider2("percentageOfForecast") %>%
#     conditional_unnest_wider2("slsForecast") %>%
#     conditional_unnest_wider2("productionForecast") %>%
#     conditional_unnest_wider2("rAndDForecast") %>%
#     conditional_unnest_wider2("totalProgramForecast") %>%
#     conditional_unnest_wider2("unitsForecast") %>%
#     conditional_unnest_wider2("slsForecast") %>%
#     conditional_unnest_wider2("productionForecast") %>%
#     conditional_unnest_wider("subsystemSuppliers_name") %>%
#     conditional_unnest_wider("subsystemSuppliers_subSystemType") %>%
#     conditional_unnest_wider("subsystemSuppliers_percentageOfForecast") %>%
#     conditional_unnest_wider("subsystemSuppliers") %>%
#     conditional_unnest_wider2("subsystemSuppliers_slsForecast") %>%
#     conditional_unnest_wider2("subsystemSuppliers_productionForecast") %>%
#     conditional_unnest_wider2("subsystemSuppliers_rAndDForecast") %>%
#     conditional_unnest_wider2("subsystemSuppliers_totalProgramForecast") %>%
#     conditional_unnest_wider2("subsystemSuppliers_unitsForecast") %>%
#     conditional_unnest_wider2("slsForecast_values") %>%
#     conditional_unnest_wider2("productionForecast_values") %>%
#     conditional_unnest_wider2("rAndDForecast_values") %>%
#     conditional_unnest_wider2("totalProgramForecast_values") %>%
#     conditional_unnest_wider2("unitsForecast_values") %>%
#     conditional_unnest_wider2("subsystemSuppliers_slsForecast_currencyIso") %>%
#     conditional_unnest_wider2("subsystemSuppliers_slsForecast_values") %>%
#     conditional_unnest_wider2("subsystemSuppliers_productionForecast_currencyIso") %>%
#     conditional_unnest_wider2("subsystemSuppliers_productionForecast_values") %>%
#     conditional_unnest_wider2("subsystemSuppliers_rAndDForecast_currencyIso") %>%
#     conditional_unnest_wider2("subsystemSusubsystemSuppliers_rAndDForecast_valuesppliers") %>%
#     conditional_unnest_wider2("subsystemSuppliers_totalProgramForecast_currencyIso") %>%
#     conditional_unnest_wider2("subsystemSuppliers_totalProgramForecast_values") %>%
#     conditional_unnest_wider2("subsystemSuppliers_unitsForecast_currencyIso") %>%
#     conditional_unnest_wider2("subsystemSuppliers_unitsForecast_values") %>%

}


#' @export
