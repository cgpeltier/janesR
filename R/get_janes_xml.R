#' @title get_janes_xml
#' @description Pulls Janes data as a xml file for all endpoints.
#'
#' @param country Country filter for news
#' @param branch Military branch
#' @param type Depends on endpoint
#' @param endpoint One of 6 options currently
#' @param query Search term
#' @param environment Of search, i.e. "Air"
#' @param operator_force Operator force
#'
#' @return Janes data in xml format.
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
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
#' @importFrom dplyr starts_with
#' @importFrom dplyr any_of
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 write_xml
#' @importFrom xml2 as_xml_document
#' @importFrom purrr possibly
#' @importFrom purrr safely
#' @importFrom dplyr filter
#' @importFrom magick image_read
#' @importFrom magick image_write


# uses https://stackoverflow.com/questions/56892518/ignore-error-when-importing-json-files-in-r

get_janes_xml <- function(country = NULL, branch = NULL, type = NULL,
                          operator_force = NULL, query = NULL, environment = NULL,
                          endpoint = "references", jdet_only = FALSE, docs = TRUE,
                          pics = FALSE, sat = FALSE){

    page_range <- get_page_range(country = country, endpoint = endpoint, branch = branch,
                                 type = type, operator_force = operator_force,
                                 environment = environment, query = query)

    temp <- map(page_range, ~ get_janes_info(x = .x, country = country, branch = branch,
                                             type = type, operator_force = operator_force,
                                             environment = environment, endpoint = endpoint,
                                             query = query)) %>%
        bind_rows()

    if(jdet_only == TRUE){
        pubs <- c("JAE_", "JAWA", "jau_", "juav", "JC4IA", "JC4IJ", "JC4IL", "JC4IM", "JECE",
                  "jfs", "JFA_", "jafv", "jaad", "JLWU", "JLSU", "jmp_", "JMEG", "jpse", "JSTS",
                  "JSD_", "jumv", "jnw_", "JALW", "JAH_", "JIW_", "JSWS", "jae_", "jawa", "JAU_",
                  "JUAV", "jc4ia", "jc4ij", "jc4il", "jc4im", "jece", "JFS", "jfa_", "JAFV",
                  "JAAD", "jlwu", "jlsu", "JMP_", "jmeg", "JPSE", "jsts", "jsd_", "JUMV",
                  "JNW_", "jalw", "jah_", "jiw_", "jsws")

        temp <- temp %>%
            dplyr::filter(stringr::str_detect(id, paste(pubs, collapse = "|")))
    }

    if(sat == TRUE){
        temp <- temp %>%
            dplyr::filter(stringr::str_detect(id, "jsia|JSIA"))
    }

    if(docs == TRUE){

        map(temp$url, purrr::safely(~ get_janes_data_xml(.x) %>%
            xml2::as_xml_document() %>%
            xml2::write_xml(., file = paste0("C:/Users/chad.peltier/OneDrive - IHS Markit/Data and Integration/data_for_delivery/",
                                             str_extract(.x, "(?<=\\/)[^\\/]+$"), ".xml"), encoding = "UTF-8")))
    }

    if(pics == TRUE){

        pics_df <- map(paste0(temp$url, "/images"), ~ get_janes_data_xml(.x) %>%
              bind_rows()) %>%
              bind_rows()

        map(pics_df$Url, purrr::safely(~ GET(url = .x,
                                             add_headers(Authorization = Sys.getenv("JANES_KEY"))) %>%
                                         content() %>%
                                         magick::image_read() %>%
                                         magick::image_write(path = paste0(getwd(), "/reference_", str_extract(.x, "(?<=\\/)[^\\/]+$")))))



    }
}


#' @export

