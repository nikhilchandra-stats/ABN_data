create_xpath_list <- function(
    removal_string = "xxxxxx",
    max_loops = 9,
    increment = 3,
    base_xpath = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[2]/div[xxxxxx]/div/div[2]/p[1]/a") {

  all_xpaths <- c()
  x_value = 1

  for (i in (1 - 1):(max_loops - 1) ) {

    all_xpaths[i + 1] <- stringr::str_replace(base_xpath,
                                              pattern = removal_string,
                                              replacement = as.character(x_value) )

    x_value <- x_value + increment

  }

  return(all_xpaths)

}


find_links <- function(remote_driver,
                       url = "https://ibd.supplynation.org.au/public/s/search-results#loc=NSW,%20Australia&search=",
                       xpath_var = first_xpath) {

  remote_driver$navigate(url)

  page_source = remote_driver$getPageSource()

  html_read_in <-  page_source %>%
    pluck(1) %>%
    xml2::read_html()

  extract_first_value <-
    html_read_in %>%
    rvest::html_element(xpath = xpath_var)

  extract_link <- extract_first_value %>%
    rvest::html_attr("href")

  base_url <- "https://ibd.supplynation.org.au"
  business_extension <- extract_link

  navigate_to_this_url <- glue::glue("{base_url}{business_extension}")

  return(navigate_to_this_url)

}


click_button <-
  function(
    remote_driver,
    xpath_var = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[3]/p/div/button[2]") {

    click_location <- remote_driver$findElement(using = "xpath",
                              value = xpath_var)

    click_location$clickElement()

}


get_scraped_data <- function(page_source_data = page_source_data) {

  xpath_var <- "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/form/section[2]/div[1]/div[3]/div[2]/div/form/div[3]"
  element_text_para <- page_source_data %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = xpath_var)

  ABN <- element_text_para %>%
    rvest::html_text() %>%
    str_extract("ABN: [0-9]+")

  ACN <- element_text_para %>%
    rvest::html_text() %>%
    str_extract("ACN: [0-9]+")

  returned <- tibble(ABN = ABN, ACN = ACN)

  business_function_xpath <- "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/form/section[2]/div[1]/div[3]/div[1]/div/span/form/div/ul"

  element_text_func <- page_source_data %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = business_function_xpath) %>%
    rvest::html_text2() %>%
    stringr::str_split(pattern = "\n", simplify = T) %>%
    paste(collapse = ";")

  returned <- returned %>%
    mutate(
      bus_function = element_text_func
    )

  bus_name_xpath <- "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/form/section[2]/div[1]/div[1]/div/div"

  bus_name <- page_source_data %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(xpath = bus_name_xpath) %>%
    rvest::html_text2()

  returned <- returned %>%
    mutate(business_name = bus_name)

  return(returned)

}
