library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

helpeR::load_custom_functions()

scrape_dt <- lubridate::now(tzone = "Australia/Sydney")

#----------------------Detects if you are on pipeline or VM
local_testing <- TRUE


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- rsDriver(browser = c("firefox"),
                     port = 4573L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()

  driver$server$output()


}


#Registered Filter - 50% owned by First Nations this is registered suppliers

navigate_function <- function(url,
                              sleepmin = 5, sleepmax = 8) {
  remote_driver$navigate(url)
  sleep_time = runif(n = 1, min = sleepmin, max = sleepmax)
  Sys.sleep(sleep_time)
  page_source_data = remote_driver$getPageSource()

  return(page_source_data)
}

safely_navigate <- purrr::safely(navigate_function, otherwise = NULL)


scraped_data <- read_csv("data/supply_nation_scrape_links.csv") %>%
  mutate(ABN = "", ACN = "", bus_function = "", business_name = "")
scrape_links <- scraped_data %>% pull(links)

safely_scrape <- safely(get_scraped_data, otherwise = NULL)
#after navigating to the link extract data.

for (i in 1455:dim(scraped_data)[1]) {

  # remote_driver$navigate(scrape_links[i])

  page_source_data = safely_navigate(scrape_links[2100]) %>%
    pluck(1)

  if(!is.null(page_source_data)) {

    scraped_dat <- safely_scrape(page_source_data = page_source_data) %>%
      pluck(1)

    if(!is.null(scraped_dat)) {
      scraped_data[i,3] = scraped_dat$ABN[1]
      scraped_data[i,4] = scraped_dat$ACN[1]
      scraped_data[i,5] = scraped_dat$bus_function[1]
      scraped_data[i,6] = scraped_dat$business_name[1]
    }


  }
}

write.csv(scraped_data, file = glue::glue("data/scraped_supply_nation_{lubridate::today()}.csv"), row.names = F)


try(remote_driver$quit())
