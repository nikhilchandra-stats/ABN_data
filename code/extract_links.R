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
                     port = 4570L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()

  driver$server$output()


}


#Registered Filter - 50% owned by First Nations this is registered suppliers NSW

link_values <- c()
count = 1

number_of_pages <- ceiling(1453/9)

for (j in 1:number_of_pages) {

all_xpaths <-
    create_xpath_list(
      removal_string = "xxxxxx",
      max_loops = 9,
      increment = 3,
      base_xpath = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[2]/div[xxxxxx]/div/div[2]/p[1]/a"
    )

  for (i in 1:length(all_xpaths)) {

    link_values[count] <-
      find_links(
        remote_driver = remote_driver,
        url = "https://ibd.supplynation.org.au/public/s/search-results#loc=NSW,%20Australia&search=",
        xpath_var = all_xpaths[i]
      )

    count = count + 1

  }

  click_button(
    remote_driver,
    xpath_var = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[3]/p/div/button[2]"
  )

  sleep_time <- runif(n = 1, min = 1,max = 1.5)

  Sys.sleep(time = sleep_time)

}

#Registered Filter - 50% owned by First Nations this is registered suppliers VIC

link_values_vic <- c()
count = 1

number_of_pages <- ceiling(408/9)

for (j in 1:number_of_pages) {

  all_xpaths <-
    create_xpath_list(
      removal_string = "xxxxxx",
      max_loops = 9,
      increment = 3,
      base_xpath = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[2]/div[xxxxxx]/div/div[2]/p[1]/a"
    )

  for (i in 1:length(all_xpaths)) {

    link_values_vic[count] <-
      find_links(
        remote_driver = remote_driver,
        url = "https://ibd.supplynation.org.au/public/s/search-results#loc=VIC,%20Australia&search=",
        xpath_var = all_xpaths[i]
      )

    count = count + 1

  }

  click_button(
    remote_driver,
    xpath_var = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[3]/p/div/button[2]"
  )

  sleep_time <- runif(n = 1, min = 1,max = 1.5)

  Sys.sleep(time = sleep_time)

}


#Registered Filter - 50% owned by First Nations this is registered suppliers QLD

link_values_qld <- c()
count = 1

number_of_pages <- ceiling(1033/9)

for (j in 1:number_of_pages) {

  all_xpaths <-
    create_xpath_list(
      removal_string = "xxxxxx",
      max_loops = 9,
      increment = 3,
      base_xpath = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[2]/div[xxxxxx]/div/div[2]/p[1]/a"
    )

  for (i in 1:length(all_xpaths)) {

    link_values_qld[count] <-
      find_links(
        remote_driver = remote_driver,
        url = "https://ibd.supplynation.org.au/public/s/search-results#loc=QLD,%20Australia&search=",
        xpath_var = all_xpaths[i]
      )

    count = count + 1

  }

  click_button(
    remote_driver,
    xpath_var = "/html/body/div[3]/div/div[4]/div/div/div[2]/div/div/div/div[3]/div/div/section/div[3]/p/div/button[2]"
  )

  sleep_time <- runif(n = 1, min = 1,max = 1.5)

  Sys.sleep(time = sleep_time)

}


link_tibble <-
  tibble(
    links = link_values,
    state = "NSW"
  ) %>%
  bind_rows(
    tibble(
      links = link_values_qld,
      state = "QLD"
    )
  ) %>%
  bind_rows(
    tibble(
      links = link_values_vic,
      state = "VIC"
    )
  )

write.csv(link_tibble, file = "data/supply_nation_scrape_links.csv", row.names = F)
