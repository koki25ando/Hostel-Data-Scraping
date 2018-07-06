# Preparation
library(tidyverse)
library(rvest)

jp.hostel.link <- "https://www.hostelworld.com/hostels/Japan"

jp.main.page <- 
  read_html(jp.hostel.link)

main.city.name <- 
  jp.main.page %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_text()

main.city.link <- 
  jp.main.page %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_nodes("a") %>% 
  html_attr("href")

hostel_num <- 
  jp.main.page %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("span.propnumber") %>% 
  html_text()

main.city <- data.frame(main.city.name, main.city.link, hostel_num)

# write.csv(main.city, file = "Main_city.csv")


# All the main information

link_get <- function (url) {
  as.character(url) %>% 
  read_html() %>% 
  html_nodes("ul.pagination") %>% 
  html_nodes("li.pagination-number") %>% 
  html_nodes("a") %>% 
  html_attr("href")
}

link.list.for.each.city <- apply(data.frame(main.city$main.city.link), 1, link_get)
link.list.for.each.city <- 
  link.list.for.each.city %>% 
  unlist()
link.list <- data.frame(link.list.for.each.city)
link.list$City <- c("Tokyo", "Tokyo", "Tokyo", "Tokyo", "Tokyo", 
                    "Kyoto", "Kyoto", "Kyoto", "Kyoto", 
                    "Osaka", "Osaka", "Osaka", "Osaka", "Osaka",
                    "Hiroshima", "Fukuoka")

names(link.list) <- c("Link", "City")

# Scraping Function

hostel_scraping <- function (url) {

page <- read_html(as.character(url))  

hostel.name <- 
  page %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.resultcontainer") %>% 
  html_nodes("div#fabResultsContainer") %>% 
  html_nodes("div.fabresult") %>% 
  html_nodes("div.resultheader") %>% 
  html_nodes("h2") %>% 
  html_text()

hostel.link <- 
  page %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.resultcontainer") %>% 
  html_nodes("div#fabResultsContainer") %>% 
  html_nodes("div.fabresult") %>% 
  html_nodes("div.resultheader") %>% 
  html_nodes("h2") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# overall.score <- 
#   page %>% 
#   html_nodes("div.row") %>% 
#   html_nodes("div.resultcontainer") %>% 
#   html_nodes("div#fabResultsContainer") %>% 
#   html_nodes("div.fabresult") %>% 
#   html_nodes("div.resultheader") %>% 
#   html_nodes("div.fabresult-details-rating") %>% 
#   html_nodes("div.hwta-rating-container") %>% 
#   html_nodes("div.hwta-rating-summary") %>% 
#   html_nodes("a.hwta-rating-score") %>% 
#   html_text()

# rating_band <- 
#   page %>% 
#   html_nodes("div.row") %>% 
#   html_nodes("div.resultcontainer") %>% 
#   html_nodes("div#fabResultsContainer") %>% 
#   html_nodes("div.fabresult") %>% 
#   html_nodes("div.resultheader") %>% 
#   html_nodes("div.fabresult-details-rating") %>% 
#   html_nodes("div.hwta-rating-container") %>% 
#   html_nodes("div.hwta-rating-summary") %>% 
#   html_nodes("div.hwta-rating-info") %>% 
#   html_nodes("span") %>% 
#   html_text()

# review_num <- 
#   page %>% 
#   html_nodes("div.row") %>% 
#   html_nodes("div.resultcontainer") %>% 
#   html_nodes("div#fabResultsContainer") %>% 
#   html_nodes("div.fabresult") %>% 
#   html_nodes("div.resultheader") %>% 
#   html_nodes("div.fabresult-details-rating") %>% 
#   html_nodes("div.hwta-rating-container") %>% 
#   html_nodes("div.hwta-rating-summary") %>% 
#   html_nodes("div.hwta-rating-info") %>% 
#   html_nodes("a.hwta-rating-counter") %>% 
#   html_text()
  
price.from <- 
  page %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.resultcontainer") %>% 
  html_nodes("div#fabResultsContainer") %>% 
  html_nodes("div.fabresult") %>% 
  html_nodes("div.resultheader") %>% 
  html_nodes("div.fabresult-prices") %>% 
  html_nodes("span.price") %>% 
  html_nodes("a") %>% 
  html_text()
  
location <- 
  page %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.resultcontainer") %>% 
  html_nodes("div#fabResultsContainer") %>% 
  html_nodes("div.fabresult") %>% 
  html_nodes("div.resultheader") %>% 
  html_nodes("div.addressline") %>% 
  html_text()

data.frame(hostel.name, hostel.link, 
           # overall.score, rating_band, review_num, 
           price.from, location)
}



# Crate a list using apply function
main.data <- apply(data.frame(link.list$Link), 1, hostel_scraping)
main.data <- do.call(rbind.data.frame, main.data)

# Clean dataset
main.data$location <- 
  as.character(main.data$location) %>% 
  str_remove("  - Show on Map\n        ")
main.data$location <- 
  main.data$location %>% 
  str_remove("\n             ")


main.data <- 
  main.data %>% 
  mutate(City = as.character(hostel.link))
main.data$City <- 
  main.data$City %>% 
  str_remove("https://www.hostelworld.com/hosteldetails.php/")
main.data <- 
  main.data %>% 
  separate(City, sep = "/",
           into = c("Name", "City", "num")) %>% 
  select(hostel.name, City, price.from, location, hostel.link)


# write.csv(main.data, file = "Hostel_list.csv")





