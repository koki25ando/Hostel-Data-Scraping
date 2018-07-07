# Preparation
library(tidyverse)
library(rvest)
library(reshape2)

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

# Rating summary score
rating.summary <- 
  page %>% 
  html_nodes("div.inner-wrap") %>% 
  html_nodes("div.page-contents") %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.resultcontainer") %>% 
  html_nodes("div.fabresult") %>% 
  html_nodes("div.fabresult-details-rating") %>% 
  html_nodes("div.hwta-rating-container") %>% 
  html_text()
rating.summary <- 
  rating.summary %>% 
  unlist()
rating.summary <- data.frame(rating.summary)
rating.summary$rating.summary <- as.character(rating.summary$rating.summary)
rating.summary$rating.summary <- 
  rating.summary$rating.summary %>% 
  str_remove_all("\n")
rating.summary$rating.summary <- 
  rating.summary$rating.summary %>% 
  str_remove("\\s")

rating.summary$rating.summary <- 
  rating.summary$rating.summary %>% 
  str_remove("                                      ")

rating.summary$rating.summary <- 
  rating.summary$rating.summary %>% 
  str_sub(1, 5)

rating.summary$rating.summary <- as.numeric(rating.summary$rating.summary)


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
           price.from, location, rating.summary)
}



# Crate a list using apply function
link.list <- 
  link.list %>% 
  filter(Link != "https://www.hostelworld.com/findabed.php/ChosenCity.Osaka/ChosenCountry.Japan?page=5" &
           Link != "https://www.hostelworld.com/hostels/Kyoto/Japan?page=4")


main.data <- apply(data.frame(link.list$Link), 1, hostel_scraping)
main.data <- do.call(rbind.data.frame, main.data)


main.data <- 
  main.data %>% 
  filter(rating.summary != 0.0)


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
  select(hostel.name, City, price.from, location, rating.summary, hostel.link)


# write.csv(main.data, file = "Hostel_list.csv")

## Scraping Next step

review_scraping <- function (url) {
  
url <- as.character(url)
page <- read_html(url)

# Hostel Name

Hostel.Name <- 
  page %>% 
  html_nodes("div.ms-content") %>% 
  html_nodes('[name=ms-hero]') %>% 
  html_nodes("div.jumbotron") %>% 
  html_nodes("div.row") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.content") %>% 
  html_nodes("h1") %>% 
  html_text()
  
# Description

# description <- 
#   page %>% 
#   html_nodes("div.ms-content") %>% 
#   html_nodes('[name=ms-rating-description]') %>% 
#   html_nodes("div.row") %>% 
#   html_nodes("div.description-property") %>% 
#   html_text()

# Rating
summary.score <- 
  page %>% 
  html_nodes("div.ms-content") %>% 
  html_nodes('[name=ms-reviews]') %>% 
  html_nodes("div.row") %>% 
  html_nodes('[name=ms-reviews-and-ratings]') %>% 
  html_nodes("div.summary-row") %>% 
  html_nodes("div.rating-summary") %>% 
  html_nodes("div.score") %>% 
  html_text()
  
rating.band <- 
  page %>% 
  html_nodes("div.ms-content") %>% 
  html_nodes('[name=ms-reviews]') %>% 
  html_nodes("div.row") %>% 
  html_nodes('[name=ms-reviews-and-ratings]') %>% 
  html_nodes("div.summary-row") %>% 
  html_nodes("div.info") %>% 
  html_nodes("p.keyword") %>% 
  html_text()
  
# Rating Breadkdown
rating.breakdown <- 
  page %>% 
  html_nodes("div.ms-content") %>% 
  html_nodes('[name=ms-reviews]') %>% 
  html_nodes("div.row") %>% 
  html_nodes('[name=ms-reviews-and-ratings]') %>% 
  html_nodes("ul.row") %>% 
  html_nodes("li.small-12") %>% 
  html_nodes("p.rating-label") %>% 
  html_text()


# Clean data
rating.breakdown <- 
  data.frame(rating.breakdown)
rating.breakdown$rating.breakdown <- as.character(rating.breakdown$rating.breakdown)
rating.breakdown$rating.breakdown <- 
  rating.breakdown$rating.breakdown %>% 
  str_remove("\\.")
rating.breakdown$rating.breakdown <- 
  rating.breakdown$rating.breakdown %>% str_to_lower()
rating.breakdown$rating.breakdown <-
  rating.breakdown$rating.breakdown %>% 
  str_remove_all(" ")
rating.breakdown$rating.breakdown <- 
  rating.breakdown$rating.breakdown %>% 
  colsplit("(?<=\\p{L})(?=[\\d+$])", c("Type", "Score"))
rating.breakdown <- data.frame(rating.breakdown)
rating.breakdown <- 
  rating.breakdown$rating.breakdown

rating.breakdown$Score <-
  rating.breakdown$Score/10

rating.breakdown <- rating.breakdown %>% 
  spread(Type, Score)

# Data frame
data.frame(Hostel.Name, summary.score, rating.band, rating.breakdown)
}


# Looping deal
dont_include <- 
  c("https://www.hostelworld.com/hosteldetails.php/Hisayo-s-Inn/Tokyo/283587",
    "https://www.hostelworld.com/hosteldetails.php/Kyo-To/Kyoto/280282",
    "https://www.hostelworld.com/hosteldetails.php/Mosaic-Machiya-KSK/Kyoto/277116",
    "https://www.hostelworld.com/hosteldetails.php/Hostel-TOKI/Fukuoka-City/286150",
    "https://www.hostelworld.com/hosteldetails.php/Capsule-Hotel-Anshin-Oyado-Shinbashi/Tokyo/281072", 
    "https://www.hostelworld.com/hosteldetails.php/Capsule-Hotel-Anshin-Oyado-Akihabara/Tokyo/281075",
    "https://www.hostelworld.com/hosteldetails.php/Kyoto-Hostel-ZEN/Kyoto/283255",
    "https://www.hostelworld.com/hosteldetails.php/3Q-House-Asakusa-Smile/Tokyo/286271",
    "https://www.hostelworld.com/hosteldetails.php/Koenji-Junjo-Hotel/Tokyo/285896",
    "https://www.hostelworld.com/hosteldetails.php/Calendar-Hotel/Kyoto/278973",
    "https://www.hostelworld.com/hosteldetails.php/YADOKARI-Namba-Hostel/Osaka/275473",
    "https://www.hostelworld.com/hosteldetails.php/Hostel-Namba-Takumi/Osaka/287035",
    "https://www.hostelworld.com/hosteldetails.php/The-Dorm-Hostel-Osaka/Osaka/271739",
    "https://www.hostelworld.com/hosteldetails.php/Hostel-Stand-By-Me/Fukuoka-City/281155")


'%ni%' <- Negate('%in%')
main.data <- 
  main.data %>% 
  filter(hostel.link %ni% dont_include)
hostel.link.list <- main.data$hostel.link

hostel.dataset <- apply(data.frame(hostel.link.list), 1, review_scraping)
do.call(rbind, hostel.dataset)