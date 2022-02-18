# Rlinkedin is depreciated and there are no other ways to the data from Linkeding without paying for the API
library(stringr)
library(RSelenium)
library(tidyverse)
library(rvest)
library(htm2txt)

# Read the data from proff.dk. This site tracks all registered Danish Companies
# We use the URL which is the search for all companies tagged with the danish word for Biotechnology: "Bioteknologi"
url <- "https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi"
html <- paste(readLines(url), collapse="\n")
ProffDK <- "https://www.proff.dk/"

# We can now get all linked from the html
AllLinks <- str_match_all(html, "<a href=\"(.*?)\"")
AllLinksDF <- data.frame(AllLinks)$X2

# We extract the company names from the html txt file. After visual inspection ####################################
# we know that company names are between two html tags: "data-name=" and "data-url="
Company_names <- str_match_all(html, "data-name=\\s*(.*?)\\s*data-url=")
# Convert from list to vector
Company_namesDF <- data.frame(Company_names)$X2
# Clean up the name
Company_namesDF <- str_sub(Company_namesDF, 2, -2)




# We now extract the company adress link #########################################################################
# We can not use the MAP link


# THIS STRING MATCHING ONLY WORKS ON STRING ON THE SAME LINES; NEED TO FIX THIS TO GET THE ADDRESS
str_match_all(html, "Bioteknologi\\s*(.*?)\\s*UTF-8")

str_match(html, "Bioteknologi\\s*(.*?)\\s*UTF-8")


# Using this code, we can extract the link to the next page #####################################################
NextPageLink <- AllLinksDF[str_detect(matchedDF, "Bioteknologi")]
NextPageLink <- NextPageLink[length(AllLinksDF)]
NextPageLink <- substring(NextPageLink, 2)
ProffDK <- "https://www.proff.dk/"
NextPageURL <- paste(ProffDK,NextPageLink, sep = "")




