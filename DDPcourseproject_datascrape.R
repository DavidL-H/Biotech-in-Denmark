# There is a strong Biotechnology industry in Denmark. It is not easy to find a
# Good list with all the different companies and information on them, therefore
# I will try to scrape it from the web

# Using Proff.DK to search for "Bioteknologi" returns a search page with some entries. 
# This webpage only shows 25 companies (of 695 as of 220219). So the data cannot be 
# scraped based on a single html. However, the page contains a button that point to the 
# next page in the search. The first task is to gather the URLs of all results
# pages in the search. As input for the written function you need the url of the 
# first and second results page:

BiotekProffurl <- "https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi"
BiotekProffurl2ndPage <- "https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/YLoFmCo_zvNZxJID58xbvLFl_00WkiF1dDlpvs12g8Dq9ML7fkP1mBywa54Z7cJoB95okX8KgB9BXw6lfF3FGii-RGvAhN41kCOaz5UdOME/?q=Bioteknologi"
Industry <- "Bioteknologi"

# Here is a function that takes the url of an industry search on Proff.dk, and the industry search term
# and spits out the URL to the next page in the search:
Next_Page_ProffDK <- function(Page_link, Industry){
      # Take proff.dk industry search page url link as input
      # Need to specifiy the industry searched for
      # For example: 
      # PageLink = "https://www.proff.dk/s%C3%B8g-p%C3%A5-s%C3%B8geord/bioteknologi/I:2669/?q=Bioteknologi"
      # Industry = "Bioteknologi"
      # The industry searched is clear in the link
      
      url <- Page_link
      # Read the HTML of page as a string
      html <- paste(readLines(url), collapse="\n")
      # Identify all links in the string and save in a dataframe
      AllLinks <- str_match_all(html, "<a href=\"(.*?)\"")
      AllLinksDF <- data.frame(AllLinks)$X2
      # Find the links that contain the Industry queried
      # This needs to be cleaned a bit as well
      NextPageLink <- AllLinksDF[str_detect(AllLinksDF, Industry)]
      NextPageLink <- NextPageLink[length(NextPageLink)]
      NextPageLink <- substring(NextPageLink, 2)
      ProffDK <- "https://www.proff.dk/"
      NextPageURL <- paste(ProffDK,NextPageLink, sep = "")
      # The function returns the URL pointing to the next page
      return(NextPageURL)
}

# lets give the website a little time between each query, so it doesn't spaz out
sleeptime <- function(x){
      p1 <- proc.time()
      Sys.sleep(x)
}

# Loop through all the pages and save the urls using the Next_Page_ProffDK() function:
# THIS TAKES A COUPLE OF MINUTES TO RUN
# IN ORDER TO GET ALL WEB PAGES IT HAS TO BE RUN TWICE. ONCE WITH THE FIRST PAGE
# AND ONCE WITH THE SECOND PAGE
QueryPage <- BiotekProffurl
Pages <- BiotekProffurl
No_page <- round(695/25)

for (i in 1:No_page){
      #Adding rest time
      # sleeptime(2)
      #Nested function to get the next page
      QueryPage <- Next_Page_ProffDK(
            Next_Page_ProffDK(QueryPage, Industry),Industry
            )
      #Store all the page URLs in one vector
      Pages <- c(Pages,QueryPage)
}

# For some reason this only return every other page, we need to save this
Pages1 <- Pages

# Run it again with the second page in search manually entered:
QueryPage <- BiotekProffurl2ndPage
Pages <- BiotekProffurl2ndPage
for (i in 1:No_page){
      # Adding rest time
      # sleeptime(2)
      # Nested function to get the next page
      QueryPage <- Next_Page_ProffDK(
            Next_Page_ProffDK(QueryPage, Industry),Industry
      )
      # Store all the page URLs in one vector
      Pages <- c(Pages,QueryPage)
}

# Combine the pages and write a file with them:
Pages <- c(Pages, Pages1)

#write(unique(Pages), "ProffDKBiotekHTML.txt")

ProffBioTekURLS <- read.csv("ProffDKBiotekHTML.txt", header = FALSE)[,1]
# We now have a list of 28 webpages that can be used to scrape company information

### COMPANY NAMES #############################################################
# This function reads the company names on a given search results page of Proff.dk
Proff_company_names <- function(url){
      # Read the html of the url
      html <- paste(readLines(url), collapse="\n")
      Company_names <- str_match_all(html, "data-name=\\s*(.*?)\\s*data-url=")
      # Convert from list to vector
      Company_namesDF <- data.frame(Company_names)$X2
      # Clean up the name
      return(str_sub(Company_namesDF, 2, -2))
}

# Lets loop through all the search results pages in and extract the company names:
AllCompanies <- c()
for (page in ProffBioTekURLS){
      AllCompanies <- c(AllCompanies, Proff_company_names(page))
      sleeptime(2)
}
# Save results
write(AllCompanies, "ProffDKBiotekAllCompanies.txt")

ProffBioTekCompanies <- read.csv("ProffDKBiotekAllCompanies.txt", header = FALSE)[,1]
length(AllCompanies)
length(ProffBioTekCompanies)


### COMPANY ADDRESSES #########################################################

Proff_company_addresses <- function(url){
      # Read the html of the url
      html <- paste(readLines(url), collapse="\n")
      Company_Address <- str_match_all(
            # Need to remove line breaks in html string with gsub, or str_match() does work
            gsub("[\r\n]", "", html), 
            "address\\s*(.*?)\\s*</span>")
      
      Company_Address <- data.frame(Company_Address)$X2
      # Remove first entry
      Company_Address <- Company_Address[-1]
      # Get the part of string after "span>" and we have the clean names
      Company_Address <- sub(".*span>", "", Company_Address)
      return(Company_Address)
}

# Lets loop through all the search results pages in and extract the company addresses:
AllAddresses <- c()
for (page in ProffBioTekURLS){
      AllAddresses <- c(AllAddresses, Proff_company_addresses(page))
      sleeptime(2)
}
# Now all addresses are stored in the "AllAddresses" variable


### COMPANY COORDINATES #######################################################

# We now want to turn the addresses into coordinates for later mapping
# Unfortunately geocoding with the "ggmap" package now require a Google maps API key, for which you need to pay
# Also tried an alternative with "RDSTK" method found here: https://www.youtube.com/watch?v=h_-HBJQzzXs
# This package is not maintained anymore, unfortunately
# Finally, I found an up to date package: "tmaptools" that geocodes with OpenStreetMap Nominatim
library(tmaptools)
# This doesn't work with some of the addresses, however, and if one doesn't work
# then the whole function returns NULL. This means that we need to loop
BioDK <- data.frame(Company = AllCompanies, Address = AllAddresses)

BioDK$Lat <- NA
BioDK$Long <- NA

# Loop through all addresses and add the latitude and longitude (MAY TAKE A WHILE)
for (adr in 1:nrow(BioDK)){
      BioDK$Lat[adr] <- geocode_OSM(BioDK$Address[adr],keep.unfound = TRUE)$coords[1]
      BioDK$Long[adr] <- geocode_OSM(BioDK$Address[adr],keep.unfound = TRUE)$coords[2]
}

# This seems to do the trick, but some of the addresses are not recognized.
# Google Maps API would likely be better, but don't want to pay for it

# After manual inspection of the addresses that did not give a hit, it seem that the issues
# are the Danish city and post number. When you remove these, the function can find them
# https://www.openstreetmap.org/
library(readr) # Needed for the "parse_number()" function

# Lets make the final dataframe
BioDK <- data.frame(Company = AllCompanies, Address = AllAddresses)
# Add the Latitude and Longitude columns
BioDK$Lat <- NA
BioDK$Long <- NA

# Loop through all addresses and add the latitude and longitude (MAY TAKE A WHILE)
for (adr in 1:nrow(BioDK)){
      # For ever row we use the address as input for the geocode_OSM() function
      # This search https://www.openstreetmap.org/
      LatLong <- geocode_OSM(BioDK$Address[adr], keep.unfound = TRUE)$coords
      # Add the llongitude and latitude to the dataframe
      BioDK$Lat[adr] <- LatLong[1]
      BioDK$Long[adr] <- LatLong[2]
      
      # If the data is not found in the first call, we try again, without the
      # City and postnumber, as this seems to be the issue for the search
      if (is.na(BioDK$Lat[adr])){
            # This removes everything after the first nume ber in the string
            # I.e."Ole MaalC8es Vej 3, 2200 KC8benhavn N" becomes "Ole MaalC8es Vej 3"
            testad <- BioDK$Address[adr]
            testadnum <- as.character(parse_number(testad))
            temp_adr <- paste(sub(paste(" ", testadnum, ".*", sep=""), "", testad), testadnum)
            # Try to search with the reduced address
            LatLong <- geocode_OSM(temp_adr,keep.unfound = TRUE)$coords
            BioDK$Lat[adr] <- LatLong[1]
            BioDK$Long[adr] <- LatLong[2]
            
      }
}

write.csv(BioDK, "Biotech_companies_Denmark.csv")
BioDK[is.na(BioDK$Lat)==TRUE,]




#### NEXT ADDITIONS ######
# Grab "Roller", "Regnskab

# Grab company links off the search page: 
paste(readLines(BiotekProffurl), collapse="\n")
Company_URL <- str_match_all(html, "firma\\s*(.*?)\\s*\" class=\"additional-link")
Company_URL <- data.frame(Company_URL)$X2
Company_URL[1]
