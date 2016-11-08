#### Geocoding Practice Addresses using Google Maps API ####

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

install.packages("RCurl")
install.packages("RJSONIO")
install.packages("plyr")

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

## Test ##

#address <- geoCode("625 G Street, Cheney, WA 99004")
#address

## Test Creating Vector ##

#Use plyr to getgeocoding for a vector
#address <- c("625 G Street, Cheney, WA","The Capitol, Washington, DC")
#locations <- ldply(address, function(x) geoCode(x))
#names(locations) <- c("lat","lon","location_type", "forAddress")

#### Calculating Distance Between Points using Google API ####

install.packages("XML")

library(XML)
library(RCurl)
distance2Points <- function(origin,destination){
  results <- list();
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  time <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  time <- as.numeric(time)/60
  distance <- distance/1000
  results[['time']] <- time
  results[['dist']] <- distance
  return(results)
}

## Test ##


#### Calculating the Physician-to-Population Ratio for Every Provider Location ####
