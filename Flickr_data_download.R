library(RCurl)
library(XML)
library(FlickrAPI)

# Specify your api_key 
api_key <- "<API_KEY>"

# Specify search keyword, enable location, and other details
keyword <- "<KEYWORD>"
perpage <- "250"
format <- "rest"
hasgeo <- "1"

# Set the URL for search
URL <- paste(
"https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",
api_key, sep="")

getData <- paste(URL, "&text=",keyword,"&has_geo=",hasgeo,
                "&per_page=",perpage,"&format=",format,sep="")

# Fetch data
getURL <- getURL(getData, ssl.verifypeer=F, useragent="flickr")

# Parse fetched data and get the photo IDs
parse_data <- xmlRoot(xmlTreeParse(getURL, useInternalNodes = T))

# looping through pages of results
pages <- data.frame(xmlAttrs(parse_data[["photos"]]))
pages[] <- lapply(pages, as.character)
pages[] <- lapply(pages, as.integer)
colnames(pages)<- "value"
total_pages <- pages["pages","value"]

final_list <- NULL

# loop through pages of photos and save the list in a DF
for(i in c(1:total_pages)){
    getData <- paste(URL,"&text=",keyword,"&has_geo=",hasgeo,
                "&per_page=",perpage,"&format=",format,sep="")

    # Fetch data
    getURL <- getURL(getData, ssl.verifypeer=F, useragent="flickr")

    # Parse fetched data and get the photo IDs
    parse_data <- xmlRoot(xmlTreeParse(getURL, useInternalNodes = T))

    id<-xpathSApply(parse_data,"//photo",xmlGetAttr,"id")

    #Save results in a dataframe
    tmp_df <- data.frame(id, stringsAsFactors=F)
    tmp_df$page <- i
    final_list<-rbind(final_list,tmp_df)
}

# Create empty vectors for data
lats <- rep(NA, nrow(final_list))
lons <- rep(NA, nrow(final_list))
urls <- rep(NA, nrow(final_list))
dates <- rep(NA, nrow(final_list))
places <- rep(NA, nrow(final_list))

# Populate the empty vectors with information
# Try function is used in the loop to handle error due to missing data
for(i in 1:nrow(final_list)){
    try({
        loc <- getPhotoInfo(api_key, final_list$id[i], output = "Location")
        url <- getPhotoInfo(api_key, final_list$id[i], output = "URL")
        date <- getPhotoInfo(api_key, final_list$id[i], output = "Date")
        lats[i] <- loc$latitude
        lons[i] <- loc$longitude
        urls[i] <- url$'_content'
        dates[i] <- date$'taken'
        places[i] <- paste(loc[,-c(1:4)], collapse=", ")
    }, silent=T)
}

# Convert vectors into dataframes
idDF <- as.data.frame(final_list$id)
latDF <- as.data.frame(lats)
lonDF <- as.data.frame(lons)
urlDF <- as.data.frame(urls)
placeDF <- as.data.frame(places)

# Remove timestamp from date
dateL <- strsplit(dates, " ")
dateDF <- data.frame(rep(NA, length(dateL)))
colnames(dateDF) <- "observation_date"
for(i in 1:length(dateL)){
    dateDF[i,] <- dateL[[i]][1]
}

# Collate all data in a dataframe
data_collated <- cbind(idDF, dateDF, placeDF, lonDF, latDF, urlDF)
colnames(data_collated) <- c("photo_id", "observation_date", "location", 
                            "longitude", "latitude", "link")

# Remove missing values (points which do not have a link to a photo)
data_NAremoved <- subset(data_collated, link != "NA")

# Write the final dataframe into CSV file
write.csv(data_NAremoved, paste(keyword, ".csv", sep=""), row.names=F)