#install.packages("ggmap")
library(ggmap)
library(ggplot2)
Mykey = "Your_key"
register_google(key = Mykey) 
LatLngData <- data.frame(
  Lat = runif(20,24.96,25.11),        # random uniform distribution 隨機產生變數
  Lng = runif(20, 121.44,121.60))     #產生20個 介於121.44 ~ 121.60的數


library(httr)   #install.packages("httr")
library(rjson)  #install.packages("rjson")

getLatLng <- function(address){
  
  urlData <- GET(paste0("https://maps.googleapis.com/maps/api/geocode/json?key=Your_key&language=zh-TW&address=",
                        address))
  
  jsonResult <- rjson::fromJSON(rawToChar(urlData$content))
   Sys.sleep(1) #延遲一秒避免被GOOGLE擋
   if (jsonResult$status != "OK"){
     print("Google geocode API Error !")
     return("error")
   }
  print("LateLng Got")
  lat <<- jsonResult$results[[1]]$geometry$location$lat
  lng <<- jsonResult$results[[1]]$geometry$location$lng
  
  return(paste(lat, lng, sep=","))
}

library(dplyr)
library(readr)
library(tidyr) #separate

address_data <- read.csv("address.csv",stringsAsFactors = FALSE,header = FALSE, fileEncoding = "UTF-8") #header 要為FALSE 才能在getLatLng(V1)時將頭改成V1

result <- address_data %>%
  rowwise() %>%
  mutate(LatLng = getLatLng(V1)) %>%
  filter(LatLng != "error") %>%
  separate(LatLng, c("Lat", "Lng"), sep = ",") %>%
  mutate(Lat = as.numeric(Lat), Lng = as.numeric(Lng))

 write.table(result,file = "address_LatLng_data.csv",sep = ",", row.names = FALSE, col.names = TRUE,fileEncoding = "UTF-8" )


google_map <- get_googlemap(center = c(121.52311,25.04126), zoom = 12, maptype = "satellite")
ggmap(google_map) + geom_point(data = result, aes(x = Lng, y = Lat), colour = "red")
