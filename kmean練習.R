address_LatLng_data <- read.csv("address_LatLng_data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

set.seed(20180102)
k <- kmeans(x = address_LatLng_data[,c("Lat", "Lng")],centers = 3, nstart = 5 ) #分成3群 defaut執行5次收斂資料區
y_kmeans = k$cluster  #kmean 為非監督式學習,沒有標準答案

library(dplyr)
result <- address_LatLng_data %>%
  mutate(category = y_kmeans)
  
library(ggmap)
library(ggplot2)
Mykey = "Your_key"
register_google(key = Mykey) 
ggmap(get_googlemap(center = c(121.52311,25.04126),zoom = 12, maptype = "satellite"), extent = "device")+
  geom_point(data = result, size = 1.8,aes(x = Lng,y=Lat,color = factor(category)))