library(ggmap)
library(ggplot2)
library(rvest)
library(dplyr)
library(maps)
library(animation)
library(stringr)

url = "http://www.starbuckseverywhere.net/StoreOpeningDates.htm"

# html <- html(url)
# 
# df <- html %>% html_nodes("table") %>% html_table()
# save(df,file = "starbucks_data.Rda")
 load("starbucks_data.Rda")

 df <- df[[1]]
 colnames(df) <- df[1,]
 df <- df[2:nrow(df),]
 # clean backslahes in teh Market column
 df$Market <-str_replace(df$Market,"\\\\"," ")

# 
# df["date"] = as.Date(df$Opened,"%m/%d/%y")
# df = df[order(df$date),]

row = df[1,]
date_info<- str_split(df$Opened,"/")
date_info <- unlist(date_info)
date_info <- as.numeric(date_info)

date_info <- as.data.frame(matrix(date_info,ncol=3,byrow = T))
names(date_info) <- c("month",'day','year')
head(date_info)
df <- cbind(df,date_info)
df <- (df[order(df$year,df$month,df$day),])


geo_locations <- data.frame()
for(i in 1:nrow(df)){
  row <- df[i,]
  location_str <- paste(row$Market,row$City,row$Name)
  geo<-geocode(location_str,source = "dsk")
  geo_locations<- rbind(geo_locations,geo)
  print(geo)
}
geo_locations

save(geo_locations,file = "geo_locations.Rda")

mapgilbert <- get_map(location = c(lon = -98.5795, lat = 39.8282), zoom = 3,
                      maptype = "satellite", scale = 2)
g<-ggmap(mapgilbert)
# g<-g+geom_point(data = geo_locations, aes(x = lon, y = lat, fill = "red"), size = 3, shape = 21,alpha=0.8)
g


plotfoo<- function(){
  for(i in 1:nrow(geo_locations)){
    each_location <- geo_locations[i,]
    print(each_location)
    g <- g+geom_point(data=each_location,aes(x = lon, y = lat, fill = "red"), size = 3, shape = 21,alpha=0.8)
    print(g)
  }
}


oopt = ani.options(interval = 1, nmax = nrow(geo_locations)) 
saveGIF(plotfoo(),interval = 0.05)



