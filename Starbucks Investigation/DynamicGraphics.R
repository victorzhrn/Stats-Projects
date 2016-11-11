library(ggmap)
library(ggplot2)
library(rvest)
library(dplyr)
library(maps)
library(animation)
library(stringr)

url = "http://www.starbuckseverywhere.net/StoreOpeningDates.htm"

html <- html(url)

# retrieve data from Winter's website
df <- html %>% html_nodes("table") %>% html_table()
save(df,file = "starbucks_data.Rda")
 load("starbucks_data.Rda")

 df <- df[[1]]
 colnames(df) <- df[1,]
 df <- df[2:nrow(df),]
 # clean backslahes in teh Market column
 df$Market <-str_replace(df$Market,"\\\\"," ")


row = df[1,]
# clean out the date infos
date_info<- str_split(df$Opened,"/")
date_info <- unlist(date_info)
date_info <- as.numeric(date_info)

# sort df by year, month, and data
date_info <- as.data.frame(matrix(date_info,ncol=3,byrow = T))
names(date_info) <- c("month",'day','year')
df <- cbind(df,date_info)
df <- (df[order(df$year,df$month,df$day),])


geo_locations <- data.frame()  # df for storing all the geo_locations of starbucks 
for(i in 1:nrow(df)){
  row <- df[i,]
  location_str <- paste(row$Market,row$City,row$Name)
  geo<-geocode(location_str,source = "dsk")
  geo_locations<- rbind(geo_locations,geo)
}
geo_locations <- cbind(df,geo_locations)

# save the geo_locations
save(geo_locations,file = "geo_locations.Rda")
load("geo_locations.Rda")

# drop cases with NA for locations
geo_locations = geo_locations[complete.cases(geo_locations), ]

# get the base satellite map of US
mapgilbert <- get_map(location = c(lon = -98.5795, lat = 39.8282), zoom = 4,
                      maptype = "satellite", scale = 2)
g<-ggmap(mapgilbert)+ggtitle("Starbucks in US")+guides(fill=FALSE)+xlab("longitude")+ylab("latitude")
g

# plot function for the animation
plotfoo<- function(){
  i=1
  while(i<nrow(geo_locations)){
    # the increment is dynamic
    increment = ceiling(i/2)
    if(increment>50){
      increment=50
    }
    bound = i+increment
    if(bound>nrow(geo_locations)){
      bound = nrow(geo_locations)
    }
    each_location <- geo_locations[i:bound,]   # for each iteraton, plot i to i+increment rows
    print(i)
    i=i+increment
    g <- g+geom_point(data=each_location,aes(x = lon, y = lat, fill = "red"), size = 2, shape = 21,alpha=0.8)
    g <- g+ggtitle(paste("Starbucks in US by",geo_locations[i,]$year))
    print(g)
  }
}

saveGIF(plotfoo(),interval = 0.1,movie.name = "starbucks.gif")



