library(tidyverse)
library(here)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(mapproj)
pizza <- read.csv("https://query.data.world/s/nwyktjpwoliijgyckzy5kuunkvzxkb?dws=00000", header=TRUE, stringsAsFactors=FALSE)

glimpse(pizza)

#pizza %>%
  #data_frame(names=c("AK","AL","AR","AZ","CA","CO","CT","DE","FL","GA","HI","IA","IL","IN","KS","KY","LA","MD","MI","MN","MO","MS","MT","NC","ND","NE","NJ","NM","NV","NY","OH","OK","OR","PA","SC","SD","TN","TX","UT","VA","WA","WI","WV"))

#sum(pizza$province == "WV")

states <- map_data("state") ##import US map
  ggplot()+
  geom_polygon(data= states, #data using for map
               aes(x= long,
                   y= lat, 
                   group= group,
                   fill = region))+
    geom_point(data= pizza,  #bigfoot data to go into map
               aes(x= longitude,
                   y= latitude),
               color="black",  #creating black marks for spots location od sightings 
               alpha = 0.5)
pizza%>%
  count(province)


