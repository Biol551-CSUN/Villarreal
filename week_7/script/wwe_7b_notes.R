install.packages(c("maps", "mapproj", "mapdata"))

# Load libraries
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

# Read in data on population in California by county
popdata<-read_csv(here("week_7","data","CApopdata.csv"))
#read in data on number of seastars at different field sites
stars<-read_csv(here("week_7","data","stars.csv"))

# get data for the entire world
world<-map_data("world")
head(world)

# get data for the USA
usa<-map_data("usa")
head(usa)
# get data for italy
italy<-map_data("italy")
head(italy)
# get data for states
states<-map_data("state")
head(states)
# get data for counties
counties<-map_data("county")
head(counties)
view(world)
ggplot()+
  geom_polygon(data= world,
               aes(x= long,
                   y= lat, 
                   group= group,
                   fill = region),
               color= "blue")+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "sinusoidal",
            xlim = c(-180,180))

##filtering out all states but california 
# Use the states dataset
CA_data<-states %>%
  filter(region == "california")

ggplot()+
  geom_polygon(data = CA_data, 
               aes(x = long, 
                   y = lat, 
                   group = group), 
               color = "blue")+
  coord_map()+
  theme_void()

head(countries)
head(popdata)


CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% # rename the county col
  inner_join(counties) %>%
  filter(region == "california") # some counties have same names in other states
ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),
               color = "white")+
  geom_point(data = stars, # add a point at all my sites
             aes(x = long,
                 y = lat,
                 size = star_no))+
  coord_map()+
  theme_void()+
  scale_fill_gradient(trans = "log10")+
  labs(size = "stars/m2")
ggsave(here("week_7","output","CApop.pdf"))
