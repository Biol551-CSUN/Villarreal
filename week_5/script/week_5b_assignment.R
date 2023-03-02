library(here)
library(tidyverse)
library(lubridate)

conductivity_data <- read_csv(here("week_5", "data", "CondData.csv"))
depth_data <- read_csv(here("week_5", "data", "DepthData.csv"))
view(conductivity_data)
view(depth_data)


##code


new_con <- conductivity_data %>%
  mutate(date= mdy_hms(date),
         date= round_date(date, "10 seconds")) ##getting rid of seconds below 10

inner_join(depth_data, new_con)%>%   ###combining data sets
  mutate(date= round_date(date, "1 minute")) %>%  ###round time in date to minutes
  group_by(date)%>%
  summarise(mean_depth=mean(Depth, na.rm= TRUE),
            mean_temp= mean(Temperature, na.rm= TRUE),
            mean_salinity= mean(Salinity, na.rm= TRUE))%>%  ###getting mean of deoth, temperature, and salinity
  ggplot(aes(x= date,
             y= mean_temp))+ ##graphing by date and average temp
  geom_point(color="#AD7DE8")+
  geom_line(color= "#AD7DE8")+ ##using purple color
  labs(x= "Time of January",
       y= "Average Temperature",
       title= "Average Temperature by Time")+ ##labeling x y and title
  theme_classic() ##changing background
  
  
view(depth_with_con)
 
ggsave(here("week_5","output","Time_to_AveTemp.png"),
       width = 7, height = 5)



view(new_con)