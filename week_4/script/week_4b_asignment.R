#### Load Libraries ######
library(tidyverse)
library(here)
### Load data ######
ChemData<-read_csv(here("week_4","data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)


NChemData<- ChemData %>%
  drop_na() %>%
  separate (col= Tide_time,  ##choose the tide time col
            into= c("Tide", "Time"),  ##separate it into two columns Tide and time
            sep= "_",   ##separate by 
            remove= FALSE) %>%  ##Keep the original tide_time column
  filter(Temp_in >23) %>%
  pivot_longer(cols = Temp_in:pH, # the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Time_on_Temp_impact", # the names of the new cols with all the column names
               values_to = "Values") %>%
  group_by(Tide, Time, Time_on_Temp_impact) %>%

  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  filter(Time_on_Temp_impact== "Temp_in")

NChemData %>%
  ggplot(aes(x=Time, y=mean_vals))+ ##x and y values
  geom_point(color= "blue")+ ##color change
  labs(x= "Time", #x title
       y= "Mean Value of Temperature", ##y title
       title= "Average Temperature of Day Time")+  ##titles
  theme_classic() ##change background
  
view(NChemData)

ggsave(here("week_4","output","Temp_to_time.png"),
       width = 7, height = 5)
