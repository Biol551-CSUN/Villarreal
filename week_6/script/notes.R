###week6b notes

##packages
install.packages("palmerpenguins")
library(tidyverse)
library(palmerpenguins)
library(here)
library(beyonce)
library(ggthemes)
###coding line for body mass and sex of each adelie species
glimpse(penguins)
summary(penguins)

aspecies <- penguins %>%
  group_by(species)%>%
  remove(NA)
  ggplot(aes(x= flipper_length_mm,
             y= body_mass_g))+
  geom_line()
view(aspecies)

|species | male | female|
  | _Adiline_ | 8 | 9| 
  | _ChinStrap_ | 9 | 8|
  |_Gentoo_| 10 | 8 |