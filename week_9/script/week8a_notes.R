#week 8 notes

#library
install.packages("PNWColors")
library(tidyverse)
library(palmerpenguins)
library(PNWColors)


#code
df <- tibble::tibble(
  a =rnorm(10),  #draw 10 random values from a normal distribution
  b =rnorm(10),
  c =rnorm(10),
  d =rnorm(10)

)
head(df)

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))
         
F_to_C <- function(temp_F){
  temp_c <- (temp_F-32)*5/9
  return(temp_c)
  }
F_to_C(1)


C_to_Kelvin <- function(temp_c){
  temp_K <- temp_c+273.15
  return(temp_K)
}
C_to_Kelvin(-273.15)

myplot <- function(data= penguins, x,y){
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
ggplot(data, aes(x = {{x}}, y = {{y}}, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
  theme_bw()
}


myplot(data = penguins, x = body_mass_g, y = bill_length_mm)
myplot(x = body_mass_g, y = flipper_length_mm)

myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")


myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}


myplot(x = body_mass_g, y = flipper_length_mm)

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)
