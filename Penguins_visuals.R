#Installing tidyverse and importing libraries
#install.packages("tidyverse")
library(tidyverse)
library(lubridate)

              ####Penguins data Visualization ######
#Work on Palmer penguins dataset
install.packages("palmerpenguins")
library("palmerpenguins")
colnames(penguins)

#Color, shape, size, x, y are all tied to variables in the aes()
ggplot(data = penguins) + geom_point(
  aes(
    x = body_mass_g,
    y = flipper_length_mm,
    color = species,
    shape = species,
    size = species
  )
) + ggtitle("Penguine Body Mass Vs. Flipper Length")

#OR

#Use function for repeated code (change it if you want other grammar of geometry)
scatterplotting <- function(data, var1, var2, color_var, size_var) {
  if (missing(color_var) && missing(size_var)) {
    ggplot(data) + geom_point(aes(x = var1, y = var2))
  } else if (missing(size_var)) {
    ggplot(data) + geom_point(aes(x = var1, y = var2, colour = color_var))
  } else if (missing(color_var)) {
    ggplot(data) + geom_point(aes(x = var1, y = var2, size = size_var))
  } else{
    ggplot(data) + geom_point(aes(
      x = var1,
      y = var2,
      colour = color_var,
      size = size_var
    ))
  }
}

#Must provide data$variable as arguments to function
scatterplotting(penguins, penguins$body_mass_g, penguins$flipper_length_mm) #x,y
scatterplotting(penguins,
                penguins$body_mass_g,
                penguins$flipper_length_mm,
                penguins$species) #x,y,color
scatterplotting(penguins,
                penguins$body_mass_g,
                penguins$flipper_length_mm,
                ,
                penguins$species) #x,y, , size
scatterplotting(
  penguins,
  penguins$body_mass_g,
  penguins$flipper_length_mm,
  penguins$species,
  penguins$species
) #x,y,color,size



#Adding the alpha aesthetic
ggplot(data = penguins) + geom_point(aes(
  x = body_mass_g,
  y = flipper_length_mm,
  alpha =
    species,
  shape = species
)) + ggtitle("Penguine Body Mass Vs. Flipper Length")

#OR

#Specific colors, shapes... that are not tied to variables should be placed outside the aes() but within geom()
ggplot(data = penguins) + geom_point(
  aes(x = body_mass_g, y = flipper_length_mm),
  color = "purple",
  shape = "triangle"
) + ggtitle("Penguine Body Mass Vs. Flipper Length")

#Using other geometrical object
#Smooth line
ggplot(data = penguins) + geom_smooth(aes(x = body_mass_g, y = flipper_length_mm)) + ggtitle("Penguine Body Mass Vs. Flipper Length")

ggplot(data = penguins) + geom_smooth(aes(x = body_mass_g, y = flipper_length_mm, linetype = species)) + ggtitle("Penguine Body Mass Vs. Flipper Length")

#Jitter for overlapping data points in a plot
ggplot(data = penguins) + geom_jitter(aes(x = body_mass_g, y = flipper_length_mm)) + ggtitle("Penguine Body Mass Vs. Flipper Length")


#Combinations of Geom in different layers (best to have aes() in the foundation ggplot() for readability with these combo geometries)
#Unless different layers have different aesthetics ex(different alpha, colors, shapes, sizes...)

#geom_smooth has two method: "loess" and "gam"; loess for <1000 data points and vice versa
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) + geom_point(alpha = 0.3) + geom_smooth(color =
                                                                                                               "red", method = "loess") + ggtitle("Penguine Body Mass Vs. Flipper Length")
ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) + geom_point(alpha = 0.3) + geom_smooth(color =
                                                                                                               "red", method = "gam", formula = y ~s(x)) + ggtitle("Penguine Body Mass Vs. Flipper Length") 
                ####Diamonds data Visualization ######
#Already found withtin ggplot2 pkg
colnames(diamonds)
unique(diamonds$cut)

#Bar chart
ggplot(diamonds) + geom_bar(aes(x=cut, fill = cut), stat = "count") + labs(title = "Diamonds Cuts", subtitle = "(count)")

#stacked chart (with a different fill)
ggplot(diamonds) + geom_bar(aes(x=cut, fill = clarity), stat = "count") + labs(title = "Diamonds Cuts", subtitle = "(count)")


#Need the scales pkg to be able to use label = comma in the scale_y_continuous() layer
install.packages("scales")
library(scales)
#gradiant coloring REQUIRES continuous y axis data
diamonds %>%
  group_by(cut) %>%
  ggplot() + geom_bar(aes(x = cut, y = mean(price), fill = cut), stat = "identity") + scale_y_continuous(labels = comma) + coord_flip() +
  theme_classic() + labs(title = "Diamonds Cuts By Average Price",
                         subtitle = "($)",
                         y = "Average Price")


