
library(ggplot2)
library(PogromcyDanych)
library(dplyr)
library(forcats)
library(grid)
library(ggpubr)

### preparing data #####

data("auta2012")

######## ploting ########

ggplot(auta2012, aes(Skrzynia.biegow,fill=Pojazd.uszkodzony))+geom_bar(position = "stack")
ggplot(auta2012, aes(Skrzynia.biegow,fill=Pojazd.uszkodzony))+geom_bar(position = "fill")

