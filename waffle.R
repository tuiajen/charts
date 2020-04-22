#Waffle Plot
library(readxl)
library(tidyverse)
library(Hmisc)
library(janitor)
library(magrittr)

setwd("C:/Users/jtuia/Documents/Mine/R projects/waffle")
df <- read_excel("waffletemplate.xlsx") %>% clean_names() #fix headers on arrival

my_percent <-  .46  #Shaded area and under so 80% is what I'm highlighting
my_plot <- ggplot(data = df, 
                       aes(x = column, y = row, label = percentage)) #not using label at this time

#label = rownames(mtcars)
# Draw the plot
my_plot + 
   geom_point() + 
  #geom_text() +
   geom_point(aes(colour=percentage<=my_percent), size=10) +
  scale_color_manual(values = c("gray", "blue"))+
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
dev.copy(png,'myplot.png')
dev.off()

 
