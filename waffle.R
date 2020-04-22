#Waffle Plot
library(tidyverse)

myrow <- c(rep(1,10), rep(2, 10),rep(3, 10),rep(4, 10),rep(5, 10),rep(6, 10),rep(7, 10),rep(8, 10),rep(9, 10),rep(10, 10))
mycol <- c(rep(1:10,10))
mypercent <- seq(.01, 1, by=.01)

df <- data.frame(myrow, mycol, mypercent)
my_percent <-  .46  #Shaded area and under so 80% is what I'm highlighting
my_plot <- ggplot(data = df, 
                       aes(x = mycol, y = myrow, label = mypercent)) #not using label now
# Draw the plot
my_plot + 
   geom_point() + 
  #geom_text() + #not using this at the moment
   geom_point(aes(colour=mypercent<=my_percent), size=10) +
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
 