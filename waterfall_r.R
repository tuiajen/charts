input_df <- read.csv("waterfall.csv", stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(magrittr)
library(grid)
library(gridExtra)

plot_df <- input_df %>%
  dplyr::mutate(
    END = cumsum(VALUE),
    STARTME = tidyr::replace_na(dplyr::lag(END),0),
    STARTME = dplyr::if_else(CATEGORY == "EOM Balance", 0,STARTME),
    LABEL = VALUE,
    LABEL_COORDINATES = dplyr::if_else(LABEL >= 0,END+1, END - 1),
    LABEL_VJUST = dplyr::if_else(LABEL >= 0,-1,1),
    INDEX = 1:nrow(input_df),
    COLOR = dplyr::if_else(LABEL >=0, "dodgerblue2","firebrick3"),
    COLOR = dplyr::if_else(CATEGORY %in% c("EOM Balance"),"forestgreen", COLOR) 
    )

plot_levels <- input_df$CATEGORY
plot <- ggplot (
   plot_df,
  # aes(CATEGORY,fill=COLOR)) +
   aes(x = INDEX, y=VALUE, fill = COLOR), 
   stat = "identity", position = "identity"
) + 
  geom_rect(aes(
   x=CATEGORY, xmin=INDEX-.3, xmax = INDEX+.3,
   ymin = STARTME, ymax = END)) +
 
  scale_x_discrete(
    label = plot_levels, limits = plot_levels, breaks = plot_levels
  ) + 
    scale_fill_manual(
       values = c("firebrick3","dodgerblue2", "forestgreen"),
       limits = c("firebrick3","dodgerblue2", "forestgreen")
    )+
    annotate(
      "text",
      x=plot_df$INDEX, y = plot_df$LABEL_COORDINATES, 
      label = plot_df$LABEL, vjust = plot_df$LABEL_VJUST, 
      size = 3
     ) +
      theme_minimal() +
      theme(
         panel.grid.major.y = element_blank(),
         panel.grid = element_blank(),
         axis.title = element_blank(),
        # scale_y_continuous(breaks=c(0,1000,2000,3000)),
        panel.grid.minor = element_line(size = 0.5, linetype = 'dashed',
                                        colour = "grey60"),
  scale_y_continuous(minor_breaks = seq(0, 1000, 500) )


## Create title
grobs <- grobTree(
  gp = gpar(fontsize = 14, fontface = "bold"),
  textGrob(label = "Surplus ", name = "title1",
           x = unit(0.2, "lines"), y = unit(1.1, "lines"),
           hjust = 0, vjust = 0, gp = gpar(col = "forestgreen")),
  textGrob(label = "at the end of the month", name = "title2",
           x = grobWidth("title1") + unit(0.2, "lines"), y = unit(1.1, "lines"),
           hjust = 0, vjust = 0)
)
## Combine title and visual
gg <- arrangeGrob(plot, top = grobs, padding = unit(2.6, "line"))
grid.newpage()
grid.draw(gg)
  
