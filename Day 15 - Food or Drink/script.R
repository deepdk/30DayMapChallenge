library(tidyverse)
library(paletteer)
library(maps)
library(mapdata)

library(repr)
options(repr.plot.width = 20, repr.plot.height =20)

annotate <- ggplot2::annotate

font_add(family = "Roboto",regular = "../input/roboto/RobotoCondensed-Regular.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
  color.text = "#363636"
    
    # Begin construction of chart
  theme_bw(base_size=15) +
  # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "right") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 20, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",color = "white",size = 10, face = "bold"))+
    theme(legend.key.size = unit(2, 'cm'))+
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=100, hjust = 0.5,face = "bold", family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_blank())+
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


df <- read_csv("../input/subway-the-fastest-growing-franchise-in-the-worl/subway.csv")
state <- map_data('state')
state <- state %>%
mutate(region = str_to_title(region))%>%
select(region, long, lat, group)

df1<- df %>%
count(state, sort = TRUE)

df1 <- df1 %>%
mutate(State = state.name[match(state,state.abb)])%>%
na.omit()

final <- df1 %>%
left_join(state, by = c('State'='region'))

p1 <- final %>%
  ggplot(aes(x=long,y=lat,group=group, fill=n)) +
  geom_polygon(color = "gray31") +
  scale_fill_paletteer_c("ggthemes::Green-Gold")+
  coord_map("bonne", parameters=45)+
  my_theme()+
  theme(legend.justification = "center",
        legend.position = "top",
        legend.title=element_blank(),
        legend.margin=margin(b=-10))+
theme(plot.margin=margin(.5,.5,.3,.5,unit="cm"))+
  labs(caption = "Data source : https://www.kaggle.com/datasets/thedevastator/subway-the-fastest-growing-franchise-in-the-worl \n Graphic : Deepali Kank")
  p1
  
  # Added the title image using figma
