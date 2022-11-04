library(tidyverse)
library(maps)
library(mapdata)
library(MetBrewer)
library(patchwork)
library(paletteer)

font_add(family = "Roboto", regular = "../input/roboto-bold/RobotoCondensed-Bold.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#DBDBDB"
  color.text ="#0D0D0D"
  
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
    # Format the legend
    theme(legend.position = "right") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 20, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",color = "#030303",size = 18, face = "bold"))+
    theme(legend.key.size = unit(1.5, 'cm'))+
    
    # Format title and axis labels
    theme(plot.title       = element_text(color="#8B3626", size=80, face = "bold", hjust = 0.5,family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color="#8B3626", size=20, face = "bold",hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color="#8B3626", size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = "#FF0000", face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df <- read.csv("../input/california-wine-production-19802020/Californa_Wine_Production_1980_2020.csv")

usa <- map_data('usa')
state <- map_data('state')

california <- subset(state, region=="california")
counties <- map_data("county")
california_county <- subset(counties, region=="california")

california_county <- california_county %>%
rename(County = subregion)%>%
mutate(County = str_to_title(County))

final <- left_join(df, california_county, on = "County")

final_1980 <- final %>%
filter(Year == 1980)

final_2020 <- final %>%
filter(Year == 2020)

p1 <- ggplot(data=california, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=california_county, fill=NA, color="black")+
  geom_polygon(data=final_1980, aes(fill=HarvestedAcres)) +
  scale_fill_paletteer_c("grDevices::Reds",breaks = c(0,15000,30000,45000,60000,75000,90000), limits = c(0,90000))+
  geom_polygon(color="black", fill=NA)  + 
  ggtitle('1980') + 
  my_theme()+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())

p2 <- ggplot(data=california, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=california_county, fill=NA, color="black")+
  geom_polygon(data=final_2020, aes(fill=HarvestedAcres)) +
  scale_fill_paletteer_c("grDevices::Reds",breaks = c(0,15000,30000,45000,60000,75000,90000), limits = c(0,90000))+
  geom_polygon(color="black", fill=NA)  + 
  ggtitle('2020') + 
  my_theme()+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
  
  P = p1 + p2
  P + plot_annotation(title = 'Wine Harvesting in California',
                    subtitle = "Harvasted Area in Acres",
                    caption = "Data Source : https://www.kaggle.com/datasets/jarredpriester/california-wine-production-19802020",
                    theme = theme(plot.title = element_text(size = 120, face = "bold", color = "#8B3626", family = 'Roboto'),
                                  plot.subtitle = element_text(size = 50, face = "bold",color = "#8B3626", family = 'Roboto'),
                                  plot.caption = element_text(size = 25, face = "bold", hjust = 0.5, color = "#8B3626", family = 'Roboto'),
                                  plot.background  = element_rect(fill="#DBDBDB", color="#DBDBDB"),
                                  legend.position = "right"))+
plot_layout(guides = "collect", widths = 5, heights = 2)
