# day 4: historic data
# africapolis: history of urbanization in africa

library(dplyr)
library(ggplot2)
library(showtext)

#font_add_google(name = 'Rubik', family = 'Rubik')
#showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

afridata <- openxlsx::read.xlsx('https://africapolis.org/download/Africapolis_agglomeration_2020.xlsx',
                                startRow = 15) 

# cities over a million
afri_over_mil <- afridata |>
  filter(Population_2015 > 1000000)

# calculate change from 1950 to 2015
afri_over_mil$change <- afri_over_mil$Population_2015 - afri_over_mil$Population_1950

# wide to long
afri_data_long <- afri_over_mil |>
  pivot_longer(
    cols = starts_with("Population_"),
    names_to = "Year",
    names_prefix = "Population_",
    values_to = "Population",
    values_drop_na = TRUE
  ) 

afri_data_long$Year <- as.numeric(afri_data_long$Year)
  
afri_data_long <- afri_data_long |>
  arrange(Agglomeration_Name, Year) |>
  group_by(Agglomeration_Name) |> 
  mutate(D = lag(Population))

highlight_list <- c('Cairo', 'Lagos')

highlight_df <- trees_sum |>
  filter(Family %in% highlight_list)

trees_other <- trees_sum |>
  filter(!Family %in% highlight_list)

## panel 1: growth ####
a <- ggplot()+
  geom_line(data = afri_data_long, 
            aes(x = Year, y = Population, text=Agglomeration_Name,
                group = Agglomeration_Name, color = Agglomeration_Name)) +
  theme(legend.position = 'none')

library(plotly)
ggplotly(a, tooltip = 'text')

## panel 2: map ####
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent == 'Africa')

b<-ggplot()+
  geom_sf(data = world, color='grey95', fill = 'grey80') +
  geom_point(data = afri_over_mil, aes(x = Longitude, y = Latitude,
                                       size = change, color = change)) + 
  theme_void() +
  theme(legend.position = '')

library(rayshader)


