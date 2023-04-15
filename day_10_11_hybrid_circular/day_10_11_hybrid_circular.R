# day 4: historic data
# africapolis: history of urbanization in africa

library(dplyr)
library(ggplot2)
library(geofacet)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'Roboto')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

afridata <- openxlsx::read.xlsx('https://africapolis.org/download/Africapolis_agglomeration_2020.xlsx',
                                startRow = 15) 
# top 25 for 5x5 grid
afri_top_25 <- afridata |>
  slice_max(order_by = Population_2015, n = 25)

# wide to long
afri_data_long <- afri_top_25 |>
  tidyr::pivot_longer(
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


# convert to year, technically not correct but the plots look weird otherwise
afri_data_long$Year <- as.factor(afri_data_long$Year)

afri_data_long <- afri_data_long |>
  mutate(pop_mils = Population / 1000000) |>
  group_by(Agglomeration_Name) |>
  mutate(max_pop = max(Population)) |>
  ungroup() 

# order by max population
afri_data_long <- afri_data_long |>
  mutate(Agglomeration_Name =   forcats::fct_reorder(Agglomeration_Name, max_pop, .desc=T))

# specify color scheme
col_pal = rev(colorspace::sequential_hcl(25, palette = "Batlow"))

## panel 1: facet grid ####
ggplot() +
  geom_col(data = afri_data_long, 
           aes(x = Year, y = pop_mils, fill = Agglomeration_Name),
           color = 'grey10') +
  facet_wrap(~Agglomeration_Name) +
  scale_fill_manual(values = col_pal) + 
  theme_minimal() +
  ylab('Population (millions)') +
  labs(title = 'Urbanisation in Africa',
       subtitle = 'Top 25 cities by population') +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#f6e6e5', color=NA),
        panel.grid = element_line(color = 'grey90'),
        axis.text = element_text(color = 'grey20', size = 20),
        axis.text.x = element_text(angle = 60),
        axis.title = element_text(color='grey20', family = 'Roboto', size = 30),
        strip.text = element_text(color = 'grey10', size = 30, family = 'Roboto'),
        plot.title = element_text(color = 'grey10', size = 50, family = 'Roboto'),
        plot.subtitle = element_text(color = 'grey10', size = 30, family = 'Roboto'))

ggsave('grid_panel.png', height = 6, width = 6)

## panel 2: map ####
# underlying plot
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent == 'Africa')

# convert coordinates for inset plotting
# linear conversion values:
# manually figure out min/max in either direction for plotting
# and then transform
new_min_y = -.03
new_max_y = .68
new_min_x = 0
new_max_x = .57

old_min_x = min(afri_data_long$Longitude)
old_max_x = max(afri_data_long$Longitude)
old_min_y = min(afri_data_long$Latitude)
old_max_y = max(afri_data_long$Latitude)

afri_data_long$plot_coord_x = ( (afri_data_long$Longitude - old_min_x) / (old_max_x - old_min_x) ) * (new_max_x - new_min_x) + new_min_x
afri_data_long$plot_coord_y = ( (afri_data_long$Latitude - old_min_y) / (old_max_y - old_min_y) ) * (new_max_y - new_min_y) + new_min_y

# loop over city names and iteratively add insets to the map
city_order <- afri_top_25 |>
  arrange(Population_2015)
city_list <- afri_top_25$Agglomeration_Name

library(cowplot)
# initialize main plot
main.plot <- ggplot() +
  geom_sf(data = world, color='#f6e6e5', fill = 'grey70') +
  # points to help double check localization
  #geom_point(data = afri_top_25, aes(x = Longitude, y = Latitude)) +
  # i really tried to add labels but it was too busy
  #ggrepel::geom_text_repel(data = afri_top_25, 
  #                         aes(x = Longitude, y = Latitude, label = Agglomeration_Name),
  #                         size = 1.5, max.overlaps = 10) +
  labs(title = ' ') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#f6e6e5', color = NA))

# test code  
ggdraw() +
  draw_plot(main.plot) +
  draw_plot(inset.plot, x = .57, y = -.03, width = .4, height = .4) +
  coord_fixed()

cur_iter <- 1
for (i in city_list){
  df_temp <- filter(afri_data_long, Agglomeration_Name == i)
  
  # create inset for current city
  inset.plot <- ggplot() +
    geom_col(data = df_temp, 
             aes(x = Year, y = Population), fill = col_pal[cur_iter]) +
    # manually set limits to the max out of any city
    scale_y_continuous(limits = c(0, 22995802))+
    coord_polar() +
    theme_void() 
    theme(legend.position = 'none')
  
  # get x & y coordinates for inset
  x_inset <- df_temp$plot_coord_x[1]
  y_inset <- df_temp$plot_coord_y[1]
  
  # add inset to main plot, overwrite main plot
  main.plot <- ggdraw() +
    draw_plot(main.plot) +
    draw_plot(inset.plot, x = x_inset, y = y_inset, width = .4, height = .4) 

  cur_iter <- cur_iter+1
}

main.plot +
  coord_fixed()
# this is a total hack but it's the only thing that worked without completely messing up the dimensions
ggsave('map_panel.png', dpi = 1000)

## full chart ####
library(magick)
img1 <- image_read('grid_panel.png')
img2 <- image_read('map_panel2.png')
img2 <- image_resize(img2,"x1800")

img_list <- c(img1, img2)
img_final <- image_append(img_list)

image_write(img_final, 'day_10_11.png')
