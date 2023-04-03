# day 3: flora & fauna
# tree root lengths

# data from https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-hfr&identifier=426

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(ggtext)

tree_data <- read.csv('https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fknb-lter-hfr%2F426%2F1%2F0489376c79e52f4c80aacb64d6d3807e') |>
  filter(Growth_form == 'Tree')

# average per species, some have multiple entries
trees_sum <- tree_data |>
  filter(Species != '' & Family != '') |>
  group_by(Family) |>
  summarise(mean_root = mean(Dr, na.rm=T), 
            mean_spread = mean(Lr, na.rm=T),
            count=n()) |>
  ungroup() |>
  arrange(mean_root) |>
  tidyr::drop_na(mean_spread) |>
  mutate(id = row_number()*10,
         root_depth = mean_root * (-1),
         depth_spread_ratio = mean_spread / mean_root)

# build the list of trees we want to highlight
# rhamnaceae: buckthorn family: lowest spread-to-depth ratio
# tamaricaceae: tamarisk family: greatest lateral root spread
# Lecythidaceae: brazil nut family: greatest spread-to-depth ratio
# Moraceae: mulberry family: deepest roots
# Taxaceae: yew family: shallowest roots

highlight_list <- c('Rhamnaceae', 'Lecythidaceae', 'Tamaricaceae','Moraceae','Taxaceae')

highlight_df <- trees_sum |>
  filter(Family %in% highlight_list)

trees_other <- trees_sum |>
  filter(!Family %in% highlight_list)

highlight_col <- '#73fec1'
text_col <- '#cad1bb'

ggplot() +
  geom_segment(data = trees_other, 
               aes(x=id, xend=id, y=0, yend=root_depth),
               color = text_col) +
  geom_point(data = trees_other,
             aes(x = id, y = root_depth, size = mean_spread),
             alpha = .3, color = text_col) +
  geom_segment(data = highlight_df,
               aes(x=id, xend=id, y=0, yend=root_depth),
               color = highlight_col) +
  ggshadow::geom_glowpoint(data = highlight_df,
             aes(x = id, y = root_depth, size = mean_spread),
             alpha = .6,
             color = highlight_col, shadowsize = .2) +
  scale_size(range = c(0, 10)) +
  geom_segment(data = highlight_df, aes(x = id, xend=id, 
                                        y = 0, yend=10),
               color = highlight_col, linewidth = .1,
               linetype = 'dashed') +
  geom_text(data = trees_other,
            aes(x = id, y = 1, label = Family), 
            angle = 60, hjust = 0, size = 3,
            color = text_col) +
  geom_text(data = highlight_df,
            aes(x = id, y = 1, label = Family), 
            angle = 60, hjust = 0, size = 4,
            color = highlight_col) +
  scale_y_continuous(breaks = c(-10,-5,0)) +
  geom_hline(yintercept = 0, color = text_col) +
  coord_cartesian(ylim=c(-10, 15), xlim=c(1,400)) +
  labs(title = "Beneath the surface: the hidden world of tree roots",
       subtitle = 'Root depth and lateral spread',
       caption = 'Data from: ') +
  annotate("text", label= "Yews:\nshallowest roots", 
           x= 0, y = 10, hjust = 0, color = highlight_col,
           lineheight =.7) +
  annotate("text", label= "Brazil nuts:\ngreatest width-to-depth ratio", 
           x= 105, y = 10, hjust = 0, color = highlight_col,
           lineheight =.7) +
  annotate("text", label= "Tamarisks:\ngreatest lateral width", 
           x= 265, y = 10, hjust = 0, color = highlight_col,
           lineheight =.7) +
  annotate("text", label= "Buckthorns:\nlowest width-to-depth ratio", 
           x= 275, y = 12, hjust = 0, color = highlight_col,
           lineheight =.7) +
  annotate("text", label= "Mulberries:\ndeepest roots", 
           x= 355, y = 10, hjust = 0, color = highlight_col,
           lineheight =.7) +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.background = element_rect(fill = '#092034'),
        text = element_text(color = '#cad1bb'),
        axis.text.y = element_text(color=text_col),
        plot.title = element_text(hjust = 0, family = )) 
  
ggsave('temp_plot.png', width = 8, height = 6, dpi = 400)

# now add little pictures
library(magick)

plot <- image_read('temp_plot.png')
yew <- image_read('yew.png')
br_nut <- image_read('brazilnut.png')
tam <- image_read('tamarisk.png')
buck <- image_read('buckthorn.png')
mul <- image_read('mulberry.png')

image1 <- image_composite(plot, image_scale(yew,"150x150"), offset = "+390+450")
image2 <- image_composite(image1, image_scale(br_nut,"150x150"), offset = "+1300+460")
image3 <- image_composite(image2, image_scale(tam,"150x150"), offset = "+2000+540")
image4 <- image_composite(image3, image_scale(buck,"150x150"), offset = "+2400+320")
image_final <- image_composite(image4, image_scale(mul,"150x150"), offset = "+3000+630")

image_write(image_final, 'test_final.png')
