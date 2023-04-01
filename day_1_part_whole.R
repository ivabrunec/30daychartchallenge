# day 1: parts to a whole
# toronto's makeup

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Rubik', family = 'Rubik')
font_add_google(name = 'Vidaloka', family = 'Vidaloka')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

census_data <- read.csv('https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/current-actuelle.cfm?Lang=E&Geo1=POPC&Code1=0944&Geo2=PR&Code2=35&B1=All&type=0&FILETYPE=CSV', skip = 1, sep=',')
census_data <- census_data[!is.na(as.numeric(census_data$Total)), ]

# keep only first 9 columns: data for toronto
# then filter to only look at minority info
census_data_to <- census_data |>
  select(1:9) |>
  filter(Topic == 'Ethnic origin population') |>
  # next row looks wild but this is how hierarchy is indicated in these data
  filter(!str_detect(Characteristics, '    ')) |>
  filter_all(any_vars(is.na(.))) 

census_data_to$Total <- as.numeric(census_data_to$Total)
# now prorate to 10,000 people (100x100 grid)
census_data_to <- census_data_to |>
  mutate(total_count = sum(Total)) |>
  mutate(prop_population = ceiling((Total / total_count) * 1021),0)

data_long <- uncount(census_data_to, prop_population)
# this is not elegant but let's drop the last row so we have an even 10k
#data_long <- head(data_long, - 1)

# generate two vectors of random values between 1 & 100 
coord_y <- rep(32:1,each=32)
coord_x <- rep(seq(1,32,by=1),32)

# cut off at even number of entries
coord_y <- head(coord_y, - 1)
coord_x <- head(coord_x, - 1)

# shuffle rows so that tiles are not adjacent
shuffled_data_long= data_long[sample(1:nrow(data_long)), ]


shuffled_data_long$coord_x <- coord_x
shuffled_data_long$coord_y <- coord_y

library(paletteer)
col_pal <- paletteer_d("PNWColors::Starfish")
col_pal[8] <- 'black'

col_pal = rev(colorspace::sequential_hcl(8, palette = "Batlow"))

shuffled_data_long <- shuffled_data_long |>
  arrange(Total) |>  
  mutate(levels=Characteristics)

# now plot
ggplot() +
  geom_tile(data = shuffled_data_long, aes(x = coord_x, y = coord_y,
                                   fill = fct_reorder(Characteristics, Total)),
             size = 2) +
  coord_equal() +
  scale_fill_manual(values = col_pal) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  labs(title = 'Out of every 1,000 Torontonians ...') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(family = 'Rubik', size = 30),
        plot.title = element_text(family = 'Vidaloka', size = 70,
                                  lineheight = .3, hjust = .2),
        plot.background = element_rect(fill = 'grey96', color=NA)) 


ggsave('temp1.png', width = 6, height = 8, dpi = 300)


# plot sorted
ggplot() +
  geom_tile(data = data_long, aes(x = coord_x, y = coord_y,
                                           fill = fct_reorder(Characteristics, Total)),
            size = .1, color = 'grey96') +
  coord_equal() +
  scale_fill_manual(values = col_pal) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  labs(title = 'Out of every 1,000 Torontonians ...') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(family = 'Rubik', size = 30),
        plot.title = element_text(family = 'Vidaloka', size = 70,
                                  lineheight = .3, hjust = .2),
        plot.background = element_rect(fill = 'grey96', color=NA)) 

ggsave('temp2.png', width = 6, height = 8, dpi = 300)

