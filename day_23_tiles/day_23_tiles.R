# day 23: tiles
# sentiment analysis of slaughterhouse-five

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)
library(pdftools)
library(tidytext)
library(showtext)

font_add_google(name = 'Inter', bold.wt = 600, family = 'Inter')
font_add_google(name = 'Abril Fatface', bold.wt = 600, family = 'Abril')
showtext_auto()


# https://content.ikon.mn/banners/2015/4/9/1471/Vonnegut_-_Cat_s_Cradle.pdf
# https://antilogicalism.com/wp-content/uploads/2018/04/slaughterhouse-five.pdf

file <- 'slaughterhouse-five.pdf'
#file <- 'Vonnegut_Cats_Cradle.pdf'
text <- pdf_text(file)
text <- as.data.frame(text)

# unnest, get sentiments
# using afinn to get numerical values
text_tokens <- text |>
  unnest_tokens(word, text) |>
  #count(word, sort = TRUE) |>
  #filter(!word %in% stop_words$word) |>
  inner_join(get_sentiments("afinn")) |>
  mutate(index = row_number()) 

# add index every 10 rows
text_tokens$index_10 <- c(0, rep(1:(nrow(text_tokens)-1)%/%10))
text_sum <- text_tokens |>
  # absolute values
  mutate(value_abs = abs(value)) |>
  group_by(index_10) |>
  mutate(word_max = word[which.max(value_abs)]) |>
  group_by(index_10, word_max) |>
  summarise(mean_val = mean(value)) 

library(ggwaffle)
text_waffle <- waffle_iron(text_sum, aes_d(group = index_10))

text_plot <- cbind(text_sum, text_waffle)

title_df <- tibble(text = 'So it goes.',
                   x = -2,
                   y = -1)

colors <- c(      "#7d4e3b", 
                  "#de8462", 
                  "#d4a178", 
                  "#cbcfc5", 
                  "#79be98", 
                  "#9cc1a2",
                  "#518568"
)


ggplot() +
  geom_tile(data = text_plot,
            aes(x = y, y = x, fill = mean_val),
            color='#262F2F', linewidth=.2) +
  geom_text(data = title_df, aes(x = x, y = y, label = text), 
            angle = -90, hjust = -.05, size = 90,
            family = 'Abril', fontface = 'bold',
            color = '#cbcfc5') +
  scale_fill_gradientn(colors = colors) +
  labs(caption = "Mean sentiment across every 10 words\nof Kurt Vonnegut's Slaughterhouse-Five") +
  xlim(c(-5, 9)) +
  scale_y_reverse() +
  coord_equal() +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(color = 'white', size = 20,
                                   family = 'Inter'),
        plot.caption = element_text(family = 'Inter', color = 'white', 
                                    size = 34, lineheight = .3, hjust=-1.7),
        plot.background = element_rect(fill = '#262F2F', color=NA),
        legend.key.width = unit(dev.size()[1] / 20, "inches"))

ggsave('day_23_tiles.png', height = 8, width = 4)

## plotly ####
library(plotly)
a <- ggplot() +
  geom_tile(data = text_plot,
            aes(x = y, y = x, fill = mean_val, text = word_max)) +
  scale_fill_gradientn(colors = colors,
                       name = 'Mean sentiment\n / 10 words') +
  scale_y_reverse() +
  labs(title = 'Sentiment analysis per 10 words in Slaughterhouse-Five') +
  coord_fixed(.3) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(color = 'black'),
        legend.key.width = unit(dev.size()[1] / 30, "inches"))



ggplotly(a, tooltip = c("text")) |>
  layout(annotations = 
           list(x = .0, y = 1, text = "Word displayed when hovering over each cell is the most emotionally charged in that set of 10", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='left', yanchor='auto', xshift=0, yshift=0,
                font=list(size=15, color="black"))
  )






