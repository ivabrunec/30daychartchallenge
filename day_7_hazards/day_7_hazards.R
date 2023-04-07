# day 7: hazards.
# endangered languages

# endangered languages project: https://www.endangeredlanguages.com/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

# read database file
lang_data <- read.csv('https://www.endangeredlanguages.com/userquery/download/',
                      header = F)

# manually add column names
# I'm not 100% sure about some of them (e.g. family vs dialect)
# can't find details on the columns on the site
colnames(lang_data) <- c('id','id_alt','language','lang_alt',
                         'status','num_speakers','lang_family',
                         'dialect_groups','notes','notes2',
                         'countries','region','coords')

# recode the status to simpler categories (removing uncertainty)
lang_data$status_simple <- sub("\\(.*", "", lang_data$status)

# recode region to simpler categories (keep only first listed)
lang_data$region <- sub("\\;.*", "", lang_data$region)
lang_data$region <- sub("Mexico, Central America, Caribbean", "Central America", lang_data$region)

# remove cases where status is unknown
lang_data <- filter(lang_data, status_simple != "" & region != "")

# keep only first language family to simplify plot
lang_data$lang_family <- sub("\\;.*", "", lang_data$lang_family)

## packed circle plot
library(circlepackeR)         
library(data.tree)
# following logic from: https://r-graph-gallery.com/338-interactive-circle-packing-with-circlepacker

# now, summarise by:
# group = region
# subgroup = status
# subsubgroup (MAYBE) = lang_family

lang_sum <- lang_data |>
  group_by(region, status_simple, lang_family) |>
  summarise(value = n()) |>
  ungroup()

lang_sum$pathString <- paste("world", 
                         lang_sum$status_simple,
                         lang_sum$region, 
                         lang_sum$lang_family,
                         sep = "/")
population <- as.Node(lang_sum)

circlepackeR(population, size = "value",
             color_min = "#003763", color_max = "#efe5ff")

