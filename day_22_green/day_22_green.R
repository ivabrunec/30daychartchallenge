# day 22: green energy
# sources of electricity in Canada

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Inter', bold.wt = 500, family = 'Inter')
showtext_auto()

# download data
temp <- tempfile()
download.file("https://www150.statcan.gc.ca/n1/tbl/csv/25100015-eng.zip",temp)
data <- read.csv(unz(temp, "25100015.csv"))
unlink(temp)

data_sum <- data |>
  # filter out only total electricity production
  filter(Class.of.electricity.producer == 'Total all classes of electricity producer') |>
  # separate year and month
  tidyr::separate(col = REF_DATE, into = c("year", "month")) |>
  # take only data 2016 and later, the way sources are coded changed at that point 
  filter(year >= 2016) |>
  # average per year, type, and province
  group_by(year,GEO, Type.of.electricity.generation) |>
  summarise(total_mwh = sum(VALUE, na.rm=T)) 

# clean up column names 
colnames(data_sum) <- c('year','name','elec_source','total_mwh')

# remove a few categories never present after 2016
# and summary categories (combustible fuels includes non-renewable)
categ_remove <- c('Combustion turbine', 'Conventional steam turbine',
                  'Internal combustion turbine',
                  'Total electricity production from non-renewable combustible fuels',
                  'Total all types of electricity generation')


data_clean <- data_sum |>
  filter(!elec_source %in% categ_remove) |>
  group_by(year, name) |>
  mutate(grand_total = sum(total_mwh, na.rm=T),
         elec_percentage = total_mwh / grand_total)

data_clean$year <- as.numeric(data_clean$year)

# clean up source labels
data_clean$elec_source <- gsub("Total electricity production from biomass", "Biomass", data_clean$elec_source)
data_clean$elec_source <- gsub("Other types of electricity generation", "Other", data_clean$elec_source)
data_clean$elec_source <- gsub("Total electricity production from combustible fuels", "Combustible fuels", data_clean$elec_source)

# get overall amount & sort
data_clean <- data_clean |>
  group_by(elec_source) |>
  mutate(overall_mwh = sum(total_mwh)) 

data_canada <- filter(data_clean, name == 'Canada')
data_prov_ter <- filter(data_clean, name != 'Canada')
  
library(geofacet)

# edited grid
mygrid <- data.frame(
  code	= c("NL", "PE", "NU", "NT", "YT", "NS", "NB", "QC", "ON", "MB", "AB", "SK", "BC"),
  name = c("Newfoundland and Labrador", "Prince Edward Island", "Nunavut", "Northwest Territories", "Yukon", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba", "Alberta", "Saskatchewan", "British Columbia"),
  row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  col = c(7, 8, 4, 2, 1, 8, 7, 6, 5, 4, 2, 3, 1),
  stringsAsFactors = FALSE
)

data_prov_ter <- merge(data_prov_ter, mygrid, by=c('name'))

ggplot(data = data_prov_ter) +
  geom_area(
            aes(x=year,y=elec_percentage,fill=forcats::fct_reorder(elec_source, overall_mwh, .desc = T))) +
  facet_geo(~name, grid = mygrid, 
            labeller = label_wrap_gen(width=10)) +
  ylab('Proportion of energy by source') +
  labs(caption = 'Data Source: Statistics Canada',
       title = 'Canada runs on...',
       subtitle = 'Hydro, renewables, and non-renewables as a proportion of total electricity generation')+
  scale_fill_manual(values = c('#CBEEF3', '#292929','#FF7F11','#7FD8BE',
                               '#FF1B1C','#FFFFFC','#BEB7A4','#ACBED8')) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        strip.text = element_text(lineheight = .25, family = 'Inter', face='bold', size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, family = 'Inter'),
        axis.text = element_text(size = 15, color = 'black', 
                                 family = 'Inter'),
        axis.text.x = element_text(angle = 50),
        plot.background = element_rect(fill = '#B7B7B7', color = NA),
        legend.key.size = unit(.2, 'cm'),
        legend.text = element_text(family = 'Inter', size = 20),
        plot.caption = element_text(size = 16, color = 'black', family = 'Inter'),
        plot.title = element_text(size = 60, color = 'black', family = 'Inter', 
                                  face='bold',lineheight = .1),
        plot.subtitle = element_text(size = 25, color = 'black', 
                                     family = 'Inter',lineheight = .1))

ggsave('day_22_green.png', width = 6, height = 4, dpi= 300)

