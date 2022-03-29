# -- Libraries
library(rnaturalearthdata)
library(rnaturalearth)
library(directlabels)
library(shinyWidgets)
library(shinythemes)
library(excessmort)
library(lubridate)
library(tidyverse)
library(shinyjs)
library(scales)
# library(cansim)
library(DT)
library(sf)

# -- Theme for figures
source("theme_sandstone.R")

# -- My color palette
my_palette <- c("#252525", "#cb181d", "#2171b5", "#6a51a3", "#238b45", "#6a51a3")

# -- Loading data
# load("rda/cdc_counts.rda")
# load("rda/counts-usa.rda")
# load("rda/ft_counts.rda")
load("rda/dat.rda")
states     <- sort(unique(percent_change_states$jurisdiction))
countries  <- sort(unique(percent_change_countries$jurisdiction))

# # -- Aggregating NY data for percent change map ---------------------------------------------------------------
# ny_dat <- percent_change %>%
#   filter(type == "weighted") %>%
#   select(date, jurisdiction, fitted) %>%
#   filter(grepl("New York", jurisdiction))
# 
# # -- New york population
# ny_pop <- cdc_counts %>%
#   filter(grepl("New York", jurisdiction)) %>%
#   select(date, jurisdiction, population)
# 
# # -- New york is a composite of NYC and rest of NY with weights proportional to pop size
# ny_dat <- ny_dat %>%
#   left_join(ny_pop, by = c("date", "jurisdiction")) %>%
#   arrange(date, jurisdiction) %>%
#   group_by(date) %>%
#   mutate(proportion = population / sum(population)) %>%
#   mutate(fitted = fitted * proportion) %>%
#   summarize(jurisdiction = "New York",
#             fitted       = sum(fitted)) %>%
#   ungroup()
# # -- END Aggregating NY data for percent change map ---------------------------------------------------------------
# 
# # -- We compute this for the usa excess deaths map. This way, we don't have to compute this for every iteration
# usa_ed_dat <- get_excess_deaths(dat = cdc_counts, jurisdictions = sort(unique(cdc_counts$jurisdiction)), start = make_date(2020,03,01), end = last(sort(cdc_counts$date)) - 7)
# 
# # -- We compute this for the world excess deaths map. This way, we don't have to compute this for every iteration
# world_ed_dat <- get_excess_deaths(dat = world_counts, jurisdictions = sort(unique(world_counts$jurisdiction)), start = make_date(2020,03,01), end = last(sort(world_counts$date)))
# 
# # -- Aggregating NY data for excess deaths map ---------------------------------------------------------------
# ny_ed_dat <- usa_ed_dat %>%
#   select(date, jurisdiction, fitted) %>%
#   filter(grepl("New York", jurisdiction))
# 
# ny_ed_dat <- ny_ed_dat %>%
#   left_join(ny_pop, by = c("date", "jurisdiction")) %>%
#   arrange(date, jurisdiction) %>%
#   group_by(date) %>%
#   mutate(proportion = population / sum(population)) %>%
#   mutate(fitted = fitted * proportion) %>%
#   summarize(jurisdiction = "New York",
#             fitted       = sum(fitted),
#             population   = sum(population)) %>%
#   ungroup()
# # -- End Aggregating NY data for excess deaths map ---------------------------------------------------------------

# -- USA map
us_map <- st_read("data/cb_2018_us_state_20m/cb_2018_us_state_20m.shp") %>%
  st_transform(crs = 4326) %>%
  st_crop(xmin = -125.0, xmax = -67.0, ymin = 20.0, ymax = 50.0)
us_map <- cbind(us_map, st_coordinates(st_centroid(us_map)))

# -- World map
world <- ne_countries(scale = "small", returnclass = "sf")
