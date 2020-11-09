# -- Libraries
library(tidyverse)
library(lubridate)
library(readxl)

## Get state popylations and NYC population
## Need to fix to avoid read an excel file
# -- DATA FROM: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html
# -- https://www1.nyc.gov/site/planning/planning-level/nyc-population/current-future-populations.page

# -- Loading and wrangling data
pop_states <- read_excel("data/nst-est2019-01.xlsx") %>%
  select(-c(2,3)) %>%
  slice(c(9:61)) %>%
  setNames(c("state", paste(2010:2019))) %>%
  mutate(state  = substr(state, 2, 15),
         `2010` = as.numeric(`2010`)) %>%
  gather(year, population, -state) %>%
  arrange(state, year) %>%
  mutate(date = make_date(as.numeric(year), 07,01)) %>%
  select(state, date, population) %>%
  na.omit() %>%
  mutate(state = ifelse(state == "uerto Rico", "Puerto Rico", state))

# -- Data for NYC
population <- 10e5 * c(8.190, 8.273, 8.348, 8.399, 8.437, 8.468, 8.476, 8.438, 8.399)
date    <- make_date(2010:2018,07,01)
state   <- rep("NYC", 9)

# -- Joinning NYC population data
pop_states <- bind_rows(pop_states, tibble(state, date, population))

# -- Function used to interpolate (see below)
do_approx <- function(tab, dates){
  ############### -- PARAMETERS -- ###############
  ###### tab  : data frame with date and population columns
  ###### dates: vector of dates for interpolation
  
  # -- Variables to be return
  return(data.frame(date       = dates,
                    population = Hmisc::approxExtrap(as.numeric(tab$date), tab$population, xout=as.numeric(dates), rule=2)$y))
}

# -- Vector of dates to be used for interpolation
dates <- seq(ymd("2017-01-01"), ymd("2020-12-31"), by="days")

# -- Example of interpolation
pop_states <- pop_states %>%
  group_by(state) %>%
  do(do_approx(., dates)) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  select(date, year, state, population)

# -- Type of estimate
the_type <- "Predicted (weighted)"

# -- Fixing some names
pop_states <- pop_states %>% 
  mutate(state = ifelse(state == "NYC", "New York City", state),
         state = ifelse(state == "District of Co", "District of Columbia", state))

# -- The CDC dataset divides NY into NYC and the rest of NY, so we need to subtract population
ny  <- filter(pop_states, state == "New York")
nyc <- filter(pop_states, state == "New York City")
ny  <- left_join(ny, nyc, by = "date") %>%
  mutate(population = population.x - population.y)
pop_states <- pop_states %>% 
  mutate(population = ifelse(state == "New York", 
                             ny$population, population))

# -- Getting CDC data 
dat <- RSocrata::read.socrata(
  "https://data.cdc.gov/resource/xkkf-xrst.json",
  app_token = "618rYGE542xoVmLBkMLm6mbEm",
  email     = "rafa@ds.dfci.harvard.edu",
  password  = "Ae&KE4Hz") %>%
  mutate_at(c("observed_number",  "excess_lower_estimate", "excess_higher_estimate"), as.numeric) %>% 
  filter(replace_na(suppress, "") != "Suppressed (counts 1-9)") %>%
  select("week_ending_date", "state", "observed_number", "type", "outcome", "excess_lower_estimate", "excess_higher_estimate") %>%
  setNames(c("date", "jurisdiction", "outcome",  "type", "cause", "cdc_lower_estimate", "cdc_higher_estimate")) %>%
  mutate(date = ymd(date)) %>%
  arrange(jurisdiction, date) %>%
  mutate(abb = state.abb[match(jurisdiction, state.name)]) %>%
  filter(cause == "All causes",
         !is.na(outcome),
         !is.na(date)) %>%
  select(jurisdiction, date, type, outcome) %>%
  pivot_wider(names_from = type, values_from = outcome) %>%
  setNames(c("jurisdiction", "date", "outcome", "outcome_unweighted"))

# -- Getting rid of the US since its just the sum of all the states
dat <- filter(dat, jurisdiction != "United States")
dat <- dat %>% left_join(pop_states, by = c("jurisdiction"="state","date")) %>% select(-year)
dat <- dat %>% mutate(jurisdiction = ifelse(jurisdiction == "New York", "New York (not including NYC)", jurisdiction))

# -- Expand state abbrevaition objects 
state.name.2 <- c(state.name, "New York City", "Puerto Rico", "District of Columbia")
state.abb.2  <- c(state.abb, "NYC", "PR", "DC")

# -- Importing covd-19 reported deaths data 
covid_nyc    <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_nyc    <- filter(covid_nyc, county =="New York City")
covid_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  mutate(abb = state.abb.2[match(state, state.name.2)]) %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(state)) %>%
  arrange(state) %>%
  rename(jurisdiction = state)

# -- Subsetting new york data
ny <- filter(covid_states, jurisdiction == "New York")

# -- Covid 19 data for the rest of new york
ny <- left_join(ny, covid_nyc, by = "date") %>%
  mutate(death = deaths.x - deaths.y, 
         jurisdiction = "Rest of New York") %>%
  select(date, jurisdiction, death)

# -- Covid 19 data for states
covid_states <- filter(covid_states, jurisdiction!="New York") %>%
  rename(death = deaths) %>%
  select(date, jurisdiction, death)

# -- Covid 19 for NYC
covid_nyc <- covid_nyc %>%
  mutate(jurisdiction = "New York City", death = deaths)%>%
  select(date, jurisdiction, death)

# -- Putting data together
covid_states <- bind_rows(covid_states, ny, covid_nyc) %>%
  filter(!is.na(death))
rm(covid_nyc, ny)

# -- Renaming
cdc_counts <- covid_states %>%
  rename(covid19 = death) %>%
  right_join(dat, by = c("date", "jurisdiction"))

# -- Saving 
save(cdc_counts, file = "rda/cdc_counts.rda", compress = "xz")
