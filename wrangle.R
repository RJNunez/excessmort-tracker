##############################################################################################
### ---------------------------------- WRANGLE CDC DATA ---------------------------------- ###
##############################################################################################
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
##############################################################################################
### -------------------------------- END WRANGLE CDC DATA -------------------------------- ###
##############################################################################################

##############################################################################################
### ---------------------------------- WRANGLE USA DATA ---------------------------------- ###
##############################################################################################
# -- Libraries
library(excessmort)
library(tidyverse)
library(lubridate)

# -- Puerto Rico hurricane dates
hurricane_dates  <- c(Hugo    = make_date(1989, 9, 18),
                      Georges = make_date(1998, 9, 21),
                      Maria   = make_date(2017, 9, 20))


# Excluding  1) years (starting in 7/1) that include events, 2) 2020 and 3) some outliers in 2001 
exclude_dates <- c(make_date(1989, 7, 1) + 0:365,
                   make_date(1998, 7, 1) + 0:365,
                   make_date(2017, 7, 1) + 0:365,
                   make_date(2014, 7, 1) + 0:365,
                   seq(make_date(2001, 1, 1), make_date(2001, 1, 15), by = "day"),
                   seq(make_date(2020, 1, 1), today(), by = "day"))


# define control regions 
control_dates <- seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by = "day")

# # -- Loading data
# source("wrangle-cdc.R")

# -- Denoting periods of interest
flu_season     <- seq(make_date(2017, 12, 16), make_date(2018, 1, 16), by = "day")
exclude_dates  <- c(flu_season, seq(make_date(2020, 1, 1), today(), by = "day"))
max_weighted   <- max(cdc_counts$date) - weeks(1)
max_unweighted <- max_weighted - weeks(6)

# -- Remove last dates
weight   <- cdc_counts %>% filter(date <= max_weighted) %>% arrange(date)
unweight <- cdc_counts %>% filter(date <= max_unweighted) %>% arrange(date)
states   <- unique(weight$jurisdiction)
states   <- setdiff(states, c("Connecticut", "North Carolina"))

# -- Percent change per state
nknots <- 16
percent_change <- map_df(states, function(x){
  if(x == "Puerto Rico"){
    exclude_dates <- unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day"))))
  }
  print(x)
  w <- weight %>% 
    filter(jurisdiction == x) %>%
    # na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(weight$date),
                 end            = max_weighted,
                 aic            = FALSE, 
                 order.max      = 7,
                 knots.per.year = nknots,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  w <- with(w,
            tibble(date     = date, 
                   expected = expected, 
                   observed = observed,
                   fitted   = fitted, 
                   se       = se,
                   sd       = sd)) %>%
    mutate(jurisdiction = x, 
           type         = "weighted")
  
  
  u <- unweight %>% 
    filter(jurisdiction == x) %>%
    select(-outcome) %>%
    rename(outcome = outcome_unweighted) %>%
    # na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(unweight$date),
                 end            = max_unweighted,
                 aic            = FALSE, 
                 order.max      = 7,
                 knots.per.year = nknots,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  u <- with(u,
            tibble(date     = date, 
                   expected = expected, 
                   observed = observed,
                   fitted   = fitted, 
                   se       = se,
                   sd       = sd)) %>%
    mutate(jurisdiction = x, 
           type         = "unweighted")
  
  bind_rows(w, u)
}) %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Percent change usa
percent_change_usa <- percent_change %>%
  filter(jurisdiction != "Puerto Rico") %>%
  group_by(date, type) %>% 
  summarize(fitted   = sum(expected * fitted) / sum(expected), 
            se       = sqrt(sum(expected^2 * se^2)) / sum(expected),
            sd       = sqrt(sum(expected^2 * sd^2)) / sum(expected),
            expected = sum(expected),
            observed = sum(observed)) %>%
  ungroup() %>%
  mutate(type = ifelse(type == "weighted", "CDC weighted", "CDC unweighted"))

# -- Excess deaths per state
excess_deaths <- map_df(states, function(x){
  if(x == "Puerto Rico"){
    exclude_dates <- unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day"))))
  }
  print(x)
  w <- weight %>% 
    filter(jurisdiction == x) %>%
    # na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(weight$date),
                 end            = max_weighted,
                 aic            = FALSE, 
                 order.max      = 7,
                 knots.per.year = nknots,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  w <- excess_cumulative(w, start = make_date(2020, 03, 01), end = max_weighted) %>%
    mutate(jurisdiction = x, type = "weighted")
  
  u <- unweight %>% 
    filter(jurisdiction == x) %>%
    select(-outcome) %>%
    rename(outcome = outcome_unweighted) %>%
    # na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(unweight$date),
                 end            = max_unweighted,
                 aic            = FALSE, 
                 knots.per.year = nknots,
                 order.max      = 7,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  u <- excess_cumulative(u, start = make_date(2020, 03, 01), end = max_unweighted) %>%
    mutate(jurisdiction = x, type = "unweighted")
  
  bind_rows(w, u)
}) %>% 
  as_tibble() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  left_join(covid_states, by = c("date", "jurisdiction")) %>%
  rename(covid19 = death)

# -- Excess deaths usa
excess_deaths_usa <- excess_deaths %>%
  filter(jurisdiction != "Puerto Rico") %>%
  group_by(date, type) %>% 
  summarize(observed = sum(observed),
            covid19  = sum(covid19, na.rm = TRUE),
            sd       = sqrt(sum(sd^2)),
            fitted   = sum(fitted),
            se       = sqrt(sum(se^2))) %>%
  ungroup() %>%
  mutate(type = ifelse(type == "weighted", "CDC weighted", "CDC unweighted"))

# -- Saving 
save(covid_states, percent_change, percent_change_usa, excess_deaths, excess_deaths_usa,
     file = "rda/counts-usa.rda", compress = "xz")
##############################################################################################
### -------------------------------- END WRANGLE USA DATA -------------------------------- ###
##############################################################################################

##############################################################################################
### ----------------------------------- WRANGLE FT DATA ---------------------------------- ###
##############################################################################################
# -- Libraries
library(excessmort)
library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
# load("rda/counts-usa.rda")

# -- Retrieving mortality data from FT
url <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv"
dat <- fread(url) %>% 
  as_tibble() %>%
  mutate(date = ymd(date))

# -- Subsetting data. Only considering country level data for now
dat <- filter(dat, country == region) %>%
  select(date, deaths, expected_deaths, country) %>%
  rename(outcome = deaths) %>%
  mutate(country = case_when(country=="US" ~ "United States of America",
                             country=="UK" ~ "United Kingdom",
                             TRUE ~ country))

# -- Available countries
countries <- unique(dat$country)

# -- Loading population level data
pop <- read_excel("data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")

# -- Retrieving columns names
names <- pop[12,]

# -- Wrangle
pop <- pop %>%
  slice(-c(1:12)) %>%
  setNames(names) %>%
  select(`Region, subregion, country or area *`, contains("201"), `2020`) %>%
  filter(`Region, subregion, country or area *` %in% countries) %>%
  rename(country = `Region, subregion, country or area *`) %>%
  gather(year, population, -country) %>%
  mutate(population = as.numeric(population),
         population = population * 1000) %>%
  arrange(country, year)

# -- Interpolation population values
pop <- approx_demographics(pop, 
                           first_day = make_date(2010, 01, 01),
                           last_day  = make_date(2020, 12, 31),
                           by        = c("year", "country"))

# -- Putting everything together
counts <- left_join(dat, pop, by=c("date", "country"))

# -- Control and exclude dates
exclude       <- seq(make_date(2020, 01, 01), make_date(2020, 12, 31), "days")
control_dates <- seq(make_date(2010, 01, 01), make_date(2019, 12, 31), "days")

# -- Computing percent_change
percent_change_countries <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
                            filter(country == x) %>%
                            arrange(date) %>%
                            excess_model(.,
                                         start                = make_date(2020, 01, 01),
                                         end                  = today(),
                                         exclude              = exclude,
                                         control.dates        = control_dates,
                                         aic            = FALSE, 
                                         order.max      = 7,
                                         weekday.effect = FALSE,
                                         verbose        = FALSE))
  
  
  tibble(date = fit$date, expected = fit$expected, observed = fit$observed, fitted = fit$fitted, se = fit$se) %>%
    mutate(lwr = fitted - 1.96*se, 
           upr = fitted + 1.96*se, 
           country = x)
})

# -- Computing excess deaths
excess_deaths_countries <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
                            filter(country == x) %>%
                            arrange(date) %>%
                            
                            excess_model(.,
                                         start                = make_date(2020, 03, 01),
                                         end                  = today(),
                                         exclude              = exclude,
                                         control.dates        = control_dates,
                                         aic            = FALSE, 
                                         order.max      = 7,
                                         weekday.effect = FALSE,
                                         verbose        = FALSE))
  
  
  excess_cumulative(fit, start = make_date(2020, 01, 01), end = make_date(2020, 06, 30)) %>%
    mutate(country = x) %>%
    mutate(lwr = fitted - 1.96 * se, 
           upr = fitted + 1.96 * se)
}) %>%
  as_tibble()

# -- Loading covid19 data from european center for disease control (ECDC)
eudat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                  na.strings = "", stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM") %>%
  as_tibble() %>%
  mutate(date = dmy(dateRep)) %>%
  select(date, cases, deaths, countriesAndTerritories, popData2019) %>%
  rename(country = countriesAndTerritories) %>%
  arrange(date, country) %>%
  filter(deaths >= 0) %>%
  group_by(country) %>%
  mutate(covid19 = cumsum(deaths),
         country = gsub("_", " ", country)) %>%
  ungroup() %>%
  select(date, country, deaths, covid19)

# -- All countries
excess_deaths_countries <- left_join(excess_deaths_countries, eudat, by=c("date", "country")) %>%
  select(-deaths)

# -- Using CDC usa data (percent change)
usa_temp <- percent_change_usa %>%
  filter(type == "CDC weighted") %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  select(date, expected, observed, fitted, se, lwr, upr) %>%
  mutate(country = "United States")

# -- Using CDC usa data (excess deaths)
colnames(excess_deaths_countries)
ed_temp <- excess_deaths_usa %>%
  filter(type == "CDC weighted") %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  mutate(country = "United States") %>%
  select(date, observed, sd, fitted, se, country, lwr, upr, covid19)

# -- Adding it to percent change
percent_change_countries <- percent_change_countries %>%
  filter(country != "United States of America") %>%
  bind_rows(usa_temp)

# -- Adding it to excess deaths
excess_deaths_countries <- excess_deaths_countries %>%
  filter(country != "United States of America") %>%
  bind_rows(ed_temp)

percent_change_countries <- rename(percent_change_countries, jurisdiction = country)
excess_deaths_countries <- rename(excess_deaths_countries, jurisdiction = country)
world_counts <- rename(counts, jurisdiction = country)

# -- World counts
counts_tmp <- cdc_counts %>%
  group_by(date) %>%
  summarize(outcome    = sum(outcome), 
            population = sum(population)) %>%
  ungroup() %>%
  mutate(jurisdiction = "United States") %>%
  select(date, outcome, jurisdiction, population)
world_counts <- world_counts %>%
  filter(jurisdiction != "United States of America") %>%
  select(-expected_deaths) %>%
  bind_rows(counts_tmp)
world_counts <- eudat %>%
  mutate(country = ifelse(country == "United States of America", "United States", country)) %>%
  select(-deaths) %>%
  right_join(world_counts, by = c("date", "country" = "jurisdiction")) %>%
  rename(jurisdiction = country)

# -- Save
the_stamp <- now()
save(world_counts, percent_change_countries, excess_deaths_countries, the_stamp, file = "rda/ft_counts.rda", compress = "xz")
##############################################################################################
### --------------------------------- END WRANGLE FT DATA -------------------------------- ###
##############################################################################################

