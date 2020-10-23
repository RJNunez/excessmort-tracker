# -- Libraries
library(excessmort)
library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
load("rda/counts-usa.rda")

# -- Retrieving mortality data from FT
url <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv"
dat <- fread(url) %>% 
  as_tibble() %>%
  mutate(date = ymd(date))

# # -- Countries to take out because of insuficient data
# out <- c("Brazil")

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
excess_deaths_countries <- left_join(excess_deaths_countries, eudat, by=c("date", "country"))


# -- Using CDC usa data (percent change)
usa_temp <- percent_change_usa %>%
  filter(type == "CDC weighted") %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  select(date, expected, observed, fitted, se, lwr, upr) %>%
  mutate(country = "United States")

# -- Using CDC usa data (excess deaths)
ed_temp <- excess_deaths_usa %>%
  filter(type == "CDC weighted") %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  select(date, observed, sd, fitted, se, lwr, upr) %>%
  mutate(country = "United States")

# -- Adding it to percent change
percent_change_countries <- percent_change_countries %>%
  filter(country != "United States of America") %>%
  bind_rows(usa_temp)

# -- Adding it to excess deaths
excess_deaths_countries <- excess_deaths_countries %>%
  filter(country != "United States of America") %>%
  bind_rows(ed_temp)

# -- Save
save(percent_change_countries, excess_deaths_countries, file = "rda/ft_counts.rda", compress = "xz")

