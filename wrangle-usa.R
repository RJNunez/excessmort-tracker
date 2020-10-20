# -- Libraries
library(excessmort)
library(tidyverse)
library(lubridate)

# -- Puerto Rico hurricane dates
hurricane_dates  <- c(Hugo    = make_date(1989, 9, 18),
                      Georges = make_date(1998, 9, 21),
                      Maria   = make_date(2017, 9, 20))


# Excluding  1) years (starting in 7/1) that include events, 2) 2020 and 3) some outliers in 2001 -------------------
exclude_dates <- c(make_date(1989, 7, 1) + 0:365,
                   make_date(1998, 7, 1) + 0:365,
                   make_date(2017, 7, 1) + 0:365,
                   make_date(2014, 7, 1) + 0:365,
                   seq(make_date(2001, 1, 1), make_date(2001, 1, 15), by = "day"),
                   seq(make_date(2020, 1, 1), today(), by = "day"))


# define control regions --------------------------------------------------
control_dates <- seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by = "day")

# -- Loading data
source("wrangle-cdc.R")

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
  arrange(state)

# -- Subsetting new york data
ny <- filter(covid_states, state == "New York")

# -- Covid 19 data for the rest of new york
ny <- left_join(ny, covid_nyc, by = "date") %>%
  mutate(death = deaths.x - deaths.y, 
         state = "Rest of New York") %>%
  select(date, state, death)

# -- Covid 19 data for states
covid_states <- filter(covid_states, state!="New York") %>%
  rename(death = deaths) %>%
  select(date, state, death)

# -- Covid 19 for NYC
covid_nyc <- covid_nyc %>%
  mutate(state = "New York City", death = deaths)%>%
  select(date, state, death)

# -- Putting data together
covid_states <- bind_rows(covid_states, ny, covid_nyc) %>%
  filter(!is.na(death))
rm(covid_nyc, ny)

# -- Denoting periods of interest
flu_season     <- seq(make_date(2017, 12, 16), make_date(2018, 1, 16), by = "day")
exclude_dates  <- c(flu_season, seq(make_date(2020, 1, 1), today(), by = "day"))
max_weighted   <- last(cdc_counts$date) - weeks(1)
max_unweighted <- max_weighted - weeks(6)

# -- Remove last dates
weight   <- cdc_counts %>% filter(date <= max_weighted)
unweight <- cdc_counts %>% filter(date <= max_unweighted)
states   <- unique(weight$state)
states   <- setdiff(states, c("Connecticut", "North Carolina"))

# -- Percent change per state
nknots <- 16
percent_change <- map_df(states, function(x){
  if(x == "Puerto Rico"){
    exclude_dates <- unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day"))))
  }
  print(x)
  w <- weight %>% 
    filter(state == x) %>%
    na.omit() %>%
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
    mutate(state = x, 
           type  = "weighted")
  
  
  u <- unweight %>% 
    filter(state == x) %>%
    select(-outcome) %>%
    rename(outcome = outcome_unweighted) %>%
    na.omit() %>%
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
    mutate(state = x, 
           type  = "unweighted")
  
  bind_rows(w, u)
}) %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Percent change usa
percent_change_usa <- percent_change %>%
  filter(state != "Puerto Rico") %>%
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
    filter(state == x) %>%
    na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(weight$date),
                 end            = max_weighted,
                 aic            = FALSE, 
                 order.max      = 7,
                 knots.per.year = nknots,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  w <- excess_cumulative(w, start = make_date(2020, 03, 01), end = max_weighted) %>%
    mutate(state = x, type = "weighted")
  
  u <- unweight %>% 
    filter(state == x) %>%
    select(-outcome) %>%
    rename(outcome = outcome_unweighted) %>%
    na.omit() %>%
    excess_model(exclude        = exclude_dates,
                 start          = min(unweight$date),
                 end            = max_unweighted,
                 aic            = FALSE, 
                 knots.per.year = nknots,
                 order.max      = 7,
                 weekday.effect = FALSE,
                 verbose        = FALSE)
  
  u <- excess_cumulative(u, start = make_date(2020, 03, 01), end = max_unweighted) %>%
    mutate(state = x, type = "unweighted")
  
  bind_rows(w, u)
}) %>% 
  as_tibble() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Excess deaths usa
excess_deaths_usa <- excess_deaths %>%
  filter(state != "Puerto Rico") %>%
  group_by(date, type) %>% 
  summarize(observed = sum(observed),
            sd       = sqrt(sum(sd^2)),
            fitted   = sum(fitted),
            se       = sqrt(sum(se^2))) %>%
  ungroup() %>%
  mutate(type = ifelse(type == "weighted", "CDC weighted", "CDC unweighted"))

# -- Saving 
save(percent_change, percent_change_usa, excess_deaths, excess_deaths_usa, 
     file = "rda/covid19-usa.rda", compress = "xz")