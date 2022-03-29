################################################################################################################################################################
### -- CDC MORTALTIY DATA ######################################################################################################################################
################################################################################################################################################################
# -- Libraries 
library(excessmort)
library(tidycensus)
library(tidyverse)
library(lubridate)
library(RSocrata)

################################################################################
### -- LOADING AND WRANGLING POPULATION DATA ###################################
################################################################################
# -- Census API key (may need to change this)
census_api_key("0382062eb1c992d6115c6c04455c1994d0c1f891")

# -- Getting yearly population estimates from the US Census ACS 5-year survey
years <- 2015:2020
population <- map_df(years, function(year) {
  
  # -- Population estimates from the ACS 5-year survey  
  tmp_population <- get_acs(geography = "state", 
                            variables = c(population = "B01001_001"),
                            year      = year, 
                            survey    = "acs5") %>%
    dplyr::select(NAME, estimate) %>%
    setNames(c("state", "population")) %>%
    mutate(date = make_date(year, 07, 01))
})

# -- Function used to interpolate (see below)
do_approx <- function(tab, dates){
  ############### -- PARAMETERS -- ###############
  ###### tab  : data frame with date and population columns
  ###### dates: vector of dates for interpolation
  
  ###### The function Hmisc::approxExtrap extrapolates using a linear model
  ###### based on the previous data points. The function approx, uses constant 
  ###### extrapolation based on the last population value. Change the function 
  ###### accordingly. A sensible sensitivity analysis would be to get results
  ###### with the constant population assumption.
  
  # -- Variables to be return
  return(data.frame(date       = dates,
                    population = round(Hmisc::approxExtrap(as.numeric(tab$date), tab$population, xout=as.numeric(dates), rule=2)$y)))
  # population = round(approx(as.numeric(tab$date), tab$population, xout=as.numeric(dates), rule=2)$y))
}

# -- Vector of dates to be used for inter/extra - polation
dates <- seq(ymd("2010-01-01"), today(), by="days")

# -- Example of interpolation
population <- population %>%
  group_by(state) %>%
  do(do_approx(., dates)) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  select(date, year, state, population)
################################################################################
### -- END LOADING AND WRANGLING POPULATION DATA ###############################
################################################################################

################################################################################
### -- LOADING AND WRANGLING MORTALITY DATA ####################################
################################################################################
# -- Getting mortality data from the CDC 
counts <- RSocrata::read.socrata(
  "https://data.cdc.gov/resource/xkkf-xrst.json",
  app_token = "618rYGE542xoVmLBkMLm6mbEm",
  email     = "rafa@ds.dfci.harvard.edu",
  password  = "Ae&KE4Hz") %>%
  as_tibble() %>%
  filter(type    == "Unweighted",
         outcome == "All causes",
         state   != "United States") %>%
  dplyr::select("week_ending_date", "state", "observed_number") %>%
  setNames(c("date", "state", "outcome")) %>%
  mutate(date      = ymd(date),
         outcome   = as.numeric(outcome),
         state_abb = state.abb[match(state, state.name)],
         state_abb = case_when(state == "District of Columbia" ~ "DC",
                               state == "New York City" ~ "NY",
                               state == "Puerto Rico" ~ "PR",
                               TRUE ~ state_abb),
         state = ifelse(state == "New York City", "New York", state)) %>%
  group_by(date, state, state_abb) %>%
  summarize(outcome = sum(outcome)) %>%
  ungroup() %>%
  arrange(state, date)
################################################################################
### -- END LOADING AND WRANGLING MORTALITY DATA ################################
################################################################################

################################################################################
### -- LOADING AND WRANGLING COVID DATA ########################################
################################################################################
# -- Loading and wrangling covid counts
covid_counts_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(state)) %>%
  arrange(state) %>%
  rename(jurisdiction = state,
         covid_cases  = cases,
         covid_deaths = deaths) %>%
  select(-fips)

# -- Population and mortality data together
cdc_counts <- counts %>%
  left_join(population, by = c("date", "state")) %>%
  select(date, state, state_abb, outcome, population) %>%
  rename(jurisdiction     = state,
         jurisdiction_abb = state_abb) %>%
  left_join(covid_counts_states, by = c("date", "jurisdiction"))
# cdc_counts_tmp <- cdc_counts
################################################################################
### -- END LOADING AND WRANGLING COVID DATA ####################################
################################################################################

################################################################################################################################################################
### -- END CDC MORTALTIY DATA ##################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################
### -- COMPUTING EXCESS MORTALITY METRICS FOR USA DATA #########################################################################################################
################################################################################################################################################################
# -- Denoting periods of interest
date_threshold <- max(cdc_counts$date) - lubridate::weeks(6)
flu_season     <- seq(make_date(2017, 12, 16), make_date(2018, 1, 16), by = "day")
exclude_dates  <- c(flu_season, seq(make_date(2020, 1, 1), ymd(date_threshold), by = "day"))

# -- Remove last dates and take unique jurisdictions
cdc_counts <- cdc_counts %>% filter(date <= date_threshold) %>% arrange(date)
states     <- unique(cdc_counts$jurisdiction)
# states   <- setdiff(states, c("Connecticut", "North Carolina"))

# -- Number of knots to use in model fitting
nknots <- 16

# -- Computing percent change per state
percent_change_states <- map_df(states, function(x){
  
  # -- Puerto Rico has different exclusion dates than the rest
  if(x == "Puerto Rico"){
    exclude_dates <- unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day"))))
  }

  # -- Data for xth jurisdictions
  tmp_counts <- filter(cdc_counts, jurisdiction == x) %>% 
    select(-covid_cases, -covid_deaths) %>%
    na.omit()
  
  # -- Model fit for xth jurisdiction
  tmp_fit <- excess_model(counts         = tmp_counts, 
                          exclude        = exclude_dates,
                          start          = min(tmp_counts$date),
                          end            = date_threshold,
                          model          = "quasipoisson",
                          knots.per.year = nknots,
                          verbose        = FALSE)
  
  # -- Putting results together
  out <- with(tmp_fit, 
              tibble(date, 
                     expected, 
                     log_expected_se,
                     observed = observed,
                     fitted, 
                     se,
                     sd)) %>%
    mutate(jurisdiction     = tmp_counts$jurisdiction[1],
           jurisdiction_abb = tmp_counts$jurisdiction_abb[1])
}) %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Percent change for the continental USA
percent_change_usa <- percent_change_states %>%
  filter(jurisdiction != "Puerto Rico") %>%
  group_by(date) %>% 
  summarize(fitted   = sum(expected * fitted) / sum(expected), 
            se       = sqrt(sum(expected^2 * se^2)) / sum(expected),
            sd       = sqrt(sum(expected^2 * sd^2)) / sum(expected),
            expected = sum(expected),
            observed = sum(observed)) %>%
  ungroup()

# -- Excess deaths per state
excess_deaths_states <- map_df(states, function(x){
  
  if(x == "Puerto Rico"){
    exclude_dates <- unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day"))))
  }
  
  # -- Data for xth jurisdictions
  tmp_counts <- filter(cdc_counts, jurisdiction == x) %>% 
    select(-covid_cases, -covid_deaths) %>%
    na.omit()
  
  # -- Model fit for xth jurisdiction
  tmp_fit <- excess_model(counts         = tmp_counts, 
                          exclude        = exclude_dates,
                          start          = min(tmp_counts$date),
                          end            = date_threshold,
                          model          = "quasipoisson",
                          knots.per.year = nknots,
                          verbose        = FALSE)
  
  # -- Computing excess mortality metrics
  out <- excess_cumulative(tmp_fit, start = make_date(2020, 03, 01), end = date_threshold) %>%
    mutate(jurisdiction = x)

}) %>% 
  as_tibble() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  left_join(covid_counts_states, by = c("date", "jurisdiction"))

# -- Excess deaths usa
excess_deaths_usa <- excess_deaths_states %>%
  filter(jurisdiction != "Puerto Rico") %>%
  group_by(date) %>% 
  summarize(observed      = sum(observed),
            covid_deaths  = sum(covid_deaths, na.rm = TRUE),
            sd            = sqrt(sum(sd^2)),
            fitted        = sum(fitted),
            se            = sqrt(sum(se^2))) %>%
  ungroup()
################################################################################################################################################################
### -- END COMPUTING EXCESS MORTALITY METRICS FOR USA DATA #####################################################################################################
################################################################################################################################################################

################################################################################################################################################################
### -- FT MORTALITY DATA #######################################################################################################################################
################################################################################################################################################################
# -- Retrieving mortality data from FT
url <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv"
dat <- data.table::fread(url) %>% 
  as_tibble() %>%
  filter(country == region,
         period  == "week") %>%
  select(date, country, deaths) %>%
  rename(outcome      = deaths,
         jurisdiction = country) %>%
  mutate(date         = ymd(date),
         jurisdiction = case_when(jurisdiction == "US" ~ "United States of America",
                                  jurisdiction == "UK" ~ "United Kingdom",
                                  jurisdiction == "S Korea" ~ "South Korea",
                                  TRUE ~ jurisdiction))

# -- Available countries
countries <- unique(dat$jurisdiction)

# # -- Downloading population data from the World bank
# population  <- wb_data(indicator   = c("pop" = "SP.POP.TOTL"), 
#                 start_date  = 2010, 
#                 end_date    = 2022, 
#                 return_wide = FALSE) %>%
#   select(country, iso3c, date, value) %>%
#   setNames(c("jurisdiction", "jurisdiction_abb", "year", "population")) %>%
#   mutate(date = make_date(year, 07, 01)) %>%
#   select(-year) %>%
#   group_by(jurisdiction, jurisdiction_abb) %>%
#   do(do_approx(., dates)) %>%
#   ungroup()

# -- Loading population level data from the UN
population <- readxl::read_excel("data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")

# -- Retrieving columns names
names <- population[12,]

# -- Wrangle
population <- population %>%
  slice(-c(1:12)) %>%
  setNames(names) %>%
  select(`Region, subregion, country or area *`, contains("201"), `2020`) %>%
  filter(`Region, subregion, country or area *` %in% countries) %>%
  rename(jurisdiction = `Region, subregion, country or area *`) %>%
  gather(year, population, -jurisdiction) %>%
  mutate(population = as.numeric(population),
         population = population * 1000,
         date       = make_date(year, 07, 01)) %>%
  arrange(jurisdiction, date) %>%
  select(-year) %>%
  group_by(jurisdiction) %>%
  do(do_approx(., dates)) %>%
  ungroup()
################################################################################################################################################################
### -- END FT MORTALITY DATA ###################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################
### -- COMPUTING EXCESS MORTALITY METRICS FOR INTERNATIONAL DATA ###############################################################################################
################################################################################################################################################################
# -- Putting everything together
world_counts <- left_join(dat, population, by=c("date", "jurisdiction")) %>%
  filter(!jurisdiction %in% c("South Korea", "Czech Republic"))

# -- Control and exclude dates
exclude <- seq(make_date(2020, 01, 01), today(), "days")

# -- Computing percent change for each country
percent_change_countries <- map_df(setdiff(countries, c("South Korea", "Czech Republic", "Mexico")), function(x){
  
  if(x == "Australia") {
    exclude = sort(unique(c(seq(ymd("2017-07-01"), ymd("2017-09-30"), by = "days"), exclude)))
  }
  
  # -- Data for xth jurisdictions
  tmp_counts <- filter(world_counts, jurisdiction == x) %>% 
    na.omit()
  
  # -- Model fit for xth jurisdiction
  tmp_fit <- excess_model(counts         = tmp_counts, 
                          exclude        = exclude,
                          start          = min(tmp_counts$date),
                          end            = date_threshold,
                          model          = "quasipoisson",
                          knots.per.year = nknots,
                          verbose        = FALSE)
  
  # -- Putting results together
  out <- with(tmp_fit, 
              tibble(date, 
                     expected, 
                     log_expected_se,
                     observed = observed,
                     fitted, 
                     se,
                     sd)) %>%
    mutate(jurisdiction = tmp_counts$jurisdiction[1])
}) %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Computing excess deaths for each country
excess_deaths_countries <- map_df(setdiff(countries, c("South Korea", "Czech Republic", "Mexico")), function(x){
  
  if(x == "Australia") {
    exclude = sort(unique(c(seq(ymd("2017-07-01"), ymd("2017-09-30"), by = "days"), exclude)))
  }

  # -- Data for xth jurisdictions
  tmp_counts <- filter(world_counts, jurisdiction == x) %>% 
    na.omit()

  # -- Model fit for xth jurisdiction
  tmp_fit <- excess_model(counts         = tmp_counts, 
                          exclude        = exclude_dates,
                          start          = make_date(2020, 03, 01),
                          end            = date_threshold,
                          model          = "quasipoisson",
                          knots.per.year = nknots,
                          verbose        = FALSE)
  
  # -- Computing excess mortality metrics
  out <- excess_cumulative(tmp_fit, start = make_date(2020, 03, 01), end = date_threshold) %>%
    mutate(jurisdiction = x)
  
})  %>% 
  as_tibble() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Loading covid19 data from european center for disease control (ECDC)
eudat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                  na.strings = "", stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM") %>%
  as_tibble() %>%
  mutate(date = dmy(dateRep)) %>%
  select(date, cases, deaths, countriesAndTerritories, popData2019) %>%
  rename(jurisdiction = countriesAndTerritories) %>%
  arrange(date, jurisdiction) %>%
  filter(deaths >= 0) %>%
  group_by(jurisdiction) %>%
  mutate(covid_deaths = cumsum(deaths),
         jurisdiction = gsub("_", " ", jurisdiction)) %>%
  ungroup() %>%
  select(date, jurisdiction, covid_deaths)

# -- All countries
excess_deaths_countries <- left_join(excess_deaths_countries, eudat, by=c("date", "jurisdiction"))


# -- Using CDC data for USA percent change instead of FT data
usa_pc_temp <- percent_change_usa %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  select(date, expected, observed, fitted, se, lwr, upr) %>%
  mutate(jurisdiction = "United States")

# -- Using CDC data for USA percent change instead of FT data
usa_ed_temp <- excess_deaths_usa %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se) %>%
  mutate(jurisdiction = "United States") %>%
  select(date, observed, sd, fitted, se, jurisdiction, lwr, upr, covid_deaths)

# -- Adding it to percent change
percent_change_countries <- percent_change_countries %>%
  filter(jurisdiction != "United States of America") %>%
  bind_rows(usa_pc_temp)

# -- Adding it to excess deaths
excess_deaths_countries <- excess_deaths_countries %>%
  filter(jurisdiction != "United States of America") %>%
  bind_rows(usa_ed_temp)


# -- Using CDC data for USA observed counts insted of FT data
usa_counts_temp <- cdc_counts %>%
  group_by(date) %>%
  summarize(outcome    = sum(outcome), 
            population = sum(population)) %>%
  ungroup() %>%
  mutate(jurisdiction = "United States") %>%
  select(date, outcome, jurisdiction, population)

# -- Adding it to world counts
world_counts <- world_counts %>%
  filter(jurisdiction != "United States of America") %>%
  bind_rows(usa_counts_temp)

# -- Adding world covid data to world_counts
world_counts <- eudat %>%
  mutate(jurisdiction = ifelse(jurisdiction == "United States of America", "United States", jurisdiction)) %>%
  right_join(world_counts, by = c("date", "jurisdiction"))

# -- Take Mexico data out since its incomplete
world_counts <- filter(world_counts, jurisdiction != "Mexico")

# -- Saving
the_stamp <- now()
save(the_stamp, cdc_counts,
     covid_counts_states, percent_change_states, excess_deaths_states,
     world_counts, percent_change_countries, excess_deaths_countries,
     file = "rda/dat.rda", compress = "xz")
################################################################################################################################################################
### -- END COMPUTING EXCESS MORTALITY METRICS FOR INTERNATIONAL DATA ###########################################################################################
################################################################################################################################################################




