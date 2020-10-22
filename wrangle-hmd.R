# -- Libraries
library(scales)
library(tidyverse)
library(lubridate)
library(excessmort)
library(data.table)

# -- Getting data
url <- "https://www.mortality.org/Public/STMF/Outputs/stmf.csv"
dat <- fread(url) %>%
  as_tibble() %>%
  mutate(date = make_date(Year, 01, 01) + weeks(Week - 1))

# -- Country names and codes
countries <- c("Austria", "Belgium", "Bulgaria", "Switzerland", "Czech Republic", 
               "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", 
               "England and Wales", "Northern Ireland", "Scotland", "Greece", 
               "Croatia", "Hungary", "Iceland", "Israel", "Italy", "Republic of Korea",
               "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Netherlands",
               "Norway", "New Zealand", "Poland", "Portugal", "Russia", "Slovakia", 
               "Slovenia", "Sweden", 'United States')
countries <- tibble(country_code = unique(dat$CountryCode), country = countries)

# -- Wrangling death data
outcomes <- dat %>%
  filter(Sex != "b") %>%
  select(date, Sex, CountryCode, contains("D"), -DTotal) %>%
  gather(agegroup, outcome, -date, -Sex, -CountryCode) %>%
  mutate(Sex      = ifelse(Sex == "m", "Male", "Female"),
         agegroup = str_replace(agegroup, "^[A-Z](\\d{1,2})_(\\d{2})$", "\\1-\\2")) %>%
  rename(sex = Sex, country_code = CountryCode) %>%
  mutate(agegroup = ifelse(agegroup == "D85p", "85-Inf", agegroup))

# -- Wrangling population data
population <- dat %>%
  filter(Sex != "b") %>%
  mutate(X0_14  = 52 * D0_14 / R0_14,
         X15_64 = 52 * D15_64 / R15_64,
         X65_74 = 52 * D65_74 / R65_74,
         X75_84 = 52 * D75_84 / R75_84,
         X85p   = 52 * D85p / R85p) %>%
  select(date, Sex, CountryCode, contains("X"), -SplitSex) %>%
  gather(agegroup, population, -date, -Sex, -CountryCode) %>%
  mutate(Sex      = ifelse(Sex == "m", "Male", "Female"),
         agegroup = str_replace(agegroup, "^[A-Z](\\d{1,2})_(\\d{2})$", "\\1-\\2")) %>%
  rename(sex = Sex, country_code = CountryCode) %>%
  mutate(agegroup = ifelse(agegroup == "X85p", "85-Inf", agegroup))

# -- Putting everything together
counts <- outcomes %>%
  filter(agegroup != "0-14") %>%
  left_join(population, by = c("date", "sex", "country_code", "agegroup")) %>%
  left_join(countries, by = "country_code") %>%
  mutate(group = paste0(country, " | ", sex, " | ", agegroup)) %>%
  filter(country != "Russia")

# -- Control and exclude dates 
control_dates <- seq(ymd("1990-01-01"), ymd("2019-12-31"), by = "day")
exclude_dates <- seq(ymd("2020-01-01"), ymd("2020-12-31"), by = "day")
groups        <- unique(counts$group)

# -- Percent change per country and demography
percent_change_countries_hmd_dem <- map_df(groups, function(x){
  
  print(x)
  tmp <- filter(counts, group == x) %>% 
    na.omit() %>%
    arrange(date)
  fit <- excess_model(counts         = tmp, 
                      start          = make_date(2020, 01, 01),
                      end            = today(),
                      control.dates  = control_dates, 
                      exclude        = exclude_dates,
                      aic            = FALSE, 
                      order.max      = 7,
                      weekday.effect = FALSE,
                      verbose        = FALSE)
  
  
  tibble(date = fit$date, observed = fit$observed, expected = fit$expected, fitted = fit$fitted, se = fit$se, country = tmp$country[1], agegroup = tmp$agegroup[1], sex = tmp$sex[1]) %>%
    mutate(lwr = fitted - 1.96 * se, 
           upr = fitted + 1.96 * se)
})

# -- Marginal effects
percent_change_countries_hmd <- res %>%
  group_by(date, country) %>%
  summarize(fitted   = sum(expected * fitted) / sum(expected), 
            se       = sqrt(sum(expected^2 * se^2)) / sum(expected),
            observed  = sum(observed),
            expected = sum(expected)) %>%
  ungroup() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Computing excess deahts
excess_deaths_countries_hmd_dem <- map_df(groups, function(x){
  
  # -- filter dat
  tmp <- filter(counts, group == x) %>% 
    na.omit() %>%
    arrange(date)
  
  # -- Vars
  country  <- tmp$country[1]
  agegroup <- tmp$agegroup[1]
  sex      <- tmp$sex[1]

  # -- Fitting model
  fit <- excess_model(counts         = tmp, 
                      start          = make_date(2020, 01, 01),
                      end            = today(),
                      control.dates  = control_dates, 
                      exclude        = exclude_dates,
                      aic            = FALSE, 
                      order.max      = 7,
                      weekday.effect = FALSE,
                      verbose        = FALSE)
  
  # -- Excess deaths
  excess_cumulative(fit, start = make_date(2020, 01, 01), end = make_date(2020, 06, 30)) %>%
    mutate(country  = country,
           agegroup = agegroup,
           sex      = sex) %>%
    mutate(lwr = fitted - 1.96 * se, 
           upr = fitted + 1.96 * se)
}) %>%
  as_tibble()

# -- Save
save(percent_change_countries_hmd_dem, excess_deaths_countries_hmd_dem, file = "rda/hmd_counts.rda", compress = "xz")







