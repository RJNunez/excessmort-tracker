library(ggthemes)
percent_change %>%
  filter(type == "weighted") %>%
  filter(date >= "2020-03-01") %>%
  left_join(tibble(state = state.name, abb = state.abb), by = "state") %>%
  mutate(abb = case_when(state == "District of Columbia" ~ "DC", 
                         state == "New York (not including NYC)" ~ "NY",
                         state == "New York City" ~ "NYC",
                         state == "Puerto Rico" ~ "PR", 
                         TRUE ~ abb)) %>%
  mutate(fitted_cat = cut(fitted, breaks = c(-Inf, 0, 0.20, 0.40, 0.60, 0.80, 1, Inf), include.lowest = TRUE, right=FALSE)) %>%
  mutate(fitted_con = ifelse(fitted < 0, 0, fitted),
         fitted_con = ifelse(fitted > 1, 1, fitted_con)) %>%
  mutate(fitted_cat = factor(fitted_cat, levels = c("[-Inf,0)", "[0,0.2)", "[0.2,0.4)", "[0.4,0.6)", "[0.6,0.8)", "[0.8,1)", "[1, Inf]"))) %>%
  ggplot(aes(date, abb, fill = fitted_cat)) +
  geom_tile(color="black", size=0.02) +
  scale_fill_manual(name   = "Percent increase from \naverage mortality",
                    values = c("[-Inf,0)" = "#4393c3", "[0,0.2)" = "#d1e5f0", "[0.2,0.4)" = "#fddbc7", 
                               "[0.4,0.6)" = "#f4a582", "[0.6,0.8)" = "#d6604d", "[0.8,1)" = "#b2182b", "[1, Inf]"="#67000d"),
                    labels = c("< 0%", "0% to 20%", "20% to 40%", "40% to 60%", "60% to 80%", "80% to 100%", "> 100%")) +
  scale_x_date(date_breaks = "1 months", date_labels = "%b") +
  ylab("State") +
  xlab("Date") +
  theme_void() +
  theme(axis.title.x = element_text(color="#737373"),
        axis.title.y = element_text(color="#737373", angle=90),
        axis.text  = element_text(color="#737373", 
                                  # margin = margin(r = 0), 
                                  margin=margin(1,-45,1,1,"pt"),
                                  size=10),
        legend.text  = element_text(color="#737373"),
        legend.title = element_text(color="#737373"))

























# scale_fill_viridis_d(name="Percent increase from \naverage mortality") +
# scale_fill_viridis_c(name="Percent increase from \naverage mortality", 
#                      labels = scales::percent, 
#                      option = "E") +