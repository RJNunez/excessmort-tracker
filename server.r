source("init.R")
load("rda/cdc_counts.rda")
load("rda/counts-usa.rda")
load("rda/ft_counts.rda")
states     <- unique(percent_change$state)
countries  <- unique(percent_change_countries$country)
my_palette <- c("#f0f0f0", "#cb181d", "#2171b5", "gold", "#238b45", "#6a51a3")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # -- Percent change plot for US states  
  output$percent_change_usa <- renderPlot({
    
    # -- State specific data
    states_dat <- percent_change %>%
      filter(type == "weighted") %>%
      filter(state %in% input$state) %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      mutate(state = factor(state, levels = input$state))
    
    # -- Used for labeling
    last_dp <- states_dat %>%
      group_by(state) %>%
      filter(date >= max(date)-8)
    
    # -- Used to determine y-axis
    y_limits <- range(states_dat$fitted)
    
    # -- Used to determine x-axis
    x_limits <- range(states_dat$date)
    if(x_limits[2] - x_limits[1] <= 30){ 
      freq  <- "week"
      edays <- weeks(2)
    } else{ 
      freq  <- "month"
      edays <- months(2)
    }
    
    # -- Making Viz
    states_dat %>%
      ggplot(aes(date, fitted, label=state, color=state)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change, type == "weighted", date >= input$range[1], date <= input$range[2])) +
      geom_line(show.legend = FALSE, data = states_dat) +
      geom_dl(method=list("last.points", fontfamily="Helvetica"), data = last_dp) +
      ylab("Percent change from average mortality") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::percent, 
                         breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
      scale_x_date(date_labels = "%b %d %Y", 
                   breaks = round_date(seq(x_limits[1], x_limits[2] + edays, length.out = 6), unit = freq),
                   limits = c(input$range[1], input$range[2] + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_slate()
})
  
  # -- Percent change plot for US states  
  output$percent_change_usa_worse <- renderPlot({
    
    # -- Worse 6 states
    print(1)
    worse_states <- percent_change %>%
      filter(type == "weighted",
             date >= ymd(input$range[2] - weeks(1)),
             date < ymd(input$range[2])) %>%
      arrange(desc(fitted)) %>%
      select(state) %>%
      unique() %>%
      slice(1:6) %>%
      pull()
    
    # -- States data
    print(2)
    states_dat <- percent_change %>%
      filter(type == "weighted",
             state %in% worse_states,
             date >= input$range[1], date <= input$range[2]) %>%
      group_by(state) %>%
      mutate(dif = c(NA, diff(fitted))) %>%
      ungroup() 
    
    # -- Adding labeling info
    print(3)
    states_dat <- states_dat %>%
      filter(date >= ymd(input$range[2]) - weeks(1)) %>%
      mutate(flag  = ifelse(dif > 0, "Upward", "Downward"),
             label = paste0("  ",round(100*fitted, 1), "%")) %>%
      select(state, label, flag) %>%
      right_join(states_dat, by = c("state")) %>%
      select(-dif) %>%
      mutate(state = factor(state, levels = worse_states))
    
    # -- Used to determine y-axis
    print(4)
    y_limits <- range(states_dat$fitted)
    
    # -- Used to determine x-axis
    print(5)
    x_limits <- range(states_dat$date)
    if(x_limits[2] - x_limits[1] <= 30){ 
      freq  <- "week"
      edays <- weeks(2)
    } else{ 
      freq  <- "month"
      edays <- months(2)
    }
    
    # -- Making Viz
    print(6)
    states_dat %>%
      ggplot(aes(date, fitted, color=flag, fill=flag, label=label)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
      geom_line(show.legend = FALSE) +
      geom_dl(method=list("last.points", fontfamily="Helvetica")) +
      scale_color_manual(name = "",
                         values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
      scale_fill_manual(name = "",
                        values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
      scale_x_date(date_labels = "%b %Y",
                   limits = c(input$range[1], input$range[2] + edays)) +
      theme_slate() +
      facet_wrap(~state) +
      theme(strip.text = element_text(face="bold"))
  })
  
  # -- Excess deaths plot for US states  
  output$excess_deaths_usa <- renderPlot({
    
    # -- Covid19 data
    covid_dat <- covid_states %>%
      filter(state %in% input$state_edeaths) %>%
      filter(date >= input$range_state_edeaths[1], date <= input$range_state_edeaths[2]) %>%
      mutate(state = factor(state, levels = input$state_edeaths))
    
    # -- State specific data
    states_dat <- excess_deaths %>%
      filter(type == "weighted") %>%
      filter(state %in% input$state_edeaths) %>%
      filter(date >= input$range_state_edeaths[1], date <= input$range_state_edeaths[2]) %>%
      mutate(state = factor(state, levels = input$state_edeaths))
    
    # -- Used for labeling
    last_dp <- states_dat %>%
      group_by(state) %>%
      filter(date >= max(date)-8)
    
    # -- Used to determine y-axis
    y_limits <- range(states_dat$fitted)
    
    # -- Used to determine x-axis
    x_limits <- range(states_dat$date)
    if(x_limits[2] - x_limits[1] <= 30){ 
      freq  <- "week"
      edays <- weeks(2)
    } else{ 
      freq  <- "month"
      edays <- months(2)
    }
    
    # -- Making Viz
    states_dat %>%
      ggplot(aes(date, fitted, label=state, color=state)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(excess_deaths, type == "weighted", date >= input$range_state_edeaths[1], date <= input$range_state_edeaths[2])) +
      geom_line(aes(date, death, color=state), lty=2, size=0.50, show.legend = FALSE, data = covid_dat) +
      geom_line(show.legend = FALSE, data = states_dat) +
      geom_dl(method=list("last.points", fontfamily="Helvetica"), data = last_dp) +
      ylab("Cumulative excess deaths") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::comma,
                         breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
      scale_x_date(date_labels = "%b %d %Y", 
                   breaks = round_date(seq(x_limits[1], x_limits[2] + edays, length.out = 6), unit = freq),
                   limits = c(input$range_state_edeaths[1], input$range_state_edeaths[2] + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_slate()
  })

  # -- Percent change plot for countries
  output$percent_change_countries <- renderPlot({
    
    # -- State specific data
    countries_dat <- percent_change_countries %>%
      filter(country %in% input$countries) %>%
      filter(date >= input$range_countries[1], date <= input$range_countries[2]) %>%
      mutate(country = factor(country, levels = input$countries))
    
    # -- Used for labeling
    last_dp <- countries_dat %>%
      group_by(country) %>%
      filter(date >= max(date)-8) %>%
      arrange(date)
    
    # -- Used to determine y-axis
    y_limits <- range(countries_dat$fitted)
    
    # -- Used to determine x-axis
    x_limits <- range(countries_dat$date)
    if(x_limits[2] - x_limits[1] <= 30){ 
      freq  <- "week"
      edays <- weeks(2)
    } else{ 
      freq  <- "month"
      edays <- months(2)
    }
    
    # -- Making Viz
    countries_dat %>%
      ggplot(aes(date, fitted, label=country, color=country)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_line(aes(date, fitted, group=country), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change_countries, date >= input$range_countries[1], date <= input$range_countries[2])) +
      geom_line(show.legend = FALSE, data = countries_dat) +
      geom_dl(method=list("last.points", fontfamily="Helvetica"), data = last_dp) +
      ylab("Percent change from average mortality") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::percent, 
                         breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
      scale_x_date(date_labels = "%b %d %Y", 
                   breaks = round_date(seq(x_limits[1], x_limits[2] + edays, length.out = 6), unit = freq),
                   limits = c(input$range_countries[1], input$range_countries[2] + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_slate()
  })
  
  # -- Excess deaths plot for countries
  output$excess_deaths_countries <- renderPlot({
    
    # -- State specific data
    countries_dat <- excess_deaths_countries %>%
      filter(country %in% input$countries_edeaths) %>%
      filter(date >= input$range_countries_edeaths[1], date <= input$range_countries_edeaths[2]) %>%
      mutate(country = factor(country, levels = input$countries_edeaths))
    
    # -- Used for labeling
    last_dp <- countries_dat %>%
      group_by(country) %>%
      filter(date >= max(date)-8) %>%
      arrange(date)
    
    # -- Used to determine y-axis
    y_limits <- range(countries_dat$fitted)
    
    # -- Used to determine x-axis
    x_limits <- range(countries_dat$date)
    if(x_limits[2] - x_limits[1] <= 30){ 
      freq  <- "week"
      edays <- weeks(2)
    } else{ 
      freq  <- "month"
      edays <- months(2)
    }
    
    # -- Making Viz
    countries_dat %>%
      ggplot(aes(date, fitted, label=country, color=country)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_line(aes(date, fitted, group=country), color="#969696", size=0.10, alpha=0.50, data = filter(excess_deaths_countries, date >= input$range_countries_edeaths[1], date <= input$range_countries_edeaths[2])) +
      geom_line(show.legend = FALSE, data = countries_dat) +
      geom_dl(method=list("last.points", fontfamily="Helvetica"), data = last_dp) +
      ylab("Cumulative excess deaths") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::comma, 
                         breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
      scale_x_date(date_labels = "%b %d %Y", 
                   breaks = round_date(seq(x_limits[1], x_limits[2] + edays, length.out = 6), unit = freq),
                   limits = c(input$range_countries_edeaths[1], input$range_countries_edeaths[2] + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_slate()
  })
  
  # -- Function to switch between hidden tabs
  switch_tab <- function(txt, panel) {
    updateTabsetPanel(session, panel, selected = txt)
  }
  
  # -- Reactive elements need to switch hidden tabs
  observeEvent(input$c_states, switch_tab("tab_states", "percent-change"))
  observeEvent(input$c_countries, switch_tab("tab_countries", "percent-change"))
  
  observeEvent(input$c_states_edeaths, switch_tab("tab_states_edeaths", "excess-deaths"))
  observeEvent(input$c_countries_edeaths, switch_tab("tab_countries_edeaths", "excess-deaths"))
  
})

# # -- State specific data
# a <- c("United States", "United Kingdom")
# countries_dat <- percent_change_countries %>%
#   # filter(countries %in% input$countries) %>%
#   filter(country %in% a) %>%
#   # filter(date >= input$range_countries[1], date <= input$range_countries[2]) %>%
#   filter(year(date) == 2020) %>%
#   mutate(country = factor(country, a))
# 
# # -- Used for labeling
# last_dp <- countries_dat %>%
#   group_by(country) %>%
#   filter(date >= max(date)-8) %>%
#   arrange(date)
# 
# # -- Used to determine y-axis
# y_limits <- range(countries_dat$fitted)
# 
# # -- Used to determine x-axis
# x_limits <- range(countries_dat$date)
# if(x_limits[2] - x_limits[1] <= 30){ 
#   freq  <- "week"
#   edays <- weeks(2)
# } else{ 
#   freq  <- "month"
#   edays <- months(2)
# }
# 
# # -- Making Viz
# countries_dat %>%
#   ggplot(aes(date, fitted, label=country, color=country)) +
#   geom_hline(yintercept = 0, color="#525252", lty=2) +
#   geom_line(aes(date, fitted, group=country), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change_countries, year(date) == 2020)) +
#   geom_line(show.legend = FALSE, data = countries_dat) +
#   geom_dl(method=list("last.points", fontfamily="Helvetica"), data = last_dp) +
#   ylab("Percent change from average mortality") +
#   xlab("Date") +
#   # coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
#   # scale_y_continuous(label = scales::percent, 
#   #                    breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
#   # scale_x_date(date_labels = "%b %d %Y", 
#   #              breaks = round_date(seq(x_limits[1], x_limits[2] + edays, length.out = 6), unit = freq),
#   #              limits = c(input$range[1], input$range[2] + edays)) +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   scale_fill_manual(name = "",
#                     values = my_palette) +
#   theme_slate()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

# temp_states <- c("Massachusetts", "Florida", "Texas", "Arkansas", "New York City")
# states_dat  <- excess_deaths %>%
#   filter(date >= "2020-01-01") %>%
#   # filter(year(date) == 2019) %>%
#   filter(type == "weighted") %>%
#   filter(state %in% temp_states)
# last_dp <- states_dat %>%
#   group_by(state) %>%
#   filter(date >= max(date)-8)
# y_limits <- range(states_dat$fitted)
# x_limits <- range(states_dat$date)
# 
# if(x_limits[2] - x_limits[1] <= 30){ freq <- "week" } else { freq <- "month" }
# if(x_limits[2] - x_limits[1] > 30){ freq <- "month" }
# 
# states_dat %>%
#   ggplot(aes(date, fitted, label=state, color=state)) +
#   geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(excess_deaths, date >= "2020-01-01", type == "weighted")) +
#   # geom_line(aes(date, fitted, color=state), size=0.50, show.legend = FALSE, data = covid_states)
#   geom_line(aes(date, death, color=state), lty=2, size=0.50, show.legend = FALSE, data = filter(covid_states, state %in% temp_states, date >= "2020-01-01")) +
#   geom_line(size=0.80, show.legend = FALSE) + 
#   geom_dl(aes(date, fitted, label=state, color=state), method=list("last.qp", fontface = "bold", fontfamily="Times"), data = last_dp) + 
#   ylab("Cumulative excess deaths") +
#   xlab("Date") +
#   scale_y_continuous(label = scales::comma,
#                      breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
#   scale_x_date(date_labels = "%b %Y %d ",
#                breaks = floor_date(seq(x_limits[1], x_limits[2], floor((x_limits[2] - x_limits[1]) / 10)), unit = freq)) +
#   coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   scale_fill_manual(name = "",
#                     values = my_palette) +
#   theme_slate()





# # 
# # 
# # # dl_names <- states_dat %>%
# # #   group_by(state) %>%
# # #   filter(date >= max(date)-8)
# # ggplot() +
# #   geom_hline(yintercept = 0, color="#525252", lty=2) +
# #   # geom_point(aes(date, observed/expected - 1), alpha=0.50, show.legend = FALSE) +
# #   # geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
# #   # geom_point(aes(date, observed/expected - 1, color=state), alpha=0.50, show.legend = FALSE, data = states_dat) +
# #   # geom_ribbon(aes(date, ymin=lwr, ymax=upr, fill=state), alpha=0.20, color=NA, show.legend = FALSE, data = states_dat) +
# #   geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change, date >= "2020-01-01", type == "weighted")) +
# #   geom_line(aes(date, fitted, color=state), size=0.80, show.legend = FALSE, data = states_dat) +
# #   geom_dl(aes(date, fitted, label=state, color=state), method=list("smart.grid", fontface = "bold", fontfamily="Times"), data = last_dp) +
# #   ylab("Percent change from average mortality") +
# #   xlab("Date") +
# #   scale_y_continuous(label = scales::percent,
# #                      breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
# #   scale_x_date(date_labels = "%b %Y") +
# #   # coord_cartesian(ylim = c(-0.10, 1)) +round(y_limits[2] / 10, 2)
# #   coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
# #   scale_color_manual(name = "",
# #                      values = my_palette) +
# #   scale_fill_manual(name = "",
# #                     values = my_palette) +
# #   theme_slate()
# # 
# # ggplot() +
# #   geom_hline(yintercept = 0, color="#969696", lty=2, size=0.20) +
# #   geom_line(aes(date, fitted, group = state), size = 0.10, color = "#737373", data = filter(percent_change, type == "weighted", date >= "2020-05-01")) +
# #   geom_line(aes(date, fitted, group=state), size=0.70, color="white", data = states_dat) +
# #   geom_line(aes(date, fitted, color=state), size=0.50, show.legend = FALSE, data = states_dat) +
# #   geom_point(aes(date, fitted, group=state), color="#1C1E22", size=3, data = last_dp, show.legend = FALSE) +
# #   geom_point(aes(date, fitted, color=state), size=3, alpha=0.50, data = last_dp, show.legend = FALSE) +
# #   geom_point(aes(date, fitted, color=state), size=3, pch=1, data = last_dp, show.legend = FALSE) +
# #   geom_dl(aes(date, fitted, label=state, color=state), method=list("smart.grid"), data = dl_names) +
# #   ylab("Percent change from average mortality") +
# #   xlab("Date") +
# #   scale_y_continuous(label = scales::percent) +
# #   coord_cartesian(ylim = c(-0.10, 1)) +
# #   scale_x_date(date_labels = "%b %Y") +
# #   scale_color_manual(name = "",
# #                      values = my_palette) +
# #   theme_slate()
# # 
# # ggplot(aes(date, fitted, color=state, fill=state)) +
# #   geom_hline(yintercept = 0, color="#525252", lty=2) +
# #   geom_line(aes(date, fitted, group = state), color = "#737373", alpha=0.50, data = filter(percent_change, type == "weighted", year(date) >= 2020)) +
# #   # geom_point(aes(date, observed/expected - 1), alpha=0.50, show.legend = FALSE) +
# #   # geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
# #   geom_line(color="white", size=0.80) +
# #   geom_line() +
# #   ylab("Percent change from average mortality") +
# #   xlab("Date") +
# #   scale_y_continuous(label = scales::percent) +
# #   scale_x_date(date_labels = "%b %Y") +
# #   scale_color_manual(name = "",
# #                      values = my_palette) +
# #   # scale_fill_manual(name = "",
# #   #                   values = my_palette) +
# #   theme_slate() +
# #   theme(legend.position = "top")
# # 
# # 
