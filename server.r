source("init.R")
load("rda/covid19-usa.rda")
my_palette <- c("#f0f0f0", "#cb181d", "#2171b5", "gold", "#238b45", "#6a51a3")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$percent_change <- renderPlot({
    
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
      geom_dl(method=list("last.qp", fontfamily="Helvetica"), data = last_dp) +
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
  
  output$percent_changeV2 <- renderPlot({
    
    states_dat <- percent_change %>%
      filter(type == "weighted") %>%
      filter(state %in% input$state) %>%
      filter(date >= input$range[1], date <= input$range[2]) %>%
      mutate(state = factor(state, levels = input$state)) %>%
      mutate(state = paste0("  ", state))
    
    last_dp <- states_dat %>%
      group_by(state) %>%
      filter(date == max(date))
    
    dl_names <- states_dat %>%
      group_by(state) %>%
      filter(date >= max(date)-8)
    
    ggplot() +
      geom_hline(yintercept = 0, color="#969696", lty=2, size=0.20) +
      geom_line(aes(date, fitted, group = state), size = 0.10, color = "#737373", data = filter(percent_change, type == "weighted", date >= input$range[1], date <= input$range[2])) +
      geom_line(aes(date, fitted, group=state), size=0.70, color="white", data = states_dat) +
      geom_line(aes(date, fitted, color=state), size=0.50, show.legend = FALSE, data = states_dat) +
      geom_point(aes(date, fitted, group=state), color="#1C1E22", size=3, data = last_dp, show.legend = FALSE) +
      geom_point(aes(date, fitted, color=state), size=3, alpha=0.50, data = last_dp, show.legend = FALSE) +
      geom_point(aes(date, fitted, color=state), size=3, pch=1, data = last_dp, show.legend = FALSE) +
      geom_dl(aes(date, fitted, label=state, color=state), method=list("last.qp"), data = dl_names) +
      ylab("Percent change from average mortality") +
      xlab("Date") +
      scale_y_continuous(label = scales::percent) +
      coord_cartesian(ylim = c(-0.10, 1)) +
      scale_x_date(date_labels = "%b %Y",
                   limits = c(input$range[1], input$range[2] + weeks(6))) +
      scale_color_manual(name = "",
                         values = my_palette) +
      theme_slate()
  })
})


# floor((x_limits[2] - x_limits[1]) / 30)
# seq(x_limits[1], x_limits[2], floor((x_limits[2] - x_limits[1]) / 5))
# 
# 
# round_date(seq(x_limits[1], x_limits[2], floor((x_limits[2] - x_limits[1]) / 5)), unit = "month")

# temp_states <- c("Massachusetts", "Florida", "Texas", "Arkansas", "New York City")
# states_dat  <- percent_change %>%
#   filter(date >= "2020-01-01") %>%
#   # filter(year(date) == 2019) %>%
#   filter(type == "weighted") %>%
#   filter(state %in% temp_states) %>%
#   mutate(state = paste0("  ", state))
# last_dp <- states_dat %>%
#   group_by(state) %>%
#   filter(date == max(date))
# y_limits <- range(states_dat$fitted)
# x_limits <- range(states_dat$date)
# 
# 
# if(x_limits[2] - x_limits[1] <= 30){ freq <- "week" } else { freq <- "month" }
# if(x_limits[2] - x_limits[1] > 30){ freq <- "month" }
# 
# states_dat %>%
#   ggplot(aes(date, fitted, label=state, color=state)) +
#   geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change, date >= "2020-01-01", type == "weighted")) +
#   geom_line(size=0.80, show.legend = FALSE, data = states_dat) +
#   geom_dl(aes(date, fitted, label=state, color=state), method=list("smart.grid", fontface = "bold", fontfamily="Times"), data = last_dp) +
#   ylab("Percent change from average mortality") +
#   xlab("Date") +
#   scale_y_continuous(label = scales::percent,
#                      breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
#   scale_x_date(date_labels = "%b %Y %d ",
#                breaks = floor_date(seq(x_limits[1], x_limits[2], floor((x_limits[2] - x_limits[1]) / 10)), unit = freq)) +
#   coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   scale_fill_manual(name = "",
#                     values = my_palette) +
#   theme_slate()
# 
# 
# 
# # dl_names <- states_dat %>%
# #   group_by(state) %>%
# #   filter(date >= max(date)-8)
# ggplot() +
#   geom_hline(yintercept = 0, color="#525252", lty=2) +
#   # geom_point(aes(date, observed/expected - 1), alpha=0.50, show.legend = FALSE) +
#   # geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
#   # geom_point(aes(date, observed/expected - 1, color=state), alpha=0.50, show.legend = FALSE, data = states_dat) +
#   # geom_ribbon(aes(date, ymin=lwr, ymax=upr, fill=state), alpha=0.20, color=NA, show.legend = FALSE, data = states_dat) +
#   geom_line(aes(date, fitted, group=state), color="#969696", size=0.10, alpha=0.50, data = filter(percent_change, date >= "2020-01-01", type == "weighted")) +
#   geom_line(aes(date, fitted, color=state), size=0.80, show.legend = FALSE, data = states_dat) +
#   geom_dl(aes(date, fitted, label=state, color=state), method=list("smart.grid", fontface = "bold", fontfamily="Times"), data = last_dp) +
#   ylab("Percent change from average mortality") +
#   xlab("Date") +
#   scale_y_continuous(label = scales::percent,
#                      breaks = seq(y_limits[1], y_limits[2], by=round((y_limits[2]-y_limits[1]) / 10, 5))) +
#   scale_x_date(date_labels = "%b %Y") +
#   # coord_cartesian(ylim = c(-0.10, 1)) +round(y_limits[2] / 10, 2)
#   coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   scale_fill_manual(name = "",
#                     values = my_palette) +
#   theme_slate()
# 
# ggplot() +
#   geom_hline(yintercept = 0, color="#969696", lty=2, size=0.20) +
#   geom_line(aes(date, fitted, group = state), size = 0.10, color = "#737373", data = filter(percent_change, type == "weighted", date >= "2020-05-01")) +
#   geom_line(aes(date, fitted, group=state), size=0.70, color="white", data = states_dat) +
#   geom_line(aes(date, fitted, color=state), size=0.50, show.legend = FALSE, data = states_dat) +
#   geom_point(aes(date, fitted, group=state), color="#1C1E22", size=3, data = last_dp, show.legend = FALSE) +
#   geom_point(aes(date, fitted, color=state), size=3, alpha=0.50, data = last_dp, show.legend = FALSE) +
#   geom_point(aes(date, fitted, color=state), size=3, pch=1, data = last_dp, show.legend = FALSE) +
#   geom_dl(aes(date, fitted, label=state, color=state), method=list("smart.grid"), data = dl_names) +
#   ylab("Percent change from average mortality") +
#   xlab("Date") +
#   scale_y_continuous(label = scales::percent) +
#   coord_cartesian(ylim = c(-0.10, 1)) +
#   scale_x_date(date_labels = "%b %Y") +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   theme_slate()
# 
# ggplot(aes(date, fitted, color=state, fill=state)) +
#   geom_hline(yintercept = 0, color="#525252", lty=2) +
#   geom_line(aes(date, fitted, group = state), color = "#737373", alpha=0.50, data = filter(percent_change, type == "weighted", year(date) >= 2020)) +
#   # geom_point(aes(date, observed/expected - 1), alpha=0.50, show.legend = FALSE) +
#   # geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
#   geom_line(color="white", size=0.80) +
#   geom_line() +
#   ylab("Percent change from average mortality") +
#   xlab("Date") +
#   scale_y_continuous(label = scales::percent) +
#   scale_x_date(date_labels = "%b %Y") +
#   scale_color_manual(name = "",
#                      values = my_palette) +
#   # scale_fill_manual(name = "",
#   #                   values = my_palette) +
#   theme_slate() +
#   theme(legend.position = "top")
# 
# 
