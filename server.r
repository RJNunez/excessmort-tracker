source("init.R")
source("functions.R")
load("rda/cdc_counts.rda")
load("rda/counts-usa.rda")
load("rda/ft_counts.rda")
states     <- sort(unique(percent_change$jurisdiction))
countries  <- sort(unique(percent_change_countries$jurisdiction))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # -- Date and time of latest update
  output$stamp = renderUI({
    p(em(paste("Last updated on:", format(the_stamp, "%B %d, %Y"), "at",format(the_stamp, "%I:%m %p"))), align = "center", style = "font-family: 'arial'; font-size: 9pt; color:#969696")
  })

  # -- Percent change plot for US states  
  output$percent_change_usa <- renderPlot({
    percent_change_plot(dat = filter(percent_change, type == "weighted"), jurisdictions = input$state, start = input$range[1], end = input$range[2], ci_ind = input$`percent-change-states-CI`)
})
  
  # -- Percent change plot for countries
  output$percent_change_countries <- renderPlot({
    percent_change_plot(dat = percent_change_countries, jurisdictions = input$countries, start = input$range_countries[1], end = input$range_countries[2], ci_ind = input$`percent-change-countries-CI`)
  })
  
  # -- Percent change plot for both
  output$percent_change_both <- renderPlot({
    
    both_dat <- percent_change %>%
      filter(type == "weighted") %>%
      select_at(colnames(percent_change_countries)) %>%
      bind_rows(percent_change_countries)
    
    percent_change_plot(dat = both_dat, jurisdictions = input$both, start = input$range_both[1], end = input$range_both[2], ci_ind = input$`percent-change-both-CI`)
  })

  # -- Reactive dataset for excess deaths in US states
  reactive_excess_deaths_usa <- reactive({ get_excess_deaths(dat = cdc_counts, jurisdictions = input$state_edeaths, start = input$range_edeaths[1], end = input$range_edeaths[2]) })
  
  # -- Reactive dataset for excess deaths in Countries
  reactive_excess_deaths_countries <- reactive({ get_excess_deaths(dat = world_counts, jurisdictions = input$countries_edeaths, start = input$range_countries_edeaths[1], end = input$range_countries_edeaths[2]) })
  
  # -- Reactive dataset for excess deaths in both
  reactive_excess_deaths_both <- reactive({ 
    
    tmp <- select(cdc_counts, -outcome_unweighted) %>%
      bind_rows(world_counts)
    
    get_excess_deaths(dat = tmp, jurisdictions = input$both_edeaths, start = input$range_both_edeaths[1], end = input$range_both_edeaths[2])
    })
  
  # -- Excess deaths plot for US states  
  output$excess_deaths_usa <- renderPlot({
    excess_deaths_plot(dat = reactive_excess_deaths_usa(), jurisdictions = input$state_edeaths, start = input$range_edeaths[1], end = input$range_edeaths[2], ci_ind = input$`excess-deaths-states-CI`, pop_ind = input$`excess-deaths-states-POP`)
  })
  
  # -- Excess deaths plot for countries
  output$excess_deaths_countries <- renderPlot({
    excess_deaths_plot(dat = reactive_excess_deaths_countries(), jurisdictions = input$countries_edeaths, start = input$range_countries_edeaths[1], end = input$range_countries_edeaths[2], ci_ind = input$`excess-deaths-countries-CI`, pop_ind = input$`excess-deaths-countries-POP`)
  })
  
  # -- Excess deaths plot for both
  output$excess_deaths_both <- renderPlot({
    excess_deaths_plot(dat = reactive_excess_deaths_both(), jurisdictions = input$both_edeaths, start = input$range_both_edeaths[1], end = input$range_both_edeaths[2], ci_ind = input$`excess-deaths-both-CI`, pop_ind = input$`excess-deaths-both-POP`)
  })
    
  # -- Percent change plot for US states  
  output$percent_change_usa_worse <- renderPlot({
    
    # -- Worse 6 states
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
    states_dat <- percent_change %>%
      filter(type == "weighted",
             state %in% worse_states,
             date >= input$range[1], date <= input$range[2]) %>%
      group_by(state) %>%
      mutate(dif = c(NA, diff(fitted))) %>%
      ungroup() 
    
    # -- Adding labeling info
    states_dat <- states_dat %>%
      filter(date >= ymd(input$range[2]) - weeks(1)) %>%
      mutate(flag  = ifelse(dif > 0, "Upward", "Downward"),
             label = paste0("  ",round(100*fitted, 1), "%")) %>%
      select(state, label, flag) %>%
      right_join(states_dat, by = c("state")) %>%
      select(-dif) %>%
      mutate(state = factor(state, levels = worse_states))
    
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
      ggplot(aes(date, fitted, color=flag, fill=flag, label=label)) +
      geom_hline(yintercept = 0, color="#525252", lty=2) +
      geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
      geom_line(show.legend = FALSE) +
      geom_dl(method=list("last.points", fontfamily="Helvetica")) +
      scale_color_manual(name = "",
                         values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
      scale_fill_manual(name = "",
                        values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_labels = "%b %Y",
                   limits = c(input$range[1], input$range[2] + edays)) +
      ylab("Percent change from average mortality") +
      xlab("Date") +
      theme_slate() +
      facet_wrap(~state) +
      theme(strip.text = element_text(face="bold"))
  })
  
  # -- Function to switch between hidden tabs
  switch_tab <- function(inputId, panel) {
    updateTabsetPanel(session, panel, selected = inputId)
  }
  
  # -- 
  observeEvent(input$`pc-panel`, switch_tab("percent-change", "global-panel"))
  observeEvent(input$c_states, switch_tab("within-percent-change-states", "within-percent-change"))
  observeEvent(input$c_countries, switch_tab("within-percent-change-countries", "within-percent-change"))
  observeEvent(input$c_both, switch_tab("within-percent-change-both", "within-percent-change"))
  observeEvent(input$`ed-panel`, switch_tab("excess-deaths", "global-panel"))
  observeEvent(input$c_states_edeaths, switch_tab("within-excess-deaths-states", "within-excess-deaths"))
  observeEvent(input$c_countries_edeaths, switch_tab("within-excess-deaths-countries", "within-excess-deaths"))
  observeEvent(input$c_both_edeaths, switch_tab("within-excess-deaths-both", "within-excess-deaths"))
})

