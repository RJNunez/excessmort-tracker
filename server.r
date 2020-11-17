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
    plot_percent_change(dat = filter(percent_change, type == "weighted"), jurisdictions = input$state, start = input$range[1], end = input$range[2], ci_ind = input$`percent-change-states-CI`)
})
  
  # -- Plot of worse states in USA
  output$percent_change_usa_worse <- renderPlot({
    plot_worse_percent_change(dat = filter(percent_change, type == "weighted"), start = input$range[1], end = input$range[2])
  })
  
  # -- Percent change plot for countries
  output$percent_change_countries <- renderPlot({
    plot_percent_change(dat = percent_change_countries, jurisdictions = input$countries, start = input$range_countries[1], end = input$range_countries[2], ci_ind = input$`percent-change-countries-CI`)
  })
  
  # -- Percent change plot for both
  output$percent_change_both <- renderPlot({
    
    both_dat <- percent_change %>%
      filter(type == "weighted") %>%
      select_at(colnames(percent_change_countries)) %>%
      bind_rows(percent_change_countries)
    
    plot_percent_change(dat = both_dat, jurisdictions = input$both, start = input$range_both[1], end = input$range_both[2], ci_ind = input$`percent-change-both-CI`)
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
    plot_excess_deaths(dat = reactive_excess_deaths_usa(), jurisdictions = input$state_edeaths, start = input$range_edeaths[1], end = input$range_edeaths[2], ci_ind = input$`excess-deaths-states-CI`, pop_ind = input$`excess-deaths-states-POP`)
  })
  
  # -- Excess deaths plot for countries
  output$excess_deaths_countries <- renderPlot({
    plot_excess_deaths(dat = reactive_excess_deaths_countries(), jurisdictions = input$countries_edeaths, start = input$range_countries_edeaths[1], end = input$range_countries_edeaths[2], ci_ind = input$`excess-deaths-countries-CI`, pop_ind = input$`excess-deaths-countries-POP`)
  })
  
  # -- Excess deaths plot for both
  output$excess_deaths_both <- renderPlot({
    plot_excess_deaths(dat = reactive_excess_deaths_both(), jurisdictions = input$both_edeaths, start = input$range_both_edeaths[1], end = input$range_both_edeaths[2], ci_ind = input$`excess-deaths-both-CI`, pop_ind = input$`excess-deaths-both-POP`)
  })
    
  # -- Data table
  output$table <- DT::renderDataTable({
    
    # -- Excess deaths data
    tmp <- select(cdc_counts, -outcome_unweighted) %>%
      bind_rows(world_counts)
    ed <- get_excess_deaths(dat = tmp, jurisdictions = input$both_data, start = input$range_both_data[1], end = input$range_both_data[2])
    
    # -- Percent change data
    pc <- percent_change %>%
      filter(type == "weighted") %>%
      select_at(colnames(percent_change_countries)) %>%
      bind_rows(percent_change_countries)
    
    # -- Making table
    make_table(pc, ed, jurisdictions = input$both_data, start = input$range_both_data[1], end = input$range_both_data[2])
  }, server = FALSE)
  
  # -- Function to switch between hidden tabs
  switch_tab <- function(inputId, panel) {
    updateTabsetPanel(session, panel, selected = inputId)
  }
  
  # -- 
  observeEvent(input$`data-panel`, switch_tab("data", "global-panel"))
  
  observeEvent(input$`pc-panel`, switch_tab("percent-change", "global-panel"))
  observeEvent(input$c_states, switch_tab("within-percent-change-states", "within-percent-change"))
  observeEvent(input$c_countries, switch_tab("within-percent-change-countries", "within-percent-change"))
  observeEvent(input$c_both, switch_tab("within-percent-change-both", "within-percent-change"))
  
  observeEvent(input$`ed-panel`, switch_tab("excess-deaths", "global-panel"))
  observeEvent(input$c_states_edeaths, switch_tab("within-excess-deaths-states", "within-excess-deaths"))
  observeEvent(input$c_countries_edeaths, switch_tab("within-excess-deaths-countries", "within-excess-deaths"))
  observeEvent(input$c_both_edeaths, switch_tab("within-excess-deaths-both", "within-excess-deaths"))
})

