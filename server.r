source("functions.R")
source("init.R")
### DO NOT DELETE ANY COMMENTS / COMMENTED LINES

# -- Shiny server
shinyServer(function(input, output, session) {

  # -- Date and time of latest update
  output$stamp = renderUI({
    p(em(paste("Last updated on:", format(the_stamp, "%B %d, %Y"), "at",format(the_stamp, "%I:%m %p"))), align = "center", style = "font-family: 'arial'; font-size: 9pt; color:#969696")
  })
  
  # -- Title for worse percent change
  output$percent_change_worse_title = renderUI({
    h3(paste0("Six worst US states on the week ending on ", format(input$range[2], "%B %d, %Y")), align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa")
  })
  
  # -- Title for worse excess deaths
  output$excess_deaths_worse_title = renderUI({
    h3(paste0("Six worst US states on the week ending on ", format(input$range[2], "%B %d, %Y")), align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa")
  })
  
  # -- US map percent change animation
  output$us_map <- renderPlot({
    plot_us_map(percent_change = percent_change, us_map = us_map, temp_date = input$map_usa_date)
  })
  
  # -- US map excess deaths animation
  output$us_ed_map <- renderPlot({
    plot_us_map_ed(usa_ed_dat, us_map, temp_date = input$map_usa_ed_date)
  })
  
  # -- World map percent change animation
  output$world_map <- renderPlot({
    plot_world_map(pc = percent_change_countries, map = world, temp_date = input$map_world_date)
  })
  
  # -- World map excess deaths animation
  output$world_ed_map <- renderPlot({
    plot_world_map_ed(world_ed_dat, map = world, temp_date = input$map_world_ed_date)
  })
  
  # -- Percent excess vs covid in US states
  output$excess_v_covid_usa <- renderPlot({
    plot_excess_v_covid(dat = cdc_counts, pc = filter(percent_change, type == "weighted"))
  })
  
  # -- Percent excess vs covid in countires
  output$excess_v_covid_world <- renderPlot({
    plot_excess_v_covid(dat = world_counts, pc = percent_change_countries)
  })

  # -- Percent change plot for US states  
  output$percent_change_usa <- renderPlot({
    plot_percent_change(dat = filter(percent_change, type == "weighted"), jurisdictions = input$state, start = input$range[1], end = input$range[2], ci_ind = input$`percent-change-states-CI`)
})
  
  # -- Plot of worse states in USA
  output$percent_change_usa_worse <- renderPlot({
    plot_worse_percent_change(dat = filter(percent_change, type == "weighted"), start = input$range[1], end = input$range[2])
  })
  
  # -- Plot of excess mortality of worse states in USA 
  output$excess_mortality_usa_worse <- renderPlot({
    plot_worse_excess_deaths(dat = cdc_counts, pc = filter(percent_change, type == "weighted"), start = input$range[1], end = input$range[2])
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
    
    plot_percent_change(dat = both_dat, jurisdictions = input$both, start = input$range[1], end = input$range[2], ci_ind = input$`percent-change-both-CI`)
  })

  # -- Reactive dataset for excess deaths in US states
  reactive_excess_deaths_usa <- reactive({ get_excess_deaths(dat = cdc_counts, jurisdictions = input$state_edeaths, start = input$range_edeaths[1], end = input$range_edeaths[2]) })
  
  # -- Reactive dataset for excess deaths in Countries
  reactive_excess_deaths_countries <- reactive({ get_excess_deaths(dat = world_counts, jurisdictions = input$countries_edeaths, start = input$range_countries_edeaths[1], end = input$range_countries_edeaths[2]) })
  
  # -- Reactive dataset for excess deaths in both
  reactive_excess_deaths_both <- reactive({ 
    
    tmp <- select(cdc_counts, -outcome_unweighted) %>%
      bind_rows(world_counts)
    
    get_excess_deaths(dat = tmp, jurisdictions = input$both, start = input$range[1], end = input$range[2])
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
    plot_excess_deaths(dat = reactive_excess_deaths_both(), jurisdictions = input$both, start = input$range[1], end = input$range[2], ci_ind = input$`excess-deaths-both-CI`, pop_ind = input$`excess-deaths-both-POP`, c19_ind = input$`excess-deaths-both-C19`)
  })
    
  # -- Data table
  output$table <- DT::renderDataTable({
    
    # -- Excess deaths data
    tmp <- select(cdc_counts, -outcome_unweighted) %>%
      bind_rows(world_counts)
    ed <- get_excess_deaths(dat = tmp, jurisdictions = input$both, start = input$range[1], end = input$range[2])
    
    # -- Percent change data
    pc <- percent_change %>%
      filter(type == "weighted") %>%
      select_at(colnames(percent_change_countries)) %>%
      bind_rows(percent_change_countries)
    
    # -- Making table
    make_table(pc, ed, jurisdictions = input$both, start = input$range[1], end = input$range[2])
  }, server = FALSE)
  
  # -- Function to switch between hidden tabs
  switch_tab <- function(inputId, panel){
    updateTabsetPanel(session, panel, selected = inputId)
  }
  
  # -- To change panels
  observeEvent(input$`data-panel`, switch_tab("data", "global-panel"))
  observeEvent(input$`pc-panel`, switch_tab("percent-change", "global-panel"))
  # observeEvent(input$c_states, switch_tab("within-percent-change-states", "within-percent-change"))
  # observeEvent(input$c_countries, switch_tab("within-percent-change-countries", "within-percent-change"))
  # observeEvent(input$c_both, switch_tab("within-percent-change-both", "within-percent-change"))
  observeEvent(input$`ed-panel`, switch_tab("excess-deaths", "global-panel"))
  # observeEvent(input$c_states_edeaths, switch_tab("within-excess-deaths-states", "within-excess-deaths"))
  # observeEvent(input$c_countries_edeaths, switch_tab("within-excess-deaths-countries", "within-excess-deaths"))
  # observeEvent(input$c_both_edeaths, switch_tab("within-excess-deaths-both", "within-excess-deaths"))
})

