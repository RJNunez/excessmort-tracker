source("init.R")
source("functions.R")
load("rda/cdc_counts.rda")
load("rda/counts-usa.rda")
load("rda/ft_counts.rda")
states     <- sort(unique(percent_change$jurisdiction))
countries  <- sort(unique(percent_change_countries$jurisdiction))
button_style <- "color: black; background-color: rgb(230, 220, 205); position: relative; 
                     text-align:center; border-radius: 6px; border-width: 2px; font-family: 'helvetica'; font-weight: bold"

shinyUI(fluidPage(theme = shinytheme("sandstone"),
  
    # -- Meta data
    tags$head(
      tags$meta(name="description", content="Excess Mortality Tracker"),
      tags$meta(name="keywords", content="Excess Mortality, COVID-19"),
      tags$meta(name="author", content="Rolando J. Acosta")
    ),
    
    # -- Background color of UI
    setBackgroundColor(color = "#F8F5F0"),

    # -- Google analytics add on
    tags$head(includeHTML(("google-analytics.html"))),
    
    # -- Header space
    br(),br(),
    
    # -- Tab Title
    h1("Excess Mortality Tracker", align = "center", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),

    # -- HTML tag showing info on latest updated
    uiOutput("stamp"),

    # -- Explain that the user can look at percent change or cumulative excess deaths
    fluidRow(column(2),
             column(8, align = "center",
                    p("Country-specific COVID-19 metrics like cases and deaths only show the instances that are caught by the health system. Therefore, for each jurisdiction, these metrics rely heavily on the quality of its health system.
                      All cause excess mortality accounts for observed and unobserved consequences of the COVID-19 pandemic. Here we amalgamate mortality data for US states and countries around the
                      world from different sources, and present two mortality metrics:", 
                      align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                    # br(),
                    p(strong("• Percent Increase in Mortality:"), "Measures deviations from expected mortality and is useful to compare jurisdictions.", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                    p(strong("• Cumulative Excess Mortality:"), "Number of deaths above expected mortality.", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),##969696))
             column(2)),
    
    # -- Hiddent tabset panel to switch between percent change and excess deaths
    tabsetPanel(
      id       = "global-panel",
      type     = "hidden",
      selected = "percent-change", 
      
      # -- Input: Countries, US states, or world cities
      br(),
      # p("Choose the contrast of interest", align = "center", style = "font-family: 'helvetica'; font-size: 10pt ; color:#969696"),
      fluidRow(align = "center", 
               actionButton("pc-panel", "Percent Increase in Mortality", style = button_style),
               actionButton("ed-panel", "Cumulative Excess Mortality", style = button_style)),

    # -- Percent change panel
    tabPanel("percent-change",
             
             # -- Text to briefly explain inputs, data, and graphics
             br(),
             p("Here you can compare percent changes in mortality for US states or countries around the world.", align = "center", style = "font-family: 'helvetica'; font-size: 10pt ; color:#969696"),
             
             # -- Input: Countries, US states, or world cities
             fluidRow(align = "center",
                      actionButton("c_states", "US States", style = button_style),
                      actionButton("c_both", "Both", style = button_style),
                      actionButton("c_countries", "Countries", style = button_style)),
             
             tabsetPanel(
               id   = "within-percent-change",
               type = "hidden",
               
               tabPanel("within-percent-change-states",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        br(),
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"),
                        
                        # -- Inputs
                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center",
                                 selectizeInput("state",
                                                label    = "Jurisdiction:",
                                                choices  = states,
                                                selected = c("Massachusetts", "Florida"),
                                                multiple = TRUE,
                                                options  = list(maxItems    = 5,
                                                                placeholder = "Choose a state"))),
                          
                          column(4, align = "center",
                                 checkboxInput("percent-change-states-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(4, align = "center",
                          #        radioButtons("percent-change-states-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                          # -- Date range input
                          column(4, align = "center",
                                 dateRangeInput("range", "Period",
                                                start  = make_date(2020,03,01),
                                                end    = max(cdc_counts$date),
                                                min    = min(cdc_counts$date),
                                                format = "M-dd-yyyy",
                                                max    = max(cdc_counts$date)))),
                        
                        plotOutput("percent_change_usa")), # End of tabPanel: within-percent-change-states
               
               tabPanel("within-percent-change-countries",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        br(),
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"),
                        
                        fluidRow( 
                          # -- Jurisdiction input
                          column(4, align = "center",
                                   selectizeInput("countries",
                                                  label    = "Jurisdiction:",
                                                  choices  = countries,
                                                  selected = c("United States", "United Kingdom"),
                                                  multiple = TRUE,
                                                  options  = list(maxItems    = 5,
                                                                  placeholder = "Choose a country"))),
                          
                          column(4, align = "center",
                                 checkboxInput("percent-change-countries-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(4, align = "center",
                          #        radioButtons("percent-change-countries-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                            # -- Date range input
                            column(4, align = "center",
                                   dateRangeInput("range_countries", "Period",
                                                  start  = make_date(2020,03,01),
                                                  end    = max(percent_change_countries$date),
                                                  min    = make_date(2020, 01, 01),
                                                  format = "M-dd-yyyy",
                                                  max    = max(percent_change_countries$date)))),

                        plotOutput("percent_change_countries")), # End of tabPanel: within-percent-change-countries
               
               tabPanel("within-percent-change-both",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        br(),
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"),
                        
                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center",
                                 selectizeInput("both",
                                                label    = "Jurisdiction:",
                                                choices  = sort(c(countries, states)),
                                                selected = c("New York City", "Ecuador"),
                                                multiple = TRUE,
                                                options  = list(maxItems    = 5,
                                                                placeholder = "Choose a country"))),
                          
                          column(4, align = "center",
                                 checkboxInput("percent-change-both-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(4, align = "center",
                          #        radioButtons("percent-change-both-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                          # -- Date range input
                          column(4, align = "center",
                                 dateRangeInput("range_both", "Period",
                                                start  = make_date(2020,03,01),
                                                end    = max(percent_change_usa$date),
                                                min    = make_date(2020, 01, 01),
                                                format = "M-dd-yyyy",
                                                max    = max(percent_change_usa$date)))),
                        
                        plotOutput("percent_change_both")) # End of tabPanel: within-percent-change-both
             )), # End of tabsetPanel: within-percent-change
             
    tabPanel("excess-deaths",

             # -- Text to briefly explain inputs, data, and graphics
             br(),
             p("Here you can compare excess mortality trends for US states or countries around the world.", align = "center", style = "font-family: 'helvetica'; font-size: 10pt ; color:#969696"),

             # -- Input: Countries, US states, or world cities
             fluidRow(align = "center",
                      actionButton("c_states_edeaths", "US States", style = button_style),
                      actionButton("c_both_edeaths", "Both", style = button_style),
                      actionButton("c_countries_edeaths", "Countries", style = button_style)),
             
             tabsetPanel(
               id   = "within-excess-deaths",
               type = "hidden", 
               
               tabPanel("within-excess-deaths-states",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"), 
                        br(),
                        # -- Inputs
                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center",
                                 selectizeInput("state_edeaths",
                                                label    = "Jurisdiction:",
                                                choices  = states,
                                                selected = c("Massachusetts", "Florida"),
                                                multiple = TRUE,
                                                options  = list(maxItems    = 5,
                                                                placeholder = "Choose a state"))),
                          
                          column(2, align = "center",
                                 checkboxInput("excess-deaths-states-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(2, align = "left",
                          #        radioButtons("excess-deaths-states-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                          column(2, align = "left",
                                 radioButtons("excess-deaths-states-POP", "Excess deaths:", 
                                              choices = c("Per 100,000", "Totals"), selected = "Per 100,000")),
                          
                          
                          # -- Date range input
                          column(4, align = "center",
                                 dateRangeInput("range_edeaths", "Period",
                                                start  = make_date(2020,03,01),
                                                end    = max(cdc_counts$date),
                                                min    = make_date(2020,03,01),
                                                format = "M-dd-yyyy",
                                                max    = max(cdc_counts$date)))),
                        
                        plotOutput("excess_deaths_usa")), # End of tabpanel: within-excess-deaths-states
               
               tabPanel("within-excess-deaths-countries",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"), 
                        br(),
                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center",
                                 selectizeInput("countries_edeaths",
                                                label    = "Jurisdiction:",
                                                choices  = countries,
                                                selected = c("United States", "United Kingdom"),
                                                multiple = TRUE,
                                                options  = list(maxItems    = 5,
                                                                placeholder = "Choose a country"))),
                          
                          column(2, align = "center",
                                 checkboxInput("excess-deaths-countries-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(2, align = "left",
                          #        radioButtons("excess-deaths-countries-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                          column(2, align = "left",
                                 radioButtons("excess-deaths-countries-POP", "Per 100,000", 
                                              choices = c("Per 100,000", "Totals"), selected = "Per 100,000")),
                          
                          # -- Date range input
                          column(4, align = "center",
                                 dateRangeInput("range_countries_edeaths", "Period",
                                                start  = make_date(2020,03,01),
                                                end    = max(percent_change_countries$date),
                                                min    = make_date(2020,03,01),
                                                format = "M-dd-yyyy",
                                                max    = max(percent_change_countries$date)))),
                        
                        plotOutput("excess_deaths_countries")), # End of tabpanel: within-excess-deaths-states
               
               tabPanel("within-excess-deaths-both",
                        
                        # -- Text to briefly explain inputs, data, and graphics
                        # p("Add a brief description of the data and figures here", align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:black"), 
                        br(),
                        # -- Inputs
                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center",
                                 selectizeInput("both_edeaths",
                                                label    = "Jurisdiction:",
                                                choices  = sort(c(countries, states)),
                                                selected = c("New York City", "Ecuador"),
                                                multiple = TRUE,
                                                options  = list(maxItems    = 5,
                                                                placeholder = "Choose a state"))),
                          
                          column(2, align = "center",
                                 checkboxInput("excess-deaths-both-CI", "95% Confidence Intervarls?", 
                                               value = FALSE)),
                          
                          # column(2, align = "left",
                          #        radioButtons("excess-deaths-both-CI", "Confidence Intervarls?", 
                          #                     choices = c("Yes", "No"), selected = "Yes")),
                          
                          column(2, align = "left",
                                 radioButtons("excess-deaths-both-POP", "Per 100,000", 
                                              choices = c("Per 100,000", "Totals"), selected = "Per 100,000")),
                          
                          
                          # -- Date range input
                          column(4, align = "center",
                                 dateRangeInput("range_both_edeaths", "Period",
                                                start  = make_date(2020,03,01),
                                                end    = max(cdc_counts$date),
                                                min    = make_date(2020,03,01),
                                                format = "M-dd-yyyy",
                                                max    = max(cdc_counts$date)))),
                        
                        plotOutput("excess_deaths_both")) # End of tabpanel: within-excess-deaths-both
             ) # End of tabsetPanel: within-excess-deaths
        ) # End of tabPanel: excess-deaths
    ), # End of tabsetPanel: global-panel
    br(),
    br(),
    p("If you have comments or suggestions, you can reach me at racosta@fas.harvard.edu or in",
      a("Twitter", href = "https://twitter.com/RJANunez"),
      align = "left", style = "font-family: 'arial'; font-size: 9pt; color:#969696"),
    p("The code to recreate the visualizations and app is ",
      a("here", href = "https://github.com/RJNunez/excessmort-tracker"), 
      align = "left", style = "font-family: 'arial'; font-size: 9pt; color:#969696; margin-top:-8pt"),
    # p("This is a beta version", align = "left", style = "font-family: 'arial'; font-size: 9pt; color:#969696"),
) # End of fluidPage
) # End of UI
