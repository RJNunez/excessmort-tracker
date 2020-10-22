source("init.R")
load("rda/cdc_counts.rda")
load("rda/counts-usa.rda")
load("rda/ft_counts.rda")
states <- unique(percent_change$state)
countries    <- unique(percent_change_countries$country)
# button_style <- "color: white; background-color: red; border-color: white; position: relative; 
#                      text-align:center; border-radius: 6px; border-width: 2px"



shinyUI(fluidPage(theme = shinytheme("slate"),
  
    # -- Title of the app              
    # titlePanel("Excess Mortality in the USA"),
                  
    tabsetPanel(
    
      tabPanel("Percent Change from Average Mortality",
               
               # -- Tab Title
               h2("Percent Change from Average Mortality", align = "center", style = "font-family: 'helvetica'; color:white"),
               
               # -- This should be an HTML tag to show when the app was last updated
               p(em("Last updated on XX"), align = "center", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),
               
               # -- Text to briefly explain inputs, data, and graphics
               p("Here you can compare excess mortality trends for countries, US states, and cities around the world.", 
                 align = "center", style = "font-family: 'helvetica'; font-size: 10pt ; color:#969696"),
               
               # -- Input: Countries, US states, or world cities
               fluidRow(align = "center", 
                        actionButton("c_countries", "Countries"),
                        actionButton("c_states", "US States"),
                        actionButton("c_cities", "World Cities")),
               
               # -- For spacing
               br(),
               
               # -- Text to briefly explain inputs, data, and graphics
               p("We can add some text explaining the basics of the app just like FT does", 
                 align = "center", style = "font-family: 'helvetica'; font-size: 12pt ; color:white"),
               
               ##
               tabsetPanel(
                 id   = "percent-change",
                 type = "hidden",
                 
                 tabPanel("tab_states", 
                          
                          fluidRow(
                            
                            # -- Jurisdiction input
                            column(6, align = "center",
                                   selectizeInput("state",
                                                  label    = "Jurisdiction:",
                                                  choices  = states,
                                                  selected = c("Massachusetts", "Florida"),
                                                  multiple = TRUE,
                                                  options  = list(maxItems    = 5,
                                                                  placeholder = "Choose a state"))),
                            
                            # -- Date range input
                            column(6, align = "center",
                                   dateRangeInput("range", "Period",
                                                  start  = make_date(2020,03,01),
                                                  end    = max(cdc_counts$date),
                                                  min    = min(cdc_counts$date),
                                                  format = "M-dd-yyyy",
                                                  max    = max(cdc_counts$date)))),
                          
                          plotOutput("percent_change_usa")), # End of tabPanel
                 
                 tabPanel("tab_countries",
                          
                          fluidRow(
                            
                            # -- Jurisdiction input
                            column(6, align = "center",
                                   selectizeInput("countries",
                                                  label    = "Jurisdiction:",
                                                  choices  = countries,
                                                  selected = c("United States", "United Kingdom"),
                                                  multiple = TRUE,
                                                  options  = list(maxItems    = 5,
                                                                  placeholder = "Choose a country"))),
                            
                            # -- Date range input
                            column(6, align = "center",
                                   dateRangeInput("range_countries", "Period",
                                                  start  = make_date(2020,03,01),
                                                  end    = max(percent_change_countries$date),
                                                  min    = make_date(2020, 01, 01),
                                                  format = "M-dd-yyyy",
                                                  max    = max(percent_change_countries$date)))),
                          
                          plotOutput("percent_change_countries")) # End of tabPanel
                 
               )
               
               
      )
    ),
               
               
               
               
               
               
               
               # uiOutput("c_statesUI"),
               # uiOutput("c_statesUI_plot"),
               # uiOutput("c_countriesUI")
               
               # # -- Second set of inputs: Jurisdiction and Period. This should change depending on the input set 1
               # fluidRow(
               # 
               #   # -- Jurisdiction input
               #   column(6, align = "center",
               #          selectizeInput("state",
               #                         label    = "Jurisdiction:",
               #                         choices  = states,
               #                         selected = c("Massachusetts", "Florida"),
               #                         multiple = TRUE,
               #                         options  = list(maxItems    = 5,
               #                                         placeholder = "Choose a state"))),
               # 
               #   # -- Date range input
               #   column(6, align = "center",
               #          dateRangeInput("range", "Period",
               #                         start  = make_date(2020,03,01),
               #                         end    = max(cdc_counts$date),
               #                         min    = min(cdc_counts$date),
               #                         format = "M-dd-yyyy",
               #                         max    = max(cdc_counts$date))))
               # ,
               # plotOutput("percent_change")
))

# Define UI for application that draws a histogram
# shinyUI(fluidPage(theme = shinytheme("slate"),
#     
#     # Application title
#     title = "Excess Mortality in the USA",
#     # titlePanel("Excess Mortality in the USA"),
#     
#     # Show a plot of the generated distribution
#       mainPanel(align = "center",
        # selectizeInput("state",
        #                label    = "Jurisdiction:",
        #                choices  = states,
        #                selected = c("Massachusetts", "Florida"),
        #                multiple = TRUE,
        #                options  = list(maxItems    = 5,
        #                                placeholder = "Choose a state")),
        # dateRangeInput("range", "Period",
        #                start  = make_date(2020,03,01),
        #                end    = max(cdc_counts$date),
        #                min    = min(cdc_counts$date),
        #                format = "M-dd-yyyy",
        #                max    = max(cdc_counts$date)),
#         plotOutput("percent_change")
#     )
# ))
