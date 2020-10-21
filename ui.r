source("init.R")
load("rda/covid19-usa.rda")
load("rda/cdc_counts.rda")
states <- unique(percent_change$state)
button_style <- "color: white; background-color: red; border-color: white; position: relative; 
                     text-align:center; border-radius: 6px; border-width: 2px"



shinyUI(fluidPage(theme = shinytheme("slate"),
  
    # -- Title of the app              
    # titlePanel("Excess Mortality in the USA"),
                  
    tabsetPanel(
      
      
      tabPanel("Percent Change per States",
               
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
               
               # -- Second set of inputs: Jurisdiction and Period. This should change depending on the input set 1
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

               plotOutput("percent_change"))
               # fluidRow(
               #   
               #   column(6, align = "center", plotOutput("percent_change")),
               #   column(6, align = "center", plotOutput("percent_changeV2")))
               
    ),
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
