source("init.R")
load("rda/covid19-usa.rda")
load("rda/cdc_counts.rda")
states <- unique(percent_change$state)




shinyUI(fluidPage(theme = shinytheme("slate"),
  
    # -- Title of the app              
    # titlePanel("Excess Mortality in the USA"),
                  
    tabsetPanel(
      
      
      tabPanel("Percent Change per States",
               
               # -- Tab Title
               h2("Percent Change from Average Mortality", align = "center", style = "font-family: 'times'; color:white"),
               p("We can add some text explaining the basics of the app just like FT does", 
                 align = "center", style = "font-family: 'times'; font-size: 12pt ; color:white"),
               
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
               
               fluidRow(
                 
                 column(6, align = "center", plotOutput("percent_change")),
                 column(6, align = "center", plotOutput("percent_changeV2"))))
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
