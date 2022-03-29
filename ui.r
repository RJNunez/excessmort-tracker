source("functions.R")
source("init.R")
button_style <- "color: black; background-color: rgb(230, 220, 205); position: relative; 
                     text-align:center; border-radius: 6px; border-width: 2px; font-family: 'helvetica'; font-weight: bold"
### DO NOT DELETE ANY COMMENTS / COMMENTED LINES

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
               actionButton("data-panel", "Data", style = button_style),
               actionButton("ed-panel", "Cumulative Excess Mortality", style = button_style)),
      br(),
      
      # -- Universal inputs: Jurisdictions and Period
      fluidRow(
        column(2, align = "center"),
        
        # -- Jurisdiction input
        column(4, align = "center",
               selectizeInput("both",#"both_data",
                              label    = "Jurisdiction:",
                              choices  = sort(c(countries, states)),
                              selected = c("United States"),
                              multiple = TRUE,
                              options  = list(maxItems    = 5,
                                              placeholder = "Choose a jurisdiction"))),
        
        # -- Date range input
        column(4, align = "center",
               dateRangeInput("range", "Period",
                              start  = make_date(2020,03,01),
                              end    = max(percent_change_states$date),
                              min    = make_date(2020, 01, 01),
                              format = "M-dd-yyyy",
                              max    = max(percent_change_states$date))),
        
        column(6, align = "center")),
      
      
    # -- Data
    tabPanel("data",

             br(),
             p("•PC  = Percent change", align = "justify", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),
             p("•C19 = Covid-19 reported death toll", align = "justify", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),
             p("•CI  = 95% confidence interval", align = "justify", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),
             p("•CEM = Cumulative excess mortality", align = "justify", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),

             DT::dataTableOutput("table")),

    # -- Percent change panel
    tabPanel("percent-change",
             
             tabsetPanel(
               id   = "within-percent-change",
               type = "hidden",
            
               tabPanel("within-percent-change-both",

                        fluidRow(
                          # -- Jurisdiction input
                          column(4, align = "center"),
                          
                          column(4, align = "center",
                                 checkboxInput("percent-change-both-CI", "Add 95% Confidence interval", 
                                               value = FALSE)),
                        
                          # -- Date range input
                          column(4, align = "center")),
                        
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        h3("Percent change from average mortality", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                                        p("• You can choose up to five jurisdictions with ", code("Jurisdiction"), "and change the period of interest with ", code("Period"),
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The solid curve represents percent change from average mortality. You can add confidence intervals with ", code("Add 95% confidence interval"),
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        plotOutput("percent_change_both"),
                        br(),
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        uiOutput("percent_change_worse_title"),
                                        p("• These six states have the top percent increases in mortality", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The solid curve represents percent change from average mortality and the shaded area is a 95% confidence interval", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• Red represents upward trends in mortality and blue correponds to downward trends", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• You can change the date of interest with ", code("Period"), align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        plotOutput("percent_change_usa_worse"),
                        # br(),
                        # fluidRow(column(2),
                        #          column(8, align = "center",
                        #                 h3("Transition maps of percent increase in mortality", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                        #                 sliderTextInput("map_usa_date",
                        #                                 label    = "",
                        #                                 choices  = format(sort(unique(filter(percent_change_states, date >= "2020-03-01")$date)), "%b %d, %Y"),
                        #                                 selected = format(sort(unique(filter(percent_change_states, date >= "2020-03-01")$date))[1], "%b %d, %Y"),
                        #                                 grid     = FALSE,
                        #                                 animate  = animationOptions(interval = 1000, loop = FALSE))),
                        #          column(2)),
                        # plotOutput("us_map"),
                        # br(),
                        # fluidRow(column(2),
                        #          column(8, align = "center",
                        #                 sliderTextInput("map_world_date",
                        #                                 label    = "",
                        #                                 choices  = format(sort(unique(filter(percent_change_countries, date >= "2020-03-01")$date)), "%b %d, %Y"),
                        #                                 selected = format(sort(unique(filter(percent_change_countries, date >= "2020-03-01")$date))[1], "%b %d, %Y"),
                        #                                 grid     = FALSE,
                        #                                 animate  = animationOptions(interval = 1000, loop = FALSE))),
                        #          column(2)),
                        # plotOutput("world_map")
                        ) # End of tabPanel: within-percent-change-both
             )), # End of tabsetPanel: within-percent-change

    tabPanel("excess-deaths",

             tabsetPanel(
               id   = "within-excess-deaths",
               type = "hidden", 
               
               tabPanel("within-excess-deaths-both",

                        # -- Inputs
                        fluidRow(
                          # -- Jurisdiction input
                          column(3, align = "center"),
                          
                          column(2, align = "center",
                                 checkboxInput("excess-deaths-both-CI", "Add 95% Confidence interval", 
                                               value = FALSE)),
                          
                          
                          column(2, align = "center",
                                 checkboxInput("excess-deaths-both-C19", "Add Covid-19 death toll", 
                                               value = FALSE)),
                          
                          column(2, align = "left",
                                 radioButtons("excess-deaths-both-POP", "Per 100,000", 
                                              choices = c("Per 100,000", "Totals"), selected = "Per 100,000")),
                          
                          # -- Date range input
                          column(3, align = "center")),
                        
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        h3("Cumulative excess mortality", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                                        p("• You can choose up to five jurisdictions with ", code("Jurisdiction"), "and change the period of interest with ", code("Period"),
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The solid curve represents percent change from average mortality. You can add confidence intervals with ", code("Add 95% confidence interval"),
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• You can visualize excess mortality in counts or per 100,000 using the ",code("Per 100,000"), " radio buttos above",
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• You can add the reported Covid-19 death toll with ", code("Add Covid-19 death toll"), ". If so, the Covid-19 death toll is represented with dash curves",
                                          align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        plotOutput("excess_deaths_both"),
                        br(),
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        uiOutput("excess_deaths_worse_title"),
                                        p("• These six states have the top percent increases in mortality and are the same as those presented in the Percent Increase in Mortality tab", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The solid curve represents cumulative excess mortality per 100,000 since March 1, 2020, and the shaded area is a 95% confidence interval", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The dash curve represents the cumulative Covid-19 death toll per 100,000 since March 1, 2020", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• Red represents upward trends in mortality and blue correponds to downward trends (these trends are based on changes from average mortality)", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• You can change the date of interest with ", code("Period"), align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        plotOutput("excess_mortality_usa_worse"),
                        br(),
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        h3("Unaccounted deaths in US states", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                                        p("• The figure below shows the difference between cumulative excess deaths and the cumulative Covid-19 death toll per 100,000 since March 1, 2020, for each US state", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The bars represent 95% confidence intervals", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• Red represents upward trends in mortality and blue correponds to downward trends (these trends are based on changes from average mortality)", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        plotOutput("excess_v_covid_usa", height = "600px"),
                        br(),
                        fluidRow(column(2),
                                 column(8, align = "center",
                                        h3("Unaccounted deaths in multiple countries", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                                        p("• The figure below shows the difference between cumulative excess deaths and the cumulative Covid-19 death toll per 100,000 since March 1, 2020 for a myriad of countries", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black"),
                                        p("• The bars and colors have the same interpretation as above", align = "justify", style = "font-family: 'helvetica'; font-size: 12pt; color:black")),
                                 column(2)),
                        p("*The range of data for different countries is not necessarily the same", align = "justify", style = "font-family: 'helvetica'; font-size: 8pt; color:#969696"),
                        plotOutput("excess_v_covid_world", height = "600px"),
                        # br(),
                        # fluidRow(column(2),
                        #          column(8, align = "center",
                        #                 h3("Transition maps of cumulative excess mortality", align = "left", style = "font-family: 'helvetica'; color:black; text-shadow: 1px 1px 1px #aaa"),
                        #                 sliderTextInput("map_usa_ed_date",
                        #                                 label    = "",
                        #                                 choices  = format(sort(unique(filter(percent_change_states, date >= "2020-03-01")$date)), "%b %d, %Y"),
                        #                                 selected = format(sort(unique(filter(percent_change_states, date >= "2020-03-01")$date))[1], "%b %d, %Y"),
                        #                                 grid     = FALSE,
                        #                                 animate  = animationOptions(interval = 800, loop = FALSE))),
                        #          column(2)),
                        # plotOutput("us_ed_map"),
                        # br(),
                        # fluidRow(column(2),
                        #          column(8, align = "center",
                        #                 sliderTextInput("map_world_ed_date",
                        #                                 label    = "",
                        #                                 choices  = format(sort(unique(filter(world_counts, date >= "2020-03-01")$date)), "%b %d, %Y"),
                        #                                 selected = format(sort(unique(filter(world_counts, date >= "2020-03-01")$date))[1], "%b %d, %Y"),
                        #                                 grid     = FALSE,
                        #                                 animate  = animationOptions(interval = 800, loop = FALSE))),
                        #          column(2)),
                        # plotOutput("world_ed_map")
                        ) # End of tabpanel: within-excess-deaths-both
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
