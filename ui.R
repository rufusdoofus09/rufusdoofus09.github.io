#
# This is the user-interface definition for the "Mortality" Shiny web application.
# JHU Data Science Specialization: Developing Data Products project
# 4 March 2017

library(shiny)

wd<-getwd()
if (file.exists(file.path(wd,"..","Mortality_fns.R"))) {
    LOCAL=TRUE
} else {
    LOCAL=FALSE
}
#LOCAL=FALSE
if (LOCAL) {
    file_dir=file.path("file://",wd,"..")
    source(file.path(wd,"../Mortality_fns.R"))
} else {
    file_dir="https://raw.githubusercontent.com/rufusdoofus09/rufusdoofus09.github.io/master"
    source(file.path(file_dir,"Mortality_fns.R"))
}

# Define UI 
shinyUI(fluidPage(
    # Application title & subtitle
    titlePanel("Infant Mortality in the USA"),
    h3("    Deaths in first year per 1000 live births"),
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            radioButtons("area", label = h3("Area"),
                         choices = list("by County" = 1, "by State" = 2), 
                         selected = 1),            
            #verbatimTextOutput('region_out'),
            #            selectInput('region_in', 'Pick Variables', columnText_county, multiple=TRUE, selectize=FALSE),
            # selectize=TRUE lets selection be NULL in which case (and at beginning - no plot
            selectInput("region_in", 
                        label = h3("Variable"), 
                        c(
                          names(columnName)), multiple=FALSE, selectize=TRUE),
            
            submitButton("Show Me")
        ),
        
        
        
        # Main panel with 4 tabs 
        mainPanel(
            tabsetPanel(type = "tabs", 
                        # Show a plot of the requested distribution
                        tabPanel(
                            "Graph", 
                            plotOutput("plot"),
                            h3("Trend Details"),
                            verbatimTextOutput("fit"),
                            br(),
                            h3("Messages"),
                            textOutput("debug"),
                            br(),
                            h4("This is a class project for the JHU Data Science Specialization and not a definitive source of data")
                        ), 
                        tabPanel("Info", 
                                 br(), 
                                 h4("About this app"),
                                 p("The OECD (Organization for Economic Cooperation 
                                   and Development) members are Australia, Austria, 
                                   Belgium, Canada, Denmark, Finland, France, Germany, 
                                   Iceland, Ireland, Italy, Japan, the Netherlands, 
                                   New Zealand, Norway, Spain, Sweden, Switzerland, 
                                   the United Kingdom, and the United States of America."),
                                 p("This app uses data from 2015 and does not download 
                                   current data. All data was acquired via online queries 
                                   (see 'Credits' tab) and downloaded, consolidated and 
                                   tidied up, then saved for use by this app."),
                                 p("The variables chosen for correlation are those that
                                   might make a smaller region in the USA more or less like the
                                   other countries in the OECD. For example income, health
                                   insurance coverage, unemployment rate. This list is not  
                                   comprehensive since the time allotted to collect the data
                                    was limited."),
                                 br(), 
                                 h4("OECD Infant Mortality Rates in 2015"),
                                 tableOutput("rateTable")
                        ), 
                        tabPanel("Credits", 
                                 br(), 
                                 "LA Times Article: ",br(),
                                 a(href="http://www.latimes.com/science/sciencenow/la-sci-sn-childhood-mortality-usa-20180108-story.html", 
                                   "Why the United States is 'the most dangerous of wealthy nations for a child to be born into'"),
                                 br(), 
                                 br(), 
                                 "USA Census",
                                 tags$ul(
                                     tags$li(a(href="https://factfinder.census.gov/","USA Census Data")),
                                     tags$li(a(href="https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_16_5YR_DP03",
                                               "2011-2015 American Community Survey, Selected Economic Characteristics"))
                                 ),
                                 
                                 "WorldHealth Organization (WHO) Global Health Observatory (GHO)",
                                 tags$ul(
                                     tags$li(a(href="http://apps.who.int/gho/data/view.main.182",
                                               "GHO Indicators: Probability of dying per 1000 live births, Data by country")),
                                     tags$li(a(href="http://apps.who.int/gho/data/view.main.POP2040",
                                               "GHO Indicators: Population Data by country (Recent years)"))
                                 ),
                                 "United Nations(UN)",
                                 tags$ul(
                                     tags$li(a(href="http://data.un.org/Data.aspx?d=POP&f=tableCode%3A55",  
                                               "UN data - Live births by month of birth"))
                                 ),
                                 "Centers for Disease Control and Prevention (CDC)",
                                 tags$ul(
                                     tags$li(a(href="https://wonder.cdc.gov/controller/datarequest/D69",
                                               "CDC Linked Birth / Infant Death Records"))
                                 )
                        ), 
                        tabPanel("Help Documentation", 
                                 br(), 
                                 h4("Reviewers: This is the documentation for the Infant Mortality App"),
                                 h4("This is just 'buttonolgy'. Read 'Info' tab for details on the data, etc."),
                                 
                                 p("Select either State or County from the 'Area' radiobox as a level of detail for the region. 
                                   County is the default."),
                                 p("Select a variable to correlate with Infant Mortality Rate from the 'Variable' pulldown.
                                   If you're at a loss, 'Unemployment Rate' is a good start, and the default."),
                                 p("Hit the 'Show Me' button."),
                                 p("Your graph will appear in the 'Graph' tab. A trend line (basic linear model) is added 
                                   along with lines indicating the Infant Mortality rate for all OECD countries 
                                   (including the USA) and one just the USA."),
                                 p("Repeat as often as you want, hitting 'Show Me' after your picks."),
                                 br(), 
                                 p("The colors, though not very helpful, indicate region in the 'by State' 
                                    graph and state in the 'by County' graph."),
                                 br(), 
                                 h4("Variable List"),
                                 tableOutput("colList")
                                 
                        )
            )
            
        )
    )
))
    
    
    