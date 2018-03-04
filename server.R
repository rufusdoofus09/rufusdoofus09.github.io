#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(rlang)


wd<-getwd()
if (file.exists(file.path(wd,"..","Mortality_fns.R"))) {
    LOCAL=TRUE
} else {
    LOCAL=FALSE
}
if (LOCAL) {
    file_dir=file.path("file://",wd,"..")
    source(file.path(wd,"../Mortality_fns.R"))
} else {
    file_dir="https://raw.githubusercontent.com/rufusdoofus09/rufusdoofus09.github.io/master"
    source(file.path(file_dir,"Mortality_fns.R"))
}
state_summary <- read.csv(file.path(file_dir,"stateData.csv"),stringsAsFactors = F)
county_summary <- read.csv(file.path(file_dir,"countyData.csv"),stringsAsFactors = F)
GHO_infantMortality_2015 <- read.csv(file.path(file_dir,"GHO_infantMortality_2015.csv"),stringsAsFactors = F)

state_colors = rainbow(50)
region_colors = rainbow(5)

shinyServer <- function(input, output, session) {
    
    #output$region_out <- renderPrint(input$region_in)
    

    modelpred <- reactive({
        if (input$region_in != "") {
            areaChoice <- input$area
            colText <- input$region_in
            if (areaChoice == 1) {
                dataset = county_summary
                colName <- paste0("county_",columnName[colText])
            } else {
                dataset = state_summary
                colName <- paste0("state_",columnName[colText])
            }
            field <- as.symbol(colName)
            #dataset_sym <- as.symbol(dataset_str)
            xs<-eval(expr(dataset$UQ(field)))

        } else {
            xs<-"Pick a correlation factor"
        }
        xs
    })
    
    debugMsg <- reactive({
        if (input$region_in != "") {
            areaChoice <- input$area
            if (areaChoice == 1) {
                msg<-paste("You picked County",input$region_in)
            }
            else {
                msg<-paste("You picked State",input$region_in)
            }

        } else {
            msg<-"Pick a correlation factor and a granularity (County or State level) and hit 'Show Me'"
        }
        msg
    })
    
    fitMsg <- reactive({
        if (input$region_in != "") {
            areaChoice <- input$area
            colText <- input$region_in
            if (areaChoice == 1) {
                dataset = county_summary
                colName <- paste0("county_",columnName[colText])
                yName = "county_Mortality_rate"
            } else {
                dataset = state_summary
                colName <- paste0("state_",columnName[colText])
                yName = "state_Mortality_rate"
            }
            field <- as.symbol(colName)
            yfield <- as.symbol(yName)
            fit <- lm(eval(expr(dataset$UQ(yfield))) ~ eval(expr(dataset$UQ(field))), data = dataset)
            #dataset_sym <- as.symbol(dataset_str)
            #xs<-eval(expr(dataset$UQ(field)))
            
            #print(summary(fit))
            summary(fit)
        } 
    })
    
    output$plot <- renderPlot({
        if (input$region_in != "") {
            areaChoice <- input$area
            if (areaChoice == 1) {
                dataset = county_summary
                ys = dataset$county_Mortality_rate
                colors=state_colors[as.factor(dataset$state)]
                }
            else {
                dataset = state_summary
                ys = dataset$state_Mortality_rate
                colors=region_colors[as.factor(stateRegion[dataset$state])]
                }
            colText <- input$region_in
            xs<-modelpred()
            legendx=max(xs)-((max(xs)-min(xs))*.25)
            legendy=ceiling(max_US_Mortality_rate)
            plot(ys ~ xs, xlab = colText, 
                 ylab = "Mortality Rate (Deaths per 1000)", pch = 16,
                 ylim = c(0, legendy),col=colors)
            abline(h=weighted_mortality_rate_inclUS,col="blue")
            abline(h=weighted_mortality_rate_US,col="red")
            fit <- lm(ys ~ xs, data = dataset)
            abline(fit, col = "maroon", lwd = 3)
            legend(legendx, legendy, legend=c("OECD rate", "USA rate", "Trend"), lwd = 2, 
                   col = c("blue", "red","maroon"), bty = "n", cex = 1.2)
        }
    })
    output$debug <- renderText({
        debugMsg()
    })
    output$fit <- renderPrint({
        #print(fitMsg())
        fitMsg()
    })    
    output$rateTable <- renderTable(GHO_infantMortality_2015)
    output$colList <- renderTable(columnText,colnames = FALSE,spacing = "s")
    

}