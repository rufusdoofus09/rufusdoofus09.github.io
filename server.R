#
# This is the server side definition for the "Mortality" Shiny web application.
# JHU Data Science Specialization: Developing Data Products project
# 4 March 2017

library(shiny)
library(dplyr)
library(tidyr)
library(rlang)


#wd<-getwd()
#if (file.exists(file.path(wd,"..","Mortality_fns.R"))) {
#    LOCAL=TRUE
#} else {
#    LOCAL=FALSE
#}
#LOCAL=FALSE
#if (LOCAL) {
#    file_dir=file.path("file://",wd,"..")
#    source(file.path(wd,"../Mortality_fns.R"))
#} else {
#    file_dir="http://raw.githubusercontent.com/rufusdoofus09/rufusdoofus09.github.io/master"
#    source(file.path(file_dir,"Mortality_fns.R"))
#}
#state_summary <- read.csv(file.path(file_dir,"stateData.csv"),stringsAsFactors = F)
#county_summary <- read.csv(file.path(file_dir,"countyData.csv"),stringsAsFactors = F)
#GHO_infantMortality_2015 <- read.csv(file.path(file_dir,"GHO_infantMortality_2015.csv"),stringsAsFactors = F)

source("Mortality_fns.R")
state_summary <- read.csv("stateData.csv",stringsAsFactors = F)
county_summary <- read.csv("countyData.csv",stringsAsFactors = F)
#GHO_infantMortality_2015 <- read.csv("GHO_infantMortality_2015.csv",stringsAsFactors = F)
GHO_Mortality_Population <- read.csv("GHO_Mortality_Population.csv",stringsAsFactors = F)

max_US_Mortality_rate_county<-max(county_summary$county_Mortality_rate)
max_US_Mortality_rate_state<-max(state_summary$state_Mortality_rate)
max_US_Mortality_rate<-max(max_US_Mortality_rate_county,max_US_Mortality_rate_state)

weighted_mortality_rate <- GHO_Mortality_Population$Mortality_Rate*GHO_Mortality_Population$Population_Thousands
weighted_mortality_rate_inclUS <- sum(weighted_mortality_rate)/sum(GHO_Mortality_Population$Population_Thousands)
weighted_mortality_rate_noUS <- sum(weighted_mortality_rate[1:19])/sum(GHO_Mortality_Population$Population_Thousands[1:19])
weighted_mortality_rate_US <- GHO_Mortality_Population$Mortality_Rate[20]

state_colors = rainbow(50)
region_colors = rainbow(5)

shinyServer <- function(input, output, session) {
    
    #output$region_out <- renderPrint(input$region_in)
    

    modelpred <- reactive({
        if (input$region_in != "") {
            areaChoice <- input$area
            colText <- input$region_in
            if (areaChoice == 1) {
                dataset <- county_summary
                colName <- paste0("county_",columnName[colText])
            } else {
                dataset <- state_summary
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
                dataset <- county_summary
                colName <- paste0("county_",columnName[colText])
                yName = "county_Mortality_rate"
            } else {
                dataset <- state_summary
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
                dataset <- county_summary
                ys <- dataset$county_Mortality_rate
                colors<-state_colors[as.factor(dataset$state)]
                }
            else {
                dataset <- state_summary
                ys <- dataset$state_Mortality_rate
                colors<-region_colors[as.factor(stateRegion[dataset$state])]
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
    output$rateTable <- renderTable(GHO_Mortality_Population[,1:2])
    output$colList <- renderTable(columnText,colnames = FALSE,spacing = "s")
    

}