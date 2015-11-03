




# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
minYear <- min(breaches$Breach.Year)
maxYear <- max(breaches$Breach.Year)
midYear <- minYear + round((maxYear - minYear) / 2)
dashboardPage(
        dashboardHeader(title = "U.S. PHI Breaches"),
        dashboardSidebar(
        headerPanel("U.S. Department of ")  ,   
        sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard")
                        , icon = icon("dashboard")
                ),
                menuItem("Breaches by Year", tabName = "breachByYear")
                , icon = icon("th"),
         menuItem("Breach Geography", tabName = "breachByGeo"), icon = icon("th"),
        sliderInput(
                              inputId = "years",
                                 label = "Years:",
                                 min = minYear,
                                 max = maxYear,
                                 value = c(minYear, maxYear),
                                 step = 1
                         ),
        checkboxGroupInput("covertedEntityType", label = h5("Covered Entity Type"), 
                           choices = list("Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"),
                           selected = list("Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"))
        ),
        dashboardBody(tabItems(
                               tabItem(tabName = "breachByYear",
                                         fluidRow(
                                                 box(plotOutput("breachPlotByYear")),
                                                 box(plotOutput("breachImpactPlotByYear")))),
                               tabItem(tabName = "breachByGeo",
                                       h1("Breach By Geography"),
                                       fluidRow(
                                               box()
                                                )
                                       )
        ))
       
        )
        #                ,
        
        # Show a plot of the generated distribution
#         dashboardBody(tabItems(
#                 tabItem(tabName = "breachByYear",
#                         fluidRow(plotOutput("breachPlotByYear")))
#         ))
#        )


