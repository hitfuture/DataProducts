





# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(DT)
require(rCharts)
#options(RCHART_LIB = 'polycharts')


sidebar <-         dashboardSidebar(
        headerPanel("Breach Dashboard ")  ,
        sliderInput(
                inputId = "years",
                label = "Years:",
                min = minYear,
                max = maxYear,
                value = c(minYear, maxYear),
                step = 1
        ),
        sidebarMenu(
               
                menuItem(
                        "Breaches by Year", tabName = "breachByYear",icon = icon("calendar")),
                menuItem("Breach Geography", tabName = "breachByGeo", icon = icon("map")),
                menuItem("Breach Data", tabName = "breachData", icon = icon("table")),
                menuItem("Help", tabName = "help", icon = icon("question-circle")
                         )),
       
        
       
        checkboxGroupInput(
                "covertedEntityType", label = h5("Covered Entity Type"),
                choices = list(
                        "Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"
                ),
                selected = list(
                        "Business Associate", "Health Plan", "Healthcare Clearing House","Healthcare Provider"
                )
        )
)
body <-  dashboardBody(tabItems(
        tabItem(tabName = "breachByYear",
                fluidRow(
                        box(title = "Breach Count",plotOutput("breachPlotByYear")),
                        box(title = "Breach Impact",plotOutput("breachImpactPlotByYear"))
                ),
                fluidRow(
                        box(title = "Breach Types - Count",plotOutput("breachTypePlotByYear")),
                        # box(title="Breach Types - Impact",showOutput("breachTypeImpactPlotByYear","polycharts"))
                        box(title = "Breach Types - Impact",plotOutput("breachTypeImpactPlotByYear"))
                        
                )),
        tabItem(tabName = "breachByGeo",
                h1("Breach By Geography"),
                   showOutput("breachesByGeo","datamaps")),
        tabItem(tabName = "breachData",
                h1("Breach Data"),
                        
                                title = "Breach Data",        
                                DT::dataTableOutput("breachData",width="100%",height="100%")
                       ),
        tabItem(tabName = "help",
                h1("Breach Dashboard Help"),
                fluidRow(title = "Help"))
))

dashboardPage(dashboardHeader(title = "U.S. PHI Breaches"),
              
              sidebar,
              body)
 