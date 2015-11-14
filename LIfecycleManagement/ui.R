





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
        
        sidebarMenu(
                menuItem("Sites", tabName = "map", icon = icon("map")),
                menuItem("Schedule", tabName = "schedule", icon = icon("calendar")),
                menuItem("End-Point Maps", tabName = "endPointMap", icon = icon("map")),
                menuItem("Technology"),
                menuSubItem("Device Profiles", tabName = "deviceProfiles", icon = icon("circle")),
                menuSubItem("User Personas", tabName = "personas", icon = icon("square")),
                
                menuItem("Help", tabName = "help", icon = icon("question-circle"))
        ),
        checkboxGroupInput(
                inputId = "region", label = "SLHS Regions",
                choices = list("East", "West", "Other"),
                selected = list("East", "West", "Other")
        ) ,
        checkboxGroupInput(
                inputId ="subregion", label = "SLHS Sub-regions",
                choices = list("Treasure Valley", "Magic Valley", "McCall","Nampa","Fruitland","Eastern Oregon", "Elmore","Wood River", "Other"),
                selected = list("Treasure Valley", "Magic Valley", "McCall","Nampa","Fruitland","Eastern Oregon", "Elmore","Wood River", "Other")
        ) ,
        selectInput("city", "City", allFacilityCities,  multiple = FALSE,
                    selectize = TRUE )
        
        
)


body <-  dashboardBody( tabItems(   tabItem(tabName = "map",
                                    fluidRow( 
                                            box(tile="Site Map",leafletOutput("siteMap" )) ),
                                    fluidRow( 
                                            box(tile="Departments",box(title="Departments",
                                                                             
                                                                       DT::dataTableOutput("departmentTable",width="100%",height="100%"))) )),
                                   tabItem(tabName = "schedule",
                                           h1("Deployment Schedule"),
                                           fluidRow(title = "Schedule",
                                                    box(tile="Schedule"),box(tile="metrics"))),
                                   tabItem(tabName = "endPointMap",
                                           h1("End Point Map"),
                                           fluidRow(title = "Area",
                                                    box(tile="Devices"),box(tile="metrics"))),
                                  
                           tabItem(tabName = "help",
                                   h1(" Dashboard Help"),
                                   fluidRow(title = "Help"))
)
        )

dashboardPage(dashboardHeader(title = "mySt Luke's Deployment Dashboard"),
              
              sidebar,
              body)

