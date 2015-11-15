

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
#library(rMaps)
library(rCharts)
library(leaflet)
library(RColorBrewer)
library(maps)
library(mapproj)
library(DT)

function(input,output) {
        facilitiesInBound <- reactive({
                if (is.null(input$siteMap_bounds))
                        return(facilities[FALSE,])
                bounds <- input$siteMap_bounds
                latRng <- range(bounds$north, bounds$south)
                lngRng <- range(bounds$east, bounds$west)
                
                facilities%>%filter(
                        lat >= latRng[1] & lat <= latRng[2] &
                                lon >= lngRng[1] & lon <= lngRng[2])
                
        })
        departmentsInBound <- reactive({
                         if (is.null(input$siteMap_bounds))
                                return(departments[FALSE,])
                        bounds <- input$siteMap_bounds
                        latRng <- range(bounds$north, bounds$south)
                        lngRng <- range(bounds$east, bounds$west)
                        
                        departments%>%filter(
                               lat >= latRng[1] & lat <= latRng[2] &
                                       lon >= lngRng[1] & lon <= lngRng[2])
               
        })
        showSitePopup <- function(siteCode, lat, lng) {
                message(sprintf("Site Code %s",siteCode))
                 selectedSite <- facilities[facilities$Facility.ID == siteCode,]
                 selectedDepartments <- (departments%>%filter(FacilityID == siteCode))
                 departmentCount <- nrow(selectedDepartments)
                message(nrow(selectedSite))
                content <- as.character(tagList(
                        tags$strong( selectedSite$FACILITY.NAME),
                        tags$br(),
                        sprintf("Number of Departments %s",  departmentCount), tags$br(),
                        sprintf("TDR Readiness Status: %s", selectedSite$statusColor), tags$br(),
                        sprintf("Deployment Status: %s", "In Progress"), tags$br(),
                        sprintf("%s ",selectedSite$fullAddress)
                        
                ))
                leafletProxy("siteMap") %>% addPopups(lng, lat, content, layerId = "siteMap")
        }
        # When map is clicked, show a popup with site info
        observeEvent(input$siteMap_marker_click , { 
                
                       event <- input$siteMap_marker_click
                       message(str(event))
                      
 
                    leafletProxy("siteMap") %>% clearPopups()
                   
                isolate( {
                        showSitePopup(input$siteMap_marker_click$id, input$siteMap_marker_click$lat, input$siteMap_marker_click$lng)
                })
         })
        
        # Store last zoom button value so we can detect when it's clicked
        
        lastZoomButtonValue <- NULL
        
        output$siteMap <- renderLeaflet({
                colors <-  facilities$statusColor
                map <- leaflet(facilities)      %>% 
                        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
                        addCircleMarkers(
                                ~lon,
                                ~lat,
                                fillColor = colors,
                                opacity = .8,
                                radius = 6,
                                layerId = ~Facility.ID
                        )%>%
                        setView(-116.2197 ,43.63117, zoom = 7)
                
                
                
                map
        })
        output$departmentTable <- DT::renderDataTable({
                 
                        depts <- departmentsInBound()%>%select(FacilityID,DeptID,Department.Name,Center,Specialty)
                        if(nrow(depts) == 0)return(datatable(depts))
                        
                        datatable(depts , extensions = c("ColReorder",'ColVis' ), options = list(dom = 'RC<"clear">lfrtip',pageLength=5, autoWidth = TRUE,
                                                                                                 colVis = list(exclude = c(0, 1), activate = 'mouseover')
                        ) )
                                                                 
                
       
        })
        output$facilityTable <- DT::renderDataTable({
                
                selectedFacilities <- facilitiesInBound() 
                if(nrow(selectedFacilities) == 0)return(datatable(selectedFacilities))
                selectedFacilities<- selectedFacilities%>%select(Facility.ID,FACILITY.NAME,fullAddress)
                
                datatable(selectedFacilities , extensions = c("ColReorder",'ColVis' ), options = list(dom = 'RC<"clear">lfrtip',pageLength=5, autoWidth = TRUE,
                                                                                         colVis = list(exclude = c(0, 1), activate = 'mouseover')
                ) )
                
                
                
        })
        
}


