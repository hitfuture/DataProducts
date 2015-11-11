

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(rMaps)
library(rCharts)
#library(leaflet)
library(RColorBrewer)
library(maps)
library(mapproj)
library(DT)
source("plotStates.R")


# if (packageVersion('shiny') > '0.7') {
#         library(shiny)
#         runGitHub("shiny-examples", "rstudio", subdir = "012-datatables")
# }

#require(devtools)
#install_github('ramnathv/rCharts@dev')
#install_github('ramnathv/rMaps')
#devtools::install_github('rstudio/shinyapps')
#devtools::install_github('rstudio/DT')
#install.packages("leaflet")

function(input, output) {
        breach.filtered <- reactive({
                selectedEntityTypes <- input$covertedEntityType
                range <- input$years
                firstYear <- range[1]
                #   message(firstYear)
                lastYear <- range[2]
                #   message(lastYear)
                breachesRange <-
                        breaches %>% filter((Breach.Year >= firstYear) &
                                                    (Breach.Year <= lastYear))
                breachesRange <-
                        breachesRange %>% filter(Covered.Entity.Type %in% selectedEntityTypes)
                breachesRange
        })
        
        
        breachTypesReactive <- reactive ({
                subsetOfBreaches <- breach.filtered()
                breachTypesByYear <-
                        subsetOfBreaches[,c(
                                "Breach.Year","Covered.Entity.Type","Individuals.Affected",names(df)
                        )]
                
                breach.types <-
                        breachTypesByYear %>% gather(
                                Breach.X,Breach.Type,-Breach.Year,-Covered.Entity.Type,-Individuals.Affected,na.rm =
                                        TRUE
                        )
                breach.types <- breach.types %>% select(-Breach.X)
                breach.types
        })
        
        output$breachPlotByYear <- renderPlot({
                breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) > 0) {
                        ggplot(
                                breachesInRange, aes(
                                        x = as.factor(Breach.Year),fill = Covered.Entity.Type
                                )
                        ) +
                                geom_histogram(color = "black") +
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Breaches") +
                                
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachImpactPlotByYear <- renderPlot({
                breachesInRange <- breach.filtered()
                
                breachesRange <-
                        breachesInRange %>% filter(!is.na(Covered.Entity.Type) &
                                                           !is.na(Individuals.Affected))  %>%
                        group_by(Breach.Year,Covered.Entity.Type) %>% summarise(impacted =
                                                                                        sum(Individuals.Affected)) %>% ungroup()
                if (nrow(breachesRange) > 0) {
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = impacted ,fill = Covered.Entity.Type
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        
        output$breachTypePlotByYear <- renderPlot({
                breachesRange <- breachTypesReactive()
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected))
                if (nrow(breachesRange) > 0) {
                        ggplot(breachesRange, aes(
                                x = as.factor(Breach.Year),fill = Breach.Type
                        )) +
                                geom_histogram(color = "black") +
                                #              facet_grid(.~Covered.Entity.Type)
                                xlab("Year - Breach Submitted") +
                                ylab("Breach Type - Count") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachTypeImpactPlotByYear <- renderPlot({
                breachesRange <- breachTypesReactive()
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected)) %>% group_by(Breach.Year,Breach.Type) %>% summarize(Individuals.Affected =
                                                                                                                                                   sum(Individuals.Affected))
                breachesRange[which(is.na(breachesRange$Individuals.Affected)),"Individuals.Affected"] <-
                        0
                #           p4 <- rPlot(Individuals.Affected~Breach.Year,color = "Breach.Type",type = "line",data=breachesRange)
                #           p4$guides(y = list(min = 0, title = ""))
                #           p4$guides(y = list(title = ""))
                #           p4$addParams(height = 300, dom = 'breachTypeImpactPlotByYear')
                #           return(p4)
                if (nrow(breachesRange) > 0) {
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = Individuals.Affected ,fill = Breach.Type
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
                } else {
                        "Failed"
                }
                
        })
        output$breachesByGeo <- renderChart2({
               # message("plot breaches")
                
                stateBreaches <- breach.filtered()%>%filter(!is.na(Individuals.Affected)&!is.na(State))%>%
                        group_by(State)%>%
                        summarize(Individuals.Affected=sum(Individuals.Affected))%>%
                       mutate(  
                                popup = sprintf("<strong>State:</strong> %s <br/><strong>Impacted:</strong> %s", 
                                                State, prettyNum(Individuals.Affected,big.mark=",") ) 
                        )
               # str(stateBreaches)
                ichoropleth(
                        cut(log10(Individuals.Affected),9,labels = FALSE) ~ State,
                        data = stateBreaches,
                        legend = TRUE,
                        geographyConfig = list(
                                popupTemplate = "#! function(geography, data){
        Shiny.onInputChange('State', geography.properties.name)
        return '<div class=hoverinfo><strong>' + data.popup + '</strong></div>';
      } !#"))

                        
                })
        
        output$breachData <- DT::renderDataTable({
                bdata <- breach.filtered()[,1:10]
               
                datatable(bdata , extensions = c("ColReorder",'ColVis'), options = list(dom = 'RC<"clear">lfrtip',pageLength=5, autoWidth = TRUE)
                ) 
#                 %>% formatStyle(
#                         'Web.Description',
#                         backgroundColor = styleInterval(3.4, c('gray', 'cyan'))

#                                                   )
                })
}



