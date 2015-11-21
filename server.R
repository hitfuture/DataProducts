

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

formatNumbers <- function(y){
       formatNumber <-  function(x) {
                if(x< 1e+03){return(comma(x))}
                if(x < 1e+06) {return(paste(x/1e03,"T"))}
                if(x < 1e+09) {return(paste(x/1e06,"M"))}
                if(x >= 1e+09) {return(paste(x/1e09,"B"))}
        }
        sapply(y,formatNumber )}

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
                 selectedEntityTypes <- input$coveredEntityType
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
                message("impact plot by year")
                breachesInRange <- breach.filtered()
                if (nrow(breachesInRange) == 0) return()
                
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
                                scale_y_continuous(labels=comma)+
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
                if (nrow(breachesRange) == 0) return()
                
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected))
                if (nrow(breachesRange) > 0) {
                        ggplot(breachesRange, aes(
                                x = as.factor(Breach.Year),fill = Breach.Type
                        )) +
                                geom_histogram(color = "black") +
                                #              facet_grid(.~Covered.Entity.Type)
                                scale_y_continuous(labels=comma)+
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
                if (nrow(breachesRange) == 0) return()
                
                breachesRange <-
                        breachesRange %>% filter(!is.na(Covered.Entity.Type) &
                                                         !is.na(Individuals.Affected)) %>% group_by(Breach.Year,Breach.Type) %>% summarize(Individuals.Affected =
                                                                                                                                                   sum(Individuals.Affected))
                
                breachesRange[which(is.na(breachesRange$Individuals.Affected)),"Individuals.Affected"] <-0
                
                 
               
                        ggplot(
                                breachesRange, aes(
                                        x = as.factor(Breach.Year),y = Individuals.Affected ,fill = Breach.Type
                                )
                        ) +
                                geom_bar(stat = "identity" ,color = "black") +
                                scale_y_continuous(labels=comma)+
                                xlab("Year - Breach Submitted") +
                                ylab("Number of Individuals Impacted") +
                                theme_bw() +
                                theme(axis.text.x = element_text(angle = 45,vjust = .5))
         
                
        })
        output$breachesByGeo <- renderChart2({
               # message("plot breaches")
                
                stateBreaches <- breach.filtered()%>%filter(!is.na(Individuals.Affected)&!is.na(State))%>%
   #                     group_by(State,Breach.Year)%>%
                        group_by(State)%>%
                        summarize(Individuals.Affected=sum(Individuals.Affected),count=n())%>%
                       mutate(  
                                popup = sprintf("<strong>State:</strong> %s <br/><strong>Impacted:</strong> %s <br/><strong>Count:</strong> %s", 
                                                State, prettyNum(Individuals.Affected,big.mark=",") ,prettyNum(count,big.mark=",") ) 
                        )%>%ungroup()#%>%
 #               arrange(Breach.Year,State)
                if(nrow(stateBreaches) == 0)stop("No Data Available")
                str(stateBreaches)
                ichoropleth(
                        Individuals.Affected ~ State,
                        data = stateBreaches,
                        ncuts = 4,
                        legend = TRUE, pal = "YlOrRd",
#                        animate='Breach.Year',
                        geographyConfig = list(
                                popupTemplate = "#! function(geography, data){
        Shiny.onInputChange('State', geography.properties.name)
        return '<div class=hoverinfo><strong>' + data.popup + '</strong></div>';
      } !#"))

                        
                })
        
        output$breachData <- DT::renderDataTable({
                bdata <- breach.filtered()[,1:10]
                if(nrow(bdata) == 0)return(datatable(bdata))
               
                datatable(bdata , extensions = c("ColReorder",'ColVis','Responsive' ), options = list(dom = 'RC<"clear">lfrtip',pageLength=20, autoWidth = TRUE,
                                                                                         colVis = list(exclude = c(0, 1), activate = 'mouseover')
                ) )

                })
        output$helpOverview <- renderUI({div(p("As required by section 13402(e)(4) of the HITECH Act, the Secretary must post a list of breaches of unsecured protected health information affecting 500 or more individuals. These breaches are now posted in a new, more accessible format that allows users to search and sort the posted breaches. Additionally, this new format includes brief summaries of the breach cases that OCR has investigated and closed, as well as the names of private practice providers who have reported breaches of unsecured protected health information to the Secretary. The following breaches have been reported to the Secretary:
"))})
        output$helpTimeRange <- renderUI({
                (div(
                        p("Personal Health Information Breaches have been tracked by the Office of the National Coordinator for Health Information Technology (ONC) of the United States government since the year 2009. The time range is controlled by the Years slider in the left side panel. "),
                        tags$table(tags$tr(tags$td(img(src="yearRange.png", height=200,width=100)),
                                           tags$td(""),
                                           tags$td(" To select the range of time that you would like to investigate, use the  'Years:` slider to select the start and finish times.  This allows you to control all tabs on the Breach Dashboard.")))
                        
                )
                )
                
        })
        output$helpCoveredEntity <- renderUI({
                (div(
                        p("Covered entities are defined in the HIPAA rules as (1) health plans, (2) health care clearinghouses, and (3) health care providers who electronically transmit any health information in connection with transactions for which HHS has adopted standards. "),
                        tags$table(tags$tr(tags$td(img(src="coveredEntityTypes.png", height=200,width=100)),
                                           tags$td(""),
                                           tags$td(" To select the covered entity type that you would like to investigate, use the  'Covered Entity Type:` slider to select the start and finish times.  This allows you to control all tabs on the Breach Dashboard.")))
                        
                )
                )
                
        })
}
       


