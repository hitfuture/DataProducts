
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
breaches <- read.csv("./data/onc_breach_report.csv" ,na.strings = c("","\\N"))
breaches$Breach.Submission.Date <- mdy(breaches$Breach.Submission.Date)
breaches$Breach.Year <- year(breaches$Breach.Submission.Date)
breaches$Web.Description <- as.character(breaches$Web.Description)
breachesWDesc<- breaches%>%filter(!is.na(Web.Description))
breachType <- c("Theft","Loss","Other ","Hacking/IT Incident","Improper Disposal","Unauthorized Acess/Disclosure","Unknown") 
breaches$breach.types <- sapply(as.character(breaches$Type.of.Breach), strsplit,split=",")
maxCols <-max(sapply(breaches$breach.types,length) )
breachTypes <- lapply(breaches$breach.types,function(r) { l <- length(r); lastCols <- rep(NA,maxCols - l); as.list(c(r,lastCols)) })
df <- data.frame(matrix(unlist(breachTypes),length(breachTypes),byrow=TRUE))
col.count <- ncol(df)
names(df) <-   paste("breach.count",1:col.count,sep=".") 
#Clean up the levels
mlevels <- c()
for(i in 1:ncol(df)) {
        mlevels <- c(levels(df[,i]),mlevels)
        
}
for(i in 1:ncol(df)) {
        levels(df[,i]) <-  mlevels
        
}
breaches<-cbind(breaches,df)
breaches <- breaches%>%select(-breach.types)
breachTypesByYear <- breaches[,c("Breach.Year","Covered.Entity.Type","Individuals.Affected",names(df))]

breach.types <- breachTypesByYear%>%gather(Breach.X,Breach.Type,-Breach.Year,-Covered.Entity.Type,-Individuals.Affected,na.rm=TRUE)
breach.types <- breach.types%>%select(-Breach.X)
  
minYear <- min(breaches$Breach.Year)
maxYear <- max(breaches$Breach.Year)
midYear <- minYear + round((maxYear - minYear) / 2)
shinyServer(function(input, output) {

  breach.filtered <- reactive(  {
          selectedEntityTypes <- input$covertedEntityType
          range <- input$years
          firstYear <- range[1]
          message(firstYear)
          lastYear <- range[2]
          message(lastYear)
          breachesRange <-
                  breaches %>% filter((Breach.Year >= firstYear) &
                                              (Breach.Year <= lastYear))
          breachesRange <-
                  breachesRange %>% filter(Covered.Entity.Type %in% selectedEntityTypes)
          breachesRange
  })
  
  
  output$breachPlotByYear <- renderPlot({
          breachesInRange <- breach.filtered()
          ggplot( breachesInRange, aes( x = as.factor(Breach.Year),fill=Covered.Entity.Type)) + 
                  geom_histogram(color="black" )+
                  theme_bw()+
                  theme( axis.text.x = element_text(angle = 45,vjust=.5)) 

  })
  output$breachImpactPlotByYear <- renderPlot({
          breachesInRange <- breach.filtered()

          breachesRange <-breachesInRange%>%filter(!is.na(Individuals.Affected))%>%filter(!is.na(Covered.Entity.Type)&!is.na(Individuals.Affected))  %>%
                  group_by(Breach.Year,Covered.Entity.Type)%>% summarise(impacted=sum(Individuals.Affected))%>%ungroup()
 
          ggplot( breachesRange, aes( x = as.factor(Breach.Year),y=impacted ,fill=Covered.Entity.Type  )) + 
                  geom_bar(stat = "identity" ,color="black" )+
                  xlab("Year - Breach Submitted") +
                  ylab("Number of Individuals Impacted") +
                  theme_bw()+
                  theme( axis.text.x = element_text(angle = 45,vjust=.5)) 
          
  })
  
  output$breachTypePlotByYear <- renderPlot({         
          selectedEntityTypes <- input$covertedEntityType 
          range <- input$years
          firstYear <- range[1]
          lastYear <- range[2]
          breachesRange <-breach.types%>%filter(((Breach.Year >= firstYear) & (Breach.Year <= lastYear)) ) 
          breachesRange <- breachesRange%>%filter(Covered.Entity.Type %in% selectedEntityTypes)
          ggplot( breachesRange, aes( x = as.factor(Breach.Year),fill=Breach.Type )) + 
                  geom_histogram(color="black")+
   #              facet_grid(.~Covered.Entity.Type)
                  xlab("Year - Breach Submitted") +
                  ylab("Breach Type - Count") +
                  theme_bw()+
                  theme( axis.text.x = element_text(angle = 45,vjust=.5)) 
          
  })
  output$breachTypeImpactPlotByYear<- renderPlot({         
          selectedEntityTypes <- input$covertedEntityType 
          range <- input$years
          firstYear <- range[1]
          lastYear <- range[2]
          breachesRange <-breach.types%>%filter(((Breach.Year >= firstYear) & (Breach.Year <= lastYear)) ) 
          breachesRange <- breachesRange%>%filter(Covered.Entity.Type %in% selectedEntityTypes)
          ggplot( breachesRange, aes( x = as.factor(Breach.Year),y=Individuals.Affected ,fill=Breach.Type  )) + 
                  geom_bar(stat = "identity" ,color="black" )+
                  xlab("Year - Breach Submitted") +
                  ylab("Number of Individuals Impacted") +
                  theme_bw()+
                  theme( axis.text.x = element_text(angle = 45,vjust=.5)) 
          
  })

})
