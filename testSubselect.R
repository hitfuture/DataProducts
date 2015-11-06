firstYear <- 2009
lastYear <- 2015
selectedEntityTypes <- list("Business Associate", "Health Plan") #"Healthcare Clearing House","Healthcare Provider"
breachesRange <-breaches%>%filter(((Breach.Year >= firstYear) & (Breach.Year <= lastYear))&!is.na(Individuals.Affected))%>%filter(!is.na(Covered.Entity.Type)&!is.na(Individuals.Affected))  %>%
        group_by(Breach.Year,Covered.Entity.Type)%>% summarise(impacted=sum(Individuals.Affected))%>%ungroup()
breachesRange <- breachesRange%>%filter(Covered.Entity.Type %in% selectedEntityTypes)


str(breaches)
\\\\




firstYear <- 1999
lastYear <- 2015
selectedEntityTypes <- levels(breaches$Covered.Entity.Type)

breachesRange <-breach.types%>%filter(((Breach.Year >= firstYear) & (Breach.Year <= lastYear)) ) 
breachesRange <- breachesRange%>%filter(Covered.Entity.Type %in% selectedEntityTypes)
breachesRange <- breachesRange %>% group_by(Breach.Year,Breach.Type)%>%summarize(Individuals.Affected=sum(Individuals.Affected))
breachesRange[which(is.na(breachesRange$Individuals.Affected)),] <- 0


b2015 <- breaches%>%filter(Breach.Year=="2015")
breaches[which(is.na(breaches$Individuals.Affected)),"Individuals.Affected"] <- 0


sum(b2015$Individuals.Affected,rm.na=TRUE)
