firstYear <- 2009
lastYear <- 2015
selectedEntityTypes <- list("Business Associate", "Health Plan") #"Healthcare Clearing House","Healthcare Provider"
breachesRange <-breaches%>%filter(((Breach.Year >= firstYear) & (Breach.Year <= lastYear))&!is.na(Individuals.Affected))%>%filter(!is.na(Covered.Entity.Type)&!is.na(Individuals.Affected))  %>%
        group_by(Breach.Year,Covered.Entity.Type)%>% summarise(impacted=sum(Individuals.Affected))%>%ungroup()
breachesRange <- breachesRange%>%filter(Covered.Entity.Type %in% selectedEntityTypes)


str(breaches)
