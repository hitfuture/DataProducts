
#Read in the data
library(xlsx)
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(leaflet)

 departments <- read.csv("./data/epicDeptClean.csv")
 facilities <-   read.csv("./data/epicFacilitiesClean.csv")
 facilities <-  facilities%>%filter(!is.na(ADDRESS))
 facilities$statusColor <- sample(c("red","yellow","green"),nrow(facilities),replace=TRUE)
 facilities$statusColor <- sample(c("red","yellow","green"),nrow(facilities),replace=TRUE)
 locations <- read.csv("./data/locations_by_dept_2015-09-22.csv")
 departmentsViaLocs <- locations%>%group_by(departmentID,department_name)%>%summarise(emp_count=sum(n))
 allFacilityCities <- unique(toupper(as.character(facilities$CITY)))
 allFacilityCities <- allFacilityCities[order(allFacilityCities)]
 ourDevices <- read.csv("./data/devprofiles.csv" )
# write.csv(ourDevices,"./data/devprofiles.csv",row.names = FALSE )
 