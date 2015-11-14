
#Read in the data
library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(leaflet)

 departments <- read.csv("./data/epicDeptClean.csv")
 facilities <-   read.csv("./data/epicFacilitiesClean.csv")
 facilities <-  facilities%>%filter(!is.na(ADDRESS))
 facilities$statusColor <- sample(c("red","yellow","green"),nrow(facilities),replace=TRUE)
 allFacilityCities <- unique(toupper(as.character(facilities$CITY)))
 allFacilityCities <- allFacilityCities[order(allFacilityCities)]
