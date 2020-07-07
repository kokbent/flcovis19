#### THIS SCRIPT IS DESIGNED TO PROCESS DATA LOCALLY AND PASS DATA TO THE SHINY APP
library(tidyverse)
library(jsonlite)
source("testApp/functions.R")

#### DATA MATTER
## Update your linelist -> "data/linelist.csv"
## If not done manually, uncomment and run the following:
# download.file("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv",
#               "data/linelist.csv")

url1 <- paste0("https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/services/Florida_COVID19_Case_Line_Data_NEW/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=true&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=")
tmp <- fromJSON(url1)

total_rec <- tmp$count
nr <- total_rec %/% 32000 + 1
start1 <- seq(0, (nr-1)*32000, by=32000)
stop1 <- seq(32000, nr*32000, by=32000)

ll <- data.frame()
for (i in 1:nr) {
  url1 <- paste0("https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/services/Florida_COVID19_Case_Line_Data_NEW/FeatureServer/0/query?where=",
                 "ObjectId+>", start1[i], "+AND+ObjectId+<%3D+", stop1[i],
                 "&objectIds=&time=&resultType=standard&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=")
  tmp <- fromJSON(url1, simplifyDataFrame = T)
  while (!is.null(tmp$error)) {
    print(paste0("API quota problem, retrying in 10s. ", i))
    Sys.sleep(10)
    tmp <- fromJSON(url1, simplifyDataFrame = T)
  }
  ll <- bind_rows(ll, tmp$features$attributes)
}

ll$EventDate <- as.POSIXct(ll$EventDate/1000, origin="1970-01-01", tz="UTC") %>%
  format(format = "%Y-%m-%d %H:%M:%S")
ll$ChartDate <- as.POSIXct(ll$ChartDate/1000, origin="1970-01-01", tz="UTC") %>%
  format(format = "%Y-%m-%d %H:%M:%S")
write_csv(ll, "data/linelist.csv")


#### NOWCASTING (THIS TAKE A WHILE, RELAX)
## Code automatically pass data to Shiny app
source("nowcast_for_app.R")

#### BUILD CASE-DATE DATAFRAME
## Case-Date DF is much smaller (save space)
ll <- read.csv("data/linelist.csv")
colnames(ll)[1] <- "County" # Causing problem in my computer...
ll$EventDate <- ymd_hms(ll$EventDate) %>% as.Date
ll$ChartDate <- ymd_hms(ll$ChartDate) %>% as.Date
caseCount(ll)

#### EXPORT THE TWO FILES TO SHINY APP
write_csv(case_ev, "testApp/data/case_ev.csv")
write_rds(split_counties, "testApp/data/split_counties.rds")

#### BUILD TEST-HOSP-DEATH DATAFRAME
source("test_hosp_death.R")

#### BUILD COUNTY POSITIVITY DATAFRAME
source("ct_pos.R")
