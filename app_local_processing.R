#### THIS SCRIPT IS DESIGNED TO PROCESS DATA LOCALLY AND PASS DATA TO THE SHINY APP
source("testApp/dataImportClean.R")

#### DATA MATTER
## Update your linelist -> "data/linelist.csv"
## If not done manually, uncomment and run the following:
download.file("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv",
              "data/linelist.csv")

#### NOWCASTING (THIS TAKE A WHILE, RELAX)
## Code automatically pass data to Shiny app
source("nowcast_for_app.R")

#### BUILD CASE-DATE DATAFRAME
## Case-Date DF is much smaller (save space)
colnames(ll)[1] <- "County" # Causing problem in my computer...
caseCount(ll)

#### EXPORT THE TWO FILES TO SHINY APP
write_csv(case_ev, "testApp/data/case_ev.csv")
write_rds(split_counties, "testApp/data/split_counties.rds")
