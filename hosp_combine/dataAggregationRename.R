library(dplyr)
library(stringr)
library(readr)
library(lubridate)

columnSyn = read.csv("colnames/col_syn.csv", stringsAsFactors = FALSE)
countyFips = read.csv("colnames/fipsCodes.csv", stringsAsFactors = FALSE)
fileList = list.files("./input", "*.csv", full.names = TRUE)

df = data.frame()
colsRemoved = c("DOH FIPS Code", "FIPS Code")

for(i in 1:length(fileList)){
   dat = read_csv(fileList[i])
   
   fileNameTMP = str_split(fileList[i], "_")
   fileDate = mdy(fileNameTMP[[1]][4])
   fileTime = str_split(fileNameTMP[[1]][5], ".csv")[[1]][1]
   dat$Date = fileDate
   dat$Time = fileTime
   
   for(j in 1:length(colnames(dat))){
     if(colnames(dat)[j] %in% columnSyn$existing){
       colnames(dat)[j] = columnSyn$rename[columnSyn$existing == colnames(dat)[j]]
     }
   }
   
   dat = dat[,!colnames(dat) %in% colsRemoved]
   dat = inner_join(dat, countyFips, by = "County")
   
   df = bind_rows(df, dat)
}

todayDate = Sys.time() %>% format(format = "%Y-%m-%d_%H%M") %>% as.character()
outputFileName = paste0("./output/aggregatedHospitalData_",todayDate,".csv")

write.csv(df, outputFileName, row.names = FALSE)
