library(tidyverse)
library(lubridate)
library(plotly)

dir.create("data")
#dir.create("plots")

#GLOBAL DATE SETTINGS
setGlobalDates <- function(setbackDate = 14) {
  effective_date <<- Sys.Date() - ddays(setbackDate) # 2 weeks of "unreliable time"
  today_date <<- as.character(Sys.Date()) %>% str_remove_all("-")
  display_date <<- Sys.Date() %>% format(format = "%B %d, %Y") %>% as.character()
}

#IMPORT DATA
getData <- function(){
  
  #CHECK IF DATA FILE EXIST, IF NOT, IMPORT DATA FROM FDOH
  in_file <- paste0("data/linelist_", today_date, ".csv")
  if (!file.exists(in_file)) {
    download.file("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv",
                  in_file)
  }
  
  dat <<- read.csv(in_file)
  colnames(dat)[1] <<- "County" # Weird character issue
}

#DATA CLEANUP FOR DATES
reDate <- function(){
  dat$EventDate <<- ymd_hms(dat$EventDate) %>% as.Date(tz = Sys.timezone())
  dat$ChartDate <<- ymd_hms(dat$ChartDate) %>% as.Date(tz = Sys.timezone())
}



#GROUP AND COUNT CASES BY DATE
caseCount <- function(){
  case_ev <<- dat %>% 
    group_by(EventDate) %>% 
    count %>% 
    ungroup
  
  county_ct <<- dat %>% 
    group_by(EventDate, County) %>% 
    count %>% 
    ungroup
}

#FILL IN MISSING DATES AND LABEL DAYS AND WEEKENDS
dateAvgFill <- function(){
  case_ev <<- complete(case_ev, 
                       EventDate = seq.Date(min(case_ev$EventDate), 
                                            max(case_ev$EventDate), by = 1),
                       fill = list(0)) %>%
    mutate(day = wday(EventDate, label = T),
           weekend = day %in% c("Sat", "Sun"),
           n = ifelse(is.na(n), 0, n))
  
  case_ev$ma7 <<- stats::filter(case_ev$n, rep(1/7, 7), sides = 1)
  case_ev$ca7 <<- stats::filter(case_ev$n, rep(1/7, 7), sides = 2)
  
  split_counties <<- split(county_ct, county_ct$County)
  
  for (i in seq(split_counties)) {
    split_counties[[i]] <<- complete(split_counties[[i]], EventDate = seq.Date(min(county_ct$EventDate), max(county_ct$EventDate), by = 1), 
                                     fill = list(0)) %>% 
                              mutate(day = wday(EventDate, label = T), 
                              weekend = day %in% c("Sat", "Sun"),
                              n = ifelse(is.na(n), 0, n))
    
    split_counties[[i]]$ma7 <<- stats::filter(split_counties[[i]]$n, rep(1/7, 7), sides = 1)
    split_counties[[i]]$ca7 <<- stats::filter(split_counties[[i]]$n, rep(1/7, 7), sides = 2)
    
  }
}
