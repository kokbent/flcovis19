#dir.create("data")
#dir.create("plots")

#GLOBAL DATE SETTINGS
setGlobalDates <- function(date, setbackDate = 14) {
  effective_date <<- date - ddays(setbackDate) # 2 weeks of "unreliable time"
  today_date <<- date
  display_date <<- date %>% format(format = "%B %d, %Y") %>% as.character()
}

#IMPORT DATA
getData <- function(){
  
  #PULL DATA FROM STORAGE (NEED TO RE-UPDATE LOCALLY EVERYDAY)
  case_ev <<- read.csv("data/case_ev.csv")
  split_counties <<- readRDS("data/split_counties.rds")
  pred_df <<- read_csv("data/statewide-nowcast-preds.csv") # Nowcasting
  
  case_ev$EventDate <<- ymd(case_ev$EventDate)
  
  #DATA CARPENTRY
  setGlobalDates(max(case_ev$EventDate, na.rm = T))
}

#DATA CLEANUP FOR DATES
reDate <- function(){
  dat$EventDate <<- ymd_hms(dat$EventDate) %>% as.Date()
  dat$ChartDate <<- ymd_hms(dat$ChartDate) %>% as.Date()
}



#GROUP AND COUNT CASES BY DATE
caseCount <- function(dat){
  case_ev <<- dat %>% 
    group_by(EventDate) %>% 
    count %>% 
    ungroup
  
  county_ct <<- dat %>% 
    group_by(EventDate, County) %>% 
    count %>% 
    ungroup
  
  dateAvgFill()
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
  
  county_ct$County <<- as.character(county_ct$County)
  split_counties <<- split(county_ct, county_ct$County)
  
  for (i in seq(split_counties)) {
    cnt <- split_counties[[i]]$County[1]
    split_counties[[i]] <<- complete(split_counties[[i]], 
                                     EventDate = seq(min(county_ct$EventDate), max(county_ct$EventDate), by = 1), 
                                     fill = list(County = cnt, n = 0)) %>% 
                              mutate(day = wday(EventDate, label = T), 
                              weekend = day %in% c("Sat", "Sun"),
                              n = ifelse(is.na(n), 0, n))
    
    split_counties[[i]]$ma7 <<- stats::filter(split_counties[[i]]$n, rep(1/7, 7), sides = 1)
    split_counties[[i]]$ca7 <<- stats::filter(split_counties[[i]]$n, rep(1/7, 7), sides = 2)
    
  }
}

#MODEBAR CONFIG FOR PLOTLY (STANDARDIZING IT EVERYWHERE)
plotly_conf <- function (p) {
  config(
    p,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
                               "toggleSpikelines")
  )
}