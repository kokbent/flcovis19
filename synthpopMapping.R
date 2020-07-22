library(tidyverse)
library(leaflet)
library(geosphere)

#READ IN SYNTHETIC POPULATION FILES
synthPop_Escambia = read.csv(file = "population-escambia.txt", header = TRUE, sep = " ")
locations_Escambia = read.csv(file = "locations-escambia.txt", header = TRUE, sep = " ")
extracurricular_Escambia = read.csv(file = "extracurricular-escambia.txt", header = TRUE, sep = " ")
network_Escambia = read.csv(file = "network-escambia.txt", header = FALSE, sep = " ")

#SELECT ONE HOME AT RANDOM
rndHomeId = (locations_Escambia %>% 
  filter(type == "h") %>% 
  sample_n(1) %>% 
  select(locid))[1,1]

#SELECT ALL MEMBERS OF THE SELECTED HOME
synthPop_Escambia_singleHome = synthPop_Escambia %>% filter(home_id == rndHomeId)

#SELECT THE EXTRACURRICULAR ACTIVITES OF ALL MEMBERS OF THE SELECTED HOME
extracurricular_Escambia_singleHome = extracurricular_Escambia %>% filter(extracurricular_Escambia$pid %in% synthPop_Escambia_singleHome$pid)

#SELECT THE HOMES THAT THE SELECTED HOME ITERACTS WITH
network_Escambia_singleHome = network_Escambia %>% filter((V1 == rndHomeId) | (V2 == rndHomeId))

#SELECT ALL LOCATIONS ASSOCIATES WITH ALL MEMBERS OF THE SELECTED HOME (HOME, EXTRACURRICULARS, OTHER HOMES, DAYTIME LOCATIONS)
locations_Escambia_singleHome = locations_Escambia %>% filter(locid == rndHomeId | 
                                                                locid %in% unlist(extracurricular_Escambia_singleHome[2:6], use.names = FALSE) | 
                                                                locid %in% unlist(network_Escambia_singleHome[1:2], use.names = FALSE) |
                                                                locid %in% synthPop_Escambia_singleHome$day_id) 

#LABEL EVERY LOCATION
locLabel = function(id) {
  if(id == rndHomeId) {
    return("Selected Home")
  }
  else if(id %in% unlist(extracurricular_Escambia_singleHome[2:6], use.names = FALSE)) {
    return("Extracurricular Business")
  }
  else if(id %in% unlist(network_Escambia_singleHome[1:2], use.names = FALSE)) {
    return("Connected Home")
  }
  else if(id %in% synthPop_Escambia_singleHome$day_id) {
    return("Daytime Workplace")
  }
}

for(i in 1:nrow(locations_Escambia_singleHome)){
  ifelse(locations_Escambia_singleHome$type[i] == "s", (locations_Escambia_singleHome$locationLabel[i] = "Daytime School"), 
         (locations_Escambia_singleHome$locationLabel[i] = locLabel(locations_Escambia_singleHome$locid[i])))
}

#COUNT TYPES OF CONNECTIONS
numConnectHome = (locations_Escambia_singleHome %>% filter(locationLabel == "Connected Home") %>% tally)[1,1]
numExBiz = (locations_Escambia_singleHome %>% filter(locationLabel == "Extracurricular Business") %>% tally)[1,1]
numDayBiz = (locations_Escambia_singleHome %>% filter(locationLabel == "Daytime Workplace") %>% tally)[1,1]
numDaySchool = (locations_Escambia_singleHome %>% filter(locationLabel == "Daytime School") %>% tally)[1,1]


#MAPPING COLOR SCHEME
pal = colorFactor(c('#ff0d00', '#3db400', '#0060ff', '#b87000', '#e300ff'), 
                  domain = c("Connected Home", "Daytime School", "Daytime Workplace", "Extracurricular Business", "Selected Home"))

#CREATE DF FOR DRAWING LINES AND CALCULATING DISTANCES (IN M AND MI)
locLines = data.frame(locid = numeric(), y = numeric(), x = numeric(), distanceM = numeric(), distanceMi = numeric(), stringsAsFactors = FALSE)
for(i in 1:nrow(locations_Escambia_singleHome)){
  if(locations_Escambia_singleHome[i,1] != rndHomeId){
    df = data.frame(locid = numeric(), y = numeric(), x = numeric(), stringsAsFactors = FALSE)
    df[1,] = locations_Escambia_singleHome %>% filter(locid == rndHomeId) %>% select(locid, y, x)
    df[2,] = locations_Escambia_singleHome[i,] %>% select(locid, y, x)
    
    df$distanceM = distHaversine(c(df[1,3], df[1,2]), c(df[2,3], df[2,2])) %>% round(digits = 3)
    df$distanceMi = (((df$distanceM)/(1609)) %>% round(digits = 3))
    
    locLines = rbind(locLines, df)
  }
}

#APPEND LOCATION DF WITH DISTANCES
locations_Escambia_singleHome = full_join(locations_Escambia_singleHome, (locLines %>% filter(locid != rndHomeId) %>% select(locid, distanceM, distanceMi)), by = "locid")

#INTERACTIVE MAPPING
#CLICK ON CIRLCES FOR MORE INFORMATION ABOUT THAT LOCATION(LOCATION ID, TYPE, DISTANCE TO SELECTED HOME)
leaflet(data = locations_Escambia_singleHome) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~x, lat = ~y, color = ~pal(locationLabel), popup = ~as.character(paste(sep = "<br/>", paste0(locid, ", ", locationLabel), 
                                                                                                paste0(locid, " is ", distanceM, " m or ", distanceMi, " mi away from ", rndHomeId))),
                   label = ~as.character(paste0(locid, ", ", locationLabel))) %>% 
  addPolylines(lat = ~locLines$y, lng = ~locLines$x, color = "black", weight = 2) %>%
  addScaleBar(position = c("bottomleft"))

#OUTPUT SOME SUMMARIZING INFORMATION ABOUT THE SELECTED HOME AND GENERATED PLOT
print(paste0("The total number of locations associate with home ", rndHomeId, " is ", (nrow(locations_Escambia_singleHome)-1), " and ", 
             ifelse(nrow(synthPop_Escambia_singleHome) > 1, paste0(nrow(synthPop_Escambia_singleHome), " people live"), paste0(nrow(synthPop_Escambia_singleHome), " person lives")), 
             " at this home (purple circle)."))
print(paste0("There are ", numConnectHome, " connected homes (red circles), ", numExBiz, " extracurricular businesses (tan circles), ", 
             numDayBiz, " daytime workplaces (blue circles), and ", numDaySchool, " daytime schools (green circles)."))
