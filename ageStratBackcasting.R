library(lubridate)
library(tidyverse)
library(cowplot)

#IMPORT DATA FROM FDOH
datDOH = read.csv(url("https://opendata.arcgis.com/datasets/37abda537d17458bae6677b8ab75fcb9_0.csv"))

#DATA CLEANUP FOR DATES
datDOH$EventDate = ymd_hms(datDOH$EventDate) %>% as.Date()
datDOH$ChartDate = ymd_hms(datDOH$ChartDate) %>% as.Date()

#BIN BY AGE
datDOH$ageBin = cut(datDOH$Age, breaks = c(-1, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109), 
                    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99", "100+"))

#CUT ONLY RELEVANT COLUMNS FROM DATA
datDOHCut = data.frame(County = datDOH$County, Age = datDOH$Age, Bin = datDOH$ageBin, Hospitalized = datDOH$Hospitalized, 
                       Died = ifelse(is.na(datDOH$Died), 0, 1), EventDate = datDOH$EventDate, ChartDate = datDOH$ChartDate)

#SPLIT DATA BY AGE BINS
splitDOH = split(datDOHCut, datDOHCut$Bin)

#COUNT DEATHS PER EVENT DATE
datBinCount = list()
for(i in 1:length(splitDOH)){
  datBinCount[[i]] <- splitDOH[[i]] %>%
    filter(Died == 1) %>% 
    group_by(EventDate) %>% 
    count %>% 
    ungroup
  
  names(datBinCount)[i] = names(splitDOH)[i]
}

#SUM ALL DEATHS
sumDeath = numeric(length(datBinCount))
for(j in 1:length(datBinCount)){
  print(sum(datBinCount[[j]][[2]]))
  sumDeath[j] = (sum(datBinCount[[j]][[2]]))
}

#COUNT HOSPITALIZATIONS PER EVENT DATE
datBinCountHosp = list()
for(i in 1:length(splitDOH)){
  datBinCountHosp[[i]] <- splitDOH[[i]] %>%
    filter(Hospitalized == "YES") %>% 
    group_by(EventDate) %>% 
    count %>% 
    ungroup
  
  names(datBinCountHosp)[i] = names(splitDOH)[i]
}

#SUM ALL HOSPITALIZATIONS
sumHosp = numeric(length(datBinCountHosp))
for(j in 1:length(datBinCountHosp)){
  print(sum(datBinCountHosp[[j]][[2]]))
  sumHosp[j] = (sum(datBinCountHosp[[j]][[2]]))
}

#GAMMA BACKCASTING MODEL
ifr = c(0.0000161, 0.0000695, 0.000309, 0.000844, 0.00161, 0.00595, 0.0193, 0.0428, 0.0780, 0.0780, 0.0780) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7158570/
hospRate = c(0.0000, 0.000408, 0.0104, 0.0343, 0.0425, 0.0816, 0.118, 0.166, 0.184, 0.184, 0.184) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7158570/
mu = 21
sd = 4
alpha = (mu/sd)^2
beta = (sd^2)/mu

#BACKCAST FROM DEATHS
for(k in 1:length(datBinCount)){
  if(nrow(datBinCount[[k]]) > 0){
    dateRevise = data.frame(EventDate = seq(as.Date("2020-01-01"), by = "day", length.out = (datBinCount[[k]]$EventDate[1] - as.Date("2020-01-01"))))
    datBinCount[[k]] = full_join(dateRevise, datBinCount[[k]], by = c("EventDate"))
    
    datBinCount[[k]]$serial = seq(1:nrow(datBinCount[[k]]))
    datBinCount[[k]]$InferInfec = 0
    
    for(d in 1:nrow(datBinCount[[k]])){
      if(datBinCount[[k]]$n[d] > 0 && (is.na(datBinCount[[k]]$n[d]) == FALSE)){
        inferRange = 1:(d-1)
        datBinCount[[k]]$InferInfec[inferRange] = datBinCount[[k]]$InferInfec[inferRange] + 
          (((datBinCount[[k]]$n[(d)])*(dgamma(x = datBinCount[[k]]$serial[d-(inferRange)], shape = alpha, scale = beta)))/(ifr[k]))
      }
    }
  }
}

#BACKCAST FROM HOSPITALIZATIONS
for(k in 1:length(datBinCountHosp)){
  if(nrow(datBinCountHosp[[k]]) > 0){
    dateRevise = data.frame(EventDate = seq(as.Date("2020-01-01"), by = "day", length.out = (datBinCountHosp[[k]]$EventDate[1] - as.Date("2020-01-01"))))
    datBinCountHosp[[k]] = full_join(dateRevise, datBinCountHosp[[k]], by = c("EventDate"))
    
    datBinCountHosp[[k]]$serial = seq(1:nrow(datBinCountHosp[[k]]))
    datBinCountHosp[[k]]$InferInfec = 0
    
    for(d in 1:nrow(datBinCountHosp[[k]])){
      if(datBinCountHosp[[k]]$n[d] > 0 && (is.na(datBinCountHosp[[k]]$n[d]) == FALSE)){
        inferRange = 1:(d-1)
        datBinCountHosp[[k]]$InferInfec[inferRange] = datBinCountHosp[[k]]$InferInfec[inferRange] + 
          (((datBinCountHosp[[k]]$n[(d)])*(dgamma(x = datBinCountHosp[[k]]$serial[d-(inferRange)], shape = alpha, scale = beta)))/(hospRate[k]))
      }
    }
  }
}

#SUM ALL INFERRED INFECTIONS FROM DEATHS
sumInferInfec = numeric(length(datBinCount))
for(l in 1:length(datBinCount)){
  if(ncol(datBinCount[[l]]) > 3){
    print(sum(datBinCount[[l]][[4]]))
    sumInferInfec[l] = (sum(datBinCount[[l]][[4]]))
  }
}

#SUM ALL INFERRED INFECTIONS FROM HOSPITALIZATIONS
sumInferInfecHosp = numeric(length(datBinCountHosp))
for(l in 1:length(datBinCountHosp)){
  if(ncol(datBinCountHosp[[l]]) > 3){
    print(sum(datBinCountHosp[[l]][[4]]))
    sumInferInfecHosp[l] = (sum(datBinCountHosp[[l]][[4]]))
  }
  
  if(sumInferInfecHosp[l] == Inf){
    sumInferInfecHosp[l] = 0
  }
}

#PRINT IFR, DEATHS, AND INFERRED INFECTIONS FROM DEATHS FOR EACH AGE BIN
for(m in 1:length(datBinCount)){
  print(paste("For the age bin ", names(datBinCount)[m], ", the reported death total is ", sumDeath[m], ", the IFR is ", ifr[m], 
              ", and the inferred number of infections from deaths is ", sumInferInfec[m], ".", sep = ""))
}
print(paste("The total number of deaths is ", sum(sumDeath), " and the total number of inferred infections from deaths is ", sum(sumInferInfec), ".", sep = ""))

#PRINT HOSPITALIZATION RATE, HOSPITALIZATIONS, AND INFERRED INFECTIONS FROM HOSPITALIZATIONS FOR EACH AGE BIN
for(m in 1:length(datBinCountHosp)){
  print(paste("For the age bin ", names(datBinCountHosp)[m], ", the reported number of hospitalizations is ", sumHosp[m], ", the IFR is ", hospRate[m], 
              ", and the inferred number of infections from deaths is ", sumInferInfecHosp[m], ".", sep = ""))
}
print(paste("The total number of hospitalization is ", sum(sumHosp), " and the total number of inferred infections from hospitalizations is ", sum(sumInferInfecHosp), ".", sep = ""))

#BASIC PLOTTING
binNum = 6
deathPlot = ggplot(data = datBinCount[[binNum]]) +
  geom_line(aes(x = EventDate, y = n, color = "Deaths")) +
  labs(x = "Event Date", 
       title = paste("Data for Age Bin ", names(datBinCount)[binNum], sep = ""))
inferPlot = ggplot(data = datBinCount[[binNum]]) +
  geom_line(aes(x = EventDate, y = InferInfec, color = "InferInfec")) + 
  labs(x = "Event Date", 
       title = paste("Data for Age Bin ", names(datBinCount)[binNum], sep = ""))
plot_grid(deathPlot, inferPlot)

#PLOTTING ALL INFERRED INFECTION PLOTS
colorList = c('#e6194B', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff',
              '#9A6324', '#aaffc3', '#000075', '#a9a9a9', '#ffffff', '#000000')

youngerPlot = ggplot() +
  geom_line(data = datBinCount[[2]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[2])) +
  geom_line(data = datBinCount[[3]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[3])) +
  geom_line(data = datBinCount[[4]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[4])) +
  geom_line(data = datBinCount[[5]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[5])) +
  geom_line(data = datBinCount[[6]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[6])) +
  # geom_line(data = datBinCount[[7]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[7])) +
  # geom_line(data = datBinCount[[8]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[8])) +
  # geom_line(data = datBinCount[[9]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[9])) +
  # geom_line(data = datBinCount[[10]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[10])) +
  # geom_line(data = datBinCount[[11]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[11])) +
  scale_color_manual(values = colorList[1:5],
                     labels = names(datBinCount)[2:6]) +
  labs(x = "Event Date", y = "Inferred Infections", color = "Age Bin",
       title = paste("Inferred Infections for Ages 10-59", sep = ""))

olderPlot = ggplot() +
  # geom_line(data = datBinCount[[2]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[2])) +
  # geom_line(data = datBinCount[[3]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[3])) +
  # geom_line(data = datBinCount[[4]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[4])) +
  # geom_line(data = datBinCount[[5]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[5])) +
  # geom_line(data = datBinCount[[6]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[6])) +
  geom_line(data = datBinCount[[7]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[7])) +
  geom_line(data = datBinCount[[8]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[8])) +
  geom_line(data = datBinCount[[9]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[9])) +
  geom_line(data = datBinCount[[10]], aes(x = EventDate, y = InferInfec, color = names(datBinCount)[10])) +
  geom_line(data = datBinCount[[11]], aes(x = EventDate, y = InferInfec, color = "final")) +
  scale_color_manual(values = colorList[6:10],
                     labels = names(datBinCount)[7:11]) +
  labs(x = "Event Date", y = "Inferred Infections", color = "Age Bin",
       title = paste("Inferred Infections for Ages 60-100+", sep = ""))
plot_grid(youngerPlot, olderPlot)
