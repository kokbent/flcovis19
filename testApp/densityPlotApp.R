library(sf)
library(tidyverse)

flPopDen <- st_read("shp/flPopDen.shp")
flPopDen$countyCode <- factor(flPopDen$countyCode, levels = flPopDen$countyCode[order(flPopDen$popDensqMi, decreasing = TRUE)])
flPopDen <- flPopDen[order(flPopDen$popDensqMi, decreasing = TRUE),]

region_1 <- flPopDen[1,] %>% 
  mutate(County = name) %>% 
  select(-name)
region_2 <- flPopDen[2,]%>% 
  mutate(County = name) %>% 
  select(-name)
region_3 <- flPopDen[c(3:7),] %>% 
  sapply(as.character) %>% 
  as_tibble() %>% 
  mutate(County = name) %>% 
  select(-name)
region_4 <- flPopDen[c(8:16),] %>% 
  sapply(as.character) %>% 
  as_tibble() %>% 
  mutate(County = name) %>% 
  select(-name)
region_5 <- flPopDen[c(17:35),] %>% 
  sapply(as.character) %>% 
  as_tibble() %>% 
  mutate(County = name) %>% 
  select(-name)
region_6 <- flPopDen[c(36:67),] %>% 
  sapply(as.character) %>% 
  as_tibble() %>% 
  mutate(County = name) %>% 
  select(-name)

flPopDenPlot <- flPopDen %>%
  ggplot(aes(x = countyCode, y = popDensqMi)) +
  scale_x_discrete(labels = flPopDen$name) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "County", y = "Population Density (ppl/sq. mi)") +
  annotate("rect", xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#E69F00") +
    geom_vline(xintercept = 1.5, size = 0.3, linetype = "dashed") +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#56B4E9") +
    geom_vline(xintercept = 2.5, size = 0.3, linetype = "dashed") +
  annotate("rect", xmin = 2.5, xmax = 7.5, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#009E73") +
    geom_vline(xintercept = 7.5, size = 0.3, linetype = "dashed") +
  annotate("rect", xmin = 7.5, xmax = 16.5, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#F0E442") +
    geom_vline(xintercept = 16.5, size = 0.3, linetype = "dashed") +
  annotate("rect", xmin = 16.5, xmax = 35.5, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#0072B2") +
    geom_vline(xintercept = 35.5, size = 0.3, linetype = "dashed") +
  annotate("rect", xmin = 35.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.4, fill = "#D55E00") +
  geom_point(size = 1.5) +
  geom_line(group = 1, size = 0.5)
