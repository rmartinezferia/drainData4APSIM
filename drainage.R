# Read and format Sustainable Corn CAP tile drainage data for APSIM

library(readxl)
library(tidyverse)
library(lubridate)
source("treatmentsAndOperations.R")

# Helper function that fills in constant values between NO3 concentration mesurements
revInt <- function (x) {
  if(all(is.na(x))){rep(NA, length(x))}
  else { rev(approx(1:length(x), rev(x), rule=2,  xout=1:length(x),method="constant")$y)}
}

# Read data dictionary
dataDict <- read_xlsx("cscap_20180117192015.xlsx",1)

# Read tile flow data
files <- list.files('tileflow')
flow <- data.frame()
for(i in 1:length(files)) flow <- rbind(flow, 
                                         read.csv(paste0("tileflow/",files[i]),
                                                  skip = 3,header = F, colClasses = "character"))
names(flow) <- c("uniqueid","plotid","date","flow")

# Tile NO3 concentration data
files <- list.files('tilenitrate')
leach <- data.frame()
for(i in 1:length(files)) leach <- rbind(leach, 
                                         read.csv(paste0("tilenitrate/",files[i]),
                                                  skip = 3,header = F, colClasses = "character"))

names(leach) <- c("uniqueid","plotid","date","no3ppm")

# Clean flow data
flow <- flow %>%
  transmute(date = as.Date(date),
            flow = as.numeric(flow),
            g = paste(uniqueid,plotid)) 

# Create a grid with continous data from the first to the last observation
cont_grid <- expand.grid(date = seq.Date(min(flow$date),max(flow$date),by = "day"),g = unique(flow$g))

flow_cont <- cont_grid %>% 
  left_join(flow) %>%
  separate(g, c("uniqueid","plotid"), sep = " ") %>%
  mutate(flow = pmax(0,ifelse(is.na(flow),0,flow))) %>% # replace NAs and negative values with 0  
  group_by(uniqueid,plotid,date) %>%
  summarise(flow = sum(flow,na.rm = T)) # sum to daily values

# Add NO3 ppm data
drain <- flow_cont %>%  
  full_join(leach %>%
              mutate(date = as.Date(date),
                     no3ppm = as.numeric(no3ppm)))

# Compute Cumulative sums by plot
drain_byPlot <- drain %>%
  arrange(uniqueid, plotid,date) %>%
  mutate(year = year(date)) %>%
  group_by(uniqueid, plotid, year) %>%
  #filter(plotid != 8) %>%
  mutate(cflow = cumsum(flow), # Cumulative Annual drain flow (mm)
         no3ppmInt = revInt(no3ppm), # Interpolated NO3 concentration 
         cleach = cumsum(flow*no3ppmInt/100)) # Cumulative annual NO3 loss (kg/ha)

drain_byPlot # See the results

# Add plot treatments and compute means and errors 
drain_byTrt <- drain_byPlot %>%
  group_by() %>%
  mutate(plotid = ifelse(uniqueid == "SERF",substr(plotid,2,2),plotid)) %>%
  left_join(readRDS("treatments.rds") %>% 
              mutate(plotid = ifelse(uniqueid == "SERF",substr(plotid,1,1),plotid),
                     plotid = ifelse(uniqueid %in% c("HICKS.B","STJOHNS"),substr(plotid,1,2),plotid)) %>%
              select(uniqueid,rep,plotid,rotation2,tillage2,nitrogen2,drainage2,trt,year,crop) %>%
              unique()) %>%
  group_by(uniqueid,rotation2,tillage2,nitrogen2,drainage2,trt,year,date) %>%
  summarise_at(.vars = c("cflow","cleach"), .funs = c("mean","sd"), na.rm = T)

# Vizualize the data
drain_byTrt %>%
  filter(uniqueid == "GILMORE") %>%
  mutate(cleach_min = pmax(0,cleach_mean - cleach_sd),
         cleach_max = pmax(0,cleach_mean + cleach_sd)) %>%
  ggplot(aes(yday(date), cleach_mean)) + 
  geom_ribbon(aes(ymin = cleach_min, ymax = cleach_max, fill = rotation2), alpha = 0.2) + 
  geom_line(aes(colour = rotation2)) + 
  facet_grid(tillage2~year) + 
  labs(y = expression("Cumulative tile nitrate lost (kg N/ha)"),
       x = "Day of year",
       fill = "Crop rotation:", colour = "Crop rotation:") +
  theme_bw() + 
  theme(legend.position = "top")

ggsave("example.jpeg", width = 8, height = 4)
# Prepare for APSIM
drain_byTrt[complete.cases(drain_byTrt),] %>%
  group_by() %>%
  arrange(trt,date) %>%
  select(trt,year,date,cflow_mean:cleach_sd) %>%
  mutate(date = as.character.Date(date, format = "%m/%d/%Y")) %>%
  `names<-`(c("trt","year","date","sum_subsurface_drain","sum_subsurface_drain_no3","sum_subsurface_drainerror","sum_subsurface_drain_no3error")) %>%
  group_by(trt) %>%
  nest() -> out

# Save into out file
path2 <- "APSIMready/"

for(i in 1:length(out$trt)){
  writeLines(c("Apsim version = 7.8",
               paste("Title =",out$trt[[i]]),
               " ",
               paste(names(out$data[[i]]), collapse = " "),
               paste(c("()","(mm/dd/yyyy)",rep("()",length(out$data[[i]])-2)),collapse = " ")),
             paste0(path2,out$trt[i],"_Measured_drain.out"))
  write.table(out$data[i],
              paste0(path2,out$trt[i],"_Measured_drain.out"),
              na = "?", row.names = F,
              col.names = F,
              append = T,
              quote = FALSE)
}
