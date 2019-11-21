# Read data
require(readxl)
require(tidyverse)

path <- "cscap_20180117192015.xlsx"
dataDict <- read_xlsx(path,1)

read_xlsx(path,2)[-1:-2,] %>%
  gather(year,crop,`2011crop`:`2015crop`) %>%
  mutate(year = as.numeric(substr(year,1,4)),
         rotation = gsub("v","",rotation)) %>%
  select(uniqueid,rep,plotid,tillage,rotation,drainage,nitrogen,year,crop) %>%
  left_join(read.csv("rotations.csv")) %>%
  mutate(tillage2 = ifelse(tillage == "TIL1", "NoTill","ConvTill"),
         #rye = ifelse(grepl("rye",rotation2),"withRye","noRye"),
         nitrogen2 = ifelse(nitrogen == "NIT2","MRTN",NA),
         crop = tolower(gsub(" w/ Rye","",crop)),
         rotation2 = ifelse(rotation2 == "CORN-soybean with rye cover","CRSR",
                            ifelse(rotation2 == "corn-SOYBEAN with rye cover","SRCR",
                                   ifelse(rotation2 == "corn-SOYBEAN","SC",
                                          ifelse(rotation2 %in% c("CORN-soybean","corn-SOYBEAN-wheat"),"CS","CC"))))) %>%
  #filter(drainage != "DWM3") %>% # no controlled drainage
  left_join(data.frame(drainage = c("DWM1","DWM2","DWM3","DWM4"), drainage2 = c("Undrained","FreeDrained","ControlDrained","ShallowDrained"))) %>%
  mutate(trt = paste(uniqueid,rotation2,tillage2,nitrogen2,drainage2,sep = "_")) %>%
  saveRDS("treatments.rds")

read_xlsx(path,6,na = "n/a")[-1:-2,] %>%
  saveRDS("operations.rds")



