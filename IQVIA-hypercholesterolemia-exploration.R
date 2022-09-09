library(tidyverse)
library(tidylog)
library(readxl)
library(lubridate)
library(tableone)
library(ggpubr)

dx_first <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/dx_first.rds")
dx_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/dx_combined_2010_2020.rds") %>% mutate(reference.date = as_date(reference.date)) %>% distinct()
ip_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/ip_combined_2010_2020.rds") %>% mutate(admission.date = as_date(ymd_hm(admission.date))) %>% mutate(discharge.date = as_date(ymd_hm(discharge.date))) %>% distinct()
px_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/px_combined_2010_2020.rds") %>% mutate(reference.date = as_date(ymd_hm(reference.date))) %>% distinct()
demo_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/demo_combined_2010_2020_by_2022_1_1.rds") %>% distinct()
bl_rx_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/bl_rx_combined_2010_2020.rds") %>% 
  mutate(dispensing.date.yyyy.mm.dd = as_date(dispensing.date.yyyy.mm.dd)) %>% 
  mutate(prescription.start.date = as_date(prescription.start.date)) %>% 
  mutate(prescription.end.date = as_date(prescription.end.date)) %>% distinct() %>% 
  select(reference.key, dispensing.date.yyyy.mm.dd, prescription.start.date, prescription.end.date, drug.item.code, drug.name, route, drug.strength, dosage, dosage.unit, drug.frequency, dispensing.duration, dispensing.duration.unit, therapeutic.classification.bnf)
fu_RX_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/fu_RX_combined_2010_2020.rds") %>% 
  mutate(dispensing.date.yyyy.mm.dd = as_date(dispensing.date.yyyy.mm.dd)) %>% 
  mutate(prescription.start.date = as_date(prescription.start.date)) %>% 
  mutate(prescription.end.date = as_date(prescription.end.date)) %>% distinct() %>% 
  select(reference.key, dispensing.date.yyyy.mm.dd, prescription.start.date, prescription.end.date, drug.item.code, drug.name, route, drug.strength, dosage, dosage.unit, drug.frequency, dispensing.duration, dispensing.duration.unit, therapeutic.classification.bnf)
lab_combined <- read_rds("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2010-2020)/lab_combined_2010_2020.rds") %>% distinct()

dx_lipid <- rbind(select(filter(dx_combined, str_detect(all.diagnosis.code.icd9, "272\\.4")), reference.key, date = reference.date),
                  select(filter(ip_combined, str_detect(dx.px.code, "272\\.4")), reference.key, date = admission.date)) %>% 
  group_by(reference.key) %>% 
  arrange(date) %>% 
  slice(1) %>% 
  ungroup()

rx <- rbind(filter(bl_rx_combined, reference.key %in% dx_lipid$reference.key), filter(fu_RX_combined, reference.key %in% dx_lipid$reference.key))

# 38217
rx_index <- filter(rx, prescription.start.date >= date("2018-01-01")) %>% 
  filter(!reference.key %in% filter(rx, prescription.start.date < date("2018-01-01"))$reference.key) %>% 
  filter(str_detect(drug.item.code, "^ATOR|^ATOR|^FLUV|^PRAV|^ROSU|^SIMV|^GLIC|^EZET|^S00125")) %>% 
  group_by(reference.key) %>% 
  arrange(prescription.start.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(sex = demo_combined$sex[match(reference.key, demo_combined$reference.key)]) %>% 
  mutate(date.of.birth = demo_combined$date.of.birth[match(reference.key, demo_combined$reference.key)]) %>% 
  mutate(dx.lipid.date = dx_lipid$date[match(reference.key, dx_lipid$reference.key)]) %>% 
  mutate(dx.lipid.year = year(dx.lipid.date)) %>% 
  mutate(age = as.integer(dx.lipid.year - year(date.of.birth))) %>% 
  mutate(age.band = if_else(age < 40, "<40", if_else(age < 50, "<50", if_else(age < 60, "<60", if_else(age < 70, "<70", "over 70")))))

# 8799
rx_index <- rx_index %>% filter(dx.lipid.date < prescription.start.date)

# 2711
rx_index <- rx_index %>% mutate(dx.CVD.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% filter(dx.CVD.date < prescription.start.date)

rx_index$dx.lipid.year %>% table()

rx_index$sex %>% table()

rx_index$age.band %>% table()

rx_combined <- bl_rx_combined %>% rbind(fu_RX_combined) %>% filter(str_detect(therapeutic.classification.bnf, "^2\\.12")) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^ATOR|^ATOR|^FLUV|^PRAV|^ROSU|^SIMV|^GLIC"), "statins", "")) %>%
  mutate(type = if_else(str_detect(drug.item.code, "^FENO|^GEMF|^S00995"), "fibrates", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^ALIR|^EVOL|^S01133|^S01159"), "PCSK9 inhibitors", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^EZET|^S00125"), "ezetimibe", type))

rx_index %>% filter(reference.key %in% filter(rx_combined, type == "statins")$reference.key)
rx_index %>% filter(reference.key %in% filter(rx_combined, type == "ezetimibe")$reference.key)
rx_index %>% filter(reference.key %in% filter(rx_combined, type == "PCSK9 inhibitors")$reference.key)



# PCI
ip_combined %>% 
  filter(str_detect(dx.px.code, "36\\.0")) %>% 
  mutate(admission.year = year(admission.date)) %>% 
  filter(admission.year %in% c(2011, 2012)) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  .$dx.px.code %>% table()

px_combined %>% 
  filter(str_detect(all.procedure.code, "36\\.0[12567]")) %>% 
  mutate(reference.year = year(reference.date)) %>% 
  filter(reference.year %in% c(2011, 2012)) %>% 
  group_by(reference.key) %>%
  slice(1) %>%
  .$all.procedure.code %>% table()

# 1 220 2 17 4 151 5 254 6 5237 7 3124 9 18