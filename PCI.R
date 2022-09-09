library(tidyverse)
library(tidylog)
library(readxl)
library(lubridate)
library(tableone)
library(ggpubr)

# # step 3 please remember the best strategy of manage a project is to create a R project and put all data files into the folder and just read the file with their names without location
# #dataset 2015-2020##################################################################################
# dx_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/dx_combined_2015_2020.rds") %>% mutate(reference.date = as_date(reference.date)) %>% distinct()
# ip_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/ip_combined_2015_2020.rds") %>% mutate(admission.date = as_date(ymd_hm(admission.date))) %>% mutate(discharge.date = as_date(ymd_hm(discharge.date))) %>% distinct()
# px_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/px_combined_2015_2020.rds") %>% mutate(reference.date = as_date(ymd_hm(reference.date))) %>% distinct()
# demo_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/demo_combined_2015_2020.rds") %>% distinct()
# bl_rx_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/bl_rx_combined_2015_2020.rds") %>% 
#   mutate(dispensing.date.yyyy.mm.dd = as_date(dispensing.date.yyyy.mm.dd)) %>% 
#   mutate(prescription.start.date = as_date(prescription.start.date)) %>% 
#   mutate(prescription.end.date = as_date(prescription.end.date)) %>% distinct()
# fu_RX_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/fu_RX_combined_2015_2020.rds") %>% 
#   mutate(dispensing.date.yyyy.mm.dd = as_date(dispensing.date.yyyy.mm.dd)) %>% 
#   mutate(prescription.start.date = as_date(prescription.start.date)) %>% 
#   mutate(prescription.end.date = as_date(prescription.end.date)) %>% distinct()
# lab_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/combined data (2015-2020)/lab_combined_2015_2020.rds") %>% distinct()
# #dataset 2015-2020##################################################################################

#dataset 2010-2020##################################################################################
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
#dataset 2010-2020##################################################################################

# step 4
dx_all <- rbind(select(px_combined, reference.key, reference.date, icd9 = all.procedure.code),
                select(dx_combined, reference.key, reference.date, icd9 = all.diagnosis.code.icd9),
                select(ip_combined, reference.key, reference.date = admission.date, icd9 = dx.px.code)) %>%
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth))

dx_PCI <- select(px_combined, reference.key, reference.date, icd9 = all.procedure.code) %>% 
  filter(str_detect(icd9, "^36\\.0[12567]")) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth)) %>% 
  mutate(year = year(reference.date)) %>% 
  mutate(month = month(reference.date)) %>% 
  mutate(quarter = quarter(reference.date)) %>% 
  mutate(age = as.integer(year - year(date.of.birth))) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(reference.date >= date("2010-01-01")) %>% 
  mutate(disease = "PCI")

dx_MI <- select(filter(ip_combined, dx.px.rank %in% c("D1", "D2", "D3", "P1", "P2", "P3")), reference.key, reference.date = admission.date, icd9 = dx.px.code) %>% 
  filter(str_detect(icd9, "^410")) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth)) %>% 
  mutate(year = year(reference.date)) %>% 
  mutate(month = month(reference.date)) %>% 
  mutate(quarter = quarter(reference.date)) %>% 
  mutate(age = as.integer(year - year(date.of.birth))) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(reference.date >= date("2010-01-01")) %>% 
  mutate(disease = "MI")

dx_ACS <- select(filter(ip_combined, dx.px.rank %in% c("D1", "D2", "D3", "P1", "P2", "P3")), reference.key, reference.date = admission.date, icd9 = dx.px.code) %>% 
  filter(str_detect(icd9, "^411")) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth)) %>% 
  mutate(year = year(reference.date)) %>% 
  mutate(month = month(reference.date)) %>% 
  mutate(quarter = quarter(reference.date)) %>% 
  mutate(age = as.integer(year - year(date.of.birth))) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(reference.date >= date("2010-01-01")) %>% 
  mutate(disease = "ACS")

dx_stroke <- select(filter(ip_combined, dx.px.rank %in% c("D1", "D2", "D3", "P1", "P2", "P3")), reference.key, reference.date = admission.date, icd9 = dx.px.code) %>% 
  filter(str_detect(icd9, "^43[346]")) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth)) %>% 
  mutate(year = year(reference.date)) %>% 
  mutate(month = month(reference.date)) %>% 
  mutate(quarter = quarter(reference.date)) %>% 
  mutate(age = as.integer(year - year(date.of.birth))) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(reference.date >= date("2010-01-01")) %>% 
  mutate(disease = "stroke")

incidence <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year) %>% count() %>%
  mutate(population = 7480000) %>%
  mutate(population = if_else(year == 2019, 7510000, population)) %>%
  mutate(population = if_else(year == 2018, 7450000, population)) %>%
  mutate(population = if_else(year == 2017, 7390000, population)) %>%
  mutate(population = if_else(year == 2016, 7340000, population)) %>%
  mutate(population = if_else(year == 2015, 7290000, population)) %>%
  mutate(population = if_else(year == 2014, 7230000, population)) %>%
  mutate(population = if_else(year == 2013, 7180000, population)) %>%
  mutate(population = if_else(year == 2012, 7150000, population)) %>%
  mutate(population = if_else(year == 2011, 7070000, population)) %>%
  mutate(population = if_else(year == 2010, 7020000, population)) %>%
  mutate(incidence = n*100000/population)
plot1 <- ggplot(incidence) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020)) +
  scale_y_continuous(breaks = seq(0, 140, 20), limits = c(0, 140)) +
  ylab("incidence per 100,000 individuals") +
  theme_classic() +
  labs(title = "incidence of each ASCVD")

incidence_sex <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year, sex) %>% count() %>%
  mutate(population = 7480000) %>%
  mutate(population = if_else(year == 2019, 7510000, population)) %>%
  mutate(population = if_else(year == 2018, 7450000, population)) %>%
  mutate(population = if_else(year == 2017, 7390000, population)) %>%
  mutate(population = if_else(year == 2016, 7340000, population)) %>%
  mutate(population = if_else(year == 2015, 7290000, population)) %>%
  mutate(population = if_else(year == 2014, 7230000, population)) %>%
  mutate(population = if_else(year == 2013, 7180000, population)) %>%
  mutate(population = if_else(year == 2012, 7150000, population)) %>%
  mutate(population = if_else(year == 2011, 7070000, population)) %>%
  mutate(population = if_else(year == 2010, 7020000, population)) %>%
  mutate(population = if_else(sex == "M", population * 0.456, population * 0.544)) %>% 
  mutate(incidence = n*100000/population)
plot2 <- ggplot(incidence_sex) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  facet_wrap(~ sex) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020), guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 200)) +
  ylab("incidence per 100,000 individuals") +
  theme_classic() +
  labs(title = "incidence of each ASCVD stratified by sex")

incidence_age <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year, age.group) %>% count() %>% 
  mutate(population = 7480000) %>%
  mutate(population = if_else(year == 2019, 7510000, population)) %>%
  mutate(population = if_else(year == 2018, 7450000, population)) %>%
  mutate(population = if_else(year == 2017, 7390000, population)) %>%
  mutate(population = if_else(year == 2016, 7340000, population)) %>%
  mutate(population = if_else(year == 2015, 7290000, population)) %>%
  mutate(population = if_else(year == 2014, 7230000, population)) %>%
  mutate(population = if_else(year == 2013, 7180000, population)) %>%
  mutate(population = if_else(year == 2012, 7150000, population)) %>%
  mutate(population = if_else(year == 2011, 7070000, population)) %>%
  mutate(population = if_else(year == 2010, 7020000, population)) %>%
  mutate(population = if_else(age.group == "70 years or older", population * 0.126, population * 0.874)) %>% 
  mutate(incidence = n*100000/population)
plot3 <- ggplot(incidence_age) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  facet_wrap(~ age.group) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020), guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = seq(0, 700, 100), limits = c(0, 700)) +
  ylab("incidence per 100,000 individuals") +
  theme_classic() +
  labs(title = "incidence of each ASCVD stratified by age groups")
# tiff("subgroup analysis/incidence.tif", width = 1500*4, height = 1200*4, res = 144*4)
ggarrange(plot1, ggarrange(plot2, plot3, nrow = 2, common.legend = T, legend = "none"), common.legend = T)
# dev.off()

# incident cases
incidence <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year) %>% count() %>%
  mutate(incidence = n)
plot1 <- ggplot(incidence) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020)) +
  scale_y_continuous(breaks = seq(0, 10000, 2000), limits = c(0, 10000)) +
  ylab("number of incident cases") +
  theme_classic() +
  labs(title = "incident cases of each ASCVD")

incidence_sex <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year, sex) %>% count() %>%
  mutate(incidence = n)
plot2 <- ggplot(incidence_sex) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  facet_wrap(~ sex) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020), guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 5000)) +
  ylab("number of incident cases") +
  theme_classic() +
  labs(title = "incident cases of each ASCVD stratified by sex")

incidence_age <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke) %>% 
  group_by(disease, year, age.group) %>% count() %>% 
  mutate(incidence = n)
plot3 <- ggplot(incidence_age) +
  geom_line(aes(year, incidence, color = disease), size = 1) +
  facet_wrap(~ age.group) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2010, 2020), guide = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = seq(0, 6000, 1000), limits = c(0, 6000)) +
  ylab("incidence per 100,000 individuals") +
  theme_classic() +
  labs(title = "incident cases of each ASCVD stratified by age groups")
# tiff("subgroup analysis/incidenct cases.tif", width = 1500*4, height = 1200*4, res = 144*4)
ggarrange(plot1, ggarrange(plot2, plot3, nrow = 2, common.legend = T, legend = "none"), common.legend = T)
# dev.off()

dx_first <- rbind(dx_PCI, dx_MI, dx_ACS, dx_stroke)

# step 9 recurrent event & mortality
ip_PCI <- ip_combined %>% 
  filter(reference.key %in% dx_PCI$reference.key) %>% 
  mutate(sex = dx_PCI$sex[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_PCI$age[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_PCI$reference.date[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(index.disease = dx_PCI$disease[match(reference.key, dx_PCI$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup()

recurrent_PCI <- ip_PCI %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(dx_PCI$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_PCI_M <- ip_PCI %>% 
  filter(sex == "Male") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_PCI, sex == "M")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_PCI_F <- ip_PCI %>% 
  filter(sex == "Female") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_PCI, sex == "F")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_PCI_old <- ip_PCI %>% 
  filter(age.group == "70 years or older") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_PCI, age.group == "70 years or older")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_PCI_young <- ip_PCI %>% 
  filter(age.group == "Under 70 years") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_PCI, age.group == "under 70 years")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

ip_MI <- ip_combined %>% 
  filter(reference.key %in% dx_MI$reference.key) %>% 
  mutate(sex = dx_MI$sex[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_MI$age[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_MI$reference.date[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(index.disease = dx_MI$disease[match(reference.key, dx_MI$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup()

recurrent_MI <- ip_MI %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(dx_MI$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_MI_M <- ip_MI %>% 
  filter(sex == "Male") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_MI, sex == "M")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_MI_F <- ip_MI %>% 
  filter(sex == "Female") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_MI, sex == "F")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_MI_old <- ip_MI %>% 
  filter(age.group == "70 years or older") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_MI, age.group == "70 years or older")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_MI_young <- ip_MI %>% 
  filter(age.group == "Under 70 years") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_MI, age.group == "under 70 years")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

ip_ACS <- ip_combined %>% 
  filter(reference.key %in% dx_ACS$reference.key) %>% 
  mutate(sex = dx_ACS$sex[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_ACS$age[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_ACS$reference.date[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(index.disease = dx_ACS$disease[match(reference.key, dx_ACS$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup()

recurrent_ACS <- ip_ACS %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(dx_ACS$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_ACS_M <- ip_ACS %>% 
  filter(sex == "Male") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_ACS, sex == "M")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_ACS_F <- ip_ACS %>% 
  filter(sex == "Female") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_ACS, sex == "F")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_ACS_old <- ip_ACS %>% 
  filter(age.group == "70 years or older") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_ACS, age.group == "70 years or older")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_ACS_young <- ip_ACS %>% 
  filter(age.group == "Under 70 years") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_ACS, age.group == "under 70 years")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

ip_stroke <- ip_combined %>% 
  filter(reference.key %in% dx_stroke$reference.key) %>% 
  mutate(sex = dx_stroke$sex[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_stroke$age[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_stroke$reference.date[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(index.disease = dx_stroke$disease[match(reference.key, dx_stroke$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup()

recurrent_stroke <- ip_stroke %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(dx_stroke$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_stroke_M <- ip_stroke %>% 
  filter(sex == "Male") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_stroke, sex == "M")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_stroke_F <- ip_stroke %>% 
  filter(sex == "Female") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_stroke, sex == "F")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_stroke_old <- ip_stroke %>% 
  filter(age.group == "70 years or older") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_stroke, age.group == "70 years or older")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_stroke_young <- ip_stroke %>% 
  filter(age.group == "Under 70 years") %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(filter(dx_stroke, age.group == "under 70 years")$disease))) %>% 
  mutate(prevalence = round(event/case*1000))


recurrent_event <- rbind(recurrent_PCI, recurrent_MI, recurrent_ACS, recurrent_stroke)
recurrent_event_M <- rbind(recurrent_PCI_M, recurrent_MI_M, recurrent_ACS_M, recurrent_stroke_M)
recurrent_event_F <- rbind(recurrent_PCI_F, recurrent_MI_F, recurrent_ACS_F, recurrent_stroke_F)
recurrent_event_old <- rbind(recurrent_PCI_old, recurrent_MI_old, recurrent_ACS_old, recurrent_stroke_old)
recurrent_event_young <- rbind(recurrent_PCI_young, recurrent_MI_young, recurrent_ACS_young, recurrent_stroke_young)

recurrent_event_subgroup <- rbind(recurrent_event_M, recurrent_event_F, recurrent_event_old, recurrent_event_young) %>% 
  mutate(group = factor(c(rep("Male", 4), rep("Female", 4), rep("70 years or older", 4), rep("Under 70 years", 4)), levels = c("Male", "Female", "70 years or older", "Under 70 years"))) %>% 
  select(group, everything())
# write_csv(recurrent_event, "recurrent_event.csv")
# write_csv(recurrent_event_subgroup, "recurrent_event_subgroup.csv")
plot1 <- recurrent_event %>%   
  mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(index.disease, prevalence), prevalence, fill = index.disease), show.legend = F) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  scale_y_continuous(breaks = seq(0, 400, 100), limits = c(0, 400)) +
  ylab("Number of recurrent hospitalization per 1,000 individuals")
plot2 <- recurrent_event_subgroup %>% 
  mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(index.disease, prevalence), prevalence, fill = index.disease), show.legend = F) +
  facet_wrap(~ group) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  scale_y_continuous(breaks = seq(0, 500, 100), limits = c(0, 500)) +
  ylab("Number of recurrent hospitalization per 1,000 individuals")

# tiff("subgroup analysis/recurrent_event.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
# dev.off()

# mortality
mortality_PCI <- dx_PCI %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(dx_PCI$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_PCI_M <- dx_PCI %>% filter(sex == "M") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_PCI, sex == "M")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_PCI_F <- dx_PCI %>% filter(sex == "F") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_PCI, sex == "F")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_PCI_old <- dx_PCI %>% filter(age >= 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_PCI, age >= 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_PCI_young <- dx_PCI %>% filter(age < 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_PCI, age < 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_MI <- dx_MI %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(dx_MI$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_MI_M <- dx_MI %>% filter(sex == "M") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_MI, sex == "M")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_MI_F <- dx_MI %>% filter(sex == "F") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_MI, sex == "F")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_MI_old <- dx_MI %>% filter(age >= 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_MI, age >= 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_MI_young <- dx_MI %>% filter(age < 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_MI, age < 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_ACS <- dx_ACS %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(dx_ACS$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_ACS_M <- dx_ACS %>% filter(sex == "M") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_ACS, sex == "M")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_ACS_F <- dx_ACS %>% filter(sex == "F") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_ACS, sex == "F")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_ACS_old <- dx_ACS %>% filter(age >= 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_ACS, age >= 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_ACS_young <- dx_ACS %>% filter(age < 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_ACS, age < 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_stroke <- dx_stroke %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(dx_stroke$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_stroke_M <- dx_stroke %>% filter(sex == "M") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_stroke, sex == "M")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_stroke_F <- dx_stroke %>% filter(sex == "F") %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_stroke, sex == "F")$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_stroke_old <- dx_stroke %>% filter(age >= 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_stroke, age >= 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_stroke_young <- dx_stroke %>% filter(age < 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_stroke, age < 70)$disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality <- rbind(mortality_PCI, mortality_MI, mortality_ACS, mortality_stroke)
mortality_M <- rbind(mortality_PCI_M, mortality_MI_M, mortality_ACS_M, mortality_stroke_M)
mortality_F <- rbind(mortality_PCI_F, mortality_MI_F, mortality_ACS_F, mortality_stroke_F)
mortality_old <- rbind(mortality_PCI_old, mortality_MI_old, mortality_ACS_old, mortality_stroke_old)
mortality_young <- rbind(mortality_PCI_young, mortality_MI_young, mortality_ACS_young, mortality_stroke_young)

mortality_subgroup <- rbind(mortality_M, mortality_F, mortality_old, mortality_young) %>% 
  mutate(group = factor(c(rep("Male", 4), rep("Female", 4), rep("70 years or older", 4), rep("Under 70 years", 4)), levels = c("Male", "Female", "70 years or older", "Under 70 years"))) %>% 
  select(group, everything())

# write_csv(mortality, "mortality.csv")
# write_csv(mortality_subgroup, "mortality_subgroup.csv")
plot1 <- mortality %>% mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(disease, prevalence), prevalence, fill = disease), show.legend = F) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  scale_y_continuous(breaks = seq(0, 800, 200), limits = c(0, 800)) +
  ylab("Number of death per 1,000 individuals")
plot2 <- mortality_subgroup %>% 
  mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(disease, prevalence), prevalence, fill = disease), show.legend = F) +
  facet_wrap(~ group) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  scale_y_continuous(breaks = seq(0, 800, 200), limits = c(0, 800)) +
  ylab("Number of death per 1,000 individuals")

# tiff("subgroup analysis/mortality.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
# dev.off()

# step 10
# duration of hospitalization of each index disease (all hospitalization)
ip_duration <- ip_combined %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  filter(str_detect(dx.px.code, "^410|^411|^43[346]|^36\\.0[12567]")) %>% 
  filter(admission.date >= date("2010-01-01")) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^410"), "MI", "")) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^411"), "ACS", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^43[346]"), "stroke", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^36\\.0[12567]"), "PCI", disease)) %>% 
  mutate(ip.duration = as.numeric(discharge.date - admission.date) + 1) %>% 
  # filter(ip.duration <= 30) %>% 
  mutate(ip.duration = if_else(ip.duration > 30, 30, ip.duration)) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth)) %>% 
  mutate(year = year(admission.date)) %>% 
  mutate(age = as.integer(year - year(date.of.birth))) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years"))

plot1 <- ip_duration %>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(disease, ip.duration), ip.duration, color = disease), show.legend = F, outlier.shape = NA) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  ylab("Duration of hospitalization (days)")

ip_subgroup <- rbind(mutate(filter(ip_duration, sex == "M"), group = "M"), mutate(filter(ip_duration, sex == "F"), group = "F"),
                     mutate(filter(ip_duration, age.group == "70 years or older"), group = "70 years or older"), 
                     mutate(filter(ip_duration, age.group == "under 70 years"), group = "under 70 years")) %>% 
  mutate(group = factor(group, levels = c("M", "F", "70 years or older", "under 70 years")))
plot2 <- ip_subgroup %>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(disease, ip.duration), ip.duration, color = disease), show.legend = F, outlier.shape = NA) +
  facet_wrap(~group) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  ylab("Duration of hospitalization (days)")

# tiff("subgroup analysis/all_hospitalization.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
# dev.off()

# # duration of hospitalization of each index disease (first hospitalization)
# tiff("first_hospitalization.tif", width = 960*4, height = 960*4, res = 144*4)
# # tiff("first_hospitalization_sex.tif", width = 960*4, height = 960*4, res = 144*4)
# # tiff("first_hospitalization_age_group.tif", width = 960*4, height = 960*4, res = 144*4)
# ip_combined %>% 
#   filter(dx.px.rank %in% c("D1", "P1")) %>% 
#   filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
#   filter(admission.date >= date("2010-01-01")) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^41[0-4]"), "chronic IHD", "")) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^410"), "myocardial infarction", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^411"), "acute coronary syndrome", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^413"), "angina", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^43[3|4|6]"), "ischemic stroke", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^435"), "transient ischemic attack", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^44[0-3]"), "peripheral vascular disease", disease)) %>% 
#   mutate(disease = if_else(str_detect(dx.px.code, "^36\\."), "cardiovascular surgery", disease)) %>% 
#   group_by(reference.key) %>% 
#   arrange(admission.date) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   mutate(ip.duration = as.numeric(discharge.date - admission.date) + 1) %>% 
#   # filter(ip.duration <= 30) %>% 
#   mutate(ip.duration = if_else(ip.duration > 30, 30, ip.duration)) %>% 
#   ggplot() +
#   geom_boxplot(aes(fct_reorder(index.disease, ip.duration), ip.duration, color = index.disease), show.legend = F) +
#   # facet_wrap(~sex) +
#   # facet_wrap(~age.group) +
#   theme_classic() +
#   scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
#   ylab("Duration of the first hospitalization (days)")
# dev.off()

labs <- lab_combined %>% 
  filter(reference.key %in% dx_PCI$reference.key) %>% 
  mutate(sex = dx_PCI$sex[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_PCI$age[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_PCI$reference.date[match(reference.key, dx_PCI$reference.key)]) %>% 
  mutate(index.disease = dx_PCI$disease[match(reference.key, dx_PCI$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  mutate(lis.test.description = if_else(lis.test.description %in% c("Cholesterol", "Cholesterol (Total)", "Cholesterol, Total", "Total Cholesterol", "MBI_Cholesterol, total_mmol/L"), "cholesterol", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "LDL"), "ldl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Non-HDL"), "non-hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "HDL"), "hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Triglyceride"), "triglyceride", lis.test.description)) %>% 
  filter(!is.na(index.date)) %>% 
  mutate(lis.reference.datetime = as_date(ymd_hm(lis.reference.datetime))) %>% 
  mutate(lis.result.numeric.result = as.numeric(lis.result.numeric.result)) %>% 
  # filter(lis.result.numeric.result >=0) %>% 
  mutate(ldl_class = if_else(lis.result.numeric.result < 1.8, "<1.8", 
                             if_else(lis.result.numeric.result < 2.7, "1.8-2.6", 
                                     if_else(lis.result.numeric.result < 4, "2.7-4", ">4")))) %>% 
  mutate(ldl_class = factor(ldl_class, levels = c("<1.8", "1.8-2.6", "2.7-4", ">4")))

# step 11&12, you may only need to keep the LDL rows and do the checking, I keep all lab tests in case Novartis may want to redo the analysis on other tests
ldl_baseline <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime <= index.date) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "baseline")

ldl_1 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date&lis.reference.datetime <= index.date +365) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 1")

ldl_2 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 365&lis.reference.datetime <= index.date +730) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 2")

ldl_3 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 730&lis.reference.datetime <= index.date +1095) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 3")

ldl_4 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1095&lis.reference.datetime <= index.date +1460) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 4")

ldl_5 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1460&lis.reference.datetime <= index.date +1825) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 5")

ldl_6 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1825&lis.reference.datetime <= index.date +2190) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 6")

ldl_PCI <- rbind(ldl_baseline, ldl_1, ldl_2, ldl_3, ldl_4, ldl_5, ldl_6)

labs <- lab_combined %>% 
  filter(reference.key %in% dx_MI$reference.key) %>% 
  mutate(sex = dx_MI$sex[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_MI$age[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_MI$reference.date[match(reference.key, dx_MI$reference.key)]) %>% 
  mutate(index.disease = dx_MI$disease[match(reference.key, dx_MI$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  mutate(lis.test.description = if_else(lis.test.description %in% c("Cholesterol", "Cholesterol (Total)", "Cholesterol, Total", "Total Cholesterol", "MBI_Cholesterol, total_mmol/L"), "cholesterol", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "LDL"), "ldl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Non-HDL"), "non-hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "HDL"), "hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Triglyceride"), "triglyceride", lis.test.description)) %>% 
  filter(!is.na(index.date)) %>% 
  mutate(lis.reference.datetime = as_date(ymd_hm(lis.reference.datetime))) %>% 
  mutate(lis.result.numeric.result = as.numeric(lis.result.numeric.result)) %>% 
  filter(lis.result.numeric.result >=0) %>% 
  mutate(ldl_class = if_else(lis.result.numeric.result < 1.8, "<1.8", 
                             if_else(lis.result.numeric.result < 2.7, "1.8-2.6", 
                                     if_else(lis.result.numeric.result < 4, "2.7-4", ">4")))) %>% 
  mutate(ldl_class = factor(ldl_class, levels = c("<1.8", "1.8-2.6", "2.7-4", ">4")))

# step 11&12, you may only need to keep the LDL rows and do the checking, I keep all lab tests in case Novartis may want to redo the analysis on other tests
ldl_baseline <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime <= index.date) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "baseline")

ldl_1 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date&lis.reference.datetime <= index.date +365) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 1")

ldl_2 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 365&lis.reference.datetime <= index.date +730) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 2")

ldl_3 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 730&lis.reference.datetime <= index.date +1095) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 3")

ldl_4 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1095&lis.reference.datetime <= index.date +1460) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 4")

ldl_5 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1460&lis.reference.datetime <= index.date +1825) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 5")

ldl_6 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1825&lis.reference.datetime <= index.date +2190) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 6")

ldl_MI <- rbind(ldl_baseline, ldl_1, ldl_2, ldl_3, ldl_4, ldl_5, ldl_6)

labs <- lab_combined %>% 
  filter(reference.key %in% dx_ACS$reference.key) %>% 
  mutate(sex = dx_ACS$sex[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_ACS$age[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_ACS$reference.date[match(reference.key, dx_ACS$reference.key)]) %>% 
  mutate(index.disease = dx_ACS$disease[match(reference.key, dx_ACS$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  mutate(lis.test.description = if_else(lis.test.description %in% c("Cholesterol", "Cholesterol (Total)", "Cholesterol, Total", "Total Cholesterol", "MBI_Cholesterol, total_mmol/L"), "cholesterol", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "LDL"), "ldl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Non-HDL"), "non-hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "HDL"), "hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Triglyceride"), "triglyceride", lis.test.description)) %>% 
  filter(!is.na(index.date)) %>% 
  mutate(lis.reference.datetime = as_date(ymd_hm(lis.reference.datetime))) %>% 
  mutate(lis.result.numeric.result = as.numeric(lis.result.numeric.result)) %>% 
  filter(lis.result.numeric.result >=0) %>% 
  mutate(ldl_class = if_else(lis.result.numeric.result < 1.8, "<1.8", 
                             if_else(lis.result.numeric.result < 2.7, "1.8-2.6", 
                                     if_else(lis.result.numeric.result < 4, "2.7-4", ">4")))) %>% 
  mutate(ldl_class = factor(ldl_class, levels = c("<1.8", "1.8-2.6", "2.7-4", ">4")))

# step 11&12, you may only need to keep the LDL rows and do the checking, I keep all lab tests in case Novartis may want to redo the analysis on other tests
ldl_baseline <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime <= index.date) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "baseline")

ldl_1 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date&lis.reference.datetime <= index.date +365) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 1")

ldl_2 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 365&lis.reference.datetime <= index.date +730) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 2")

ldl_3 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 730&lis.reference.datetime <= index.date +1095) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 3")

ldl_4 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1095&lis.reference.datetime <= index.date +1460) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 4")

ldl_5 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1460&lis.reference.datetime <= index.date +1825) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 5")

ldl_6 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1825&lis.reference.datetime <= index.date +2190) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 6")

ldl_ACS <- rbind(ldl_baseline, ldl_1, ldl_2, ldl_3, ldl_4, ldl_5, ldl_6)

labs <- lab_combined %>% 
  filter(reference.key %in% dx_stroke$reference.key) %>% 
  mutate(sex = dx_stroke$sex[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_stroke$age[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_stroke$reference.date[match(reference.key, dx_stroke$reference.key)]) %>% 
  mutate(index.disease = dx_stroke$disease[match(reference.key, dx_stroke$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  mutate(lis.test.description = if_else(lis.test.description %in% c("Cholesterol", "Cholesterol (Total)", "Cholesterol, Total", "Total Cholesterol", "MBI_Cholesterol, total_mmol/L"), "cholesterol", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "LDL"), "ldl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Non-HDL"), "non-hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "HDL"), "hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Triglyceride"), "triglyceride", lis.test.description)) %>% 
  filter(!is.na(index.date)) %>% 
  mutate(lis.reference.datetime = as_date(ymd_hm(lis.reference.datetime))) %>% 
  mutate(lis.result.numeric.result = as.numeric(lis.result.numeric.result)) %>% 
  filter(lis.result.numeric.result >=0) %>% 
  mutate(ldl_class = if_else(lis.result.numeric.result < 1.8, "<1.8", 
                             if_else(lis.result.numeric.result < 2.7, "1.8-2.6", 
                                     if_else(lis.result.numeric.result < 4, "2.7-4", ">4")))) %>% 
  mutate(ldl_class = factor(ldl_class, levels = c("<1.8", "1.8-2.6", "2.7-4", ">4")))

# step 11&12, you may only need to keep the LDL rows and do the checking, I keep all lab tests in case Novartis may want to redo the analysis on other tests
ldl_baseline <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime <= index.date) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "baseline")

ldl_1 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date&lis.reference.datetime <= index.date +365) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 1")

ldl_2 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 365&lis.reference.datetime <= index.date +730) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 2")

ldl_3 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 730&lis.reference.datetime <= index.date +1095) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 3")

ldl_4 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1095&lis.reference.datetime <= index.date +1460) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 4")

ldl_5 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1460&lis.reference.datetime <= index.date +1825) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 5")

ldl_6 <- labs %>% filter(lis.test.description == "ldl") %>% filter(lis.reference.datetime > index.date + 1825&lis.reference.datetime <= index.date +2190) %>% 
  group_by(reference.key) %>% arrange(desc(lis.reference.datetime)) %>% slice(1) %>% ungroup() %>% mutate(period = "year 6")

ldl_stroke <- rbind(ldl_baseline, ldl_1, ldl_2, ldl_3, ldl_4, ldl_5, ldl_6)

ldl_all <- rbind(ldl_PCI, ldl_MI, ldl_ACS, ldl_stroke)

plot1 <- ggplot(ldl_all) +
  geom_boxplot(aes(fct_reorder(index.disease, lis.result.numeric.result), lis.result.numeric.result, color = period), outlier.shape = NA) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")
plot2 <- ggplot(ldl_all) +
  geom_boxplot(aes(sex, lis.result.numeric.result, color = period), outlier.shape = NA) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")
plot3 <- ggplot(ldl_all) +
  geom_boxplot(aes(age.group, lis.result.numeric.result, color = period), outlier.shape = NA) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")

# tiff("subgroup analysis/LDL_trend.tif", width = 1000*8, height = 1000*8, res = 144*6)
ggarrange(plot1, ggarrange(plot2, plot3, common.legend = T, legend = "none"), nrow = 2, common.legend = T, legend = "right")
# dev.off()






# Extra, no need to cross check
rx_combined <- bl_rx_combined %>% rbind(fu_RX_combined) %>% filter(str_detect(therapeutic.classification.bnf, "^2\\.12")) %>% 
  filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(date.of.register.death = dx_first$date.of.register.death[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(sex = dx_first$sex[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_first$age[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  filter(prescription.start.date >= index.date) %>% 
  mutate(dispensing.duration = ifelse(dispensing.duration.unit == "Week(s)", as.numeric(dispensing.duration)*7, as.numeric(dispensing.duration))) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^ATOR|^ATOR|^FLUV|^PRAV|^ROSU|^SIMV|^GLIC"), "statins", "")) %>%
  # mutate(type = if_else(str_detect(drug.item.code, "^ATOR"), "atorvastatin", "")) %>%
  # mutate(type = if_else(str_detect(drug.item.code, "^FLUV"), "fluvastatin", type)) %>%
  # mutate(type = if_else(str_detect(drug.item.code, "^PRAV"), "pravastatin", type)) %>%
  # mutate(type = if_else(str_detect(drug.item.code, "^ROSU"), "rosuvastatin", type)) %>%
  # mutate(type = if_else(str_detect(drug.item.code, "^SIMV|^GLIC"), "simvastatin", type)) %>%
  mutate(type = if_else(str_detect(drug.item.code, "^FENO|^GEMF|^S00995"), "fibrates", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^ALIR|^EVOL|^S01133|^S01159"), "PCSK9 inhibitors", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^EZET|^S00125"), "ezetimibe", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^CHOL"), "cholestyramine", type)) %>% 
  mutate(type = if_else(str_detect(drug.item.code, "^NICO"), "nicotinic acid", type)) %>% 
  select(reference.key, type, index.date, everything()) %>% 
  mutate(prescription.end.date = if_else(prescription.end.date <= prescription.start.date, prescription.start.date + dispensing.duration, prescription.end.date)) %>%
  filter(prescription.start.date <= date("2020-12-31")) %>%
  filter(prescription.start.date <= date.of.register.death|is.na(date.of.register.death)) %>%
  mutate(prescription.end.date = if_else(prescription.end.date > date("2020-12-31") | (prescription.end.date > date.of.register.death & !is.na(date.of.register.death)), pmin(date("2020-12-31"), date.of.register.death, na.rm = T), prescription.end.date)) %>%
  arrange(reference.key, type, prescription.start.date) %>% 
  group_by(reference.key, type) %>%
  filter(as.numeric(prescription.end.date) >= cummax(as.numeric(prescription.end.date))) %>%
  group_by(reference.key, prescription.end.date) %>%
  filter(prescription.start.date == min(prescription.start.date)) %>%
  slice(1) %>%
  ungroup()

rx_combined %>% group_by(reference.key) %>% slice(1) %>% .$type %>% table()
rx_combined %>% group_by(reference.key, type) %>% slice(1) %>% .$type %>% table()
rx_combined$prescription.start.date %>% summary()

rx_combined1 <- rx_combined %>% 
  group_by(reference.key, type) %>% 
  mutate(previous.end.date = lag(prescription.end.date)) %>%
  mutate(prescription.start.date = if_else(!is.na(previous.end.date)&as.numeric(prescription.start.date - previous.end.date) <= 30, previous.end.date, prescription.start.date)) %>%
  mutate(discontinuation = if_else(is.na(previous.end.date)|as.numeric(prescription.start.date - previous.end.date) <= 1, 0, 1)) %>%
  mutate(dis.group = cumsum(discontinuation)) %>%
  ungroup() %>%
  mutate(diff = prescription.end.date - prescription.start.date) %>%
  group_by(reference.key, dis.group) %>%
  mutate(prescription.start.date = prescription.end.date - cumsum(as.numeric(diff))) %>%
  group_by(reference.key, prescription.start.date) %>%
  filter(prescription.end.date == max(prescription.end.date)) %>%
  ungroup()

multiple_drug_users <- rx_combined1 %>% group_by(reference.key, type) %>% count() %>% group_by(reference.key) %>% count() %>% filter(n > 1) %>% .$reference.key

rx_combined %>% 
  # filter(sex == "Male") %>%
  # filter(sex == "Female") %>%
  # filter(age.group == "70 years or older") %>%
  # filter(age.group == "Under 70 years") %>%
  # filter(type == "statins") %>%
  # filter(str_detect(drug.item.code, "^SIMV|^GLIC")) %>%
  # filter(str_detect(drug.item.code, "^ATOR")) %>%
  # filter(str_detect(drug.item.code, "^ROSU")) %>%
  # filter(str_detect(drug.item.code, "^FLUV")) %>%
  # filter(str_detect(drug.item.code, "^PRAV")) %>%
  # filter(type == "fibrates") %>%
# filter(type == "ezetimibe") %>%
# filter(type == "cholestyramine") %>%
# filter(type == "PCSK9 inhibitors") %>%
.$reference.key %>% unique() %>% length()
rx_combined %>% 
  # filter(sex == "Male") %>%
  # filter(sex == "Female") %>%
  # filter(age.group == "70 years or older") %>%
  # filter(age.group == "Under 70 years") %>%
  # filter(type == "statins") %>%
  # filter(str_detect(drug.item.code, "^SIMV|^GLIC")) %>%
  # filter(str_detect(drug.item.code, "^ATOR")) %>%
  # filter(str_detect(drug.item.code, "^ROSU")) %>%
  # filter(str_detect(drug.item.code, "^FLUV")) %>%
  # filter(str_detect(drug.item.code, "^PRAV")) %>%
  # filter(type == "fibrates") %>%
# filter(type == "ezetimibe") %>%
# filter(type == "cholestyramine") %>%
# filter(type == "PCSK9 inhibitors") %>%
group_by(reference.key) %>% arrange(prescription.start.date) %>% slice(1) %>% 
  mutate(gap = as.integer(prescription.start.date - index.date)) %>% 
  .$gap %>% summary()
rx_combined1 %>% 
  # filter(sex == "Male") %>%
  # filter(sex == "Female") %>%
  # filter(age.group == "70 years or older") %>%
  # filter(age.group == "Under 70 years") %>%
  # filter(type == "statins") %>%
  # filter(str_detect(drug.item.code, "^SIMV|^GLIC")) %>%
  # filter(str_detect(drug.item.code, "^ATOR")) %>%
  # filter(str_detect(drug.item.code, "^ROSU")) %>%
  # filter(str_detect(drug.item.code, "^FLUV")) %>%
  # filter(str_detect(drug.item.code, "^PRAV")) %>%
  # filter(type == "fibrates") %>%
# filter(type == "ezetimibe") %>%
# filter(type == "cholestyramine") %>%
# filter(type == "PCSK9 inhibitors") %>%
filter(!reference.key %in% multiple_drug_users) %>% group_by(reference.key) %>% slice(1) %>% ungroup() %>% 
  mutate(dispensing.duration = as.numeric(prescription.end.date - prescription.start.date + 1)) %>% .$dispensing.duration %>% summary()
rx_combined1 %>% 
  # filter(sex == "Male") %>%
  # filter(sex == "Female") %>%
  # filter(age.group == "70 years or older") %>%
  # filter(age.group == "Under 70 years") %>%
  # filter(type == "statins") %>%
  # filter(str_detect(drug.item.code, "^SIMV|^GLIC")) %>%
  # filter(str_detect(drug.item.code, "^ATOR")) %>%
  # filter(str_detect(drug.item.code, "^ROSU")) %>%
  # filter(str_detect(drug.item.code, "^FLUV")) %>%
  # filter(str_detect(drug.item.code, "^PRAV")) %>%
  # filter(type == "fibrates") %>%
# filter(type == "ezetimibe") %>%
# filter(type == "cholestyramine") %>%
# filter(type == "PCSK9 inhibitors") %>%
filter(reference.key %in% multiple_drug_users) %>% group_by(reference.key) %>% slice(1) %>% ungroup() %>%
  mutate(dispensing.duration = as.numeric(prescription.end.date - prescription.start.date + 1)) %>% .$dispensing.duration %>% summary()


# # number of PCI
# px_combined %>% filter(str_detect(all.procedure.code, "36\\.0")) %>% filter(reference.date >= date("2010-01-01")) %>% 
#   filter(reference.key %in% dx_first$reference.key) %>% group_by(reference.key) %>% arrange(reference.date) %>% slice(1)
# 
# px_PCI <- px_combined %>% filter(reference.date >= date("2010-01-01")) %>% 
#   filter(str_detect(all.procedure.code, "^36\\.[012]"))
# px_PCI$reference.key %>% unique()
# px_PCI$all.procedure.code %>% table()