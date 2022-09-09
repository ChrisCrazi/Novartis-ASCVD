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

dx_ASCVD <- rbind(select(px_combined, reference.key, reference.date, icd9 = all.procedure.code),
                  # select(dx_combined, reference.key, reference.date, icd9 = all.diagnosis.code.icd9),
                  # select(ip_combined, reference.key, reference.date = admission.date, icd9 = dx.px.code)) %>% 
                  select(filter(ip_combined, dx.px.rank %in% c("D1", "D2", "D3", "P1", "P2", "P3")), reference.key, reference.date = admission.date, icd9 = dx.px.code)) %>% 
  filter(str_detect(icd9, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  left_join(demo_combined, by = "reference.key") %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(date.of.birth))

# step 5
dx_first <- dx_ASCVD %>% 
  filter(!is.na(icd9)) %>% 
  filter(str_detect(icd9, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(reference.date) %>% 
  filter(reference.date >= date("2010-01-01")) %>% # remember to change for 2010/2015 onset data analysis
  mutate(index.disease = if_else(str_detect(icd9, "^41[0-4]"), "chronic CHD", "")) %>%
  mutate(index.disease = if_else(str_detect(icd9, "^410"), "myocardial infarction", index.disease)) %>%
  mutate(index.disease = if_else(str_detect(icd9, "^411"), "other acute coronary syndrome", index.disease)) %>%
  mutate(index.disease = if_else(str_detect(icd9, "^413"), "angina", index.disease)) %>%
  mutate(index.disease = if_else(str_detect(icd9, "^43[3|4|6]"), "ischemic stroke", index.disease)) %>% 
  mutate(index.disease = if_else(str_detect(icd9, "^435"), "transient ischemic attack", index.disease)) %>% 
  mutate(index.disease = if_else(str_detect(icd9, "^44[0-3]"), "peripheral vascular disease", index.disease)) %>% 
  mutate(index.disease = if_else(str_detect(icd9, "^36\\."), "cardiovascular surgery", index.disease))

# step 6&7, this part will be hard, you can try skip first
rx_bl <- bl_rx_combined %>% filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  filter(dispensing.date.yyyy.mm.dd <= index.date)

dx_bl <- dx_all %>% filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  filter(reference.date <= index.date)

dx_first$dx.ihd = 0
dx_first$dx.cvd = 0
dx_first$dx.pvd = 0
dx_first$dx.cabg = 0
dx_first$dx.pci = 0
dx_first$dx.other.cardiac.px = 0
dx_first$dx.heart.failure = 0
dx_first$dx.liver.disease = 0
dx_first$dx.gallbladder = 0
dx_first$dx.myopathy = 0
dx_first$dx.dm.renal = 0
dx_first$dx.renal.disease = 0
dx_first$dx.hypertension = 0
dx_first$dx.diabetes = 0
dx_first$dx.hyperlipidemia = 0
dx_first$dx.obesity = 0
dx_first$dx.atrial.fibrillation = 0
dx_first$dx.arrhythmia = 0
dx_first$dx.cardiomyopathy = 0
dx_first$dx.copd = 0
dx_first$dx.asthma = 0
dx_first$dx.thyroid.disease = 0
dx_first$dx.rheumatoid.arthritis = 0
dx_first$dx.dementia = 0
dx_first$dx.autism = 0
dx_first$dx.alcohol.history = 0
dx_first$dx.smoking = 0
dx_first$dx.adhd = 0
dx_first$dx.personality.ds = 0
dx_first$dx.schizophrenia = 0
dx_first$dx.bipolar.disorder = 0
dx_first$dx.depression = 0
dx_first$dx.anxiety = 0
dx_first$dx.cancer = 0

icd9_code <- c("^41[0-4]", # ischemic heart disease
               "^43[0-8]", # cerebrovascular disease
               "^44[0-3]", # peripheral vascular disease
               "^36\\.1[0-7|9]|^36\\.2", # coronary artery bypass graft surgery (cabg)
               "^36\\.0[1-2|5-7]", # percutaneous coronary intervention (pci)
               "^36\\.0[0|3|4|9]|^36\\.[2-3]|^36\\.9[1|9]", # other coronary procedures
               "^398\\.91|^402\\.[0|1|9]1|^404.0[1|3]|^404\\.1[1|3]|^404\\.9[1|3]|^428", # heart failure
               "^070\\.2[2-3]|^070\\.3[2-3]|^070\\.[4-5]4|^070\\.[6|9]|^57[0-1]|^573\\.[3|4|8|9]|^V42\\.7|^456\\.[0-2]|^572\\.[2-4|8]", # liver disease
               "^57[4-6]", # gallbladder and biliary tract disorders
               "^729\\.1", # myopathy, or myalgia and myositis
               "^250\\.4", # diabetes with renal disease
               "^403\\.01|^403\\.11|^403\\.91|^404\\.02|^404\\.03|^404\\.12|^404\\.13|^404\\.92|^404\\.93|^582|^583\\.[0-7]|^585|^586|^588\\.0|^V42\\.0|^V45\\.1|^V56|^39\\.95|^54\\.98", # renaldisease
               "^40[1-5]", # hypertension
               "^250", # diabetes
               "^272", # hyperlipidemia
               "^278", # obesity
               "^427\\.3", # atrial fibrillation
               "^42[6-7]", # arrhythmia
               "^425", # cardiomyopathy
               "^49[0-2]|^494|^496", # copd
               "^493",# asthma
               "^24[2-4]", # thyroid disease, used to be 240-244, changed to 242-244
               "^446.5|^710\\.[0-4]|^714\\.[0-2]|^714\\.8|^725", # rheumatoid disease
               "^290|^294\\.1|^331\\.2", # dementia
               "^299", # autism
               "^265\\.2|^291|^303|^305\\.0|^357\\.5|^425\\.5|^535\\.3|^571\\.[0-3]|^980|^V11\\.3", # alcohol abuse
               "^305\\.1|^V15\\.82|^V15\\.83|^649\\.0", # smoking history
               "^314", # adhd
               "^301", # personality ds
               "^295|^297|^298\\.3|^298\\.4|^298\\.8|^298\\.9", # schizophrenia and psychosis
               "^296\\.0|^296\\.1|^296\\.[4-7]|^296\\.80|^296\\.81|^296\\.89", # bipolar
               "^296\\.2|^296\\.3|^296\\.82|^298\\.0|^300\\.4|^311", # depression
               "^300", # anxiety
               "^1[4-9][0-9]|^20[0-9]|^23[0-9]") # cancer

# function to add 1/0 to dx by checking icd9 from all_dx and ip data
icd9_dx <- function(icd9_code){
  pts_with_disease <- dx_bl %>% 
    filter(str_detect(icd9, icd9_code)) %>% 
    .$reference.key %>% 
    unique()
  if_else(dx_first$reference.key %in% pts_with_disease, 1, 0)
}
m <- map(icd9_code, icd9_dx)

for (i in seq(str_which("dx.ihd", colnames(dx_first)), str_which("dx.cancer", colnames(dx_first)))) {
  n = str_which("dx.ihd", colnames(dx_first)) - 1
  dx_first[i] = m[[i-n]]
}

# check baseline dx
setdiff(unique(filter(dx_bl, str_detect(icd9, "^300"))$reference.key), filter(dx_first, dx.anxiety == 1)$reference.key)
setdiff(filter(dx_first, dx.anxiety == 1)$reference.key, unique(filter(dx_bl, str_detect(icd9, "^300"))$reference.key))

dx_first <- dx_first %>% 
  mutate(rx.h2ra = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^1\\.3\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.ppi = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^1\\.3\\.5$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.cardiac.glycosides = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.1\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.nonloop.diuretic = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.2\\.1$|^2\\.2\\.3$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>%
  mutate(rx.thiazide.diuretics = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.2\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>%
  mutate(rx.potassium.sparing.diuretics = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.2\\.3$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>%
  mutate(rx.potassium.sparing.combination.diuretics = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.2\\.4$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>%
  mutate(rx.antiarrhythmics = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.3\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.beta.blockers = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.4$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.vasodilators = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.centrally.acting.antihypertensives = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.alpha.blockers = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.4$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.ace.inhibitors = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.5\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.arbs = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.5\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.renin.inhibitors = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.5\\.5\\.3$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.calcium.channel.blockers = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.6\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.oral.anticoagulants = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.8\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.antiplatelets = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^2\\.9$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.substance.dep = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^4\\.10\\.2$|^4\\.10$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.dementia.drugs = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^4\\.11$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.short.acting.insulins = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.1\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.long.and.intermediate.actin.insulins = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.1\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.sulfonylureas = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.2\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.biguanides = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.2\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.other.antidiabetic.drugs = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.2\\.3$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.dmdrug = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.1\\.1\\.1$|^6\\.1\\.1\\.2$|^6\\.1\\.2\\.1$|^6\\.1\\.2\\.2$|^6\\.1\\.2\\.3$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>%
  mutate(rx.corticosteroids = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.3\\.1$|^6\\.3\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.estrogens = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.4\\.1\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.male.hormones = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^6\\.4\\.2$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0)) %>% 
  mutate(rx.nsaids = if_else(reference.key %in% unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^10\\.1\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), 1, 0))

# check baseline rx
setdiff(unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^1\\.3\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key), filter(dx_first, rx.h2ra == 1)$reference.key)
setdiff(filter(dx_first, rx.h2ra == 1)$reference.key, unique(filter(rx_bl, str_detect(therapeutic.classification.bnf, "^1\\.3\\.1$")&dispensing.date.yyyy.mm.dd>=index.date-365&dispensing.date.yyyy.mm.dd<index.date)$reference.key))

dx_first <- dx_first %>% 
  mutate(date.of.birth = as_date(date.of.birth)) %>%
  mutate(age = as.numeric(year(reference.date) - year(date.of.birth))) %>% 
  mutate(date.of.register.death = as_date(date.of.register.death)) %>% 
  mutate(fatal_event = if_else(date.of.register.death - reference.date < 30, "fatal", "non_fatal", missing = "non_fatal")) %>% 
  mutate(reference.year = as.character(year(reference.date))) %>% 
  # filter(age >= 18) %>%
  filter(is.na(date.of.register.death)|date.of.register.death > reference.date)

dx_first %>% mutate(censor.date = pmin(date.of.register.death, date("2020-12-31"), na.rm = T)) %>% 
  mutate(follow.up = as.integer(censor.date - reference.date)) %>% 
  .$follow.up %>% sum()

# write_rds(dx_first, "dx_first.rds")

# dx_first %>% 
#   ggplot() +
#   geom_bar(aes(age)) +
#   facet_wrap(~fatal_event)

# df_tableone <- dx_first %>% select(reference.year, sex, age, index.disease, starts_with("dx"), starts_with("rx"))
# df_tableone[,seq(5,68)] <- map(df_tableone[,seq(5,68)], as.character)
# tableone <- CreateTableOne(colnames(df_tableone), data = df_tableone) %>% print(smd = T, printToggle = F)
# write.csv(print(tableone, quote = F, noSpaces = F), file = "baseline.csv", row.names = T)
# tableone <- CreateTableOne(colnames(df_tableone), strata = c("index.disease"), data = df_tableone) %>% print(smd = T, printToggle = F)
# write.csv(print(tableone, printToggle = F), file = "baseline_strata.csv", row.names = T)

# step 8 incidence
dx_ASCVD <- dx_ASCVD %>%
  filter(str_detect(icd9, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  # mutate(disease = if_else(str_detect(icd9, "^41[0-4]"), "coronary heart disease", "")) %>% 
  mutate(disease = if_else(str_detect(icd9, "^41[0-4]"), "chronic CHD", "")) %>%
  mutate(disease = if_else(str_detect(icd9, "^410"), "myocardial infarction", disease)) %>%
  mutate(disease = if_else(str_detect(icd9, "^411"), "other acute coronary syndrome", disease)) %>%
  mutate(disease = if_else(str_detect(icd9, "^413"), "angina", disease)) %>%
  mutate(disease = if_else(str_detect(icd9, "^43[3|4|6]"), "ischemic stroke", disease)) %>% 
  mutate(disease = if_else(str_detect(icd9, "^435"), "transient ischemic attack", disease)) %>% 
  mutate(disease = if_else(str_detect(icd9, "^44[0-3]"), "peripheral vascular disease", disease)) %>% 
  mutate(disease = if_else(str_detect(icd9, "^36\\."), "cardiovascular surgery", disease)) %>% 
  filter(reference.date >= date("2010-01-01")) %>% 
  filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(sex = dx_first$sex[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(age = dx_first$age[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "under 70 years")) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(year = year(reference.date)) %>% 
  mutate(month = month(reference.date)) %>% 
  mutate(quarter = quarter(reference.date))
  

incidence <- dx_ASCVD %>% group_by(reference.key, disease) %>% slice(1) %>% group_by(disease, year) %>% count() %>%
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

incidence_sex <- dx_ASCVD %>% group_by(reference.key, disease) %>% slice(1) %>% group_by(disease, year, sex) %>% count() %>%
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

incidence_age <- dx_ASCVD %>% group_by(reference.key, disease) %>% slice(1) %>% group_by(disease, year, age.group) %>% count() %>% 
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
tiff("incidence_all.tif", width = 1500*4, height = 1200*4, res = 144*4)
ggarrange(plot1, ggarrange(plot2, plot3, nrow = 2, common.legend = T, legend = "none"), common.legend = T)
dev.off()

# step 9 recurrent event & mortality
ip_combined <- ip_combined %>% 
  filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(sex = dx_first$sex[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_first$age[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age))
recurrent_event <- ip_combined %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = as.vector(table(dx_first$index.disease))) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_event_M <- ip_combined %>% 
  filter(sex == "Male") %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, sex == "M")$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_event_F <- ip_combined %>% 
  filter(sex == "Female") %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, sex == "F")$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_event_old <- ip_combined %>% 
  filter(age >= 70) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first,age >= 70)$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_event_young <- ip_combined %>% 
  filter(age < 70) %>% 
  filter(admission.date > index.date + 30) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  group_by(reference.key) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(index.disease) %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first,age < 70)$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

recurrent_event_subgroup <- rbind(recurrent_event_M, recurrent_event_F, recurrent_event_old, recurrent_event_young) %>% 
  mutate(group = factor(c(rep("Male", 8), rep("Female", 8), rep("70 years or older", 8), rep("Under 70 years", 8)), levels = c("Male", "Female", "70 years or older", "Under 70 years"))) %>% 
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
  ylab("Number of recurrent hospitalization per 1,000 individuals")

tiff("recurrent_event.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
dev.off()

mortality <- dx_first %>% filter(!is.na(date.of.register.death)) %>% group_by(index.disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(dx_first$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_M <- dx_first %>% filter(sex == "M") %>% filter(!is.na(date.of.register.death)) %>% group_by(index.disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, sex == "M")$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_F <- dx_first %>% filter(sex == "F") %>% filter(!is.na(date.of.register.death)) %>% group_by(index.disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, sex == "F")$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_old <- dx_first %>% filter(age >= 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(index.disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, age >= 70)$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_young <- dx_first %>% filter(age < 70) %>% filter(!is.na(date.of.register.death)) %>% group_by(index.disease) %>% count() %>% ungroup() %>% 
  rename(event = n) %>% 
  mutate(case = table(filter(dx_first, age < 70)$index.disease)) %>% 
  mutate(prevalence = round(event/case*1000))

mortality_subgroup <- rbind(mortality_M, mortality_F, mortality_old, mortality_young) %>% 
  mutate(group = factor(c(rep("Male", 8), rep("Female", 8), rep("70 years or older", 8), rep("Under 70 years", 8)), levels = c("Male", "Female", "70 years or older", "Under 70 years"))) %>% 
  select(group, everything())

# write_csv(mortality, "mortality.csv")
# write_csv(mortality_subgroup, "mortality_subgroup.csv")
plot1 <- mortality %>% mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(index.disease, prevalence), prevalence, fill = index.disease), show.legend = F) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  ylab("Number of death per 1,000 individuals")
plot2 <- mortality_subgroup %>% 
  mutate(prevalence = as.numeric(prevalence)) %>% ggplot() +
  geom_col(aes(fct_reorder(index.disease, prevalence), prevalence, fill = index.disease), show.legend = F) +
  facet_wrap(~ group) +
  theme_classic(base_size = 10) +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  scale_y_continuous(breaks = seq(0, 800, 200), limits = c(0, 800)) +
  ylab("Number of death per 1,000 individuals")

tiff("mortality.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
dev.off()

# step 10
# duration of hospitalization of each index disease (all hospitalization)
ip_duration <- ip_combined %>% 
  filter(dx.px.rank %in% c("D1", "P1")) %>% 
  filter(str_detect(dx.px.code, "^41[0-4]|^43[3-6]|^44[0-3]|^36\\.")) %>% 
  filter(admission.date >= date("2010-01-01")) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^41[0-4]"), "chronic IHD", "")) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^410"), "myocardial infarction", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^411"), "other acute coronary syndrome", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^413"), "angina", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^43[3|4|6]"), "ischemic stroke", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^435"), "transient ischemic attack", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^44[0-3]"), "peripheral vascular disease", disease)) %>% 
  mutate(disease = if_else(str_detect(dx.px.code, "^36\\."), "cardiovascular surgery", disease)) %>% 
  mutate(ip.duration = as.numeric(discharge.date - admission.date) + 1) %>% 
  # filter(ip.duration <= 30) %>% 
  mutate(ip.duration = if_else(ip.duration > 30, 30, ip.duration))

plot1 <- ip_duration%>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(index.disease, ip.duration), ip.duration, color = index.disease), show.legend = F) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  ylab("Duration of hospitalization (days)")

ip_subgroup <- rbind(mutate(filter(ip_duration, sex == "Male"), group = "Male"), mutate(filter(ip_duration, sex == "Female"), group = "Female"),
                     mutate(filter(ip_duration, age.group == "70 years or older"), group = "70 years or older"), 
                     mutate(filter(ip_duration, age.group == "Under 70 years"), group = "Under 70 years")) %>% 
  mutate(group = factor(group, levels = c("Male", "Female", "70 years or older", "Under 70 years")))
plot2 <- ip_subgroup %>% 
  ggplot() +
  geom_boxplot(aes(fct_reorder(index.disease, ip.duration), ip.duration, color = index.disease), show.legend = F) +
  facet_wrap(~group) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  ylab("Duration of hospitalization (days)")

tiff("all_hospitalization.tif", width = 1200*4, height = 960*4, res = 144*4)
ggarrange(plot1, plot2)
dev.off()

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
  filter(reference.key %in% dx_first$reference.key) %>% 
  mutate(sex = dx_first$sex[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(sex = if_else(sex == "F", "Female", "Male")) %>% 
  mutate(age = dx_first$age[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(age.group = if_else(age >= 70, "70 years or older", "Under 70 years")) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age)) %>% 
  mutate(lis.test.description = if_else(lis.test.description %in% c("Cholesterol", "Cholesterol (Total)", "Cholesterol, Total", "Total Cholesterol", "MBI_Cholesterol, total_mmol/L"), "cholesterol", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "LDL"), "ldl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Non-HDL"), "non-hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "HDL"), "hdl", lis.test.description)) %>% 
  mutate(lis.test.description = if_else(str_detect(lis.test.description, "Triglyceride"), "triglyceride", lis.test.description)) %>% 
  mutate(index.date = dx_first$reference.date[match(reference.key, dx_first$reference.key)]) %>% 
  mutate(index.disease = dx_first$index.disease[match(reference.key, dx_first$reference.key)]) %>% 
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

ldl_all <- rbind(ldl_baseline, ldl_1, ldl_2, ldl_3, ldl_4, ldl_5, ldl_6)

plot1 <- ggplot(ldl_all) +
  geom_boxplot(aes(fct_reorder(index.disease, lis.result.numeric.result), lis.result.numeric.result, color = period)) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45), name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")
plot2 <- ggplot(ldl_all) +
  geom_boxplot(aes(sex, lis.result.numeric.result, color = period)) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")
plot3 <- ggplot(ldl_all) +
  geom_boxplot(aes(age.group, lis.result.numeric.result, color = period)) +
  scale_y_continuous(breaks = c(0, 1.8, 2.6, 4), limits = c(0, 4)) +
  theme_classic() +
  scale_x_discrete(name = NULL) +
  geom_hline(yintercept = 1.8, linetype = 2)+
  ylab("LDL level in each follow-up period (mmol/L)")

tiff("LDL_trend.tif", width = 1000*8, height = 1000*8, res = 144*6)
ggarrange(plot1, ggarrange(plot2, plot3, common.legend = T, legend = "none"), nrow = 2, common.legend = T, legend = "right")
dev.off()

# period ldl class n
ldl_all %>% 
  group_by(period, ldl_class) %>% count() %>% write_csv("ldl_class_n.csv")

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