library(tidyverse)
library(lubridate)

dx_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/DX (follow up 1993-2020)/combined/dx_combined.rds")
px_combined <- readRDS("C:/Users/LabPC14CSMPR/Desktop/Chris/ASCVD cohort (Novartis)/PX (follow up 1993-2020)/combined/px_combined.rds")

dx_combined <- dx_combined %>% filter(str_detect(all.diagnosis.code.icd9, "^41[0-4]|^43[3-6]|^44[0-3]"))
px_combined <- px_combined %>% filter(str_detect(all.procedure.code, "^36"))
dx_combined$reference.date <- as_date(dx_combined$reference.date)
px_combined$reference.date <- as_date(ymd_hm(px_combined$reference.date))

df <- select(rename(dx_combined, code = all.diagnosis.code.icd9), reference.key, reference.date, code) %>% 
  rbind(select(rename(px_combined, code = all.procedure.code), reference.key, reference.date, code))
df <- df %>% 
  filter(reference.date >= date("2015-01-01")) %>% 
  group_by(reference.key) %>% 
  arrange(reference.date) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(reference.date)
df$reference.key <- as.numeric(df$reference.key)
ggplot(df) +
  geom_bar(aes(reference.date))
df %>% filter(reference.date >= date("2019-12-01")&reference.date < date("2020-01-01"))


for (i in seq(2015, 2020)) {
  for (j in seq(1, 12)) {
    if(j == 12) {
      df %>% filter(reference.date >= date(str_c(i, "-", j, "-1"))&reference.date < date(str_c(i+1, "-", 1, "-1"))) %>% 
        select(reference.key) %>% write_csv(str_c(i, "-", j, ".txt"), col_names = F)
      }
    else{
      df %>% filter(reference.date >= date(str_c(i, "-", j, "-1"))&reference.date < date(str_c(i, "-", j+1, "-1"))) %>% 
        select(reference.key) %>% write_csv(str_c(i, "-", j, ".txt"), col_names = F)
    }
  }
}
