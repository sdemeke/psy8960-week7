#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)



#Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>% 
  mutate(across(c(timeStart,timeEnd),ymd_hms)) %>% 
  mutate(condition = factor(condition, levels=c("A","B","C"), labels=c("Block A","Block B","Control")),
         gender = factor(gender, levels=c("M","F"), labels=c("Male","Female"))) %>% 
  filter(q6==1) %>% 
  select(-c(q6)) %>% 
  mutate(timeSpent = timeEnd - timeStart)
