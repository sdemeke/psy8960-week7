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





#Visualization
week7_tbl %>% 
  GGally::ggpairs(columns = 5:13) #non index way to reference cols?

p1 <- ggplot(data=week7_tbl,aes(x=timeStart,y=q1)) +
  geom_point() +
  labs(x="Date of Experiment",y="Q1 Score") 
ggsave(filename ="../figs/fig1.png",plot=p1) #pipe within generation


p2 <- ggplot(data=week7_tbl,aes(x=q1,y=q2,color=gender)) +
  geom_jitter() +
  labs(color = "Participant Gender")
ggsave(filename ="../figs/fig2.png",plot=p2) #sizing not great, legend half too big
