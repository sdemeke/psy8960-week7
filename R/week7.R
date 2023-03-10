#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)
library(GGally)


#Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>% 
  mutate(across(c(timeStart),ymd_hms)) %>% 
  mutate(condition = factor(condition, levels=c("A","B","C"), labels=c("Block A","Block B","Control")),
         gender = factor(gender, levels=c("M","F"), labels=c("Male","Female"))) %>% 
  filter(q6==1) %>% 
  select(-c(q6)) %>% 
  mutate(timeSpent = timeEnd - timeStart)





#Visualization
week7_tbl %>% 
   select(starts_with("q")) %>% 
   ggpairs() 
(ggplot(data=week7_tbl,aes(x=timeStart,y=q1)) +
  geom_point() +
  labs(x="Date of Experiment",y="Q1 Score") ) %>% 
ggsave(filename ="../figs/fig1.png",width = 6, height = 4) 


(ggplot(data=week7_tbl,aes(x=q1,y=q2,color=gender)) +
  geom_jitter() +
  labs(color = "Participant Gender"))  %>% 
ggsave(filename ="../figs/fig2.png",width = 6, height = 4) 

(ggplot(data=week7_tbl,aes(x=q1,y=q2,color=gender)) +
  geom_jitter(color="black") +
  labs(x="Score on Q1",y="Score on Q2") + 
  facet_grid(.~ gender) + 
  theme(legend.position = "none") ) %>% 
ggsave(filename ="../figs/fig3.png",width = 6, height = 4) 

(ggplot(data = week7_tbl,aes(x=gender,y=timeSpent)) +
  geom_boxplot() +
  labs(x="Gender",y="Time Elapsed (mins)") +
  scale_y_continuous()) %>% 
ggsave(filename ="../figs/fig4.png",width = 6, height = 4) 

(ggplot(data = week7_tbl,aes(x=q5,y=q7,color=condition,group=condition)) +
  geom_jitter() +
  geom_smooth(se=F,method = "lm",formula = 'y ~ x') +
  labs(x="Score on Q5",y="Score on Q7",color="Experimental Condition") +
  theme(legend.position="bottom",legend.background = element_rect(fill="#DFDFDF")) ) %>%  
ggsave(filename ="../figs/fig5.png",width = 6, height = 4) 
   