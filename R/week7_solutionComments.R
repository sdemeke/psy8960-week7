#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
#library(lubridate) #no need to call, already included in tidyverse
library(GGally)
library(conflicted) #what does this call? -->
#Use the conflicted package to force all conflicts to become errors

#Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>% 
  mutate(timeStart = ymd_hms(timeStart), 
         condition = factor(condition, levels=c("A","B","C"), labels=c("Block A","Block B","Control")),
         gender = factor(gender, levels=c("M","F"), labels=c("Male","Female")),
         timeSpent = timeEnd - timeStart) %>% 
  dplyr::filter(q6==1) %>%  #without [conflicted], error between dplyr::filter and stats::filter
  select(-c(q6))




#Visualization
week7_tbl %>% 
   select(starts_with("q")) %>% 
   ggpairs() 
(ggplot(data=week7_tbl,
        aes(x=timeStart,y=q1)) +
  geom_point() +
  labs(x="Date of Experiment",y="Q1 Score") ) %>% 
ggsave(filename ="../figs/fig1.png",width = 1920, height = 1080, units = "px") 
#he sets all images to 1920 x 1080 px or 10 by 11.25 inches

(ggplot(data=week7_tbl,
        aes(x=q1,y=q2,color=gender)) +
  geom_jitter() +
  labs(color = "Participant Gender"))  %>% #or scale_color_discrete("")
ggsave(filename ="../figs/fig2.png",width = 1920, height = 1080, units = "px") 

(ggplot(data=week7_tbl,
        aes(x=q1,y=q2,color=gender)) +
  geom_jitter(color="black") + #no need to set color as mapping, then don't need this call to color either
  labs(x="Score on Q1",y="Score on Q2") + 
  facet_grid(.~ gender) + #facet_wrap is simpler with only one variable
  theme(legend.position = "none") ) %>%  #no color mapping would also remove this requirement
ggsave(filename ="../figs/fig3.png",width = 1920, height = 1080, units = "px") 

(ggplot(data = week7_tbl,
        aes(x=gender,y=timeSpent)) +
  geom_boxplot() +
  labs(x="Gender",y="Time Elapsed (mins)") +
  scale_y_continuous()) %>% #not necessarily to suppress the message with this call
ggsave(filename ="../figs/fig4.png",width = 1920, height = 1080, units = "px") 

(ggplot(data = week7_tbl,
        aes(x=q5,y=q7,color=condition,group=condition)) +
  geom_jitter() + #he adds width=.1?
  geom_smooth(se=F,method = "lm",formula = 'y ~ x') +
  labs(x="Score on Q5",y="Score on Q7",color="Experimental Condition") +
  theme(legend.position="bottom",legend.background = element_rect(fill="#DFDFDF")) ) %>%  #correct hex is #e0e0e0, very similar to mine
ggsave(filename ="../figs/fig5.png",width = 1920, height = 1080, units = "px") 
   