#load library
library(babynames)
library(ggplot2)
library(dplyr)
library(tidyverse)

bb_names <- babynames

ggplot(data= bb_names %>% filter(name== 'Marie'), aes(x= year)) + geom_histogram()

ggplot(data= bb_names %>% filter(name== "Joe"), aes(x= year, y=n , color=sex)) + geom_line(alpha= 0.5, linewidth=2)

ggplot(data= bb_names %>% filter(name== "Joe"), aes(x= year, y=n , color=sex)) + geom_line(alpha= 0.5, linewidth=2) + labs(title= 'Proportion for Joe', x='Year',y= 'Count')

nrow(bb_names)

women<- bb_names %>% 
 filter(sex== 'F', year== "2002") %>%
  top_n(10,n) #take 10 popular female names
ggplot(data= women, aes(x=name, y= n)) + geom_col(alpha=0.5,fill="blue")

the_nineties <-  bb_names %>%
  filter(year >= 1990, year <=1999)
write_csv(the_nineties,'the_ninties.csv')

# 19) Now that everything is up to date, make a visualisation of you and your team member’s names for a year of your choice.
team_name <- bb_names%>%
  filter(year=='2004', name %in% c('Mason','Ruth','Lucy','Cheyenne') ) 

# 20) Make a visual that looks at your name over time. What happens if you color by sex?

ggplot(data=team_name, aes(x=name, y= n,fill= sex))+ geom_col() + facet_wrap(~sex)


  

