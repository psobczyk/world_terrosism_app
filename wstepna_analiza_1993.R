setwd("GitHub/world_terrosism_app/")
library(dplyr)
dane <- read.csv("gtd1993_0615dist.csv")

colnames(dane)

dane %>%
  select(iyear, imonth, iday, country, country_txt,region_txt, city, suicide, crit1,
         crit2, crit3, doubtterr, attacktype1_txt, suicide, weaptype1_txt,
         targtype1_txt, gname, nperps, nperpcap, nkill, nkillter, nwound, nwoundte) -> dane2

dane2 %>% group_by(country_txt) %>%
  summarise(numb.incidents=n()) %>% arrange(desc(numb.incidents)) -> most_attacks_93

dane2 %>%
  group_by(country_txt) %>%
  summarise(total_kills=sum(nkill, na.rm=T)) %>% arrange(desc(total_kills)) -> total_kills_93

# why some many from Sri Lanka?
dane %>% filter(country_txt=="Sri Lanka") %>% select(scite1)
#It was part of the civil var

dane2 %>% filter(doubtterr==0) %>%
  group_by(country_txt) %>%
  summarise(total_kills=floor(sum(nkill, na.rm=T))) %>% arrange(desc(total_kills)) -> total_kills_93
