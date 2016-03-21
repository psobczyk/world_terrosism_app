setwd("~/world_terrosism_app/")
library(dplyr)

# terrorism <- read.csv("globalterrorismdb_0615dist.csv")
# 
# terrorism %>%
# 	select(iyear, imonth, iday, country, country_txt,region_txt, city, latitude, longitude, suicide, crit1,
# 				 crit2, crit3, doubtterr, attacktype1_txt, suicide, weaptype1_txt, weapsubtype1_txt,
# 				 targtype1_txt, gname, motive, claimed, nperps, nperpcap, nkill, nkillter, nwound, nwoundte,
# 				 property, propextent_txt, ishostkid, nhostkid, nhours, ndays, 
# 				 ransom, ransomamt, hostkidoutcome_txt, nreleased, scite1) -> terrorism2
# write.csv(terrorism2, file = "globalterrorism_selected_cols_0615.csv", row.names = FALSE)

terrorism <- read.csv("globalterrorism_selected_cols_0615.csv")

terrorism %>% filter(doubtterr==0) -> confirmed.terrorism

confirmed.terrorism %>% group_by(country_txt) %>%
	summarise(numb.incidents=n()) %>% arrange(desc(numb.incidents)) -> most_attacked_countries

confirmed.terrorism %>% group_by(country_txt) %>%
	summarise(total_kills=floor(sum(nkill, na.rm=T))) %>% arrange(desc(total_kills)) -> total_killed

#how about attack in Poland?
confirmed.terrorism %>% filter(country_txt=="Poland") %>% 
	select(iyear, imonth, iday, city, nkill, nkillter, nwound, property, propextent_txt, scite1) -> terrorism_poland
#some are mafia-related http://niniwa22.cba.pl/krotka_historia_mafii.htm

#terrorist attacks in Ukraine are mostly related to separatist attacks in the eastern part of the country
confirmed.terrorism %>% filter(country_txt=="Ukraine") %>% 
	select(iyear, imonth, iday, city, nkill, nkillter, nwound, property, propextent_txt, scite1) -> terrorism_ukraine
