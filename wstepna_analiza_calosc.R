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

library(ggplot2)
library(ggthemes)
confirmed.terrorism %>% filter(iyear>2005) %>% group_by(country_txt) %>%
	summarise(total_kills=floor(sum(nkill, na.rm=T)),
						region_txt=head(region_txt,1)) %>% arrange(desc(total_kills)) %>%
	select(country_txt, total_kills, region_txt) %>% top_n(20, total_kills) -> total_killed_10y
total_killed_10y$country_txt <- factor(total_killed_10y$country_txt, levels=rev(total_killed_10y$country_txt))

ggplot(total_killed_10y, aes(x=country_txt, y=total_kills, fill=region_txt)) +
	geom_bar(stat = "identity") + theme_tufte() +
	theme(legend.position = c(0.8, 0.2),
				legend.text = element_text(size=18),
				legend.title = element_text(size=22),
				plot.title = element_text(size=24)) +
	xlab("") + ylab("Łączna liczba zabitych") +
	guides(fill=guide_legend("Region")) +
	coord_flip() + expand_limits(x = 0, y = 0) +
	ggtitle("Kraje o największej liczbie zabitych w zamachach terrorystycznych w latach 2006-2015")


confirmed.terrorism %>% filter(iyear>2005) %>% group_by(region_txt) %>%
	summarise(total_kills=floor(sum(nkill, na.rm=T))) %>% arrange(desc(total_kills)) %>%
	select(total_kills, region_txt) %>% top_n(20, total_kills) -> total_killed_10y_regions
total_killed_10y_regions$region_txt <- factor(total_killed_10y_regions$region_txt, levels=rev(total_killed_10y_regions$region_txt))
total_killed_10y_regions$continent <- factor(total_killed_10y_regions$region_txt, levels=rev(total_killed_10y_regions$region_txt))
levels(total_killed_10y_regions$continent)=c("Australasia & Oceania", "America", "Asia", "Europe", "America", "Asia",
																						 				  "America", "Europe", "Asia", "Africa", "Asia", 
																						 "Middle East & North Africa")


ggplot(total_killed_10y_regions, aes(x=region_txt, y=total_kills, fill=continent)) +
	geom_bar(stat = "identity") + theme_tufte() +
	theme(legend.position = c(0.8, 0.2),
				legend.text = element_text(size=18),
				legend.title = element_text(size=22),
				plot.title = element_text(size=24)) +
	xlab("") + ylab("Łączna liczba zabitych") +
	guides(fill=guide_legend("Kontynent")) +
	coord_flip() + expand_limits(x = 0, y = 0) +
	ggtitle("Regiony o największej liczbie zabitych w zamachach terrorystycznych w latach 2006-2015")
