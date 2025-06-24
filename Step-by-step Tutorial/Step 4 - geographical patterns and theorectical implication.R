library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
library(mmgsem)

#########################################################################################
####################### geographical patterns ###########################################
#########################################################################################

##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.4clus<-ClusterRes.4clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_4clusters.PM <- eu_map %>%
  left_join(ClusterRes.4clus, by = "region")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62",  # Warm Coral
                    "3" = "#8DA0CB",  # Mint Green
                    "4" = "#E78AC3")

##lay out on the map:
ggplot(map_with_4clusters.PM, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  scale_fill_manual(values = cluster_colors)+
  labs(
    fill = "Cluster"
  ) +
  theme_minimal()

##Map it also with the characteristics:
ClusterRes.4clus<-ClusterRes.4clus %>%
  mutate(ClusterChar=case_when(
    ClusMembership == 1 ~ "Cluster 1: strongest ST and Con",
    ClusMembership == 2 ~ "Cluster 2: weakest ST and Con",
    ClusMembership == 3 ~ "Cluster 3: strong ST and Con",
    ClusMembership == 4 ~ "Cluster 4: weak ST and Con"
  ))

ClusterRes.4clus$ClusterChar<-factor(ClusterRes.4clus$ClusterChar, 
                                     levels = c("Cluster 1: strongest ST and Con", 
                                                "Cluster 2: weakest ST and Con",
                                                "Cluster 3: strong ST and Con",
                                                "Cluster 4: weak ST and Con"))


map_with_4clusters.PM <- eu_map %>%
  left_join(ClusterRes.4clus, by = "region")

cluster_colors <- c("Cluster 1: strongest ST and Con" = "#66C2A5",  # Soft Blue
                    "Cluster 2: weakest ST and Con" = "#FC8D62",  # Warm Coral
                    "Cluster 3: strong ST and Con" = "#8DA0CB",  # Mint Green
                    "Cluster 4: weak ST and Con" = "#E78AC3")

##lay out on the map:
ggplot(map_with_4clusters.PM, aes(long, lat, group = group, fill = ClusterChar)) +
  geom_polygon(color = "white") +
  scale_fill_manual(values = cluster_colors)+
  labs(
    fill = "Cluster & Characteristics"
  ) +
  theme_minimal()


######################################################################################
############## country-level indicators ##############################################
######################################################################################

##start with GDP per capita: data retrieved from World Bank
#
##read the data in:
GDPperCapita<-read.csv("GDP per Capita.csv", skip = 4)
#
##select data from 2014, 2015, 2016, 2017:
GDPperCapita<-GDPperCapita[,c("Country.Name","X2014","X2015","X2016","X2017")]
#
##filter the data for the countries we have:
GDPperCapita<-GDPperCapita %>%
  filter(Country.Name=="Austria" | Country.Name=="Belgium" | Country.Name=="Switzerland" | 
           Country.Name=="Czechia" | Country.Name=="Germany" | Country.Name=="Estonia" | Country.Name=="Spain" | 
           Country.Name=="Finland" | Country.Name=="France" | Country.Name=="United Kingdom" | Country.Name=="Hungary" | 
           Country.Name=="Ireland" | Country.Name=="Israel" | Country.Name=="Iceland" | Country.Name=="Italy" | 
           Country.Name=="Lithuania" | Country.Name=="Netherlands" | Country.Name=="Norway" | Country.Name=="Poland" | 
           Country.Name=="Portugal" | Country.Name=="Russian Federation" | Country.Name=="Sweden" | Country.Name=="Slovenia" )


##rename the data from the clustering data frame:
ClusterRes.4clus<-ClusterRes.4clus %>%
  mutate(Country.Name=case_when(
    country=="AT" ~ "Austria",
    country=="BE" ~ "Belgium",
    country=="CH" ~ "Switzerland",
    country=="CZ" ~ "Czechia",
    country=="DE" ~ "Germany",
    country=="EE" ~ "Estonia",
    country=="ES" ~ "Spain",
    country=="FI" ~ "Finland",
    country=="FR" ~ "France",
    country=="GB" ~ "United Kingdom",
    country=="HU" ~ "Hungary",
    country=="IE" ~ "Ireland",
    country=="IL" ~ "Israel",
    country=="IS" ~ "Iceland",
    country=="IT" ~ "Italy",
    country=="LT" ~ "Lithuania",
    country=="NL" ~ "Netherlands",
    country=="NO" ~ "Norway",
    country=="PL" ~ "Poland",
    country=="PT" ~ "Portugal",
    country=="RU" ~ "Russian Federation",
    country=="SE" ~ "Sweden",
    country=="SI" ~ "Slovenia"
  ))

##merge the 2 data frame:
GDPPerCapita_4clusters<-merge(ClusterRes.4clus, GDPperCapita,
                              by.x = "Country.Name",
                              by.y="Country.Name")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62", # Warm Coral
                    "3" = "#8DA0CB",  # Mint Green
                    "4" = "#E78AC3")

ggplot(GDPPerCapita_4clusters, aes(x=factor(ClusMembership), y=X2016, fill=factor(ClusMembership)))+
  geom_boxplot()+
  scale_fill_manual(values = cluster_colors)+
  xlab("cluster")+ylab("GDP per Capita of 2016")+
  labs(title = "GDP per Capita by Clusters", fill="cluster")+
  theme_bw()


###----------------------------------------------------------------------------------------
##Explore the environment policy stringency according to policy feedback theory:
##Environmental Policy Stringency Index: data from
#https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEnvironment%20and%20climate%20change%23ENV%23%7CEnvironmental%20policy%23ENV_POL%23&pg=0&fc=Topic&bp=true&snb=7&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EPS%40DF_EPS&df[ag]=OECD.ECO.MAD&df[vs]=1.0&dq=.A..EPS&lom=LASTNPERIODS&lo=5&to[TIME_PERIOD]=false&vw=tb

#read data in:
EPSI<-read.csv("./Environmental Policy Stringency Index.csv")

##filter:
ESPI_2016<-EPSI %>%
  filter(TIME_PERIOD == 2016) %>%
  dplyr::select(Reference.area, OBS_VALUE)

##Change country names:
ClusterRes.4clus <- ClusterRes.4clus %>%
  mutate(Reference.area=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czechia",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "United Kingdom",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ NA,
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  ))

ESPI_clus<-merge(ClusterRes.4clus, ESPI_2016,
                 by.x = "Reference.area", by.y = "Reference.area")

cluster_colors <- c("2" = "#FC8D62",  # Warm Coral
                    "3" = "#8DA0CB",  # Mint Green
                    "4" = "#E78AC3")

ggplot(data = ESPI_clus, aes(x=factor(ClusMembership), y=OBS_VALUE, fill=factor(ClusMembership)))+
  geom_boxplot()+
  xlab("Cluster")+ylab("Environmental Policy Stringency Index")+
  scale_fill_manual(values = cluster_colors)+
  labs(fill="cluster", title = "Environmental Policy Stringency Index by Clusters")+
  theme_bw()
