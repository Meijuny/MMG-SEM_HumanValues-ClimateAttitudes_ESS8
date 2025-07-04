library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
library(mmgsem)


#library(devtools)
#library(usethis)
#library(gitcreds)
#gitcreds_set()
#devtools::install_github("AndresFPA/mmgsem", force = T)

###################################################################################
####################### Data Management ###########################################
###################################################################################

##Data is downloaded from: https://ess.sikt.no/en/data-builder/?rounds=7&seriesVersion=888

##Read the data in:
ESS8<-read.csv("./ESS8.csv")

##Select the variables:
ESS8<-ESS8 %>%
  select(idno, cntry, ##grouping variable,
         ipeqopt, ipudrst, impenv, iphlppl, iplylfr, ##self-transcendence
         ipmodst, imptrad, ipfrule, ipbhprp, impsafe, ipstrgv, ##conservation
         ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ##Opennes to change
         ipshabt, ipsuces, imprich, iprspot, ##self-enhancement
         clmchng, ccnthum, ccgdbd, ##climate change skepticism
         wrclmch, ##worries about climate change
         cflsenr, ccrdprs, ##personal efficacy regarding climate change
         inctxff, sbsrnen, banhhap, ##opposition for climate change policy
         lrscale, gndr, agea, eduyrs, ##demographic
         hincfel, ##income insecurity
         trstprl, trstplt,trstprt ##political trust
  )

##Data cleaning:
##rename the variable cntry to country
ESS8<-ESS8 %>% rename(country=cntry)

##Self-transcendence value:
#
##define 7,8,9 as missing value
ESS8[,c("ST1","ST2","ST3","ST4","ST5")]<-lapply(ESS8[,c("ipeqopt", "ipudrst", "impenv",  "iphlppl", "iplylfr")],
                                                function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("ST1","ST2","ST3","ST4","ST5")]<-lapply(ESS8[,c("ST1","ST2","ST3","ST4","ST5")],
                                                function(x) -x+7)
##Conservation value:
#
##define 7,8,9 as missing value
ESS8[,c("C1","C2","C3","C4","C5","C6")]<-lapply(ESS8[,c("ipmodst", "imptrad", "ipfrule", "ipbhprp", "impsafe","ipstrgv")],
                                                function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("C1","C2","C3","C4","C5","C6")]<-lapply(ESS8[,c("C1","C2","C3","C4","C5","C6")],
                                                function(x) -x+7)
##Openness to change:
#
##define 7,8,9 as missing value
ESS8[,c("O1","O2","O3","O4","O5","O6")]<-lapply(ESS8[,c("ipcrtiv", "impfree", "impdiff", "ipadvnt", "ipgdtim", "impfun")],
                                                function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("O1","O2","O3","O4","O5","O6")]<-lapply(ESS8[,c("O1","O2","O3","O4","O5","O6")],
                                                function(x) -x+7)
##Self-enhancement:
#
##define 7,8,9 as missing value
ESS8[,c("SE1","SE2","SE3","SE4")]<-lapply(ESS8[,c("ipshabt", "ipsuces", "imprich", "iprspot")],
                                          function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale:
ESS8[,c("SE1","SE2","SE3","SE4")]<-lapply(ESS8[,c("SE1","SE2","SE3","SE4")],
                                          function(x) -x+7)

##climate change belief (higher value indicates higher level of awareness and belief in climate change)
ESS8<-ESS8 %>%
  mutate(TrendBelief=ifelse(clmchng %in% c(7,8,9), NA, clmchng),
         AttriBelief=ifelse(ccnthum %in% c(55,66,77,88,99), NA, ccnthum),
         ImpactBelief=ifelse(ccgdbd %in% c(66,77,88,99), NA, ccgdbd)) %>%
  mutate(TrendBelief=-(TrendBelief)+5,
         ImpactBelief=-(ImpactBelief)+10) %>%
  mutate(ImpactBelief=ImpactBelief/2)

##Climate change concern
ESS8<-ESS8 %>%
  mutate(ClimateConcern=ifelse(wrclmch %in% c(6,7,8,9), NA, wrclmch))

##Personal Efficacy regarding climate change
ESS8<-ESS8 %>%
  mutate(PE1=ifelse(cflsenr %in% c(77,88,99), NA, cflsenr),
         PE2=ifelse(ccrdprs %in% c(66,77,88,99), NA, ccrdprs))

##opposition to climate change policy
ESS8[,c("support1","support2","support3")]<-lapply(ESS8[,c("inctxff","sbsrnen","banhhap")],
                                                   function(x) ifelse(x %in% c(7,8,9), NA, x))
ESS8[,c("support1","support2","support3")]<-lapply(ESS8[,c("support1","support2","support3")],
                                                   function(x) -x+6)

##Political placement & demographics:
ESS8<-ESS8 %>%
  mutate(PoliScale=ifelse(lrscale %in% c(77,88,99), NA, lrscale),
         male=case_when(
           gndr == 1 ~ 1,
           gndr == 2 ~ 0,
           gndr == 9 ~ NA
         ),
         age=ifelse(agea==999, NA, agea),
         eduyrs=ifelse(eduyrs>30, NA, eduyrs))

##Income insecurity:
ESS8$IncomeInsecure<-ifelse(ESS8$hincfel %in% c(7,8,9),NA, ESS8$hincfel)


