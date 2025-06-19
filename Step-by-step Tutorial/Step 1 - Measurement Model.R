library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
library(mmgsem)

#####################################################################################
################## Measurement Block 1: Human Values ############################
#####################################################################################

##Configural model 1 --> corresponding to M1 in the paper
NoOpen.HV.Config.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit1<-cfa(model = NoOpen.HV.Config.M1,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit1.txt")
summary(NoOpen.HV.Config.Fit1, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M1<-modindices(NoOpen.HV.Config.Fit1, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M1<-NoOpen.MI.Config.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M1 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M1_mi_count.RData")
save(NoOpen.MI.Config.M1, file="./Sink Output/report1/M1_mi.RData")


##Configural Model 2: add cross-loading: Conser=~SE4 --> corresponding to M2 in the paper
NoOpen.HV.Config.M2<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit2<-cfa(model = NoOpen.HV.Config.M2,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit2.txt")
summary(NoOpen.HV.Config.Fit2, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M2<-modindices(NoOpen.HV.Config.Fit2, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M2<-NoOpen.MI.Config.M2 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M2 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M2_mi_count.RData")
save(NoOpen.MI.Config.M2, file="./Sink Output/report1/M2_mi.RData")


##Configural Model 3: add cross-loading: SelfTran=~SE3 --> corresponding to M3 in the paper
NoOpen.HV.Config.M3<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit3<-cfa(model = NoOpen.HV.Config.M3,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit3.txt")
summary(NoOpen.HV.Config.Fit3, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M3<-modindices(NoOpen.HV.Config.Fit3, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M3<-NoOpen.MI.Config.M3 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M3 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M3_mi_count.RData")
save(NoOpen.MI.Config.M3, file="./Sink Output/report1/M3_mi.RData")

##Configural Model 4: add cross-loading: SelfEnhan=~C1 --> corresponding to M4 in the paper
NoOpen.HV.Config.M4<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1
'

NoOpen.HV.Config.Fit4<-cfa(model = NoOpen.HV.Config.M4,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit4.txt")
summary(NoOpen.HV.Config.Fit4, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M4<-modindices(NoOpen.HV.Config.Fit4, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M4<-NoOpen.MI.Config.M4 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M4 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M4_mi_count.RData")
save(NoOpen.MI.Config.M4, file="./Sink Output/report1/M4_mi.RData")


##Configural Model 5: add error term correlation: C5~~C6 --> corresponding to M5 in the paper
NoOpen.HV.Config.M5<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit5<-cfa(model = NoOpen.HV.Config.M5,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit5.txt")
summary(NoOpen.HV.Config.Fit5, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M5<-modindices(NoOpen.HV.Config.Fit5, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M5<-NoOpen.MI.Config.M5 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M5 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M5_mi_count.RData")
save(NoOpen.MI.Config.M5, file="./Sink Output/report1/M5_mi.RData")

##Configural Model 6: Allow cross-loading SelfTran=~C3 --> corresponding to M6 in the paper
NoOpen.HV.Config.M6<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit6<-cfa(model = NoOpen.HV.Config.M6,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit6.txt")
summary(NoOpen.HV.Config.Fit6, fit.measures=T, standardized=T)
sink()

#check modification indices
NoOpen.MI.Config.M6<-modindices(NoOpen.HV.Config.Fit6, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M6<-NoOpen.MI.Config.M6 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M6 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

save(ParameterCount, file="./Sink Output/report1/M6_mi_count.RData")
save(NoOpen.MI.Config.M6, file="./Sink Output/report1/M6_mi.RData")

##Configural Model 7: Allow cross-loading SelfTran=~C4 --> corresponding to M7 in the paper
NoOpen.HV.Config.M7<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit7<-cfa(model = NoOpen.HV.Config.M7,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/report1/HV_Config_fit7.txt")
summary(NoOpen.HV.Config.Fit7, fit.measures=T, standardized=T)
sink()


##(full) Metric Model 1 --> corresponding to M8 in the paper
NoOpen.HV.Metric.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit1<-cfa(model = NoOpen.HV.Metric.M1,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           std.lv=T)

sink("./Sink Output/report1/HV_Metric_fit1.txt")
summary(NoOpen.HV.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##request modification indices:
NoOpen.MI.Metric.M1<-lavTestScore(NoOpen.HV.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-NoOpen.MI.Metric.M1$uni
EPC.MI.M1<-NoOpen.MI.Metric.M1$epc

Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M1 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


save(EPC.Chi2Diff.Summary, file="./Sink Output/report1/M8_mi_count.RData")
save(EPC.Chi2Diff.M1, file="./Sink Output/report1/M8_mi.RData")



##(Partial) Metric Model 2: let SelfEnhan=~SE3 to be freely estimated --> corresponding to M9 in the paper
NoOpen.HV.Metric.M2<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2<-cfa(model = NoOpen.HV.Metric.M2,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelfEnhan=~SE3"),
                           std.lv=T)

sink("./Sink Output/report1/HV_Metric_fit2.txt")
summary(NoOpen.HV.Metric.Fit2, fit.measures=T, standardized=T)
sink()

##selecting the partial Metric Model (M9 in the paper) as the final measurement model for human values
##Change to Marker variable approach to set the scale of the factor for MMG-SEM step 2
#markers are ST4, C2, and SE2 for self-transcendence, conservation, and self-enhancement respectively
#
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))


sink("./Sink Output/report1/Marker_HV_Metric_fit2.txt")
summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
sink()



#####################################################################################
################# Measurement block 2: Climate Policy Support #######################
#####################################################################################


###---------------------------------------------------------------------------------------
##Configural Invariance Model 1 with wide bounded estimation for convergence:
CCPolSupport.Config.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Config.Fit1.WideBound<-cfa(model = CCPolSupport.Config.M1,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        std.lv=T,
                                        bounds="wide")

sink("./Sink Output/report1/CCPolicySupport_Config_fit1_WideBound.txt")
summary(CCPolSupport.Config.Fit1.WideBound, fit.measures=T, standardized=T)
sink()

##(full) metric Invariance Model 1:
CCPolSupport.Metric.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit1<-cfa(model = CCPolSupport.Metric.M1,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              std.lv=T)

sink("./Sink Output/report1/CCPolicySupport_Metric_fit1.txt")
summary(CCPolSupport.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##request modification indices:
CCPolSupport.MI.Metric.M1<-lavTestScore(CCPolSupport.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-CCPolSupport.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-CCPolSupport.MI.Metric.M1$epc
EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M1 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
##Suggests to free support3 item


##(full) metric Invariance Model 1 with wide bounded estimation:
CCPolSupport.Metric.M1.wide<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support1+
                  L2*support2+
                  L3*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit1.WideBound<-cfa(model = CCPolSupport.Metric.M1.wide,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        group.equal="loadings",
                                        bounds="wide")

sink("./Sink Output/report1/CCPolicySupport_Metric_fit1_WideBound.txt")
summary(CCPolSupport.Metric.Fit1.WideBound, fit.measures=T, standardized=T)
sink()
##yielding same results as the standard no-bounded estimation

##Partial metric invariance with support3 item freely estimated:
##use wide bound estimation for convergence
CCPolSupport.Metric.M2.wide<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit2.WideBound<-cfa(model = CCPolSupport.Metric.M2.wide,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        bounds="wide")

sink("./Sink Output/report1/CCPolicySupport_Metric_fit2_WideBound.txt")
summary(CCPolSupport.Metric.Fit2.WideBound, fit.measures=T, standardized=T)
sink()

#Select the partial metric invariance model
##switch to marker variable approach with support2 as marker to set the scale for MMG-SEM step 2
##wide bound
CCPolSupport.Metric.M2.Marker<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3
'

CCPolSupport.Metric.Fit2.Marker.WideBound<-cfa(model = CCPolSupport.Metric.M2.Marker,
                                               data = ESS8,
                                               group = "country",
                                               estimator="MLR",
                                               missing="FIML",
                                               bounds="wide")

sink("./Sink Output/report1/CCPolicySupport_Metric_fit2_marker_wideBound.txt")
summary(CCPolSupport.Metric.Fit2.Marker.WideBound, fit.measures=T, standardized=T)
sink()


#####################################################################################
############### Measurement Block 3: Climate Change Belief ###########################
#####################################################################################

##Configural invariance Model 1: perfect fit since the model is just identified
CCBelief.Config.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
'

CCBelief.Config.Fit1<-cfa(model = CCBelief.Config.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          std.lv=T)

sink("./Sink Output/report1/CCBelief_Config_fit1.txt")
summary(CCBelief.Config.Fit1, fit.measures=T, standardized=T)
sink()

##(Full) Metric Model 1: 
CCBelief.Metric.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
'

CCBelief.Metric.Fit1<-cfa(model = CCBelief.Metric.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          group.equal="loadings",
                          std.lv=T)

sink("./Sink Output/report1/CCBelief_Metric_fit1.txt")
summary(CCBelief.Metric.Fit1, fit.measures=T, standardized=T)
sink()


#Full metric model is good
##we switch to marker variable approach to set the scale of the factor for MMG-SEM step 2
##(Full) Metric Model 1: 
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")


sink("./Sink Output/report1/Marker_CCBelief_Metric_fit1.txt")
summary(CCBelief.Metric.Fit1.Marker, fit.measures=T, standardized=T)
sink()
