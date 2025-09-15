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
############################# Basic Model  ##########################################
#####################################################################################

##First, run all the necessary measurement models with marker variable approach:
#
##Human Values
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

##climate change policy support
##partial metric invariance model with support 2 as marker and let support 3 freely estimated (wide bound estimation for convergence)
CCPolSupport.PMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.PMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.PMetric.M1.MarkerSup2,
                                          data = ESS8,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings",
                                          group.partial=c("CCPolicySupport=~support3"),
                                          bounds="wide")

##--------------------------------------------------------------------------------------------------------
##Specify Structural model
Str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

##--------------------------------------------------------------------------------------------------------
##Model selection 
#
BasicModel.PMetricCCPolSup.marker2.Selection<-ModelSelection(dat=ESS8,
                                                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2), ##objects with lavaan syntax for the measurement model
                                                             S2 = Str_model, ##object with syntax to specify the structural model
                                                             group = "country",
                                                             nclus = c(1,8), ##run from 1-8 clusters
                                                             seed = 100,
                                                             userStart = NULL,
                                                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2), #the lavaan cfa objects from the measurement model
                                                             max_it = 10000L,
                                                             nstarts = 50L, #50 random starts
                                                             printing = FALSE,
                                                             partition = "hard",
                                                             endogenous_cov = TRUE,
                                                             endo_group_specific = TRUE,
                                                             sam_method = "local",
                                                             meanstr = FALSE,
                                                             rescaling = F,
                                                             missing="FIML",
                                                             s1_type="lavaan")

View(BasicModel.PMetricCCPolSup.marker2.Selection$Overview)

#set the canvas to have CHull plot on the left and BIC_G plot on the right
par(mfrow = c(1, 2))

#CHull plot
hull_indices <- chull(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL)
plot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL, 
     pch=4, col="black", main="CHull Scree Plot",xlab="Number of free parameters", ylab="Loglikelihood")
lines(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar[hull_indices], 
      BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL[hull_indices], col="black", lwd=1)

##BIC_G plot:
plot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$Clusters, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$BIC_G, 
     pch=4, col="black", main="BIC_G Plot",xlab="Number of clusters", ylab="BIC_G")
lines(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$Clusters, 
      BasicModel.PMetricCCPolSup.marker2.Selection$Overview$BIC_G, col="black", lwd=1)

##set back the canvans to default:
par(mfrow = c(1, 1))

##suggest 3 clusters 
##--> however, when extracting the regression coefficients from each country to do the validation in Step 3,
##it is revealed that the 3-cluster solution putting Lithuania (with strongest effect) and Hungary (with weakest effect) together
BasicModel.PMetricCCPolSup.marker2.Selection$Models$`3-cluster model`$posteriors #3-cluster membership

##Then we come back here to extract the 4-cluster solution
##4-cluster solution separates Lithuania and Hungary into its own cluster
BasicModel.PMetricCCPolSup.marker2.Selection$Models$`4-cluster model`$posteriors #4-cluster membership
BasicModel.PMetricCCPolSup.marker2.Selection$Models$`4-cluster model`$param$beta_ks #4-cluster solution: regression coefficients for each cluster
#
##Therefore, we go with 4-cluster to allow the Lithuania and Hungary to be separated into two clusters

###-----------------------------------------------------------------------------------------------------------------------------
##Run 4-cluster solution to interpret the clusters
Str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolicySupport.4clus.50S.MarkSup2.PM<-mmgsem(dat=ESS8,
                                              S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                              S2 = Str_model,
                                              s1_type="lavaan",
                                              group = "country",
                                              nclus=4, ##4-cluster solution
                                              seed = 100,
                                              userStart = NULL,
                                              s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                              max_it = 10000L,
                                              nstarts = 50L,
                                              printing = FALSE,
                                              partition = "hard",
                                              endogenous_cov = TRUE,
                                              endo_group_specific = TRUE,
                                              sam_method = "local",
                                              meanstr = FALSE,
                                              rescaling = F,
                                              missing="FIML")

#inspect the classification uncertainty
CCPolicySupport.4clus.50S.MarkSup2.PM$posteriors

#inspect the regression coefficients of each cluster
CCPolicySupport.4clus.50S.MarkSup2.PM$param$beta_ks

##hypothesis testing to see if effect is significant from 0
StartTime<-Sys.time()
se.4clus<-compute_se(CCPolicySupport.4clus.50S.MarkSup2.PM,
                     d = 0.005,
                     naive = F)
EndTime<-Sys.time()
StartTime-EndTime

##for d=0.001 with correction: computation time 2.37 hours
summary(model=CCPolicySupport.4clus.50S.MarkSup2.PM,se=se.4clus)
##se for cluster 1 SelfEnhan cannot be computed
##se for cluster 4 all three HV cannot be computed

##for d=0.01 with correction: after 2 hours, throw and error

##for d=0.0001 with correction: computation time 3.32 hours
summary(model=CCPolicySupport.4clus.50S.MarkSup2.PM,se=se.4clus)
##se for cluster 1 all three HV cannot be computed
##se for cluster 3 SelfTran and SelfHan cannot be computed
##se for cluster 4 SelfTran and SelfHan cannot be computed

##Return to the previous function:
StartTime<-Sys.time()
se.BasicModel.4clus<-se(object = CCPolicySupport.4clus.50S.MarkSup2.PM,
                        d = 0.00001)
EndTime<-Sys.time()
StartTime-EndTime

##the SE object has been saved since the last run, now we can directly load it:
load("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/se.BasicModel.4clus.RData")

summary(model=CCPolicySupport.4clus.50S.MarkSup2.PM,se=se.BasicModel.4clus)


##hypothesis testing to see if effect differs between groups
test.mmgsem(model = CCPolicySupport.4clus.50S.MarkSup2.PM,
            se = se.BasicModel.4clus,
            multiple_comparison = T)


##cluster membership of the countries for next step of making validation plots:
clustering.4clus<-CCPolicySupport.4clus.50S.MarkSup2.PM$posteriors
clustering.4clus[,2:5]<-t(apply(clustering.4clus[,2:5],1,function(x) as.numeric(x==max(x))))
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,2,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,3,0)
clustering.4clus[,5]<-ifelse(clustering.4clus[,5]==1,4,0)

ClusterRes.4clus<-clustering.4clus %>%
  mutate(ClusMembership=`Cluster 1`+`Cluster 2`+`Cluster 3`+`Cluster 4`) %>%
  select(Group, ClusMembership) %>%
  rename(country=Group) %>%
  mutate(group=row_number())
