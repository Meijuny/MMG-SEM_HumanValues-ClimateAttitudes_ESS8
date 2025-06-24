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
##Option C: partial metric invariance model with support 2 as marker and let support 3 freely estimated (wide bound estimation for convergence)
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
##option C: CCPolSupport partial metric with support 2 as marker:
BasicModel.PMetricCCPolSup.marker2.Selection<-ModelSelection(dat=ESS8,
                                                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2), #the lavaan objects from the measurement model
                                                             S2 = Str_model,
                                                             group = "country",
                                                             clusters=c(1,8), ##run from 1-8 clusters
                                                             seed = 100,
                                                             userStart = NULL,
                                                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                             max_it = 10000L,
                                                             nstarts = 50L, #50 random starts
                                                             printing = FALSE,
                                                             partition = "hard",
                                                             endogenous_cov = TRUE,
                                                             endo_group_specific = TRUE,
                                                             sam_method = "local",
                                                             meanstr = FALSE,
                                                             rescaling = F,
                                                             missing="FIML")

View(BasicModel.PMetricCCPolSup.marker2.Selection$Overview)

#set the canvas to have CHull plot on the left and BIC_G plot on the right
par(mfrow = c(1, 2))

#CHull plot
hull_indices <- chull(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL)
plot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL, 
     pch=4, col="black", main="CHull plot",xlab="Number of free parameters", ylab="Loglikelihood")
lines(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$nrpar[hull_indices], 
      BasicModel.PMetricCCPolSup.marker2.Selection$Overview$LL[hull_indices], col="black", lwd=1)

##BIC_G plot:
plot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$Clusters, BasicModel.PMetricCCPolSup.marker2.Selection$Overview$BIC_G, 
     pch=4, col="black", main="BIC_G plot",xlab="Number of clusters", ylab="BIC_G")
lines(BasicModel.PMetricCCPolSup.marker2.Selection$Overview$Clusters, 
      BasicModel.PMetricCCPolSup.marker2.Selection$Overview$BIC_G, col="black", lwd=1)

##set back the canvans to default:
par(mfrow = c(1, 1))

##suggest 3 clusters --> however, when comparing the 3-cluster solution and the 4-cluster solution with the below codes,
#we notice that the 3-cluster solution put the extreme values at both sides together
round(BasicModel.PMetricCCPolSup.marker2.Selection$Models$`3-cluster model`$posteriors[,1:3],digits = 4) #3-cluster membership
round(BasicModel.PMetricCCPolSup.marker2.Selection$Models$`4-cluster model`$posteriors[,1:4],digits = 4) #4-cluster membership
BasicModel.PMetricCCPolSup.marker2.Selection$Models$`3-cluster model`$param$beta_ks #3-cluster solution: regression coefficients for each cluster
BasicModel.PMetricCCPolSup.marker2.Selection$Models$`4-cluster model`$param$beta_ks #4-cluster solution: regression coefficients for each cluster
#
##Therefore, we go with 4-cluster to allow the countries with the opposite extreme values to be separated into two clusters

###-----------------------------------------------------------------------------------------------------------------------------
##Run 4-cluster solution to interpret the clusters
CCPolicySupport.4clus.50S.MarkSup2.PM<-MMGSEM(dat=ESS8,
                                              S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                              S2 = Str_model,
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
round(CCPolicySupport.4clus.50S.MarkSup2.PM$posteriors[,1:4], digits = 5)

#inspect the regression coefficients of each cluster
CCPolicySupport.4clus.50S.MarkSup2.PM$param$beta_ks


##cluster membership of the countries:
clustering.4clus<-t(apply(CCPolicySupport.4clus.50S.MarkSup2.PM$posteriors[,1:4],1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)

ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))
ClusterRes.4clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.4clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")
