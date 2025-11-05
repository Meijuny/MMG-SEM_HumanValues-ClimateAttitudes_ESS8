library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
library(mmgsem)

##use the same data ESS8 (data preparation step see 'Step-by-step Tutorial' --> Step 0 - Data Preparation.R)

####------------------------------------------------------------------------
#First re-run the three measurement blocks that was estimated with all 23 countries with the marker variable approach
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

##climate change belief
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")


###-----------------------------------------------------------------------------------------------------------------
##Model selection:
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

Mediation.ModelSelection<-ModelSelection(dat=ESS8,
                                         S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                         S2 = Str_model,
                                         s1_type = "lavaan",
                                         group = "country",
                                         nclus=c(1,8),
                                         seed = 100,
                                         userStart = NULL,
                                         s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                         max_it = 10000L,
                                         nstarts = 500L,
                                         printing = F,
                                         partition = "hard",
                                         endogenous_cov = TRUE,
                                         endo_group_specific = TRUE,
                                         sam_method = "local",
                                         meanstr = FALSE,
                                         rescaling = F,
                                         missing="FIML")
View(Mediation.ModelSelection$Overview)


#set the canvas to have CHull plot on the left and BIC_G plot on the right
par(mfrow = c(1, 2))

#CHull plot
hull_indices <- chull(Mediation.ModelSelection$Overview$nrpar, Mediation.ModelSelection$Overview$LL)
plot(Mediation.ModelSelection$Overview$nrpar, Mediation.ModelSelection$Overview$LL, 
     pch=4, col="black", main="CHull Scree Plot",xlab="Number of free parameters", ylab="Loglikelihood")
lines(Mediation.ModelSelection$Overview$nrpar[hull_indices], 
      Mediation.ModelSelection$Overview$LL[hull_indices], col="black", lwd=1)

##BIC_G plot:
plot(Mediation.ModelSelection$Overview$Clusters, Mediation.ModelSelection$Overview$BIC_G, 
     pch=4, col="black", main="BIC_G Plot",xlab="Number of clusters", ylab="BIC_G")
lines(Mediation.ModelSelection$Overview$Clusters, 
      Mediation.ModelSelection$Overview$BIC_G, col="black", lwd=1)

##set back the canvans to default:
par(mfrow = c(1, 1))


##MMG-SEM 6 clusters:
##300 starts was sufficient to reach a stable solution for 6 clusters --> use 300 random starts below
##specify structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

##6 clusters:
Mediation.6clus.PM.MarkSup2<-mmgsem(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    s1_type = "lavaan",
                                    group = "country",
                                    nclus=6,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 300L,
                                    printing = F,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")

##check the posterior membership probabilities:
Mediation.6clus.PM.MarkSup2$posteriors

##check the regression coefficients of each cluster:
Mediation.6clus.PM.MarkSup2$param$beta_ks

##hypothesis testing to see if effects differ from 0:
##for d=0.00001: 6.710037 hours --> no NaNs produced
StartTime<-Sys.time()
se.Mediation.6clus<-se(object = Mediation.6clus.PM.MarkSup2,
                        d = 0.00001)
EndTime<-Sys.time()
StartTime-EndTime

##check the results in a nice output
summary(model=Mediation.6clus.PM.MarkSup2,se=se.Mediation.6clus)

##hypothesis testing to see if the parameters differ between groups
test.mmgsem(model = Mediation.6clus.PM.MarkSup2,
            se = se.Mediation.6clus,
            multiple_comparison = T)

##Cluster Membership in a concise df to make validation plot in the next step:
clustering.Mediation.6clus<-Mediation.6clus.PM.MarkSup2$posteriors
clustering.Mediation.6clus[,2:7]<-t(apply(clustering.Mediation.6clus[,2:7],1,function(x) as.numeric(x==max(x))))
clustering.Mediation.6clus[,3]<-ifelse(clustering.Mediation.6clus[,3]==1,2,0)
clustering.Mediation.6clus[,4]<-ifelse(clustering.Mediation.6clus[,4]==1,3,0)
clustering.Mediation.6clus[,5]<-ifelse(clustering.Mediation.6clus[,5]==1,4,0)
clustering.Mediation.6clus[,6]<-ifelse(clustering.Mediation.6clus[,6]==1,5,0)
clustering.Mediation.6clus[,7]<-ifelse(clustering.Mediation.6clus[,7]==1,6,0)

ClusterRes.Mediation.6clus<-clustering.Mediation.6clus %>%
  mutate(ClusMembership=`Cluster 1`+`Cluster 2`+`Cluster 3`+`Cluster 4`+`Cluster 5`+`Cluster 6`) %>%
  select(Group, ClusMembership) %>%
  rename(country=Group) %>%
  mutate(group=row_number())



###############################################################################
##################### Mediation - Validation ##################################
###############################################################################

Mediation_lavaanFREEsam_str_model_PM_MarkSup2<-'
##measurement model:
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##structural model:

CCBelief~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)*SelfTran+
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23)*Conser+
          c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23)*SelfEnhan

CCPolicySupport~c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)*CCBelief+SelfTran+Conser+SelfEnhan

STindirect_g1:=a1*d1
STindirect_g2:=a2*d2
STindirect_g3:=a3*d3
STindirect_g4:=a4*d4
STindirect_g5:=a5*d5
STindirect_g6:=a6*d6
STindirect_g7:=a7*d7
STindirect_g8:=a8*d8
STindirect_g9:=a9*d9
STindirect_g10:=a10*d10
STindirect_g11:=a11*d11
STindirect_g12:=a12*d12
STindirect_g13:=a13*d13
STindirect_g14:=a14*d14
STindirect_g15:=a15*d15
STindirect_g16:=a16*d16
STindirect_g17:=a17*d17
STindirect_g18:=a18*d18
STindirect_g19:=a19*d19
STindirect_g20:=a20*d20
STindirect_g21:=a21*d21
STindirect_g22:=a22*d22
STindirect_g23:=a23*d23

ConIndirect_g1:=b1*d1
ConIndirect_g2:=b2*d2
ConIndirect_g3:=b3*d3
ConIndirect_g4:=b4*d4
ConIndirect_g5:=b5*d5
ConIndirect_g6:=b6*d6
ConIndirect_g7:=b7*d7
ConIndirect_g8:=b8*d8
ConIndirect_g9:=b9*d9
ConIndirect_g10:=b10*d10
ConIndirect_g11:=b11*d11
ConIndirect_g12:=b12*d12
ConIndirect_g13:=b13*d13
ConIndirect_g14:=b14*d14
ConIndirect_g15:=b15*d15
ConIndirect_g16:=b16*d16
ConIndirect_g17:=b17*d17
ConIndirect_g18:=b18*d18
ConIndirect_g19:=b19*d19
ConIndirect_g20:=b20*d20
ConIndirect_g21:=b21*d21
ConIndirect_g22:=b22*d22
ConIndirect_g23:=b23*d23

SEindirect_g1:=c1*d1
SEindirect_g2:=c2*d2
SEindirect_g3:=c3*d3
SEindirect_g4:=c4*d4
SEindirect_g5:=c5*d5
SEindirect_g6:=c6*d6
SEindirect_g7:=c7*d7
SEindirect_g8:=c8*d8
SEindirect_g9:=c9*d9
SEindirect_g10:=c10*d10
SEindirect_g11:=c11*d11
SEindirect_g12:=c12*d12
SEindirect_g13:=c13*d13
SEindirect_g14:=c14*d14
SEindirect_g15:=c15*d15
SEindirect_g16:=c16*d16
SEindirect_g17:=c17*d17
SEindirect_g18:=c18*d18
SEindirect_g19:=c19*d19
SEindirect_g20:=c20*d20
SEindirect_g21:=c21*d21
SEindirect_g22:=c22*d22
SEindirect_g23:=c23*d23
'


Mediation.lavaanFreeSAM.fit<-sam(model = Mediation_lavaanFREEsam_str_model_PM_MarkSup2,
                                 data = ESS8, 
                                 mm.list = list(c("SelfTran","Conser","SelfEnhan"), "CCBelief","CCPolicySupport"), ##to separate the measurement block
                                 cmd = "sem",
                                 group="country",
                                 group.equal="loadings",
                                 group.partial=c("SelfEnhan=~SE3","CCPolicySupport=~support3"),
                                 sam.method = "local", ##local SEM where we use factor covariance matrix from step 1 to estimate the structural model in step 2
                                 missing="FIML",  #handle missing values
                                 )

####------------------------------------------------------------------------------
##Visualize the separation between sub-cluster 3.1 and sub-cluster 3.2
#
##subset the Cluster 3 countries 
#(sub-cluster 3.1 corresponds to cluster 3 in the current mediation result)
#(sub-cluster 3.2 corresponds to cluster 2 in the current mediation result)
ClusterRes.Cluster3Countries<-ClusterRes.Mediation.6clus %>%
  filter((ClusMembership==3)|(ClusMembership==2)) %>%
  mutate(ClusMembership=case_when(
    ClusMembership == 3 ~ "3.1",
    ClusMembership == 2 ~ "3.2"
  ))

#climate change belief to climate policy support
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

Mediation.CCBelief_regPar<-Mediation.FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

Cluster3Country.CCBelief_regPar<-merge(Mediation.CCBelief_regPar, ClusterRes.Cluster3Countries, 
                                       by.x = "group", by.y = "group")

Cluster3Country.CCBelief_regPar$country <- fct_reorder(Cluster3Country.CCBelief_regPar$country, 
                                                       Cluster3Country.CCBelief_regPar$ClusMembership)

cluster_colors <- c("3.1" = "#5E3C99",
                    "3.2" = "#7FA6D6")

ggplot(Cluster3Country.CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.435, color="red", linetype="dashed")+
  scale_color_manual(values = cluster_colors)+
  labs(title = "Effect of Climate Change Belief on Climate Policy Support for Cluster-3 Countries",
       color="sub-cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
#3D plot: direct effect of HV on climate policy support
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

HVDirect.param.Mediation<-Mediation.FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

HVDirect.param.Mediation.wide<-HVDirect.param.Mediation %>%
  select(rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

HVDirect.param.wide.Cluster3Countries<-merge(HVDirect.param.Mediation.wide, ClusterRes.Cluster3Countries, 
                                             by.x = "group", by.y = "group")

cluster_colors <- c("3.1" = "#5E3C99",
                    "3.2" = "#7FA6D6")

Mediation_Cluster3Countries_direct_3D<-plot_ly(HVDirect.param.wide.Cluster3Countries, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                               colors = cluster_colors,
                                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - Cluster-3 Countries - direct effect of human values",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_Cluster3Countries_direct_3D), 
                        file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/Mediation_Cluster3Countries_directHV_3D.html")

#
###3D plot for indirect effects of human values on climate policy support via climate change belief:
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

HVIndirect.par.Mediation<-Mediation.FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par.Mediation$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par.Mediation$lhs))

HVIndirect.par.Mediation<-HVIndirect.par.Mediation %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par.Mediation$human_values <- gsub("_g.*", "", HVIndirect.par.Mediation$lhs)

HVIndirect.par.wide.Mediation<-HVIndirect.par.Mediation %>%
  select(human_values, group, est) %>%
  pivot_wider(names_from = human_values, values_from = est)

HVIndirect.par.wide.Cluster3Countries<-merge(HVIndirect.par.wide.Mediation, ClusterRes.Cluster3Countries, 
                                             by.x = "group", by.y = "group")

cluster_colors <- c("3.1" = "#5E3C99",
                    "3.2" = "#7FA6D6")

Mediation_Cluster3Countries_indirect_3D<-plot_ly(HVIndirect.par.wide.Cluster3Countries, x= ~STindirect, y= ~ConIndirect, z= ~SEindirect, text= ~country, color = ~factor(ClusMembership),
                                                 colors = cluster_colors,
                                                 type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - Cluster-3 Countries - indirect effect of human values via climate change belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_Cluster3Countries_indirect_3D), 
                        file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/Mediation_Cluster3Countries_indirectHV_3D.html")


####------------------------------------------------------------------------------
##Visualize the separation between sub-cluster 4.1 and sub-cluster 4.2
#
##subset the Cluster 4 countries 
#(sub-cluster 4.1 corresponds to cluster 1 in the current mediation result)
#(sub-cluster 4.2 corresponds to cluster 6 in the current mediation result)
ClusterRes.Cluster4Countries<-ClusterRes.Mediation.6clus %>%
  filter((ClusMembership==1)|(ClusMembership==6)) %>%
  mutate(ClusMembership=case_when(
    ClusMembership == 1 ~ "4.1",
    ClusMembership == 6 ~ "4.2"
  ))

#climate change belief to climate policy support
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

Mediation.CCBelief_regPar<-Mediation.FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

Cluster4Country.CCBelief_regPar<-merge(Mediation.CCBelief_regPar, ClusterRes.Cluster4Countries, 
                                       by.x = "group", by.y = "group")

Cluster4Country.CCBelief_regPar$country <- fct_reorder(Cluster4Country.CCBelief_regPar$country, 
                                                       Cluster4Country.CCBelief_regPar$ClusMembership)

cluster_colors <- c("4.1" = "#6E6E6E",
                    "4.2" = "#E78AC3")

ggplot(Cluster4Country.CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  #geom_vline(xintercept = 0.435, color="red", linetype="dashed")+
  scale_color_manual(values = cluster_colors)+
  labs(title = "Effect of Climate Change Belief on Climate Policy Support for Cluster-4 Countries",
       color="sub-cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
#3D plot: direct effect of HV on climate policy support
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

HVDirect.param.Mediation<-Mediation.FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

HVDirect.param.Mediation.wide<-HVDirect.param.Mediation %>%
  select(rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

HVDirect.param.wide.Cluster4Countries<-merge(HVDirect.param.Mediation.wide, ClusterRes.Cluster4Countries, 
                                             by.x = "group", by.y = "group")

cluster_colors <- c("4.1" = "#6E6E6E",
                    "4.2" = "#E78AC3")

Mediation_Cluster4Countries_direct_3D<-plot_ly(HVDirect.param.wide.Cluster4Countries, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                               colors = cluster_colors,
                                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - Cluster-4 Countries - direct effect of human values",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_Cluster4Countries_direct_3D), 
                        file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/Mediation_Cluster4Countries_directHV_3D.html")

#
###3D plot for indirect effects of human values on climate policy support via climate change belief:
Mediation.FreeSAMparam<-parameterEstimates(Mediation.lavaanFreeSAM.fit)

HVIndirect.par.Mediation<-Mediation.FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par.Mediation$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par.Mediation$lhs))

HVIndirect.par.Mediation<-HVIndirect.par.Mediation %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par.Mediation$human_values <- gsub("_g.*", "", HVIndirect.par.Mediation$lhs)

HVIndirect.par.wide.Mediation<-HVIndirect.par.Mediation %>%
  select(human_values, group, est) %>%
  pivot_wider(names_from = human_values, values_from = est)

HVIndirect.par.wide.Cluster4Countries<-merge(HVIndirect.par.wide.Mediation, ClusterRes.Cluster4Countries, 
                                             by.x = "group", by.y = "group")

cluster_colors <- c("4.1" = "#6E6E6E",
                    "4.2" = "#E78AC3")

Mediation_Cluster4Countries_indirect_3D<-plot_ly(HVIndirect.par.wide.Cluster4Countries, x= ~STindirect, y= ~ConIndirect, z= ~SEindirect, text= ~country, color = ~factor(ClusMembership),
                                                 colors = cluster_colors,
                                                 type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - Cluster-4 Countries - indirect effect of human values via climate change belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_Cluster4Countries_indirect_3D), 
                        file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/Mediation_Cluster4Countries_indirectHV_3D.html")
