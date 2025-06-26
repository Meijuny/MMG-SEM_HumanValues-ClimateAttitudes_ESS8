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
#Re-run the three measurement blocks needed for the mediation model
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


####------------------------------------------------------------------------
#Model selection
##specify the structural model:
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

MediationModel.Selection.PM.MarkSup2<-ModelSelection(dat=ESS8,
                                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                                     S2 = Str_model,
                                                     group = "country",
                                                     clusters=c(1,8),
                                                     seed = 100,
                                                     userStart = NULL,
                                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                     max_it = 10000L,
                                                     nstarts = 300L,
                                                     printing = FALSE,
                                                     partition = "hard",
                                                     endogenous_cov = TRUE,
                                                     endo_group_specific = TRUE,
                                                     sam_method = "local",
                                                     meanstr = FALSE,
                                                     rescaling = F,
                                                     missing="FIML")
View(MediationModel.Selection.PM.MarkSup2$Overview)

##set up the cavans for CHull plot to be at the left and BIC_G plot to be at the right:
par(mfrow = c(2, 1))

#CHull plot
hull_indices <- chull(MediationModel.Selection.PM.MarkSup2$Overview$nrpar, MediationModel.Selection.PM.MarkSup2$Overview$LL)
plot(MediationModel.Selection.PM.MarkSup2$Overview$nrpar, MediationModel.Selection.PM.MarkSup2$Overview$LL, 
     pch=4, col="black", main="CHull plot",xlab="Number of free parameters", ylab="Loglikelihood")
lines(MediationModel.Selection.PM.MarkSup2$Overview$nrpar[hull_indices], 
      MediationModel.Selection.PM.MarkSup2$Overview$LL[hull_indices], col="black", lwd=1)
points(MediationModel.Selection.PM.MarkSup2$Overview$nrpar[hull_indices], MediationModel.Selection.PM.MarkSup2$Overview$LL[hull_indices], pch=16, col="black")

##BIC_G:
plot(MediationModel.Selection.PM.MarkSup2$Overview$Clusters, MediationModel.Selection.PM.MarkSup2$Overview$BIC_G, 
     pch=4, col="black", main="BIC_G plot",xlab="Number of clusters", ylab="BIC_G")
lines(MediationModel.Selection.PM.MarkSup2$Overview$Clusters, 
      MediationModel.Selection.PM.MarkSup2$Overview$BIC_G, col="black", lwd=1)

##reset the canvas back to default:
par(mfrow = c(1, 1))

##select 2 and 6 clusters

####----------------------------------------------------------------------------------------------------
##MMGSEM for the 2 full metric CCPolSupport options:

##specify structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

Mediation.2clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=2,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 50L,
                                    printing = F,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")

##inspect the posterior membership probability:
round(Mediation.2clus.PM.MarkSup2$posteriors[,1:2], digits = 5)

##inspect the regression coefficients of each cluster:
Mediation.2clus.PM.MarkSup2$param$beta_ks


##cluster membership:
clustering.2clus<-t(apply(Mediation.2clus.PM.MarkSup2$posteriors[,1:2],1,function(x) as.numeric(x==max(x))))
clustering.2clus[,2]<-ifelse(clustering.2clus[,2]==1,2,0)
ClusMembership.2clus<-apply(clustering.2clus,1,function(x) sum(x))
ClusterRes.2clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.2clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.2clus<-merge(ClusterRes.2clus, countries,
                        by.x = "group", by.y = "group")


####----------------------------------------------------------------------------------------------------
##Validation for 2 clusters:
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##extract the loadings and residual variances from CCPolSupport
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]),colnames(lambda_CCBelief_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]),rownames(lambda_CCBelief_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]),colnames(theta_CCBelief_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]),rownames(theta_CCBelief_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}

#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
Mediation_FREEsam_str_model_PM_MarkSup2<-'
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

Mediation.FreeSAM.PM.MarkSup2<-cfa(model = Mediation_FREEsam_str_model_PM_MarkSup2,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))



##Plotting the 2-cluster solution for validations:
#
#climate change belief to climate policy support
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)

CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.2clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62")

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.375, color="red", linetype="dashed")+
  scale_color_manual(values = cluster_colors)+
  labs(title = "Effect of Climate Change Belief on Climate Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()
#
#3D plot: direct effect of HV on climate policy support
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)

HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

HVDirect.param.wide<-HVDirect.param %>%
  select(rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

HVDirect.param.wide<-merge(HVDirect.param.wide, ClusterRes.2clus, 
                           by.x = "group", by.y = "group")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62")

Mediation_2clus_direct_3D<-plot_ly(HVDirect.param.wide, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                   colors = cluster_colors,
                                   type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - 2 clusters - direct effect of human values",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_2clus_direct_3D), "Mediation_2clus_directHV_3D.html")

#
###3D plot for indirect effects of human values on climate policy support via climate change belief:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)

HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par.wide<-HVIndirect.par %>%
  select(human_values, group, est) %>%
  pivot_wider(names_from = human_values, values_from = est)

HVIndirect.par.wide<-merge(HVIndirect.par.wide, ClusterRes.2clus, 
                           by.x = "group", by.y = "group")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62")

Mediation_2clus_indirect_3D<-plot_ly(HVIndirect.par.wide, x= ~STindirect, y= ~ConIndirect, z= ~SEindirect, text= ~country, color = ~factor(ClusMembership),
                                     colors = cluster_colors,
                                     type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM Mediation Model - 2 clusters - indirect effect of human values via climate change belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(Mediation_2clus_indirect_3D), "Mediation_2clus_indirectHV_3D.html")



####---------------------------------------------------------------------------------------
##geographical pattern:
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
ClusterRes.2clus<-ClusterRes.2clus %>%
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
map_with_2clusters <- eu_map %>%
  left_join(ClusterRes.2clus, by = "region")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62")

##lay out on the map:
ggplot(map_with_2clusters, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  scale_fill_manual(values = cluster_colors)+
  labs(
    title = "",
    fill = "Cluster"
  ) +
  theme_minimal()

###----------------------------------------------------------------------------------------
##optional for the SEM lab meeting July 4:
sam_mediation_2clus_PM_MarkSup2<-'
CCBelief~c(a2,a1,a1,a2,a1,a2,a1,a1,a1,a1,a2,a2,a1,a1,a1,a1,a1,a1,a2,a1,a2,a1,a2)*SelfTran+
          c(b2,b1,b1,b2,b1,b2,b1,b1,b1,b1,b2,b2,b1,b1,b1,b1,b1,b1,b2,b1,b2,b1,b2)*Conser+
          c(c2,c1,c1,c2,c1,c2,c1,c1,c1,c1,c2,c2,c1,c1,c1,c1,c1,c1,c2,c1,c2,c1,c2)*SelfEnhan

CCPolicySupport~c(d2,d1,d1,d2,d1,d2,d1,d1,d1,d1,d2,d2,d1,d1,d1,d1,d1,d1,d2,d1,d2,d1,d2)*CCBelief+
                c(e2,e1,e1,e2,e1,e2,e1,e1,e1,e1,e2,e2,e1,e1,e1,e1,e1,e1,e2,e1,e2,e1,e2)*SelfTran+
                c(f2,f1,f1,f2,f1,f2,f1,f1,f1,f1,f2,f2,f1,f1,f1,f1,f1,f1,f2,f1,f2,f1,f2)*Conser+
                c(g2,g1,g1,g2,g1,g2,g1,g1,g1,g1,g2,g2,g1,g1,g1,g1,g1,g1,g2,g1,g2,g1,g2)*SelfEnhan
'

Mediation.SAM.2clus.PM.MarkSup2<-cfa(model = sam_mediation_2clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/Conference/EAM 2025/outputs for SEM lab - July 4/Mediation_SAM_2clus_PM_MarkSup2.txt")
summary(Mediation.SAM.2clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()


####---------------------------------------------------------------------------------------
##country level indicators with GDP per capita

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
ClusterRes.2clus<-ClusterRes.2clus %>%
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
GDPPerCapita_2clusters<-merge(ClusterRes.2clus, GDPperCapita,
                              by.x = "Country.Name",
                              by.y="Country.Name")

##make a new column about the characteritics of the CCBelief translating into CCPolSupport
cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62")

ggplot(GDPPerCapita_2clusters, aes(x=factor(ClusMembership), y=X2016, fill=factor(ClusMembership)))+
  geom_boxplot()+
  scale_fill_manual(values = cluster_colors)+
  xlab("cluster")+ylab("GDP per Capita of 2016")+
  labs(title = "GDP per Capita by Clusters", fill="cluster")+
  theme_bw()



###-----------------------------------------------------------------------------
##MMG-SEM 6 clusters:
##specify structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

##6 clusters:
Mediation.6clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=6,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 50L,
                                    printing = F,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")

##check the posterior membership probabilities:
round(Mediation.6clus.PM.MarkSup2$posteriors[,1:6], digits = 5)

##check the regression coefficients of each cluster:
Mediation.6clus.PM.MarkSup2$param$beta_ks
