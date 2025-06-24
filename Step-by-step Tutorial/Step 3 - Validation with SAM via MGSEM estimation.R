library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
library(mmgsem)



############################################################################################
############## SAM with MGSEM estimation to validate the clusters ##########################
############################################################################################

##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from climate policy support
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
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
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

CCPolicySupport=~support2+support1+support3
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
##compute the step-1 factor covariance matrix for each group that will be used for the second step to estimate the regression coefficients of each group:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.PM.Mark2<-cfa(model = CCPolSupport_FREEsam_str_model,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))



##plot the regression coefficients of each group and separate the cluster using color-coded
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

cluster_colors <- c("1" = "#66C2A5",  # Soft Blue
                    "2" = "#FC8D62",  # Warm Coral
                    "3" = "#8DA0CB",  # Mint Green
                    "4" = "#E78AC3")

CCPolSupport_4clus_3D_PMMark2<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                       colors = cluster_colors,
                                       type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 4 clusters - Human Values on Climate Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(CCPolSupport_4clus_3D_PMMark2, "BasicModel3D.html", selfcontained = T)


########Alternative mapping for the paper:
#
#extract the group-specific parameter from free SAM:
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)
#
#merge with the cluster results:
FreeSAM_reg_param_4clus<-merge(ClusterRes.4clus, FreeSAM_reg_param,
                               by.x = "group",
                               by.y = "group")
#
#for the dimension of self-transcendence value and conservation value:
Convex_hull1<-FreeSAM_reg_param_4clus %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(FreeSAM_reg_param_4clus, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Self-transcendence and Conservation Dimension in 4 clusters",
       color="cluster", fill="cluster")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull1, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)+
  guides(color=guide_legend(override.aes = list(fill=NA)))
#
#for the dimension of conservation and self-enhancement value:
Convex_hull2<-FreeSAM_reg_param_4clus %>%
  group_by(ClusMembership) %>%
  slice(chull(Conser,SelfEnhan))

ggplot(FreeSAM_reg_param_4clus, aes(x=Conser, y=SelfEnhan, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Self-transcendence and Conservation Dimension in 4 clusters",
       color="cluster", fill="cluster")+
  xlab("Conservation Value")+ylab("Self-Enhancement Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull2, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)+
  guides(color=guide_legend(override.aes = list(fill=NA)))


##################################################
############## optional ##########################
##################################################

sam_CCPolSupport_4clus_PMMark2<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a4,a3,a3,a3,a3,a2,a4,a4,a4,a3,a1,a3,a3,a4,a3,a4,a3,a4)*SelfTran+
                c(b3,b3,b3,b3,b3,b4,b3,b3,b3,b3,b2,b4,b4,b4,b3,b1,b3,b3,b4,b3,b4,b3,b4)*Conser+
                c(c3,c3,c3,c3,c3,c4,c3,c3,c3,c3,c2,c4,c4,c4,c3,c1,c3,c3,c4,c3,c4,c3,c4)*SelfEnhan
'

CCPolSupport.SAM.4clus.PMMark2<-cfa(model = sam_CCPolSupport_4clus_PMMark2,
                                    sample.cov = Var_eta,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/Conference/EAM 2025/outputs for SEM lab - July 4/BasicModel_4clus_ConstrainEst.txt")
summary(CCPolSupport.SAM.4clus.PMMark2, fit.measures=T, standardized=T)
sink()