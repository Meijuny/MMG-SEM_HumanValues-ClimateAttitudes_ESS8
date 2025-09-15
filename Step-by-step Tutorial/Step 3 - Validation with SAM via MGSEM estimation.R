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

####----------------------------------------------------------------------------------------------------
#first, specify all the measurement models and structural model inside the same lavaan syntax
HV.CliPolSup.FreeSAM.model<-'
#measurement model for human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1
C5~~C6

#measurement model for climate policy support
CCPolicySupport=~support2+support1+support3

#structural model
CCPolicySupport~SelfTran+Conser+SelfEnhan
'


#then use sam() to estimate the regression coefficient for each country
CCPolSupport.lavaanFreeSAM.PM.Mark2<-sam(model = HV.CliPolSup.FreeSAM.model,
                                   data = ESS8,
                                   mm.list = list(c("SelfTran","Conser","SelfEnhan"),"CCPolicySupport"), ##to separate the measurement block
                                   cmd = "sem",
                                   group="country",
                                   sam.method = "local", ##local SEM where we use factor covariance matrix from step 1 to estimate the structural model in step 2
                                   group.equal="loadings", #metric invariance
                                   group.partial=c("SelfEnhan=~SE3", "CCPolicySupport=~support3"), #non-invariant items from the results of Step 1 - Measurement Model
                                   missing="FIML",  #handle missing values
                                   bounds="wide" ##since CCPolicySupport measurement block needs the wide bounded estimation before
)

sink("C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/BasicModel_lavaanSAM.txt")
summary(CCPolSupport.lavaanFreeSAM.PM.Mark2, standardized=T)
sink()
####----------------------------------------------------------------------------------------------------


####---------------------------------------------------------------------------------------------------
#4 clusters
##plot the regression coefficients of each group and separate the cluster using color-coded
FreeSAMparam<-parameterEstimates(CCPolSupport.lavaanFreeSAM.PM.Mark2)
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
         scene = list(
           xaxis = list(title = list(text = "Self-Transcendence", font = list(size = 18))),
           yaxis = list(title = list(text = "Conservation", font = list(size = 18))),
           zaxis = list(title = list(text = "Self-Enhancement", font = list(size = 18)))
         ))


htmlwidgets::saveWidget(CCPolSupport_4clus_3D_PMMark2, 
                        file = "C:/Users/U0172378/OneDrive - KU Leuven/Desktop/Meijun - PhD/R/MMGSEM_ESS_HumanValues-AntiImmigrant/Sink Output/BasicModel3D.html", 
                        selfcontained = T)
