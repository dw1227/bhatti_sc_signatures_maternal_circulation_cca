# name: assign_samples_cca_filter_ptb_control.R
#
# author: Gaurav Bhatti
#
# input: data/sample_annotation/cfrna387/cfRNA_387_Transcriptomics_ano.csv      
#        data/sample_annotation/crnadream/DREAM_Transcriptomics_ano.csv
#       
# output:data/sample_annotation/cfrna387/ano_cfrna_cca.csv
#        data/sample_annotation/crnadream/ano_crna_dream.csv

library(tidyverse)
library(here)


myf0=function(x){ifelse(length(x[!is.na(x)])==0,NA,sum(abs(x[!is.na(x)])))}


# cfRNA
ano_387<-read_csv("data/sample_annotation/cfrna387/cfRNA_387_Transcriptomics_ano.csv")

ano_387$cca<-ifelse(apply(ano_387[, c("PH_G1_ST11",
                                                 "PH_G2_ST12",
                                                 "PH_G3_ST21",
                                                 "PH_G4_ST22",
                                                 "PH_G5_CPI")],1,myf0)>=1,1,0)


write_csv(ano_387,file="data/sample_annotation/cfrna387/cfRNA_387_Transcriptomics_ano.csv")


ano_387<-ano_387 |> 
  filter(Group %in% c("Labor","NoLabor","PTL","PPROM")) |> 
  mutate(Group2= if_else(Group %in% c("Labor","NoLabor"),"control","ptb"))

table(ano_387$cca,ano_387$Group2)


write_csv(ano_387,file="data/sample_annotation/cfrna387/ano_cfrna_cca.csv")

# cRNA
ano_crna_dream<-read_csv("data/sample_annotation/crnadream/DREAM_Transcriptomics_ano.csv")

ano_crna_dream$cca<-ifelse(apply(ano_crna_dream[, c("PH_G1_ST11",
                                      "PH_G2_ST12",
                                      "PH_G3_ST21",
                                      "PH_G4_ST22",
                                      "PH_G5_CPI")],1,myf0)>=1,1,0) 
write_csv(ano_crna_dream,file="data/sample_annotation/crnadream/DREAM_Transcriptomics_ano.csv")



ano_crna_dream<-ano_crna_dream |> 
  mutate(Group2= if_else(Group %in% c("Control"),"control","ptb"))

table(ano_crna_dream$cca,ano_crna_dream$Group2)


write_csv(ano_crna_dream,file="data/sample_annotation/crnadream/ano_crna_dream.csv")
