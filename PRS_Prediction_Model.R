rm(list=ls(all.names = T))
library(data.table)
system('ls')
##read the data 
df<-read.csv("UKB_data.csv")
head(df)
df_org<-subset(df,select = c(BMI, CigarettesPerDay, DrinksPerWeek,MDD,EA,
                             PRS_BMI, PRS_CigarettesPerDay, PRS_DrinksPerWeek,PRS_MDD,PRS_EA,
                             sex_f31_0_0, age_at_recruitment_f21022_0_0,uk_biobank_assessment_centre_f54_0_0,
                             genotype_measurement_batch_f22000_0_0,genetic_principal_components_f22009_0_1,genetic_principal_components_f22009_0_2,
                             genetic_principal_components_f22009_0_3,genetic_principal_components_f22009_0_4,genetic_principal_components_f22009_0_5,
                             genetic_principal_components_f22009_0_6,genetic_principal_components_f22009_0_7,genetic_principal_components_f22009_0_8,
                             genetic_principal_components_f22009_0_9,genetic_principal_components_f22009_0_10,genetic_principal_components_f22009_0_11,
                             genetic_principal_components_f22009_0_12,genetic_principal_components_f22009_0_13,genetic_principal_components_f22009_0_14,
                             genetic_principal_components_f22009_0_15,genetic_principal_components_f22009_0_16,genetic_principal_components_f22009_0_17,
                             genetic_principal_components_f22009_0_18,genetic_principal_components_f22009_0_19,genetic_principal_components_f22009_0_20,
                             genetic_principal_components_f22009_0_21,genetic_principal_components_f22009_0_22,genetic_principal_components_f22009_0_23,
                             genetic_principal_components_f22009_0_24,genetic_principal_components_f22009_0_25,genetic_principal_components_f22009_0_26,
                             genetic_principal_components_f22009_0_27,genetic_principal_components_f22009_0_28,genetic_principal_components_f22009_0_29,
                             genetic_principal_components_f22009_0_30,genetic_principal_components_f22009_0_31,genetic_principal_components_f22009_0_32,
                             genetic_principal_components_f22009_0_33,genetic_principal_components_f22009_0_34,genetic_principal_components_f22009_0_35,
                             genetic_principal_components_f22009_0_36,genetic_principal_components_f22009_0_37,genetic_principal_components_f22009_0_38,
                             genetic_principal_components_f22009_0_39,genetic_principal_components_f22009_0_40))

pheno<-'BMI' 
##select sepecific phenotype and PRS
df<-subset(df_org,select = c(get(pheno),get(paste("PRS",pheno,sep='_')), sex_f31_0_0, age_at_recruitment_f21022_0_0,uk_biobank_assessment_centre_f54_0_0,
                                            genotype_measurement_batch_f22000_0_0,genetic_principal_components_f22009_0_1,genetic_principal_components_f22009_0_2,
                                            genetic_principal_components_f22009_0_3,genetic_principal_components_f22009_0_4,genetic_principal_components_f22009_0_5,
                                            genetic_principal_components_f22009_0_6,genetic_principal_components_f22009_0_7,genetic_principal_components_f22009_0_8,
                                            genetic_principal_components_f22009_0_9,genetic_principal_components_f22009_0_10,genetic_principal_components_f22009_0_11,
                                            genetic_principal_components_f22009_0_12,genetic_principal_components_f22009_0_13,genetic_principal_components_f22009_0_14,
                                            genetic_principal_components_f22009_0_15,genetic_principal_components_f22009_0_16,genetic_principal_components_f22009_0_17,
                                            genetic_principal_components_f22009_0_18,genetic_principal_components_f22009_0_19,genetic_principal_components_f22009_0_20,
                                            genetic_principal_components_f22009_0_21,genetic_principal_components_f22009_0_22,genetic_principal_components_f22009_0_23,
                                            genetic_principal_components_f22009_0_24,genetic_principal_components_f22009_0_25,genetic_principal_components_f22009_0_26,
                                            genetic_principal_components_f22009_0_27,genetic_principal_components_f22009_0_28,genetic_principal_components_f22009_0_29,
                                            genetic_principal_components_f22009_0_30,genetic_principal_components_f22009_0_31,genetic_principal_components_f22009_0_32,
                                            genetic_principal_components_f22009_0_33,genetic_principal_components_f22009_0_34,genetic_principal_components_f22009_0_35,
                                            genetic_principal_components_f22009_0_36,genetic_principal_components_f22009_0_37,genetic_principal_components_f22009_0_38,
                                            genetic_principal_components_f22009_0_39,genetic_principal_components_f22009_0_40))
df<-df[complete.cases(df),]
summary(lm(get(pheno)~get(paste("PRS",pheno,sep='_')),df))
              
dim(df)
#  accumulative inclusion of PC1-PC40
pc1<-colnames(df)[7]
pc2<-colnames(df)[7:8]
pc3<-colnames(df)[7:9]
pc4<-colnames(df)[7:10]
pc5<-colnames(df)[7:11]
pc6<-colnames(df)[7:12]
pc7<-colnames(df)[7:13]
pc8<-colnames(df)[7:14]
pc9<-colnames(df)[7:15]
pc10<-colnames(df)[7:16]
pc11<-colnames(df)[7:17]
pc12<-colnames(df)[7:18]
pc13<-colnames(df)[7:19]
pc14<-colnames(df)[7:20]
pc15<-colnames(df)[7:21]
pc16<-colnames(df)[7:22]
pc17<-colnames(df)[7:23]
pc18<-colnames(df)[7:24]
pc19<-colnames(df)[7:25]
pc20<-colnames(df)[7:26]
pc21<-colnames(df)[7:27]
pc22<-colnames(df)[7:28]
pc23<-colnames(df)[7:29]
pc24<-colnames(df)[7:30]
pc25<-colnames(df)[7:31]
pc26<-colnames(df)[7:32]
pc27<-colnames(df)[7:33]
pc28<-colnames(df)[7:34]
pc29<-colnames(df)[7:35]
pc30<-colnames(df)[7:36]
pc31<-colnames(df)[7:37]
pc32<-colnames(df)[7:38]
pc33<-colnames(df)[7:39]
pc34<-colnames(df)[7:40]
pc35<-colnames(df)[7:41]
pc36<-colnames(df)[7:42]
pc37<-colnames(df)[7:43]
pc38<-colnames(df)[7:44]
pc39<-colnames(df)[7:45]
pc40<-colnames(df)[7:46]

# creat a result matrix for unadjusted model and adjusted model without PCs.
res_nopc<-data.frame(cov1=rep(NA,16),cov2=NA,cov3=NA, cov4=NA,pc=0, ncov=NA, beta=NA,SD=NA,P=NA,r2=NA,AIC=NA, BIC=NA,modelSS=NA,  modelP=NA )
res_nopc$ncov<-c(0, rep(1,4),rep(2,6),rep(3,4),4) 
head(res_nopc)
res_nopc$cov1[2]<-colnames(df)[c(3)]
res_nopc$cov1[3]<-colnames(df)[c(4)]
res_nopc$cov1[4]<-colnames(df)[c(5)]
res_nopc$cov1[5]<-colnames(df)[c(6)]

res_nopc[6,1:2]<-colnames(df)[c(3,4)]
res_nopc[7,1:2]<-colnames(df)[c(3,5)]
res_nopc[8,1:2]<-colnames(df)[c(3,6)]
res_nopc[9,1:2]<-colnames(df)[c(4,5)]
res_nopc[10,1:2]<-colnames(df)[c(4,6)]
res_nopc[11,1:2]<-colnames(df)[c(5,6)]

res_nopc[12,1:3]<-colnames(df)[c(3,4,5)]
res_nopc[13,1:3]<-colnames(df)[c(3,4,6)]
res_nopc[14,1:3]<-colnames(df)[c(3,5,6)]
res_nopc[15,1:3]<-colnames(df)[c(4,5,6)]
res_nopc[16,1:4]<-colnames(df)[c(3,4,5,6)]
summary(df$uk_biobank_assessment_centre_f54_0_0)

## for accessment centre and measurement bacth, define them as factors
df$uk_biobank_assessment_centre_f54_0_0<-factor(df$uk_biobank_assessment_centre_f54_0_0)
table(df$genotype_measurement_batch_f22000_0_0)
df$genotype_measurement_batch_f22000_0_0<-factor(df$genotype_measurement_batch_f22000_0_0)

###Conduct the prediction models and extract the statistics parameters 
for (i in 1:16){
rm(vars)
vars<-as.character(c(colnames(df)[1:2],as.character(res_nopc[i,1:4])))
  vars<-subset(vars,!is.na(vars))
  vars
  df_tmp<-subset(df,select = vars)
  head(df_tmp)
  modelA<-summary(lm(df_tmp))
  modelA$coefficients[2,4]
  modelmore<-lm(df_tmp)
  modelbase<-lm(df_tmp[,1:2])
  res_nopc$AIC[i]<- AIC(modelmore)
  res_nopc$BIC[i]<-BIC(modelmore)
  modelcompare<- anova(modelbase,modelmore)
  res_nopc$modelSS[i]<- modelcompare$`Sum of Sq`[2]
  res_nopc$modelF[i]<- modelcompare$F[2]
  res_nopc$modelP[i]<-modelcompare$`Pr(>F)`[2]
  res_nopc$beta[i]<-modelA$coefficients[2,1]
  res_nopc$SD[i]<-modelA$coefficients[2,2]
  res_nopc$P[i]<-modelA$coefficients[2,4]
  res_nopc$r2[i]<-modelA$adj.r.squared
  modelA$coefficients[2,4]
}
res_nopc

# creat a result matrix for adjusted  model with PCs.
res_pc<-data.frame(cov1=rep(res_nopc$cov1,40),cov2=rep(res_nopc$cov2,40),cov3=rep(res_nopc$cov3,40), cov4=rep(res_nopc$cov4,40),pc=NA, ncov=rep(res_nopc$ncov,40), beta=NA,SD=NA,P=NA,r2=NA,AIC=NA, BIC=NA,modelSS=NA, modelF=NA, modelP=NA )
res_pc$pc<-rep(paste('pc',1:40,sep=''),16)
res_pc$pc<-res_pc$pc[order(res_pc$pc)]
head(res_pc)
res_pc$npc<-as.numeric(gsub('pc','',res_pc$pc))
res_pc$ncov<- res_pc$ncov+ res_pc$npc

for (i in 1:nrow(res_pc)){
  vars<-c(colnames(df)[1:2],as.character(res_pc[i,1:4]),get(res_pc$pc[i]))
  vars<-factor(vars,levels = vars)
  vars<-subset(vars,!is.na(vars))
  df_tmp<-subset(df, select =vars)
  head(df_tmp)
  modelA<-summary(lm(df_tmp))
  modelmore<-lm(df_tmp)
  modelbase<-lm(df_tmp[,1:(ncol(df_tmp)-1)])
  res_pc$AIC[i]<- AIC(modelmore)
  res_pc$BIC[i]<-BIC(modelmore)
  modelcompare<- anova(modelbase,modelmore)
  res_pc$modelSS[i]<- modelcompare$`Sum of Sq`[2]
  res_pc$modelF[i]<- modelcompare$F[2]
  res_pc$modelP[i]<-modelcompare$`Pr(>F)`[2]
  res_pc$beta[i]<-modelA$coefficients[2,1]
  res_pc$SD[i]<-modelA$coefficients[2,2]
  res_pc$P[i]<-modelA$coefficients[2,4]
  res_pc$r2[i]<-modelA$adj.r.squared
}
res_pc<-subset(res_pc, select = -c(npc))

# combine two results matrix into one matrix 
res<-rbind(res_nopc,res_pc)

res$logp<- -log10(res$P)
res$ModelNumber<-1:nrow(res)
system('ls')
summary(res$logp)

decile<- quantile(res$logp, probs = seq(.1, .9, by = .1))[9]
res$logp_decile<-ifelse(res$logp>decile, 1, 0)
decile<- quantile(res$beta, probs = seq(.1, .9, by = .1))[9]
res$beta_decile<-ifelse(res$beta>decile, 1, 0)
writexl::write_xlsx(res,paste(pheno_name,'_all_model_15Nov_2022.xlsx',sep=''))
