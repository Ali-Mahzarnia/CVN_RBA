library(readxl)
library(dplyr)
library(magrittr)
library(xlsx)
library(ggplot2)
library(ggpubr)





filter_score_path = '/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/master_sheet_cvn2.xlsx'
scores = read_xlsx(filter_score_path) %>% select(`SAMBA Brunno` , score_CBF, cbf  )
scores_removal = scores[which(scores$score_CBF<4 | is.na(scores$score_CBF)),]


path_vol="/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/individual_label_statistics/"
file_list=list.files(path_vol)


temp=read.delim( paste0(path_vol,file_list[1]) )
len=length(temp$CBF_mean)
volumes=matrix(NA,length(file_list),333)
noreadcsf=c(148,152,161,314,318,327)



Atlas_path = '/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/CHASSSYMM3AtlasLegends.xlsx'
Atlas = read.xlsx(Atlas_path, sheetIndex = 1)


for (i in 1:length(file_list)) {
  #print(file)
  temp=read.delim( paste0(path_vol,file_list[i]) , header = T)
  len=length(temp$CBF_mean)
  # temp$CBF_mean
  #print(sum(temp$CBF_mean))
  # print( sum(temp$CBF_mean[2:len]))
  #if (sum(temp$CBF_mean[2:len])==0) {volumes[i,2:len] = 0}
  #else 
  #temp = temp[2:len,, drop =T]
  #rownames(temp) = as.numeric(rownames(temp))-1
  ind= which(temp$ROI>166)
  #temp$ROI[ind]
  temp_index = temp$ROI[ind] 
  temp$ROI[ind] = temp_index-1000+166
  missings= setdiff(seq(1,332),temp$ROI)
  if (length(missings)>0){
  for (j in missings) {
    temp=tibble::add_row(temp, ROI = j )
  }
  
  temp = temp[order(temp$ROI,decreasing = FALSE), ]
  temp[missings,]$CBF_mean =0 
  temp[missings,]$structure = Atlas$Structure[Atlas$index%in%missings] 
  
  }
  
  temp[is.na(temp)] = 0
  
  volumes[i,2:333]=as.numeric(temp$CBF_mean[1:332])#/ sum(temp$CBF_mean[1:332])
  
  index_CBF_glob = which(  substr( file_list[i] , 1 , 6)  ==  scores$`SAMBA Brunno` )
  if ( length(index_CBF_glob) > 0 ) {
    volumes[i,2:333]=as.numeric(temp$CBF_mean[1:332])/as.numeric(scores$cbf[index_CBF_glob]  )
  }
  else
  { volumes[i,2:333]=as.numeric(temp$CBF_mean[1:332])}#/ sum(temp$CBF_mean[1:332])}
  # print(sum(as.numeric(volumes[i,2:len])))
  # whole_volume[i,2]=sum(temp$CBF_mean[-noreadcsf])
  volumes[i,1]=substr( file_list[i] , 1, 6)
}

 
 
 volumes=as.data.frame(volumes);
 
 index_score_removal = which( volumes[,1] %in%unlist(scores_removal))
 volumes = volumes[-index_score_removal,] 
 
 # volumes$V1=as.numeric(substr(volumes$V1,2,9)) # make dwi numeric
 # volumes[,2:len]=as.numeric(volumes[,2:len])
# xlsx::write.xlsx2(volumes, "volume_list.xlsx" )
#  
# 

path_master='/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/MasterSheet_Experiments2021.xlsx'
data=read.xlsx(path_master, sheetName = 'CVN_20abb15' )
datatemp=data%>%dplyr::select(SAMBA.Brunno, treatment)#subselect

# 
# DWI = datatemp$DWI
# 
# for (k in 1:length(Animal.ID_temp)) {
#   j = Animal.ID_temp[k]
#   if (!is.na(j)) {
#     if ( grepl( "-",j  ) ){  j= gsub("-","_",j)}
#     if ( grepl( "_",j  ) ){   after_dash = sub(".*_", "", j)  }
#     j= gsub(":","",j)
#        if (as.numeric(after_dash)<9) { Animal.ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0("0",after_dash)) }
#       else { Animal.ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0(after_dash)) }
#   }
# 
# }
# datatemp$Animal.ID = Animal.ID_temp

# 
# datatemp$CIVMID = gsub('_','',datatemp$CIVMID)
# datatemp$CIVMID = gsub('-','',datatemp$CIVMID)

#nchar(datatemp[111,1])
# datatemp=na.omit(datatemp)
# datatemp[nchar(datatemp$DWI)==1,]=matrix(NA,1,dim(datatemp)[2])
# datatemp=na.omit(datatemp)


datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring
# datatemp$ARunno=as.numeric(substr(datatemp$ARunno,1,9)) # make dwi numeric
#datatemp=datatemp[datatemp$Treatment=="APOE22",]

len = sum(datatemp$SAMBA.Brunno%in%volumes$V1)
temp_bind = matrix(NA, len, (dim(datatemp)[2]+ dim(volumes)[2]) )

# 
# excercise_path= '/Users/ali/Desktop/Aug23/primary_rba_excercise/Mice_Inventory_Proteomic.xlsx'
# excercise=read.xlsx(excercise_path, sheetName = 'ExerciseVsControl' )
# excercise_temp = excercise%>%dplyr::select(AnimalID,Exercise)#subselect
# excercise_temp = na.omit(excercise_temp)
# excercise_ID_temp = excercise_temp$AnimalID
# 
# for (k in 1:length(excercise_ID_temp)) {
#   j = excercise_ID_temp[k]
#   if (!is.na(j)) {
#     if ( grepl( "-",j  ) ){  j= gsub("-","_",j)}
#     if ( grepl( "_",j  ) ){   after_dash = sub(".*_", "", j)  }
#     if (as.numeric(after_dash)<9) { excercise_ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0("0",after_dash)) }
#     else { excercise_ID_temp[k]= paste0( sub("\\_.*", "", j) ,paste0(after_dash)) }
#   }
#   
# }
# excercise_temp$AnimalID = as.numeric(excercise_ID_temp )


for (j in 1:dim(temp_bind)[1]) {
  
  
  index_master = which(volumes[,1] == datatemp$SAMBA.Brunno[j])
  # index_exercise =  which(volumes[j,1] == excercise_temp$AnimalID)
  
  if (length(index_master) >0)
  {
    temp_row = cbind(volumes[index_master,], datatemp[j,] )
    temp_bind [j,] =  unlist(temp_row)

    
    
  }
  
  
}



# 
# 
# temp_bind=cbind(volumes[indeces_of_whole,],datatemp[indeces_of_master,])
temp_bind=as.data.frame(temp_bind)
temp_bind=na.omit(temp_bind)
len_temp_bind = dim(temp_bind)[2]
colnames(temp_bind)[(len_temp_bind-2+1):len_temp_bind] = c("ID", "Treatment")
# temp_bind$Age_Months = as.numeric(temp_bind$Age_Months)
# temp_bind=cbind(volumes[indeces_of_whole,],datatemp)

# temp_bind=temp_bind%>%mutate( age_cat=case_when(  Age_Months<median(Age_Months)~1 ,
                                                 # Age_Months>=median(Age_Months)~2            ), .after = "ID" )




pathnames='/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/mouse_anatomy.csv'
ROI=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
ROI=paste0(ROI$Bigpart, " ",ROI$ROI)
# 


library(emmeans)
library(effectsize)

len = dim(volumes)[2]
result=matrix(NA,1,(len-1))
for (i in 2:len) {
  var2=as.numeric(temp_bind[,i])
  lm <- lm( var2 ~  as.factor(Treatment) ,data=temp_bind )
  # lm <- lm( var2 ~ as.numeric(Age_Months)*as.factor(Sex),data=temp_bind )
  
  an=anova(lm)
  pvals=an$`Pr(>F)`
  eff=eta_squared(lm, partial = TRUE)
  
  
  result[1:(dim(an)[1]-1),(i-1)]=pvals[1:(dim(an)[1]-1)]
  # result[4:6,(i-1)]=eff$Eta2_partial
  
  
  #emmeans(lm, ~ Age_Months, contr="tukey") 
  # sd(temp_bind$V2[temp_bind$age_cat==1]) 
  # 
  # mean(temp_bind$V2[  temp_bind$age_cat==1  ])
  # median(temp_bind$V2[  temp_bind$age_cat==1  ])
  # mean(temp_bind$V2[  temp_bind$age_cat==2  ])
  # median(temp_bind$V2[  temp_bind$age_cat==2 ])
  # sd(temp_bind$Age_Months[temp_bind$age_cat==1])
  # sd(temp_bind$Age_Months[temp_bind$age_cat==2])
}

rownames(result)= rownames(an)[1:(dim(an)[1]-1)]

adjustresult=result
for (j in 1:1) {
  adjustresult[j,]=p.adjust(adjustresult[j,], method = "fdr")
  
}
adjustresult [is.na(adjustresult)] =1
sum(is.na(adjustresult))



# # 
#  sum(adjustresult[1,]<0.05) #age
#  sum(adjustresult[2,]<0.05) #sex
#  sum(adjustresult[3,]<0.05) #age*sex


pathnames='/Users/ali/Desktop/Aug23/CVN/rba/in_vivo/CBF_GLOBAL/mouse_anatomy.csv'
ROI=read.csv(pathnames, header = TRUE, sep = ",", quote = "")
ROI=paste0(ROI$Bigpart, " ",ROI$ROI)
  
  

colnames(adjustresult) = ROI
  
# write.xlsx2(adjustresult, "adjusted_pvalue_volumes.xlsx", sheetName = "All_Pvalues" )


# write.xlsx2(0, "volume.xlsx", sheetName = "0" )


for (j in 1:dim(adjustresult)[1]) {
  




var_index_sig=which(adjustresult[j,] <0.05)
var_index_sig=setdiff(var_index_sig, noreadcsf)
sig_result=adjustresult[j,var_index_sig]
# colnames(sig_result)=var_index_sig
# sig_result=sig_result[,order(sig_result[4,], decreasing = T)]

# 
# 
# table=matrix(NA, length(var_index_sig) ,12 )
# colnames(table_age) = c("Number" , "Index", "Name of the region" , "FDR corrected Pvalue", "Effect Size Eta^2", 
#                         "CI lower bound", "CI upper bound", "Mean group 1", "Mean group 2", 
#                         "SD group 1", "SD group 2", "F-value")
# for (i in 1:length(var_index_sig)) {
#   table[i,1]=i
#   index=as.numeric(colnames(sig_result)[i])
#   table_age[i,2] = index
#   table_age[i,3] = ROI[index]
#   
#   var2=as.numeric(temp_bind[,index+1])
#   lm <- lm( as.numeric(var2) ~Age_Months*as.factor(Sex)*as.factor(Treatment)* as.factor(Treatment),data=temp_bind )
#   
#   # pvals=an$`Pr(>F)`
#   temp=sig_result[,i]
#   eff=eta_squared(lm, partial = TRUE)
#   print=cbind(temp[1],t(unlist(eff[1,2:5]))  ) 
#   table[i,4:7] = print[-c(3)]
#   means=by(var2,as.factor(temp_bind$age_cat), mean )
#   table_age[i,8:9]=c(means[1], means[2])
#   sds=by(var2,as.factor(temp_bind$age_cat), sd )
#   table_age[i,10:11]=c(sds[1], sds[2])
#   an=anova(lm)
#   table_age[i,12]=an$`F value`[1]
# }

# print(paste0(rownames(adjustresult)[j]))
#name_of_region = gsub("as.factor","",(gsub(":","-",rownames(adjustresult)[j])))
sig_result_withindex = sig_result
sig_result_withindex = as.data.frame(sig_result_withindex)
sig_result_withindex$index = var_index_sig

sig_result_withindex = sig_result_withindex[order(as.numeric(sig_result_withindex$sig_result),decreasing = FALSE),]

#sig_result_withindex = sig_result_withindex[order(as.numeric(sig_result_withindex$sig_result),decreasing = FALSE),]


sig_result = as.data.frame(sig_result_withindex)%>%select(sig_result_withindex)

var_index_sig = sig_result_withindex$index
#write.xlsx2(sig_result , "adjusted_pvalue_volumes.xlsx", sheetName =  name_of_region , append=TRUE,  overwrite = TRUE )

}





##### make a table:
table = matrix( NA, dim(sig_result)[1] , 11)
colnames(table) = c("ROI" , "Mean (Se)" , "Mean (Tr)" , "Mean (Wh)", "SD (Se)" , "SD (Tr)" , "SD (Wh)" , "P value" , "Adjusted P value", "Eta Effect Size", "CI" )

for (j in 1:length(var_index_sig)) {
  
  vol_ind = var_index_sig[j]
  
  var = temp_bind[,(vol_ind+1)] 
  
  means= aggregate(as.numeric(var),list(temp_bind$Treatment), FUN=mean) 
  sds = aggregate(as.numeric(var),list(temp_bind$Treatment), FUN=sd) 
  lm <- lm( var ~  as.factor(Treatment) ,data=temp_bind )
  an=anova(lm)
  eff=eta_squared(lm, partial = TRUE)

  table[j,1] = unlist(rownames(sig_result)[j])
  table[j,2:4]  = means$x
  table[j,5:7] = sds$x
  table[j,8] = result[vol_ind]
  table[j,9] = unlist(sig_result[[1]])[j]
    table[j,10] = eff$Eta2
  table[j,11] =paste0("(", round(as.numeric(eff$CI_low),3), " , ",eff$CI_high, ")" )
  
  }




write.xlsx2(table, "table_CBF.xlsx", sheetName = "Significant" )





table = as.data.frame(table)
table$diff= as.numeric( table$`Mean (Tr)`)-as.numeric(table$`Mean (Se)`)
pos_ind_table = which(table$diff>0)



name_of_survivors_9 = table[pos_ind_table[1:9],1]
plot_list = vector(mode = "list", length = length(name_of_survivors_9) )

for (j in 1:length(name_of_survivors_9)) {
  #print(name)
  ind_survivors = which(ROI==name_of_survivors_9[j]) 
  volumes_survive= as.numeric(temp_bind[,ind_survivors+1])
  treatment = temp_bind$Treatment
  df =as.data.frame( cbind(volumes_survive,treatment) )
  dodge <- position_dodge(width = 0.5 )
  mycolors <- c('blueviolet', 'chartreuse1', 'red', 'azure3')
  plot_list[[j]]<-ggplot(df, aes(x=treatment, y=as.numeric(volumes_survive), fill = treatment)) +
    geom_violin(inherit.aes=TRUE,position=dodge) +
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    #facet_grid(. ~ Diet)  +
    #facet_grid( Sex~Diet)+
    #facet_wrap(~Diet) +
    scale_alpha_discrete(range = c(0.4,0.8)) +
    geom_boxplot(color="black", outlier.color="black", width=0.3, alpha=0.6, position=dodge) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1.5, alpha=0.6, position=dodge)+
    #geom_jitter(size = 0.1, height = 0, width = 0.1, aes(color = Sex)) + 
    labs(title = "Volume")+
    theme_minimal()+
    #background_grid(major = 'xy', minor = "none") + # add thin horizontal lines 
    #panel_border() + 
    theme_bw()+
    labs(x = "Treatment", y = paste0("ROI CBF Mean"), title = paste0(name_of_survivors_9[j] ) ) +
    stat_summary(fun.y=mean, geom="point", size=4, color="black", position=dodge) +
    theme(legend.position="bottom", legend.text = element_text(size = 24),
          legend.title = element_text(size = 18), plot.title = element_text(size = 24), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
  
  
  print(aggregate(as.numeric(volumes_survive)~treatment,data=df,mean))
}

plot<-ggarrange(plotlist =plot_list ,
                ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

lastplot=annotate_figure(plot, top = text_grob("CBF_9_violin", 
                                               color = "black", face = "bold", size = 14))
mywidth<-8
myheight<-4

ggsave(paste0('CBF_9_violins.pdf'), plot = lastplot, device='pdf', scale=1, width=5*mywidth, height=5*myheight, unit=c("in"), dpi=200)










table = as.data.frame(table)
table$diff= as.numeric( table$`Mean (Tr)`)-as.numeric(table$`Mean (Se)`)
pos_ind_table = which(table$diff<0)



name_of_survivors_9 = table[pos_ind_table[1:6],1]
plot_list = vector(mode = "list", length = length(name_of_survivors_9) )

for (j in 1:length(name_of_survivors_9)) {
  #print(name)
  ind_survivors = which(ROI==name_of_survivors_9[j]) 
  volumes_survive= as.numeric(temp_bind[,ind_survivors+1])
  treatment = temp_bind$Treatment
  df =as.data.frame( cbind(volumes_survive,treatment) )
  dodge <- position_dodge(width = 0.5 )
  mycolors <- c('blueviolet', 'chartreuse1', 'red', 'azure3')
  plot_list[[j]]<-ggplot(df, aes(x=treatment, y=as.numeric(volumes_survive), fill = treatment)) +
    geom_violin(inherit.aes=TRUE,position=dodge) +
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    #facet_grid(. ~ Diet)  +
    #facet_grid( Sex~Diet)+
    #facet_wrap(~Diet) +
    scale_alpha_discrete(range = c(0.4,0.8)) +
    geom_boxplot(color="black", outlier.color="black", width=0.3, alpha=0.6, position=dodge) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1.5, alpha=0.6, position=dodge)+
    #geom_jitter(size = 0.1, height = 0, width = 0.1, aes(color = Sex)) + 
    labs(title = "Volume")+
    theme_minimal()+
    #background_grid(major = 'xy', minor = "none") + # add thin horizontal lines 
    #panel_border() + 
    theme_bw()+
    labs(x = "Treatment", y = paste0("ROI CBF Mean"), title = paste0(name_of_survivors_9[j] ) ) +
    stat_summary(fun.y=mean, geom="point", size=4, color="black", position=dodge) +
    theme(legend.position="bottom", legend.text = element_text(size = 24),
          legend.title = element_text(size = 18), plot.title = element_text(size = 24), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
  
  
  print(aggregate(as.numeric(volumes_survive)~treatment,data=df,mean))
}

plot<-ggarrange(plotlist =plot_list ,
                ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

lastplot=annotate_figure(plot, top = text_grob("CBF_9_violin", 
                                               color = "black", face = "bold", size = 14))
mywidth<-8
myheight<-4

ggsave(paste0('CBF_9_violins_opposite.pdf'), plot = lastplot, device='pdf', scale=1, width=5*mywidth, height=5*myheight, unit=c("in"), dpi=200)








##### table_all
table = matrix( NA, 332 , 11)
colnames(table) = c("ROI" , "Mean (Se)" , "Mean (Tr)" , "Mean (Wh)", "SD (Se)" , "SD (Tr)" , "SD (Wh)" , "P value" , "Adjusted P value", "Eta Effect Size", "CI" )
var_index_sig = seq(1,332)

for (j in 1:length(var_index_sig)) {
  
  vol_ind = var_index_sig[j]
  
  var = temp_bind[,(vol_ind+1)] 
  
  means= aggregate(as.numeric(var),list(temp_bind$Treatment), FUN=mean) 
  sds = aggregate(as.numeric(var),list(temp_bind$Treatment), FUN=sd) 
  lm <- lm( var ~  as.factor(Treatment) ,data=temp_bind )
  an=anova(lm)
  eff=eta_squared(lm, partial = TRUE)
  
  table[j,1] = unlist(colnames(adjustresult)[j])
  table[j,2:4]  = means$x
  table[j,5:7] = sds$x
  table[j,8] = result[vol_ind]
  table[j,9] = unlist(adjustresult)[j]
  table[j,10] = eff$Eta2
  table[j,11] =paste0("(", round(as.numeric(eff$CI_low),3), " , ",eff$CI_high, ")" )
  
}

table = as.data.frame(table)
table = table[order(table$`Adjusted P value`),]



# table$dif = as.numeric(table$`Mean (Se)`)- as.numeric(table$`Mean (Tr)`)
# 
# table$dif<0.0
# table3 = table[table$dif<0.0,]


write.xlsx2(table, "table_CBF.xlsx", sheetName = "All", append=TRUE )









#####SANITY check

inds_of_surv = c(51, 57, 64 )  
plot_list = vector(mode = "list", length = length(inds_of_surv) )
name_of_survivors_9 = c("Left Hippocampus", "Left Piriform_Cortex" , "Left Striatum")
for (j in 1:length(inds_of_surv)) {
  #print(name)
  ind_survivors = inds_of_surv[j]
  volumes_survive= as.numeric(temp_bind[,ind_survivors+1])
  treatment = temp_bind$Treatment
  df =as.data.frame( cbind(volumes_survive,treatment) )
  dodge <- position_dodge(width = 0.5 )
  mycolors <- c('blueviolet', 'chartreuse1', 'red', 'azure3')
  plot_list[[j]]<-ggplot(df, aes(x=treatment, y=as.numeric(volumes_survive), fill = treatment)) +
    geom_violin(inherit.aes=TRUE,position=dodge) +
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    #facet_grid(. ~ Diet)  +
    #facet_grid( Sex~Diet)+
    #facet_wrap(~Diet) +
    scale_alpha_discrete(range = c(0.4,0.8)) +
    geom_boxplot(color="black", outlier.color="black", width=0.3, alpha=0.6, position=dodge) +
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1.5, alpha=0.6, position=dodge)+
    #geom_jitter(size = 0.1, height = 0, width = 0.1, aes(color = Sex)) + 
    labs(title = "Volume")+
    theme_minimal()+
    #background_grid(major = 'xy', minor = "none") + # add thin horizontal lines 
    #panel_border() + 
    theme_bw()+
    labs(x = "Treatment", y = paste0("ROI CBF Mean"), title = paste0(name_of_survivors_9[j] ) ) +
    stat_summary(fun.y=mean, geom="point", size=4, color="black", position=dodge) +
    theme(legend.position="bottom", legend.text = element_text(size = 24),
          legend.title = element_text(size = 18), plot.title = element_text(size = 24), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
  
  
  print(aggregate(as.numeric(volumes_survive)~treatment,data=df,mean))
}

plot<-ggarrange(plotlist =plot_list ,
                ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

lastplot=annotate_figure(plot, top = text_grob("CBF_9_violin", 
                                               color = "black", face = "bold", size = 14))
mywidth<-8
myheight<-4

ggsave(paste0('CBF_9_violins_opposite.pdf'), plot = lastplot, device='pdf', scale=1, width=5*mywidth, height=5*myheight, unit=c("in"), dpi=200)







