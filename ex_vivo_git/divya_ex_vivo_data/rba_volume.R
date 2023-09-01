library(readxl)
library(dplyr)
library(magrittr)
library(xlsx)
library(ggplot2)
library(ggpubr)

noreadcsf=c(148,152,161,314,318,327)



Atlas_path = '/Users/ali/Desktop/Aug23/CVN/rba/exvivo/divya_ex_vivo_data/CHASSSYMM3AtlasLegends.xlsx'
Atlas = read.xlsx(Atlas_path, sheetIndex = 1)


path_master='/Users/ali/Desktop/Aug23/CVN/rba/exvivo/divya_ex_vivo_data/MasterSheet_Experiments2021.xlsx'
data=read.xlsx(path_master, sheetName = 'CVN_20abb15' )
datatemp=data%>%dplyr::select(DWI, treatment)#subselect


datatemp=na.omit(datatemp) ## ommit all na and zero character dwi and died durring



pathnames='/Users/ali/Desktop/Aug23/CVN/rba/exvivo/divya_ex_vivo_data/mouse_anatomy.csv'
ROI=read.csv(pathnames, header = F, sep = ",", quote = "")
ROI=paste0(ROI$Bigpart, " ",ROI$ROI)
# 

#########

data_path='/Users/ali/Desktop/Aug23/CVN/rba/exvivo/divya_ex_vivo_data/Volumes.csv'
data_div=read.csv(data_path, header = TRUE, sep = ",", quote = "")

data_div=t(na.omit(t(data_div)))

sed_index = grep ("Sedentary", colnames(data_div)) 
data_div[1,sed_index] = "Sedentary"

tr_index = grep ("X.Voluntary...training.", colnames(data_div)) 
data_div[1,tr_index] = "Treadmil"

Wheel_index =  grepl ("Voluntary", colnames(data_div)) & !grepl ("X.Voluntary...training.", colnames(data_div))
data_div[1,Wheel_index] = "Wheel"


data_div = t(data_div)
#data_div$X ==ROI$V2





library(emmeans)
library(effectsize)

len = dim(data_div)[2]
result=matrix(NA,1,(len-1))
for (i in 2:len) {
  var2=as.numeric(data_div[2:dim(data_div)[1],i]) / sum(as.numeric(data_div[2:dim(data_div)[1],i]))
  Treatment2 = data_div[2:dim(data_div)[1],1]
  lm <- lm( var2 ~  as.factor(Treatment2) )
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


pathnames='/Users/ali/Desktop/Aug23/CVN/rba/exvivo/mouse_anatomy.csv'
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
#   lm <- lm( as.numeric(var2) ~Age_Months*as.factor(Sex)*as.factor(Genotype)* as.factor(Treatment),data=temp_bind )
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
  
  var =as.numeric(data_div[2:dim(data_div)[1],vol_ind+1])
  Treatment = data_div[2:dim(data_div)[1],1]
  means= aggregate(as.numeric(var),list(Treatment), FUN=mean) 
  sds = aggregate(as.numeric(var),list(Treatment), FUN=sd) 
  lm <- lm( var ~  as.factor(Treatment))
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




write.xlsx2(table, "table_volume.xlsx", sheetName = "Significants" )





table = as.data.frame(table)
table$diff= as.numeric( table$`Mean (Tr)`)-as.numeric(table$`Mean (Se)`)
pos_ind_table = which(table$diff>0)



name_of_survivors_9 = table[pos_ind_table[1:9],1]
plot_list = vector(mode = "list", length = length(name_of_survivors_9) )

for (j in 1:length(name_of_survivors_9)) {
  #print(name)
  ind_survivors = which(ROI==name_of_survivors_9[j]) 
  volumes_survive= as.numeric(data_div[2:dim(data_div)[1],ind_survivors+1])
  treatment = data_div[2:dim(data_div)[1],1]
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
    labs(x = "Treatment", y = paste0("ROI Volume"), title = paste0(name_of_survivors_9[j] ) ) +
    stat_summary(fun.y=mean, geom="point", size=4, color="black", position=dodge) +
    theme(legend.position="bottom", legend.text = element_text(size = 24),
          legend.title = element_text(size = 18), plot.title = element_text(size = 24), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
  
  
  print(aggregate(as.numeric(volumes_survive)~treatment,data=df,mean))
}

 plot<-ggarrange(plotlist =plot_list ,
                ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

lastplot=annotate_figure(plot, top = text_grob("Volume_9_violin", 
                                               color = "black", face = "bold", size = 14))
mywidth<-8
myheight<-4

ggsave(paste0('volume_9_violins.pdf'), plot = lastplot, device='pdf', scale=1, width=5*mywidth, height=5*myheight, unit=c("in"), dpi=200)





table = as.data.frame(table)
table$diff= as.numeric( table$`Mean (Tr)`)-as.numeric(table$`Mean (Se)`)
pos_ind_table = which(table$diff<0)



name_of_survivors_9 = table[pos_ind_table[1:1],1]
plot_list = vector(mode = "list", length = length(name_of_survivors_9) )

for (j in 1:length(name_of_survivors_9)) {
  #print(name)
  ind_survivors = which(ROI==name_of_survivors_9[j]) 
  volumes_survive= as.numeric(data_div[2:dim(data_div)[1],ind_survivors+1])
  treatment = data_div[2:dim(data_div)[1],1]
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
    labs(x = "Treatment", y = paste0("ROI Volume"), title = paste0(name_of_survivors_9[j] ) ) +
    stat_summary(fun.y=mean, geom="point", size=4, color="black", position=dodge) +
    theme(legend.position="bottom", legend.text = element_text(size = 24),
          legend.title = element_text(size = 18), plot.title = element_text(size = 24), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18))
  
  
  print(aggregate(as.numeric(volumes_survive)~treatment,data=df,mean))
}


plot<-ggarrange(plotlist =plot_list ,
                ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")

lastplot=annotate_figure(plot, top = text_grob("Volume_9_violin", 
                                               color = "black", face = "bold", size = 14))
mywidth<-8
myheight<-4

ggsave(paste0('volume_9_violins_opposite.pdf'), plot = lastplot, device='pdf', scale=1, width=5*mywidth, height=5*myheight, unit=c("in"), dpi=200)







##### table_all
table = matrix( NA, 332 , 11)
colnames(table) = c("ROI" , "Mean (Se)" , "Mean (Tr)" , "Mean (Wh)", "SD (Se)" , "SD (Tr)" , "SD (Wh)" , "P value" , "Adjusted P value", "Eta Effect Size", "CI" )
var_index_sig = seq(1,332)

for (j in 1:length(var_index_sig)) {
  
  vol_ind = var_index_sig[j]
  
  var =as.numeric(data_div[2:dim(data_div)[1],vol_ind+1])
  Treatment = data_div[2:dim(data_div)[1],1]
  means= aggregate(as.numeric(var),list(Treatment), FUN=mean) 
  sds = aggregate(as.numeric(var),list(Treatment), FUN=sd) 
  lm <- lm( var ~  as.factor(Treatment))
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

write.xlsx2(table, "table_volume.xlsx", sheetName = "All", append=TRUE )




