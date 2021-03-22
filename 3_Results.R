########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Display of results (3)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# R PACKAGES REQUIRED 
library(ggplot2)

########################################################################################################################

# BERGM RESULTS 
rm(list=ls())
load("bergm_results.RData")
rm(list=setdiff(ls(), c('bergm_results')))

########################################################################################################################

# Exclusion of classrooms that did not converge
bergm_results$wave1 <- bergm_results$wave1[c(2,3,5,6,8,9)]
bergm_results$wave2 <- bergm_results$wave2[c(1,7,9,10,11,13)]
bergm_results$wave3 <- bergm_results$wave3[c(2,3,4,5,6)]

# Extraction of BERGM information (all posteriors)
bergm_info <- vector('list',length(bergm_results))
names(bergm_info) <- names(bergm_results)
for(wave in seq_along(bergm_results)){
  for(room in seq_along(bergm_results[[wave]])){
    bergm_info[[wave]][[room]] <- as.data.frame(matrix(NA,nrow=nrow(bergm_results[[wave]][[room]]$Theta),
                                                       ncol=ncol(bergm_results[[wave]][[room]]$Theta)))
    names(bergm_info[[wave]][[room]]) <- names(bergm_results[[wave]][[room]]$ess)
    bergm_info[[wave]][[room]]$classroom <- paste(names(bergm_results[[wave]])[[room]],' (time ',wave,')',sep='')
    for(i in 1:ncol(bergm_results[[wave]][[room]]$Theta)){
      bergm_info[[wave]][[room]][,i] <- bergm_results[[wave]][[room]]$Theta[,i] 
    }
  }
  names(bergm_info[[wave]]) <- names(bergm_results[[wave]])
}

# For special model specfications
bergm_info[[1]][['6300']]$nodeifactor.roma.1 <- NA
bergm_info[[1]][['7100']]$mutual <- NA

bergm_info[[2]][['1100']]$nodeifactor.roma.1 <- NA
bergm_info[[2]][['5400']]$mutual <- NA
bergm_info[[2]][['6200']]$nodeifactor.roma.1 <- NA
bergm_info[[2]][['6300']]$mutual <- NA
bergm_info[[2]][['6300']]$nodeifactor.roma.1 <- NA
bergm_info[[2]][['6400']]$nodeifactor.roma.1 <- NA

bergm_info[[3]][['2100']]$nodeofactor.gender.1 <- NA
bergm_info[[3]][['2100']]$nodeifactor.gender.1 <- NA
bergm_info[[3]][['2100']]$nodematch.gender <- NA
bergm_info[[3]][['6100']]$nodeifactor.roma.1 <- NA
bergm_info[[3]][['6200']]$nodeifactor.roma.1 <- NA
bergm_info[[3]][['6400']]$nodeifactor.roma.1 <- NA

# Creation of a single dataset containing all the posteriors
for(wave in seq_along(bergm_info)){
  for(room in seq_along(bergm_info[[wave]])){
    bergm_info[[wave]][[room]] <- bergm_info[[wave]][[room]][,order(names(bergm_info[[wave]][[room]]),decreasing=FALSE)]
    names(bergm_info[[wave]][[room]]) <- c('classroom','dislike','2nd_dislike','otherslookup','othersscorn',
                                           'shared_dislike','edges','gwdsp',
                                           'pop_spread','act_spread','mutual','female_alt','roma_alt','gender_sam',
                                           'popular_ego','popular2_ego','female_ego','sinks')
  }
  bergm_info[[wave]] <- do.call('rbind',bergm_info[[wave]])
}
bergm_info <- do.call('rbind',bergm_info)

# Conversion into a summary table 

# Extraction of the mean of posteriors and the Bayes p
bergm_mean <- matrix(NA,length(unique(bergm_info$classroom)),length(names(bergm_info[-1])))
rownames(bergm_mean) <- unique(bergm_info$classroom)
colnames(bergm_mean) <- names(bergm_info[-1])
bergm_pval <- bergm_mean

for(i in rownames(bergm_mean)){
  for(j in colnames(bergm_mean)){
    bergm_mean[i,j] <- mean(bergm_info[bergm_info$classroom == i,j])
    bergm_pval[i,j] <- ifelse(mean(bergm_info[bergm_info$classroom == i,j])>0,
                              sum(bergm_info[bergm_info$classroom == i,j]<0)/length(bergm_info[bergm_info$classroom == i,j]),
                              sum(bergm_info[bergm_info$classroom == i,j]>0)/length(bergm_info[bergm_info$classroom == i,j]))
  }
}

# Difference in prob
bergm_diff <- bergm_mean

for(i in rownames(bergm_diff)){
  for(j in colnames(bergm_diff)){
    if(j == "edges"){
      # exp(b_1) / (1 + exp(b_1))
      bergm_diff[i,j] <- exp(bergm_diff[i,j])/(1+exp(bergm_diff[i,j]))
    }else{
      # {exp(b_1+b_x)/(1+ exp(b_1+b_x))} - {exp(b_1)/(1+ exp(b_1))} 
      bergm_diff[i,j] <- exp(bergm_diff[i,j]+bergm_diff[i,"edges"])/(1+exp(bergm_diff[i,j]+bergm_diff[i,"edges"])) - 
        exp(bergm_diff[i,"edges"])/(1+exp(bergm_diff[i,"edges"]))
    }
  }
}

# Data set with all information
bergm_info <- data.frame(predictor=rep(colnames(bergm_mean),each=nrow(bergm_mean)),
                         unit=rep(rownames(bergm_mean),times=ncol(bergm_mean)),
                         par=as.vector(bergm_mean),
                         pval=as.vector(bergm_pval),
                         difference=as.vector(bergm_diff))

bergm_info$classroom <- substr(bergm_info$unit,1,4)
bergm_info$time <- substr(bergm_info$unit,12,12)

# VISUALISATION OF RESULTS 
bergm_info$`Bayesian p-value` <- ifelse(bergm_info$pval < .05,'<.05','>=.05')
set.seed(290691)
bergm_info$time2 <- jitter(as.numeric(bergm_info$time),factor=.75)

bergm_info$predictor <- factor(bergm_info$predictor,
                               levels=c('edges','mutual','act_spread','pop_spread','sinks','gwdsp',
                                        'female_ego','female_alt','gender_sam','roma_alt','popular_ego','popular2_ego',
                                        'othersscorn','otherslookup','dislike','shared_dislike','2nd_dislike'))
levels(bergm_info$predictor) <- c('Edges/Density','Mutual','Act. spread','Pop. spread','Sinks','Multiple two-paths',
                                  'Female (sender)','Female (target)','Same gender',
                                  'Roma (target)','Popularity (sender)','Popularity^2 (sender)',
                                  'Notoriety (target)','Notability (target)',
                                  'Direct antipathy','Shared antipathy','Undirect antipathy')

grid.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='BERGM_results.jpeg',width=12,height=9,units='in',res=1500)
ggplot(data=bergm_info)+
  geom_hline(yintercept=0,color='blue',alpha=.5)+
  geom_line(aes(x=time2,y=difference,group=classroom),alpha=.5,linetype='dashed')+
  geom_point(aes(x=time2,y=difference),colour='black',size=2.75)+
  geom_point(aes(x=time2,y=difference,colour=`Bayesian p-value`),size=2,alpha=.9)+
  scale_colour_manual(values = c('red','darkgrey'))+
  #ylim(-1,1)+
  facet_wrap(~predictor,nrow=3,ncol=6)+
  xlab('Time')+
  ylab('Difference in probability')+
  scale_x_continuous(breaks=c(1,2,3))+
  grid.background 
dev.off()

########################################################################################################################

# TABLE OF RESULTS (APPENDIX)

# Significance levels
bergm_info$sign <- ifelse(bergm_info$pval < .001,'***',
                          ifelse(bergm_info$pval < .01,'**',
                                 ifelse(bergm_info$pval < .05,'*','')))

bergm_info$par <- round(bergm_info$par,2)
bergm_info$pval <- round(bergm_info$pval,3)
bergm_info <- bergm_info[order(bergm_info$predictor),]
bergm_info <- bergm_info[order(bergm_info$unit),]

write.table(bergm_info,'bergm_results.csv',row.names=FALSE,sep=';')
