########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Descriptive statistics (2)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# R PACKAGES REQUIRED 
library(statnet)

########################################################################################################################

# DATA LOADING
rm(list=ls())
load('data.RData')
load('tidieddata.RData')

########################################################################################################################

# GENERAL DESCRIPTIVE STATS

# Number of unique students in the final sample
unique_students <- union(union(rownames(students$wave1),rownames(students$wave2)),rownames(students$wave3))
length(unique_students)
c(nrow(students$wave1),nrow(students$wave2),nrow(students$wave3)) # Students per wave

# Age of students
for(i in 1:nrow(std)){
  if(is.na(std$yrbrth_1[i]) & is.na(std$yrbrth_2[i]) & is.na(std$yrbrth_3[i])){
    std$yrbrth[i] <- std$yrbrth_4[i]
  }else if(is.na(std$yrbrth_1[i]) & is.na(std$yrbrth_2[i])){
    std$yrbrth[i] <- std$yrbrth_3[i]
  }else if(is.na(std$yrbrth_1[i])){
    std$yrbrth[i] <- std$yrbrth_2[i]
  }else{
    std$yrbrth[i] <- std$yrbrth_1[i]
  }
}

for(i in 1:nrow(std)){
  if(is.na(std$mobrth_1[i]) & is.na(std$mobrth_2[i]) & is.na(std$mobrth_3[i])){
    std$mobrth[i] <- std$mobrth_4[i]
  }else if(is.na(std$mobrth_1[i]) & is.na(std$mobrth_2[i])){
    std$mobrth[i] <- std$mobrth_3[i]
  }else if(is.na(std$mobrth_1[i])){
    std$mobrth[i] <- std$mobrth_2[i]
  }else{
    std$mobrth[i] <- std$mobrth_1[i]
  }
}

# Age in wave 1
2011.5 - mean(std[rownames(std) %in% rownames(students$wave1),]$yrbrth + 
                std[rownames(std) %in% rownames(students$wave1),]$mobrth/12,na.rm=TRUE)
# Age in wave 2
2012.5 - mean(std[rownames(std) %in% rownames(students$wave2),]$yrbrth + 
                std[rownames(std) %in% rownames(students$wave2),]$mobrth/12,na.rm=TRUE)
# Age in wave 3
2013.5 - mean(std[rownames(std) %in% rownames(students$wave3),]$yrbrth + 
                std[rownames(std) %in% rownames(students$wave3),]$mobrth/12,na.rm=TRUE)

# Proportion of women
students$wave1$ID <- rownames(students$wave1)
students$wave2$ID <- rownames(students$wave2)
students$wave3$ID <- rownames(students$wave3)

final_sample <- do.call(rbind,students)
final_sample <- final_sample[!duplicated(final_sample),]

sum(final_sample$female)/nrow(final_sample)

# Proportion of Roma
sum(final_sample$roma,na.rm=TRUE)/nrow(final_sample)

# Roma missing
sum(is.na(final_sample$roma))

########################################################################################################################

# DESCRIPTIVE RESULTS

# Gossip networks: descriptive statistics
gossip_ntw <- c(networks$wave1$gossip,networks$wave2$gossip,networks$wave3$gossip)

gossip_desc <- data.frame(matrix(NA,nrow=length(gossip_ntw),ncol=13))
names(gossip_desc) <- c('class','time','female','roma','ties',
                        'ties_missing','potential_ties','missing',
                        'ave_degree','density','recip','transitivity','isolates')
gossip_desc$class <- names(gossip_ntw)
gossip_desc$time <- c(rep(1,9),rep(2,13),rep(3,6))

# Extraction of information from the gossip networks
for(i in seq_along(gossip_ntw)){
  gossip_desc$ties[i] <- sum(gossip_ntw[[i]],na.rm=TRUE)
  gossip_desc$ties_missing[i] <- sum(is.na(gossip_ntw[[i]]))
  gossip_desc$potential_ties[i] <- nrow(gossip_ntw[[i]]) * (nrow(gossip_ntw[[i]])-1)
  gossip_desc$ave_degree[i] <- round(mean(degree(gossip_ntw[[i]])),1)
  gossip_desc$density[i] <- round(gden(gossip_ntw[[i]])*100,1)
  gossip_desc$recip[i] <- round(grecip(gossip_ntw[[i]],measure='edgewise')*100,1)
  gossip_desc$transitivity[i] <- round(igraph::transitivity(igraph::graph_from_adjacency_matrix(gossip_ntw[[i]]))*100,1)
  gossip_desc$isolates[i] <- length(isolates(gossip_ntw[[i]]))
}
gossip_desc$missing <- round(gossip_desc$ties_missing / gossip_desc$potential_ties*100,1)

# Addition of actor-attributes
specifications <- vector('list',length(networks))
names(specifications) <- names(networks)
for(wave in seq_along(specifications)){
  specifications[[wave]] <- data.frame(row.names=names(networks[[wave]]$gossip))
}

for(wave in seq_along(specifications)){
  for(room in rownames(specifications[[wave]])){
    specifications[[wave]][room,'roma'] <- sum(students[[wave]][students[[wave]]$class==room,]$roma==1,na.rm=TRUE) 
    specifications[[wave]][room,'female'] <- sum(students[[wave]][students[[wave]]$class==room,]$female==1,na.rm=TRUE)
  }
}

specifications <- as.data.frame(do.call('rbind',specifications))

gossip_desc$female <- specifications$female
gossip_desc$roma <- specifications$roma

# Print table
write.table(gossip_desc[order(gossip_desc$class),],'descriptive_table.csv',row.names=FALSE,sep=';')