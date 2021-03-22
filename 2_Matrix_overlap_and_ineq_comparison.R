########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Matrix overlap and inequality comparison (2)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: March 5th, 2021
########################################################################################################################

# R PACKAGES REQUIRED 
library(statnet);library(ineq);library(ggplot2)

########################################################################################################################

# DATA LOADING
rm(list=ls())
load('tidieddata.RData')

########################################################################################################################

# MATRIX OVERLAP

ties <- overlap <- jacc <- qap <- networks

# Number of gossip and dislike ties per classroom-observation
for(i in seq_along(ties)){
  ties[[i]] <- ties[[i]][c('gossip','dislike')]
  for(j in seq_along(ties[[i]])){
    for(k in seq_along(ties[[i]][[j]])){
      ties[[i]][[j]][[k]] <- sum(ties[[i]][[j]][[k]],na.rm=TRUE)
    }
    ties[[i]][[j]] <- unlist(ties[[i]][[j]])
  }
  ties[[i]] <- data.frame(do.call(cbind,ties[[i]]))  
  ties[[i]]$Time <- paste(i)
}

ties <- do.call(rbind,ties)
ties$classroom <- as.factor(substr(rownames(ties),7,10))
ties <- ties[,c('classroom','Time','gossip','dislike')]

# Overlap between gossip and dislike ties
for(i in seq_along(overlap)){
  for(j in seq_along(overlap[[i]]$gossip)){
    overlap[[i]][['overlap']][[j]] <- sum((overlap[[i]][['gossip']][[j]]+overlap[[i]][['dislike']][[j]])==2,na.rm=TRUE)
    overlap[[i]][['overlap']] <- unlist(overlap[[i]][['overlap']])
  }
  overlap[[i]] <- overlap[[i]]['overlap']
}

ties$overlap <- as.vector(unlist(overlap))

# Jaccard indices
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}


for(i in seq_along(jacc)){
  jacc[[i]][['jaccard']] <- vector('list',length=length(jacc[[i]]$gossip))
  for(j in seq_along(jacc[[i]]$gossip)){
    jacc[[i]][['jaccard']][[j]] <- round(Jaccard(jacc[[i]][['gossip']][[j]],jacc[[i]][['dislike']][[j]]),2)
  }
  jacc[[i]] <- jacc[[i]][['jaccard']]
}

ties$jaccard <- as.vector(unlist(jacc))

# QAP
for(i in seq_along(qap)){
  qap[[i]][['qap']] <- vector('list',length=length(qap[[i]]$gossip))
  for(j in seq_along(qap[[i]]$gossip)){
    set.seed(0708)
    qap[[i]][['qap']][[j]] <- netlogit(qap[[i]][['gossip']][[j]],qap[[i]][['dislike']][[j]],
                                       nullhyp='qap',reps=1000)
  }
  qap[[i]] <- qap[[i]][['qap']]
}

qap_coef <- qap_p <- qap

for(i in seq_along(qap)){
  for(j in seq_along(qap[[i]])){
    qap_coef[[i]][[j]] <- qap[[i]][[j]]$coefficients[2]
    qap_p[[i]][[j]] <- qap[[i]][[j]]$pgreqabs[2]
  }
}

ties$qap_coef <- round(as.vector(unlist(qap_coef)),2)
ties$qap_p <- round(as.vector(unlist(qap_p)),3)
ties$qap_sign <- ifelse(ties$qap_p < .001,'***',
                        ifelse(ties$qap_p < .01,'**',
                               ifelse(ties$qap_p < .05,'*','')))

########################################################################################################################

# INEQUALITY COMPARISON

# Calculation of Gini indices for the gossip and dislike/hate networks
for(i in seq_along(networks)){
  networks[[i]] <- networks[[i]][c('gossip','dislike')]
  for(j in seq_along(networks[[i]])){
    for(k in seq_along(networks[[i]][[j]])){
      networks[[i]][[j]][[k]] <- colSums(networks[[i]][[j]][[k]],na.rm=TRUE)
      networks[[i]][[j]][[k]] <- ineq(networks[[i]][[j]][[k]],type='Gini')
    }
    networks[[i]][[j]] <- unlist(networks[[i]][[j]])
  }
  networks[[i]] <- data.frame(do.call(cbind,networks[[i]]))
}

networks <- do.call(rbind,networks)
networks$diff <- networks$gossip / networks$dislike

ties$gossip_gini <- round(networks$gossip,2)
ties$dislike_gini <- round(networks$dislike,2)
ties$gini_diff <- round(networks$diff,2)

# Plot
grid.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

# To organise the axis in increasing order
axis_order <- data.frame(classroom=levels(ties$classroom),mean_ratio=NA)
for(i in axis_order$classroom){
  axis_order[axis_order$classroom == i,]$mean_ratio <- mean(ties[ties$classroom == i,]$gini_diff)
}
axis_order <- axis_order$classroom[order(axis_order$mean_ratio,decreasing=FALSE)]

jpeg(filename='Gini_coeff_ratio.jpeg',width=6,height=6,units='in',res=1200)
ggplot(data=ties)+
  geom_rect(xmin=1/1.1,xmax=1.1,ymin=axis_order[1],ymax=axis_order[length(axis_order)],colour='skyblue',fill='skyblue')+
  geom_vline(aes(xintercept=1),colour='blue',alpha=.5)+
  geom_point(aes(x=gini_diff,y=classroom,shape=Time),size=5)+
  geom_point(aes(x=gini_diff,y=classroom,shape=Time,colour=Time),size=4)+
  xlab('Indegree inequality (gossip over antipathy)')+ylab('Classroom')+
  scale_y_discrete(limits=axis_order)+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  grid.background
dev.off()

write.table(ties[order(ties$classroom,decreasing=FALSE),],'overlap_gini.csv',row.names=FALSE,sep=';')