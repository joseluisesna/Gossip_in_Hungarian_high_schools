########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Ancillary analyses and extra plots (1.1)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# R PACKAGES REQUIRED 
library(tidyverse);library(networkD3);library(ggplot2);library(ggrepel)

########################################################################################################################

# DATA LOADING
rm(list=ls())
load('data.RData')

########################################################################################################################

# Removal of wave 1
networks$wave1 <- NULL

# SANKEY PLOT TO SEE CLASSROOM COMPOSITION CHANGE

alluv <- networks

for(wave in seq_along(alluv)){
  alluv[[wave]] <- alluv[[wave]]$affect
  for(room in seq_along(alluv[[wave]])){
    alluv[[wave]][[room]] <- data.frame(student = rownames(alluv[[wave]][[room]]))
    alluv[[wave]][[room]]$class <- names(alluv[[wave]])[[room]]
  }
  alluv[[wave]] <- do.call(rbind,alluv[[wave]])
}

alluv_dat <- merge(alluv$wave2,alluv$wave3,by="student",all=TRUE)
alluv_dat <- merge(alluv_dat,alluv$wave4,by="student",all=TRUE)

names(alluv_dat) <- c('student','class_1','class_2','class_3')

# Nodes (classrooms)
nodes_1 <- c(unique(alluv_dat$class_1[!is.na(alluv_dat$class_1)]),"Off sample")
nodes_2 <- c(unique(alluv_dat$class_2[!is.na(alluv_dat$class_2)]),"Off sample")
nodes_3 <- c(unique(alluv_dat$class_3[!is.na(alluv_dat$class_3)]),"Off sample")
(nodes <- data.frame(name=c(nodes_1,nodes_2,nodes_3)))

nodes$group <- substr(nodes$name,1,1) # to group the classrooms by school
nodes$group[nodes$group == 9] <- '2'
nodes$group <- as.factor(nodes$group)

# Links (classroom composition)
for(i in 1:nrow(alluv_dat)){
  if(!is.na(alluv_dat$class_1[i])){
    alluv_dat$class_1[i] <- which(nodes_1==alluv_dat$class_1[i])-1
  }else{
    alluv_dat$class_1[i] <- length(nodes_1)-1
  }
  if(!is.na(alluv_dat$class_2[i])){
    alluv_dat$class_2[i] <- which(nodes_2==alluv_dat$class_2[i])-1+length(nodes_1)
  }else{
    alluv_dat$class_2[i] <- length(nodes_1)+length(nodes_2)-1
  }
  if(!is.na(alluv_dat$class_3[i])){
    alluv_dat$class_3[i] <- which(nodes_3==alluv_dat$class_3[i])-1+length(nodes_1)+length(nodes_2)
  }else{
    alluv_dat$class_3[i] <- length(nodes_1)+length(nodes_2)+length(nodes_3)-1
  }
}

links <- data.frame(rbind(as.matrix(alluv_dat[c('class_1','class_2')]),
                    as.matrix(alluv_dat[c('class_2','class_3')])))
names(links) <- c('source','target')
links_2 <- links[!duplicated(links),]
links_2$value <- NA

for(i in 1:nrow(links_2)){
  if(!is.na(links_2$source[i]) & !is.na(links_2$target[i])){
    links_2$value[i] <- table(links$source,links$target)[links_2$source[i],links_2$target[i]]
  }
}
links <- na.omit(links_2)

links$source <- as.numeric(links$source)
links$target <- as.numeric(links$target)

# Drop outs
drops <- c(length(nodes_1)-1,
           length(nodes_1)+length(nodes_2)-1,
           length(nodes_1)+length(nodes_2)+length(nodes_3)-1) # dropouts
links$dropout <- NA

for(i in 1:length(links$dropout)){
  if(links$source[i] %in% drops & links$target[i] %in% drops){
    links$dropout[i] <- 'Gone'
  }else{
    if(links$target[i] %in% drops){
      links$dropout[i] <- 'Dropped out'
    }else{
      links$dropout[i] <- 'In school'
    }
  }
}
links$dropout <- as.factor(links$dropout)

# Sankey Plot
my_color <- 'd3.scaleOrdinal() .domain(["In school","Gone","Dropped out","1","2","3","4","5","6","7","N"]) 
.range(["steelblue","tomato","grey","green","yellow","blue","red","white","purple","orange","grey"])'

sankeyNetwork(Links=links,Nodes=nodes,Source='source',
              Target='target',Value='value',NodeID='name',NodeGroup='group',LinkGroup='dropout',
              colourScale=my_color,
              units='students',iterations=0,
              fontSize=11,nodeWidth=75)

########################################################################################################################

# GOSSIP STABILITY (JACCARD INDICES)

gos_evo <- networks
for(wave in 1:length(gos_evo)){
  gos_evo[[wave]] <- gos_evo[[wave]]$gossip
}

surv_class <- intersect(intersect(names(gos_evo$wave2),names(gos_evo$wave3)),names(gos_evo$wave4)) # 29 classes remain from t1 to t3

for(wave in 1:length(gos_evo)){
  gos_evo[[wave]] <- gos_evo[[wave]][surv_class]
}

gos_evo12 <- gos_evo[c('wave2','wave3')]
gos_evo23 <- gos_evo[c('wave3','wave4')]

# Overlap of students that remain in consecutive waves
for(wave in 1:length(gos_evo12)){
  for(room in 1:length(gos_evo12[[wave]])){
    gos_evo12[[wave]][[room]] <- gos_evo12[[wave]][[room]][intersect(colnames(gos_evo12[[1]][[room]]),
                                                                     colnames(gos_evo12[[2]][[room]])),
                                                           intersect(colnames(gos_evo12[[1]][[room]]),
                                                                     colnames(gos_evo12[[2]][[room]]))]
  }
}

for(wave in 1:length(gos_evo23)){
  for(room in 1:length(gos_evo23[[wave]])){
    gos_evo23[[wave]][[room]] <- gos_evo23[[wave]][[room]][intersect(colnames(gos_evo23[[1]][[room]]),
                                                                     colnames(gos_evo23[[2]][[room]])),
                                                           intersect(colnames(gos_evo23[[1]][[room]]),
                                                                     colnames(gos_evo23[[2]][[room]]))]
  }
}

# Jaccard indices
gos_Jac <- data.frame(classroom=names(gos_evo12$wave2),
                      Jac_12=NA,Jac_23=NA)

Jaccard <- function(changetable) {
  return(changetable['1','1']/(changetable['0','1']+changetable['1','0']+changetable['1','1']))
}

for(i in 1:nrow(gos_Jac)){
  gos_Jac$Jac_12[i] <- Jaccard(table(factor(gos_evo12$wave2[[i]],levels=c('0','1')),
                                     factor(gos_evo12$wave3[[i]],levels=c('0','1'))))
  gos_Jac$Jac_23[i] <- Jaccard(table(factor(gos_evo23$wave3[[i]],levels=c('0','1')),
                                     factor(gos_evo23$wave4[[i]],levels=c('0','1'))))
}

# Plot of the change
no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='Gossip_stab.jpeg',width=5,height=5,units='in',res=1000)
ggplot(data=gos_Jac)+
  geom_rect(xmin=0,xmax=1,ymin=0,ymax=1,colour='black',fill='tomato1')+  
  geom_rect(xmin=.1,xmax=1,ymin=.1,ymax=1,colour='black',fill='tan2')+  
  geom_rect(xmin=.2,xmax=1,ymin=.2,ymax=1,colour='black',fill='lightgreen')+  
  geom_label_repel(aes(x=Jac_12,y=Jac_23,label=classroom))+
  geom_point(aes(x=Jac_12,y=Jac_23),colour='black',size=2.75,alpha=.75)+
  xlab('Jaccard index (time 1, time 2)')+
  ylab('Jaccard index (time 2, time 3)')+
  xlim(0,.35)+ylim(0,.35)+
  no.background
dev.off()