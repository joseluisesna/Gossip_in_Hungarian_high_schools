########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## BERGM analysis (2)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# R PACKAGES REQUIRED 
library(Bergm)

########################################################################################################################

# DATA LOADING
rm(list=ls())
load('tidieddata.RData')

# PRELIMINARY

# Removal of the diagonal in all matrices but gossip
for(wave in seq_along(networks)){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in seq_along(networks[[wave]][[item]])){
      diag(networks[[wave]][[item]][[room]]) <- NA
    }
  }
}

# NAs to zeroes in predictors
for(wave in seq_along(networks)){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in seq_along(networks[[wave]][[item]])){
      networks[[wave]][[item]][[room]][is.na(networks[[wave]][[item]][[room]])] <- 0
    }
  }
}

# 2nd-degree dislike and shared dislike
for(wave in seq_along(networks)){
  networks[[wave]]$shared_dislike <-  networks[[wave]]$dislike_2 <- networks[[wave]]$dislike
  for(room in seq_along(networks[[wave]]$gossip)){
    # being friends with somebody who dislikes the target
    networks[[wave]]$dislike_2[[room]] <- 1*(networks[[wave]]$friend[[room]] %*% networks[[wave]]$dislike[[room]]!=0)
    # shared dislike with somebody else
    networks[[wave]]$shared_dislike[[room]] <- 1*(networks[[wave]]$dislike[[room]]+networks[[wave]]$dislike_2[[room]]==2)
  }
}

# Students with unknown ethnicity sent to non-Roma
for(wave in seq_along(students)){
  for(i in 1:nrow(students[[wave]])){
    if(is.na(students[[wave]]$roma[i])){
      students[[wave]]$roma[i] <- 0
    }
  }
}

# Creation of popularity, and popularity square
for(wave in seq_along(networks)){
  networks[[wave]]$popular2 <-  networks[[wave]]$popular <- networks[[wave]]$friend
  for(room in seq_along(networks[[wave]]$gossip)){
    # popularity
    networks[[wave]]$popular[[room]] <- as.numeric(scale(colSums(networks[[wave]]$popular[[room]],na.rm=TRUE),
                                                        center=TRUE))
    # popularity^2
    networks[[wave]]$popular2[[room]] <- as.numeric(scale(networks[[wave]]$popular[[room]]^2,center=TRUE))
  }
}

# Gossip into a network object, and addition of nodes' attributes (gender, roma, popular, sb_disliked, and sh_dislike):
for(wave in seq_along(networks)){
  for(room in names(networks[[wave]]$gossip)){
    networks[[wave]]$gossip[[room]] <- network(networks[[wave]]$gossip[[room]],directed=TRUE)
    network::set.vertex.attribute(networks[[wave]]$gossip[[room]],'gender',
                                  students[[wave]][students[[wave]]$class == room,]$female)
    network::set.vertex.attribute(networks[[wave]]$gossip[[room]],'roma',
                                  students[[wave]][students[[wave]]$class == room,]$roma)
    network::set.vertex.attribute(networks[[wave]]$gossip[[room]],'popular',networks[[wave]]$popular[[room]])
    network::set.vertex.attribute(networks[[wave]]$gossip[[room]],'popular2',networks[[wave]]$popular2[[room]])
  }
}

########################################################################################################################

# Possible specifications: with/without gender, with/without ethnicity
specifications <- vector('list',length(networks))
names(specifications) <- names(networks)
for(wave in seq_along(specifications)){
  specifications[[wave]] <- data.frame(row.names=names(networks[[wave]]$gossip))
}

for(wave in seq_along(specifications)){
  for(room in rownames(specifications[[wave]])){
    specifications[[wave]][room,'roma'] <- sum(students[[wave]][students[[wave]]$class==room,]$roma==1) 
    specifications[[wave]][room,'no_roma'] <- sum(students[[wave]][students[[wave]]$class==room,]$roma==0) 
    specifications[[wave]][room,'females'] <- sum(students[[wave]][students[[wave]]$class==room,]$female==1)
    specifications[[wave]][room,'males'] <- sum(students[[wave]][students[[wave]]$class==room,]$female==0)
    specifications[[wave]][room,'ethnicity'] <- ifelse(specifications[[wave]][[room,'roma']] >= 3 & 
                                                      specifications[[wave]][[room,'no_roma']] >= 3,TRUE,FALSE)
    specifications[[wave]][room,'gender'] <- ifelse(specifications[[wave]][[room,'females']] >= 3 & 
                                                      specifications[[wave]][[room,'males']] >= 3,TRUE,FALSE)
  }
}

# ERGM descriptive
bergm_descript <- vector('list',length(networks))
names(bergm_descript) <- names(networks)
for(wave in seq_along(bergm_descript)){
  bergm_descript[[wave]] <- vector('list',length(networks[[wave]]$gossip))
  names(bergm_descript[[wave]]) <- names(networks[[wave]]$gossip)
}

for(wave in seq_along(bergm_descript)){
  for(room in seq_along(bergm_descript[[wave]])){
    if(specifications[[wave]][[room,'ethnicity']] == TRUE & specifications[[wave]][[room,'gender']] == TRUE){
      bergm_descript[[wave]][[room]] <- summary(networks[[wave]]$gossip[[room]]~
                                                  edges+mutual+
                                                  gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                                  odegree(d=0)+ 
                                                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                                  nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                                  nodeifactor('roma')+
                                                  nodeocov('popular')+nodeocov('popular2')+
                                                  edgecov(networks[[wave]]$dislike[[room]])+
                                                  edgecov(networks[[wave]]$dislike_2[[room]])+
                                                  edgecov(networks[[wave]]$shared_dislike[[room]])+
                                                  edgecov(networks[[wave]]$otherslookup[[room]])+
                                                  edgecov(networks[[wave]]$othersscorn[[room]]))
    }else if(specifications[[wave]][[room,'roma']] == FALSE){
      bergm_descript[[wave]][[room]] <- summary(networks[[wave]]$gossip[[room]]~
                                                  edges+mutual+
                                                  gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                                  odegree(d=0)+ 
                                                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                                  nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                                  nodeocov('popular')+nodeocov('popular2')+
                                                  edgecov(networks[[wave]]$dislike[[room]])+
                                                  edgecov(networks[[wave]]$dislike_2[[room]])+
                                                  edgecov(networks[[wave]]$shared_dislike[[room]])+
                                                  edgecov(networks[[wave]]$otherslookup[[room]])+
                                                  edgecov(networks[[wave]]$othersscorn[[room]]))
    }else if(specifications[[wave]][[room,'gender']] == FALSE){
      bergm_descript[[wave]][[room]] <- summary(networks[[wave]]$gossip[[room]]~
                                                  edges+mutual+
                                                  gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                                  odegree(d=0)+ 
                                                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                                  nodeifactor('roma')+
                                                  nodeocov('popular')+nodeocov('popular2')+
                                                  edgecov(networks[[wave]]$dislike[[room]])+
                                                  edgecov(networks[[wave]]$dislike_2[[room]])+
                                                  edgecov(networks[[wave]]$shared_dislike[[room]])+
                                                  edgecov(networks[[wave]]$otherslookup[[room]])+
                                                  edgecov(networks[[wave]]$othersscorn[[room]]))
    }
  }  
  bergm_descript[[wave]] <- lapply(bergm_descript[[wave]],data.frame)
}

########################################################################################################################

# BERGM ANALYSIS 

# Object for results
bergm_results <- vector('list',length(networks))
names(bergm_results) <- names(networks)
for(wave in seq_along(bergm_results)){
  bergm_results[[wave]] <- vector('list',length(networks[[wave]]$gossip))
  names(bergm_results[[wave]]) <- names(networks[[wave]]$gossip)
}

### TIME 1 ###
# classroom 2200
set.seed(0708)
bergm_results[[1]][[2]] <- bergmM(networks[[1]]$gossip[[2]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[2]])+
                                     edgecov(networks[[1]]$dislike_2[[2]])+
                                     edgecov(networks[[1]]$shared_dislike[[2]])+
                                     edgecov(networks[[1]]$othersscorn[[2]])+
                                     edgecov(networks[[1]]$otherslookup[[2]]),
                                   burn.in=1000,main.iters=5000,gamma=0.30,
                                   prior.mean=c(rep(0,17)),prior.sigma=diag(5,17))
# classroom 3400 (no gender)
set.seed(0708)
bergm_results[[1]][[3]] <- bergmM(networks[[1]]$gossip[[3]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     #nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[3]])+
                                     edgecov(networks[[1]]$dislike_2[[3]])+
                                     edgecov(networks[[1]]$shared_dislike[[3]])+
                                     edgecov(networks[[1]]$othersscorn[[3]])+
                                     edgecov(networks[[1]]$otherslookup[[3]]),
                                   burn.in=1000,main.iters=5000,gamma=0.25,
                                   prior.mean=c(rep(0,14)),prior.sigma=diag(5,14))
# classroom 6100
set.seed(0708)
bergm_results[[1]][[5]] <- bergmM(networks[[1]]$gossip[[5]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[5]])+
                                     edgecov(networks[[1]]$dislike_2[[5]])+
                                     edgecov(networks[[1]]$shared_dislike[[5]])+
                                     edgecov(networks[[1]]$othersscorn[[5]])+
                                     edgecov(networks[[1]]$otherslookup[[5]]),
                                   burn.in=1000,main.iters=5000,gamma=0.22,
                                   prior.mean=c(rep(0,17)),prior.sigma=diag(5,17))
# classroom 6300 (no ethnicity)
set.seed(0708)
bergm_results[[1]][[6]] <- bergmM(networks[[1]]$gossip[[6]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[6]])+
                                     edgecov(networks[[1]]$dislike_2[[6]])+
                                     edgecov(networks[[1]]$shared_dislike[[6]])+
                                     edgecov(networks[[1]]$othersscorn[[6]])+
                                     edgecov(networks[[1]]$otherslookup[[6]]),
                                   burn.in=1000,main.iters=5000,gamma=0.30,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 7100 (no mutual ties and no gender)
set.seed(0708)
bergm_results[[1]][[8]] <- bergmM(networks[[1]]$gossip[[8]]~
                                     edges+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[8]])+
                                     edgecov(networks[[1]]$dislike_2[[8]])+
                                     edgecov(networks[[1]]$shared_dislike[[8]])+
                                     edgecov(networks[[1]]$othersscorn[[8]])+
                                     edgecov(networks[[1]]$otherslookup[[8]]),
                                   burn.in=1000,main.iters=5000,gamma=0.30,
                                   prior.mean=c(rep(0,13)),prior.sigma=diag(5,13))
# classroom 7800
set.seed(0708)
bergm_results[[1]][[9]] <- bergmM(networks[[1]]$gossip[[9]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[1]]$dislike[[9]])+
                                     edgecov(networks[[1]]$dislike_2[[9]])+
                                     edgecov(networks[[1]]$shared_dislike[[9]])+
                                     edgecov(networks[[1]]$othersscorn[[9]])+
                                     edgecov(networks[[1]]$otherslookup[[9]]),
                                   burn.in=1000,main.iters=5000,gamma=0.25,
                                   prior.mean=c(rep(0,17)),prior.sigma=diag(5,17))


### TIME 2 ###
# classroom 1100 (no ethnicity)
set.seed(0708)
bergm_results[[2]][[1]] <- bergmM(networks[[2]]$gossip[[1]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+
                                     gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[2]]$dislike[[1]])+
                                     edgecov(networks[[2]]$dislike_2[[1]])+
                                     edgecov(networks[[2]]$shared_dislike[[1]])+
                                     edgecov(networks[[2]]$othersscorn[[1]])+
                                     edgecov(networks[[2]]$otherslookup[[1]]),
                                   burn.in=1000,main.iters=5000,gamma=0.30,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 5400 (no mutual ties)
set.seed(0708)
bergm_results[[2]][[7]] <- bergmM(networks[[2]]$gossip[[7]]~
                                     edges+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[2]]$dislike[[7]])+
                                     edgecov(networks[[2]]$dislike_2[[7]])+
                                     edgecov(networks[[2]]$shared_dislike[[7]])+
                                     edgecov(networks[[2]]$othersscorn[[7]])+
                                     edgecov(networks[[2]]$otherslookup[[7]]),
                                   burn.in=1000,main.iters=5000,gamma=0.32,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 6200 (no ethnicity)
set.seed(0708)
bergm_results[[2]][[9]] <- bergmM(networks[[2]]$gossip[[9]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+
                                     gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[2]]$dislike[[9]])+
                                     edgecov(networks[[2]]$dislike_2[[9]])+
                                     edgecov(networks[[2]]$shared_dislike[[9]])+
                                     edgecov(networks[[2]]$othersscorn[[9]])+
                                     edgecov(networks[[2]]$otherslookup[[9]]),
                                   burn.in=1000,main.iters=5000,gamma=0.25,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 6300 (no ethnicity and no mutual ties)
set.seed(0708)
bergm_results[[2]][[10]] <- bergmM(networks[[2]]$gossip[[10]]~
                                      edges+
                                      gwodegree(decay=log(2),fixed=TRUE)+
                                      gwidegree(decay=log(2),fixed=TRUE)+
                                      odegree(d=0)+ 
                                      dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                      nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                      nodeocov('popular')+nodeocov('popular2')+
                                      edgecov(networks[[2]]$dislike[[10]])+
                                      edgecov(networks[[2]]$dislike_2[[10]])+
                                      edgecov(networks[[2]]$shared_dislike[[10]])+
                                      edgecov(networks[[2]]$othersscorn[[10]])+
                                      edgecov(networks[[2]]$otherslookup[[10]]),
                                    burn.in=1000,main.iters=5000,gamma=0.30,
                                    prior.mean=c(rep(0,15)),prior.sigma=diag(5,15))
# classroom 6400 (no ethnicity)
set.seed(0708)
bergm_results[[2]][[11]] <- bergmM(networks[[2]]$gossip[[11]]~
                                      edges+mutual+
                                      gwodegree(decay=log(2),fixed=TRUE)+
                                      gwidegree(decay=log(2),fixed=TRUE)+
                                      odegree(d=0)+ 
                                      dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                      nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                      nodeocov('popular')+nodeocov('popular2')+
                                      edgecov(networks[[2]]$dislike[[11]])+
                                      edgecov(networks[[2]]$dislike_2[[11]])+
                                      edgecov(networks[[2]]$shared_dislike[[11]])+
                                      edgecov(networks[[2]]$othersscorn[[11]])+
                                      edgecov(networks[[2]]$otherslookup[[11]]),
                                    burn.in=1000,main.iters=5000,gamma=0.22,
                                    prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 7600 (no gender and no sinks)
set.seed(0708)
bergm_results[[2]][[13]] <- bergmM(networks[[2]]$gossip[[13]]~
                                      edges+mutual+
                                      gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                      dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                      nodeifactor('roma')+
                                      nodeocov('popular')+nodeocov('popular2')+
                                      edgecov(networks[[2]]$dislike[[13]])+
                                      edgecov(networks[[2]]$dislike_2[[13]])+
                                      edgecov(networks[[2]]$shared_dislike[[13]])+
                                      edgecov(networks[[2]]$othersscorn[[13]])+
                                      edgecov(networks[[2]]$otherslookup[[13]]),
                                    burn.in=1000,main.iters=5000,gamma=0.30,
                                    prior.mean=c(rep(0,13)),prior.sigma=diag(5,13))

### TIME 3 ###
# classroom 2100 (no gender & no sinks)
set.seed(0708)
bergm_results[[3]][[2]] <- bergmM(networks[[3]]$gossip[[2]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[3]]$dislike[[2]])+
                                     edgecov(networks[[3]]$dislike_2[[2]])+
                                     edgecov(networks[[3]]$shared_dislike[[2]])+
                                     edgecov(networks[[3]]$othersscorn[[2]])+
                                     edgecov(networks[[3]]$otherslookup[[2]]),
                                   burn.in=1000,main.iters=5000,gamma=0.33,
                                   prior.mean=c(rep(0,13)),prior.sigma=diag(5,13))
# classroom 5100 (no gender)
set.seed(0708)
bergm_results[[3]][[3]] <- bergmM(networks[[3]]$gossip[[3]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     #nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeifactor('roma')+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[3]]$dislike[[3]])+
                                     edgecov(networks[[3]]$dislike_2[[3]])+
                                     edgecov(networks[[3]]$shared_dislike[[3]])+
                                     edgecov(networks[[3]]$othersscorn[[3]])+
                                     edgecov(networks[[3]]$otherslookup[[3]]),
                                   burn.in=1000,main.iters=5000,gamma=0.35,
                                   prior.mean=c(rep(0,14)),prior.sigma=diag(5,14))
# classroom 6100 (no ethnicity)
set.seed(0708)
bergm_results[[3]][[4]] <- bergmM(networks[[3]]$gossip[[4]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[3]]$dislike[[4]])+
                                     edgecov(networks[[3]]$dislike_2[[4]])+
                                     edgecov(networks[[3]]$shared_dislike[[4]])+
                                     edgecov(networks[[3]]$othersscorn[[4]])+
                                     edgecov(networks[[3]]$otherslookup[[4]]),
                                   burn.in=1000,main.iters=5000,gamma=0.28,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 6200 (no ethnicity)
set.seed(0708)
bergm_results[[3]][[5]] <- bergmM(networks[[3]]$gossip[[5]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[3]]$dislike[[5]])+
                                     edgecov(networks[[3]]$dislike_2[[5]])+
                                     edgecov(networks[[3]]$shared_dislike[[5]])+
                                     edgecov(networks[[3]]$othersscorn[[5]])+
                                     edgecov(networks[[3]]$otherslookup[[5]]),
                                   burn.in=1000,main.iters=5000,gamma=0.22,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))
# classroom 6400 (no ethnicity)
set.seed(0708)
bergm_results[[3]][[6]] <- bergmM(networks[[3]]$gossip[[6]]~
                                     edges+mutual+
                                     gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
                                     odegree(d=0)+ 
                                     dgwdsp(decay=log(2),fixed=TRUE,type='OTP')+
                                     nodeofactor('gender')+nodeifactor('gender')+nodematch('gender',diff=FALSE)+
                                     nodeocov('popular')+nodeocov('popular2')+
                                     edgecov(networks[[3]]$dislike[[6]])+
                                     edgecov(networks[[3]]$dislike_2[[6]])+
                                     edgecov(networks[[3]]$shared_dislike[[6]])+
                                     edgecov(networks[[3]]$othersscorn[[6]])+
                                     edgecov(networks[[3]]$otherslookup[[6]]),
                                   burn.in=1000,main.iters=5000,gamma=0.22,
                                   prior.mean=c(rep(0,16)),prior.sigma=diag(5,16))

########################################################################################################################

# Acceptance rate
for(wave in seq_along(bergm_results)){
  for(room in seq_along(bergm_results[[wave]])){
    print(bergm_results[[wave]][[room]]$AR)
  }
}

# Save image
save.image('bergm_results.RData')

########################################################################################################################

# Bayesian GOF
bayesian_gof <- vector('list',length(networks))
names(bayesian_gof) <- names(networks)
for(room in c(2,3,5,6,8,9)){
  jpeg(filename=paste('gof_t1_',room,'.jpeg'),width=7,height=5,units='in',res=500)
  bayesian_gof[[1]][[room]] <- bgof(bergm_results[[1]][[room]])
  dev.off()
}
for(room in c(1,7,9,10,11,13)){
  jpeg(filename=paste('gof_t2_',room,'.jpeg'),width=7,height=5,units='in',res=500)
  bayesian_gof[[2]][[room]] <- bgof(bergm_results[[2]][[room]])
  dev.off()
}
for(room in c(2:6)){
  jpeg(filename=paste('gof_t3_',room,'.jpeg'),width=7,height=5,units='in',res=500)
  bayesian_gof[[3]][[room]] <- bgof(bergm_results[[3]][[room]])
  dev.off()
}
