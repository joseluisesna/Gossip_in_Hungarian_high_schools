########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Data tidying (1)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# DATA LOADING
rm(list=ls())
load('data.RData')

########################################################################################################################

# DATA CLEANING AND VARIALBE CREATION

# 1) ATTRIBUTES (gender and ethnicity)
# 1.1) Gender (female 1, male 0)
std$gender_1 <- std$gender_1 - 1
for(i in 2:4){
  std[[paste('gender',i,sep='_')]] <- std$gender_1
}

# 1.2) Roma: whether the student self-identified as Roma or Roma-Hungarian (1) or not (0)
# Dichotomisation of ethnicity as either Roma or not
for(i in seq_along(networks)){
  # Roma and Roma & Hungarian to 1, 0 otherwise
  std[[paste('ethnic',i,sep='_')]][std[[paste('ethnic',i,sep='_')]] %in% c(1,4)] <- 0
  std[[paste('ethnic',i,sep='_')]][std[[paste('ethnic',i,sep='_')]] %in% c(2,3)] <- 1
}

# if a student did not report their ethnicity but reported being member of a Roma group, the ethnicity is Roma
for(i in 1:nrow(std)){
  if(is.na(std$ethnic_1[i]) & !is.na(std$romag1_1[i])){
    std$ethnic_1[i] <- 1
  }
  if(is.na(std$ethnic_3[i]) & !is.na(std$romag1_3[i])){
    std$ethnic_3[i] <- 1
  }
}

# If the student reported at least one time being Roma, her/his ethnicity is always Roma (no matter the wave)
std$roma <- NA
for(i in 1:nrow(std)){
  std$roma[i] <- max(c(std$ethnic_1[i],std$ethnic_2[i],std$ethnic_3[i],std$ethnic_4[i]),na.rm=TRUE)
}
std$roma[std$roma == -Inf] <- NA

for(i in seq_along(networks)){
  std[[paste('roma',i,sep='_')]] <- std$roma
}

########################################################################################################################

# 2) RELATIONAL VARIABLES
# 2.1) Ensure that all classrooms have the same subjects in all the matrices of the same wave
names_nodes <- vector('list',length(networks))
for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    names_nodes[[wave]] <- vector('list',length(networks[[wave]][[item]]))
  }
}

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    for(room in seq_along(networks[[wave]][[item]])){
      names_nodes[[wave]][[room]][[item]] <- rownames(networks[[wave]][[item]][[room]])
    }
  }
}

nodes_class <- vector('list',length(networks))
for(wave in seq_along(names_nodes)){
  for(room in seq_along(names_nodes[[wave]])){
    nodes_class[[wave]][[room]] <- names_nodes[[wave]][[room]][[1]] 
  }
}

for(wave in seq_along(names_nodes)){
  for(room in seq_along(names_nodes[[wave]])){
    for(nodes in 2:length(names_nodes[[wave]][[room]])){
      # the intersection of nodes across all networks in the same wave and classroom
      nodes_class[[wave]][[room]] <- intersect(nodes_class[[wave]][[room]],names_nodes[[wave]][[room]][[nodes]])
    }
  }
}

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    for(room in seq_along(networks[[wave]][[item]])){
      # leave only the nodes in the intersection across all items in the same wave and classroom
      networks[[wave]][[item]][[room]] <-  networks[[wave]][[item]][[room]][nodes_class[[wave]][[room]],
                                                                            nodes_class[[wave]][[room]]] 
    }
  }
}

# 2.2) Relational dimensions: friendship, dislike from affect
dichotomise <- function(mtx,zero,one,na){
  for(i in rownames(mtx)){
    for(j in colnames(mtx)){
      if(mtx[i,j] %in% zero){
        mtx[i,j] <- 0
      } else if(mtx[i,j] %in% one){
        mtx[i,j] <- 1
      } else if(mtx[i,j] %in% na)
        mtx[i,j] <- NA
    }
  }
  return(as.matrix(mtx))
}

for(wave in seq_along(networks)){
  networks[[wave]]$friend <- lapply(networks[[wave]]$affect,dichotomise,zero=-2:1,one=2,na=NA)
  networks[[wave]]$dislike <- lapply(networks[[wave]]$affect,dichotomise,zero=0:2,one=c(-2,-1),na=NA)
  networks[[wave]]$affect <- NULL
}

########################################################################################################################

# 3) ATTRIBUTES: For the non-relational data, split by wave, and select only corresponding students and variables
students <- vector('list',length(networks))
names(students) <- names(networks)

for(wave in seq_along(students)){
  students[[wave]] <- std[rownames(std) %in% unlist(nodes_class[[wave]]),endsWith(names(std),paste(wave))]
}

########################################################################################################################

## IMPUTATION OF MISSING TIES 

# Creation of large matrices with all ties in the same wave (no matter in which classroom exactly)
dataimp <- vector('list',length=length(networks))
names(dataimp) <- names(networks)
for(wave in seq_along(dataimp)){
  dataimp[[wave]] <- vector('list',6)
  names(dataimp[[wave]]) <- c('friend','dislike','otherslookup','othersscorn')
}

for(wave in seq_along(networks)){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in seq_along(networks[[wave]][[item]])){
      dataimp[[wave]][[item]][[room]] <- rownames(networks[[wave]][[item]][[room]])
    }
    dataimp[[wave]][[item]] <- unlist(dataimp[[wave]][[item]])
    dataimp[[wave]][[item]] <- matrix(NA,nrow=length(dataimp[[wave]][[item]]),ncol=length(dataimp[[wave]][[item]]),
                                      dimnames=list(dataimp[[wave]][[item]],dataimp[[wave]][[item]]))
  }
}

for(wave in seq_along(networks)){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in names(networks[[wave]][[item]])){
      for(i in rownames(networks[[wave]][[item]][[room]])){
        for(j in colnames(networks[[wave]][[item]][[room]])){
          networks[[wave]][[item]][[room]][i,j] -> dataimp[[wave]][[item]][i,j]
        }
      }
    }
  }
}

'%!in%' <- function(x,y)!('%in%'(x,y))
for(wave in 2:3){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in names(networks[[wave]][[item]])){
      for(i in rownames(networks[[wave]][[item]][[room]])){
        for(j in colnames(networks[[wave]][[item]][[room]])){
          if(is.na(networks[[wave]][[item]][[room]][i,j])){
            # If information from both previous and posterior wave is available
            if(i %in% rownames(dataimp[[wave-1]][[item]]) & j %in% rownames(dataimp[[wave-1]][[item]]) &
               i %in% rownames(dataimp[[wave+1]][[item]]) & j %in% rownames(dataimp[[wave+1]][[item]])){
              x <- dataimp[[wave-1]][[item]][i,j]
              y <- dataimp[[wave+1]][[item]][i,j]
              if((!is.na(x) & x == 1 & !is.na(y) & y == 1) | 
                 (is.na(x) & !is.na(y) & y == 1) |
                 (!is.na(x) & x == 1 & is.na(y))){
                networks[[wave]][[item]][[room]][i,j] <- 1
              }else{
                networks[[wave]][[item]][[room]][i,j] <- 0
              }
            } 
            # If only information from the previous wave is available
            else if(i %in% rownames(dataimp[[wave-1]][[item]]) & j %in% rownames(dataimp[[wave-1]][[item]]) &
                    (i %!in% rownames(dataimp[[wave+1]][[item]]) | j %!in% rownames(dataimp[[wave+1]][[item]]))){
              networks[[wave]][[item]][[room]][i,j] <- dataimp[[wave-1]][[item]][i,j] 
            } 
            # If only information from the posterior wave is available
            else if((i %!in% rownames(dataimp[[wave-1]][[item]]) | j %!in% rownames(dataimp[[wave-1]][[item]])) &
                    i %in% rownames(dataimp[[wave+1]][[item]]) & j %in% rownames(dataimp[[wave+1]][[item]])){
              networks[[wave]][[item]][[room]][i,j] <- dataimp[[wave+1]][[item]][i,j] 
            }
          }
        }
      }
    }
  }
}

# For wave 4, ties imputed only from from wave 3
for(wave in 4){
  for(item in c('friend','dislike','otherslookup','othersscorn')){
    for(room in names(networks[[wave]][[item]])){
      for(i in rownames(networks[[wave]][[item]][[room]])){
        for(j in colnames(networks[[wave]][[item]][[room]])){
          if(is.na(networks[[wave]][[item]][[room]][i,j])){
            if(i %in% rownames(dataimp[[wave-1]][[item]]) & j %in% rownames(dataimp[[wave-1]][[item]])){
              networks[[wave]][[item]][[room]][i,j] <- dataimp[[wave-1]][[item]][i,j] 
            } 
          }
        }
      }
    }
  }
}

########################################################################################################################

# SAMPLE SELECTION

# Exclusion of wave 1
networks$wave1 <- NULL
students$wave1 <- NULL

# Exclusion of classrooms with MISSING-TIE FRACTION LARGER THAN 20% 
go_missing <- go_density <- go_gossip_resp <- vector('list',length(networks))
for(wave in seq_along(networks)){
  for(room in seq_along(networks[[wave]]$gossip)){
    # extraction of gossip missing-tie fraction per classroom and wave
    go_missing[[wave]][[room]] <- sum(is.na(networks[[wave]]$gossip[[room]])/
                                        (nrow(networks[[wave]]$gossip[[room]])*(nrow(networks[[wave]]$gossip[[room]])-1)))
  }
}

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    networks[[wave]][[item]] <- networks[[wave]][[item]][go_missing[[wave]]<.20]
  }
}

# Exclusion of classrooms with a fewer than 25 ties
for(wave in seq_along(networks)){
  for(room in seq_along(networks[[wave]]$gossip)){
    # extraction of gossip ties per classroom and wave
    go_density[[wave]][[room]] <- sum(networks[[wave]]$gossip[[room]],na.rm=TRUE)
  }
}

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    networks[[wave]][[item]] <- networks[[wave]][[item]][go_density[[wave]]>=25]
  }
}

# Sample selection in the attribute object
for(wave in seq_along(students)){
  students[[wave]] <- students[[wave]][students[[wave]]$class %in% names(networks[[wave]]$gossip),]
}

########################################################################################################################

# Renaming
names(networks) <- names(students) <- c('wave1','wave2','wave3')

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    names(networks[[wave]][[item]]) <- names(networks[[wave]]$gossip)
  }
}

students$wave1 <- students$wave1[c('class_2','gender_2','roma_2')] 
students$wave2 <- students$wave2[c('class_3','gender_3','roma_3')] 
students$wave3 <- students$wave3[c('class_4','gender_4','roma_4')] 
names(students$wave1) <- names(students$wave2) <- names(students$wave3) <- c('class','female','roma')

# Rearrange of the variables
for(wave in seq_along(networks)){
  networks[[wave]] <- networks[[wave]][c('gossip','friend','dislike','otherslookup','othersscorn')]
}

# Removal of unnecessary objects
rm(list=ls()[-c(11,16)])

# Save image
save.image('tidieddata.RData')