########################################################################################################################
## GOSSIP IN HUNGARIAN HIGH SCHOOLS
## Data preparation (0)
## R script written by Jose Luis Estevez (University of Linkoping)
## Date: October 19th, 2020
########################################################################################################################

# DATA LOADING 

# Loading of non-relational data (covariates)
rm(list=ls())
std <- read.csv('data/OTKA_student_w1234.csv',header=TRUE,row.names=1,sep=',',fill=FALSE,na.strings='NA')
tch <- read.csv('data/OTKA_teacher_w1234.csv',header=TRUE,row.names=1,sep=',',fill=FALSE,na.strings='NA')

# Loading of relational data (network variables)
names_mtx <- vector('list',4)
for(wave in 1:length(names_mtx)){
  names_mtx[[wave]] <- list.files(paste('data/wave',wave,sep='')) # extraction of file names, per wave
}

ntw <- vector('list',4) 
for(wave in seq_along(names_mtx)){
  for(mtx in seq_along(names_mtx[[wave]])){
    # loading of the matrices as R objects
    ntw[[wave]][[mtx]] <- read.csv(paste('data/wave',wave,'/',names_mtx[[wave]][[mtx]],sep=''),header=TRUE,row.names=1,
                                   sep=',',fill=FALSE,na.strings='NA')
  }
}

# Removal the X in the columns
destroyX <- function(es){
  f <- es
  for(col in c(1:ncol(f))){ # for each column in dataframe
    if (startsWith(colnames(f)[col],'X')==TRUE) { # if starts with 'X'
      colnames(f)[col] <- substr(colnames(f)[col],2,100) # remove it
    }
  }
  assign(deparse(substitute(es)),f,inherits=TRUE) # assign corrected data to original name
}

for(wave in seq_along(ntw)){
  ntw[[wave]] <- lapply(ntw[[wave]],destroyX)
}

# Conversion into matrix objects
for(wave in seq_along(ntw)){
  for(mtx in seq_along(ntw[[wave]])){
    ntw[[wave]][[mtx]] <- as.matrix(ntw[[wave]][[mtx]])
  }
}

# Organisation of the matrices by wave (level 1), item (level 2), and classroom (level 3)
networks <- list(vector('list',15),vector('list',16),vector('list',16),vector('list',15))
names(networks) <- c('wave1','wave2','wave3','wave4')
names(networks$wave1) <- c('affect','nerd','pet','lookup','scorn','handsome','othersview','beat','beaten','gossip',
                           'gossiped','mock','mocked','humiliate','humiliated')
names(networks$wave2) <- c(names(networks$wave1[1:6]),'otherslookup','othersscorn',names(networks$wave1[8:15]))
names(networks$wave3) <- names(networks$wave2)
names(networks$wave4) <- names(networks$wave2[-3])

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    networks[[wave]][[item]] <- ntw[[wave]][seq(item,length(ntw[[wave]]),by=length(networks[[wave]]))]
  }
}

# Naming of the classrooms
for(wave in seq_along(names_mtx)){
  names_mtx[[wave]] <- names_mtx[[wave]][seq(1,length(names_mtx[[wave]]),by=length(networks[[wave]]))]
}

for(wave in seq_along(names_mtx)){
  # classrooms were named with 4 digits
  names_mtx[[wave]] <- unlist(lapply(names_mtx[[wave]],substr,1,4)) 
}

for(wave in seq_along(networks)){
  for(item in seq_along(networks[[wave]])){
    names(networks[[wave]][[item]]) <- names_mtx[[wave]]
  }
}

# Exclusion of relational measures that won't be employed in further analyses 
'%!in%' <- function(x,y)!('%in%'(x,y))
for(wave in seq_along(networks)){
  networks[[wave]] <- networks[[wave]][names(networks[[wave]]) %!in% c('pet','nerd','handsome','beat','beaten','gossiped',
                                                                       'mock','mocked','humiliate','humiliated','lookup',
                                                                       'scorn')]
}

# Remove of objects that will not be used for the analyses
rm(tch);rm(ntw);rm(names_mtx);rm('%!in%');rm(destroyX);rm(item);rm(mtx);rm(wave);rm('X[[i]]')
std <- std[,c('gender_1','gender_2','gender_3','gender_4','ethnic_1','ethnic_2','ethnic_3','ethnic_4','romag1_1',
              'romag1_3','yrbrth_1','yrbrth_2','yrbrth_3','yrbrth_4','mobrth_1','mobrth_2','mobrth_3','mobrth_4',
              'class_1','class_2','class_3','class_4')]

# Save image
save.image('data.RData')
