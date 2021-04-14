##
## TEST TraMineR
##
library(TraMineR)
library(DescTools)

data("mvad")


mvad.lab <- c("Employment", "Further education", "Higher education",
              "Joblessness", "School", "Training")

mvad.shortlab <- c("EM", "FE", "HE", "JL", "SC", "TR")


mvad.seq <- seqdef(mvad, 17:86, states = mvad.shortlab, labels = mvad.lab)

subm.custom <- matrix(c(0, 1, 1, 2, 1, 1, 1, 0, 1, 2,
                        1, 2, 1, 1, 0, 3, 1, 2, 2, 2, 3, 0, 3, 1, 1, 1, 1,
                        3, 0, 2, 1, 2, 2, 1, 2, 0), 
                      nrow = 6, ncol = 6, byrow = TRUE,
                      dimnames = list(mvad.shortlab, mvad.shortlab))
subm.custom
mvad.dist.custom <- seqdist(mvad.seq, method = "OM",
                            indel = 1.5, sm = subm.custom)






## GAMMAS

  measure <- 'gamma'
  dat <- model[[measure]]
  df <- data.frame()
  for (t in 1:length(dat)) {
    dft <- melt(dat[[t]], varnames = c('action1','action2'), value.name = measure)
    # dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
    df <- rbind(df, dft)
  }
  # names(df)[which(names(df)=='L1')] <- 'firm'  ## list name at Level 1 is the firm name; rename column 'L1' to firmname
  file <- sprintf('%s-%s.csv', measure,ts)
  write.csv(df, file=file, row.names = F)
  files <- c(files, file)


  
  library(TraMineR)
  library(reshape2)
  library(tidyverse)
  library(ggpubr)


  plots <- list()
  
  
  measure <- 'distance'
  for (i in 1:length(model[[measure]])) {
    dflong <- melt(model[[measure]][[i]], varnames = c('firm1','firm2'), value.name = 'distance')
    plt <- ggplot(data = dflong, aes(x = firm1, y = firm2)) +
      geom_tile(aes(fill = distance))  + 
      geom_text(aes(label = round(distance, 1)), colour='#FFFFFF') +
      scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
      ggtitle(sprintf('%s: period %s',measure,i))
    plots[[length(plots)+1]] <- plt
  }
  
  
  measure <- 'motif'
  dat <- model[[measure]]
  dflong <- data.frame()
  for (t in 1:length(dat)) {
    dft <- model[[measure]][[t]]
    dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
    dflong <- rbind(dflong, dft)
  }
  plt <- ggplot(data = dflong, aes(x = as.integer(period), y = motif, colour=as.factor(firm))) +
    geom_line()  + 
    geom_point() +
    ggtitle(sprintf('%s',measure)) + xlab('Period') + theme_bw()
  plots[[length(plots)+1]] <- plt
  
  ggarrange(plotlist = plots, ncol=3, nrow = 2)


# KWgamma <- function(x){
#   
# }
## as.character( model$analysis_alphabet$x$action )
# as.character( model$analysis_alphabet$x$firm )
# seqDef2conDistPairsMat <- function(x, alphabet, firms) {
#   
#   alph <- as.character(alphabet)
#   
#   
# } #x x = seqdef

seqdefs <- list()

df <- read.csv('sequencer_test_data.csv', header = T, sep=',', fill=TRUE, stringsAsFactors=F)
alphabet <- read.csv('test_alphabet.csv', header = T, sep=',', fill=TRUE, stringsAsFactors=F)
sm <- read.csv('test_subcostmat.csv', header = T, row.names = 1, sep=',', fill=TRUE, stringsAsFactors=F)
sm <-as.matrix(sm)
storage.mode(sm) <- "numeric"
periods <- 1:3

pd <- 2
t <- 2

actionAlphabet <- as.character(alphabet[['action']])
periodCol <- 'period'
tidx <- which(df[,periodCol] == pd)
t.df <- df[tidx, ]
t.l <- longDf2SeqList(t.df, firms, 'firm', 'action')
t.ldf <- seqList2Df(t.l)
right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
seqdefs[[pd]] <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)


tlagl <- list(t=as.character(unlist(seqdefs[[t]][1,])), 
              t1=as.character(unlist(seqdefs[[t-1]][1,])))
tlagseq <- seqList2Df(tlagl)
seqdeflag <- seqdef(tlagseq, alphabet=actionAlphabet, right=right)
seqdist(seqdeflag,  method='OM', sm=sm)

##--------------
seqlaglist <- list()
for (i in 1:length(firms)) {
  firm <- firms[i]
  lagl <- lapply(seqdefs, function(x) as.character(unlist(x[i,])))
  seqlaglist[[firms]] <-  seqList2Df(lagl)
}



firms <- as.character(firms)


seqdefs.t <- model$seqdefs[[1]]



firmidx <- 1
seqx <- unname(sapply(x[firmidx,],function(y)as.character(y)))
alph <- as.character(alphabet)

##
# GET input table for pairwiseGamma()
#   output [matrix] 2xN binary = 1 if equals row name, else 0
#   EX: 
#     seqx = c( 'A , 'A', 'B', 'A')
#     a = 'A'; 
#     b = 'B'
#     getTable4GKgamma( seqx, a, b ) -->
#       outpur: 
#             [,1] [,2] [,3] [,4]
#       [1,]    1    1    0    1
#       [2,]    0    0    1    0
##
getTable4GKgamma <- function(seqx, a, b) {
  z <- matrix(c(  
    seqx == a, 
    seqx == b 
  ), nrow=2, byrow = T)
  return( z * 1 ) #convert bool to integer
}

##
# GET Pairwise Gamma
#   for `seqx` sequence of actions (a character vector)
##
pairwiseGammaMatrix <- function(seqx, alphabet, na.fill=NA) {
  ## GET 1 firms sequence pairwise gamma table
  mat <- matrix(NA,nrow=length(alph), ncol=length(alph),
                dim=list(alph, alph))
  for (i in 1:length(alph)) {
    a <- alph[i]
    for (j in 1:length(alph)) {
      if (i == j) 
        next
      b <- alph[j]
      gammainput<- getTable4GKgamma(seqx, a, b)
      ## CHECK if any row is empty (one action type not in seq)
      mat[i,j] <- if (all(rowSums(gammainput) > 0 )) {
        GoodmanKruskalGamma(gammainput)
      } else {
        NA
      }
    }
  }
  if (all(!is.na(na.fill))) {
    mat[is.na(mat)] <- na.fill
  }
  return(mat)
}


#a
#b
#seqx

gammas <- list()
pd
gammas[[pd]] <- list()
for (i in 1:length(firms)) {
  firm <- firms[i]
  gammas[[pd]][[firm]] <- pairwiseGammaMatrix(seqdefs[[t]][i,], actionAlphabet, na.fill=0)
}


# gammainput<- getTable4GKgamma(seqx, a, b)
# ConDisPairs(gammainput)

pwgamma <- pairwiseGammaMatrix(seqx, alph, na.fill = 0)

mean(pwgamma, na.rm = T)


## the precedence score for a given element type is calculated as
## the mean of all of its pair-wise gamma values 
## (Holmes, 1995; Holmes and Sykes, 1993).
precedence <- rowMeans(pwgamma, na.rm = T)

## Motif
## determine the extent to which the entire sequence 
## exhibits internal structuredness, we calculated 
## **the variance of the averages in the pair-wise precedence scores 
## across all action types. Averaging across action types indicates 
## the relative position—beginning or end—in the overall sequence 
## in which the particular type of action appears. 
## Variance in average precedence scores captures the ordinal specificity 
## and stability of elements in a sequence.
seqmotif <- var( precedence, na.rm = T)


## separation score for a given type of element is calculated as 
## the mean of the absolute value of its pair-wise gamma values 
## (Holmes, 1995; Holmes and Sykes, 1993). 
separation <- rowMeans(abs(pwgamma), na.rm = T)

## Grouping
## To calculate the extent to which the entire sequence exhibits grouping, 
## we calculated the mean of the separation scores across all action types.
## We reversed the direction of the scores (multiplying them by −1.0) 
## so that higher scores indicate higher levels of grouping 
## among actions in a sequence.
seqgrouping <- mean( separation, na.rm = T) * -1



## DEBUG predictabilities

seqdefs <- model$seqdefs

## loop over firms  (per period)
seqlaglist <- list()
# for (i in 1:length(firms)) {
#   firm <- firms[i]
#   for (t in 1:length(periods)) {
#     seqlaglist[[firm]] <- lapply(seqdefs, function(x) {
#       xseq <- as.character(unlist( x[i,]))
#       return( xseq[which(xseq %in% actionAlphabet)] )
#     })
#   }
#   names(seqlaglist[[firm]]) <- periods
#   tlagseq <- seqList2Df(seqlaglist[[firm]])
#   seqdeflag <- seqdef(tlagseq, alphabet=actionAlphabet, right=right)
#   predictabilities[[firm]] <- seqdist(seqdeflag,  method=method, norm=norm, sm=sm)
# }


for (i in 1:length(firms)) {
  firm <- firms[i]
  for (t in 1:length(periods)) {
    seqlaglist[[firm]] <- lapply(seqdefs, function(x) {
      xseq <- as.character(unlist( x[i,]))
      return( xseq[which(xseq %in% actionAlphabet)] ) #remove filled-in "%" for empty levels
    })
  }
  # names(seqlaglist[[firm]]) <- periods
  tlagseq <- seqList2Df(seqlaglist[[firm]])
  seqdeflag <- seqdef(tlagseq, alphabet=actionAlphabet, right=right)
  predictabilities[[firm]] <- seqdist(seqdeflag,  method=method, norm=norm, sm=sm)
  dimnames(predictabilities[[firm]]) <- list(periods, periods)
}


## check output predictabilities
if ('predictabilities' %in% measures) {
  measure <- 'predictabilities'
  dat <- model[[measure]]
  df <- data.frame()
  for (i in 1:length(dat)) {
  	firm <- firms[i]
  	dft <- melt(dat[[t]], varnames = c('period1','period2'), value.name = measure)
  	dft$firm <- firm # ifelse(length(names(dat))>0, names(dat)[t], t)
  	df <- rbind(df, dft)
  }
  # names(df)[which(names(df))=='L1'] <- 'firm'  ## list name at Level 1 is the firm name; rename column 'L1' to firmname
  file <- sprintf('%s-%s.csv',measure,ts)
  write.csv(df, file=file, row.names = F)
  files <- c(files, file)
}




measures <- c('motifs','distances','precedneces')

write.table(data.frame(time=systime, measures=paste(measures, collapse=', ')), file = 'text_write_table.txt')


.checkInput <- function()
{
  sequencer_wd <- 'C:\\Users\\steph\\Google Drive\\Rapp\\SequenceR\\R-Portable'
  input_file <- file.path(sequencer_wd,'tmp_sequencer_input.rds')
  if(file.exists(input_file)) {
    input <- readRDS(input_file)
    print(str(input))
    print(input)
    return(input)
  } else {
    print("INPUT NOT EXIST")
    return(list())
  }
}
.checkModel <- function()
{
    sequencer_wd <- 'C:\\Users\\steph\\Google Drive\\Rapp\\SequenceR\\R-Portable'
  model_file <- file.path(sequencer_wd,'tmp_sequencer_data_model.rds')
  if(file.exists(model_file)) {
    model <- readRDS(model_file)
    print(str(model))
    print(model)
    return(model)
  } else {
    print("MODEL NOT EXIST")
    return(list())
  }
}
model <- .checkModel()
summary(model)
print(model$analysis_dist_settings)
input <- .checkInput()


# mvad.dist.custom <- seqdist(mvad.seq, method = "OM",
#                             indel = 1.5, sm = subm.custom)

"latin1","UTF-8","UTF-8-BOM","UCS-2LE", "UTF-16LE", "UCS-2", "UTF-16"


settings <- model$analysis_dist_settings
dat <- model$analysis_data$x

sm <- model$analysis_subcostmat$x
storage.mode(sm) <- "numeric"


# xseqdef <- seqdef(dat, 1:ncol(dat))
# xdist <- seqdist(xseqdef, 
#                  method = settings$method,
#                  indel = settings$indel, 
#                  norm = settings$norm,
#                  sm = model$analysis_subcostmat$x
# )

# dcast(dat, firm ~ period)


periods <- unique(dat$period)

t<- 1
dat.t <- dat[dat$period==periods[t],]
firms <- unique(dat$firm)

##
# Create list of sequences by `seqname` attribute (e.g., firm)
# for sequence listed in `varname` column (e.g., action)
##
longDf2SeqList <- function(df, seqnames, seqname='firm', varname='action')
{
  l <- list()
  for (i in 1:length(seqnames)) {
    seq_i <- as.character(seqnames[i])
    rows <- which(df[,seqname]==seq_i)
    l[[seq_i]] <- as.character(df[rows, varname])
  }
  return(l)
}


##
# Transform named list of sequences to 1 dataframe of sequences rows
#   paddings NAs at end to match seq lengths 
##
seqList2Df <- function(l)
{
  maxlen <- max(sapply(l,length))
  for (i in 1:length(l)) {
    xi <- c(l[[i]], rep(NA, maxlen - length(l[[i]])) )
    if (i == 1) {
      ldf <- data.frame(xi)
      names(ldf) <- names(l)[i]
    } else {
      ldf <- cbind(ldf, xi)
      names(ldf)[i] <- names(l)[i]
    }
  }
  return(t(ldf))
}

l <- longDf2SeqList(dat.t, 'firm', 'action')
ldf <- seqList2Df(l)

alphabet <- LETTERS[1:10]
right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
xseq <- seqdef(ldf, alphabet = alphabet, 
               right=right)
plot(xseq)

xd <- seqdist(xseq, method = 'OM', sm = sm)
dimnames(xd) <- list(names(l), names(l))
xd










model <- .checkModel()
summary(model)


dat <- model$analysis_data$x
alphabet <- model$analysis_alphabet$x
method <- model$analysis_dist_settings$method
indel <- model$analysis_dist_settings$indel
norm <- model$analysis_dist_settings$norm
sm <- model$analysis_subcostmat$x

actionCol <- 'action'
periodCol <- 'period'
firmCol <- 'firm'

right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'

periods <- unique(dat[,periodCol])
firms <- as.character(alphabet[[firmCol]])
actionAlphabet <- as.character(alphabet[[actionCol]])

tlist <- list() # period list
for (t in 1:length(periods))
{
  pd <- periods[t]
  tidx <- which(dat[,periodCol] == pd)
  t.dat <- dat[tidx, ]
  t.l <- longDf2SeqList(t.dat, firms, 'firm', 'action')
  t.ldf <- seqList2Df(t.l)
  t.xseqdef <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)
  # plot(t.xseqdef)
  t.xdist <- seqdist(t.xseqdef, 
                     method = method, indel = indel, norm = norm, sm = sm)
  # dimnames(t.xdist) <-  list(names(firms), names(firms))
  tlist[[pd]] <- t.xdist
}

model$tlist <- tlist
model$analysis_run <- 'ANALYSIS RUN COMPLETED'



model <- .checkModel()
summary(model)

