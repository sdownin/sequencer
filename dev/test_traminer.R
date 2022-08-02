##
## TEST TraMineR
##
library(TraMineR)

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

