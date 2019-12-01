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




sequencer_wd <- 'C:\\Users\\steph\\Google Drive\\Rapp\\SequenceR\\R-Portable'
model_file <- file.path(sequencer_wd,'tmp_sequencer_data_model.rds')
if(file.exists(model_file)) {
  model <- readRDS(model_file)
  print(model)
} else {
  print("MODEL NOT EXIST")
}





