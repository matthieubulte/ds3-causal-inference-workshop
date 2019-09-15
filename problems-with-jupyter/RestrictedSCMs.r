
library(mgcv)
library(dHSIC)
source("utils.R")

XX <- read.csv('./RestrictedSCMsData1.txt', sep = "\t")
XX <- XX[1:1000,]
Y <- XX[,2]
X <- XX[,1]
plot(X, Y, pch = 19, cex = .8)

load(file = "./RestrictedSCMsData2.RData")             # loads dat

load(file = "./RestrictedSCMsallDagsWith4Nodes.RData") # loads all DAGs as allDags
load(file = "./RestrictedSCMsallFullDagsWith4Nodes.RData") # loads all fully connected DAGs as allFullDags

numDags <- dim(allFullDags)[1]
independence.p.vals <- rep(NA, numDags)
scores <- rep(NA, numDags)
pvals <- array(NA, dim = c(numDags,4,4))

for(j in 1:numDags){
  currentDag <- matrix(allFullDags[j,], 4, 4)
  residual.log.var <- rep(NA, 4)
  residual.matrix <- matrix(NA, dim(dat)[1], dim(dat)[2])
  for(i in 1:4){
      # regress the ith node onto its parents
      # store p-values for regressors and compute log residuals variances
  }
    # test for joint independence of residuals
    # compute score
    
    filledInIndep <- all(!is.na(independence.p.vals))
    filledInScores <- all(!is.na(scores))
}

# Now we can choose the maxima as estimates.
if(filledInIndep){
    print("The independence p-values equal")
    show(independence.p.vals)
    Ghat1.num <- which.max(independence.p.vals)
    Ghat1 <- matrix(allFullDags[Ghat1.num, ], 4, 4)
    print(paste("The estimated DAG using independence is number", Ghat1.num, ". It looks like this:"))
    show(Ghat1[])
    print("Considering only significant edges yields")
    show(Ghat1 * (pvals[Ghat1.num,,] < 0.05))

    print("===")
    print("===")
}

if(filledInIndep){
    print("The scores equal")
    show(scores)
    Ghat2.num <- which.max(scores)
    Ghat2 <- matrix(allFullDags[Ghat2.num, ], 4, 4)
    print(paste("The estimated DAG using scores is number", Ghat2.num, ". It looks like this:"))
    show(Ghat2[])
    print("Considering only significant edges yields")
    show(Ghat2 * (pvals[Ghat2.num,,] < 0.05))

    print("===")
    print("===")
}
