
library(InvariantCausalPrediction)
library(seqICP)

# Generate n=1000 observations from the observational distribution,
# and store observations in vectors called "Xa" and "Ya"
#########
# fill in
#########

# Generate n=1000 observations from the interventional distribution,
# and store observations in vectors called "Xb" and "Yb"
#########
# fill in
#########

if(exists("Xa") & exists("Xb") & exists("Ya") & exists("Yb")){
    # Plot both samples
    red <- rgb(1,0,0,alpha=0.4)
    blue <- rgb(0,0,1,alpha=0.4)
    # Y vs X1
    plot(Xa,Ya,pch=16,col=blue,xlim=range(c(Xa,Xb)),ylim=range(c(Ya,Yb)),xlab="X",ylab="Y")
    points(Xb,Yb,pch=17,col=red)
    legend("topright",c("observational","interventional"),pch=c(16,17),col=c(blue,red),inset=0.02)
}

load(file = "./InvariantCausalPredictionData1.RData")  # load data

# extract response and predictors
Y <- data[,1]
Xmat  <- data[,2:4]

# define the potential parent sets
S <- list( c(1), c(2), c(3), c(1,2), c(1,3), c(2,3), c(1,2,3))

# perform regression for each set in S
resid <- fitted <- vector("list", length(S))
for(i in 1:length(S)){
  # regress Y linearly on the i'th set S (e.g. using lm.fit)
  # store the residuals in resid[[i]]
  # and the fitted values in fitted[[i]]
  #############
  ## fill in
  #############
}

filledInResid <- all(unlist(lapply(resid, length)) == length(Y))
filledInFitted <- all(unlist(lapply(fitted, length)) == length(Y))

if(filledInResid & filledInFitted){
    # plot the resulting
    env <- c(rep(0,140),rep(1,80))
    par(mfrow=c(2,2))
    red <- rgb(1,0,0,alpha=0.4)
    blue <- rgb(0,0,1,alpha=0.4)
    names <- c("X1", "X2", "X3", "X1, X2", "X1, X3", "X2, X3", "X1, X2, X3")
    # plot Y vs index (empty set)
    plot((1:length(Y))[env==0], Y[env==0], pch=16, col=blue, xlim=c(0,220), ylim=range(Y), xlab="index", ylab="Y", main="empty set")
    points((1:length(Y))[env==1], Y[env==1], pch=17, col=red)
    legend("topleft",c("observational","interventional"),pch=c(16,17),col=c(blue,red),inset=0.02)
    # all remaining potential sets
    for(i in 1:length(S)){
        plot(fitted[[i]][env==0], resid[[i]][env==0], pch=16, col=blue, xlim=range(fitted[[i]]), ylim=range(resid[[i]]), xlab="fitted values", ylab="residuals", main=names[i])
        points(fitted[[i]][env==1], resid[[i]][env==1], pch=17, col=red)
        legend("topleft",c("observational","interventional"),pch=c(16,17),col=c(blue,red),inset=0.02)
    }
}

load(file = "./InvariantCausalPredictionData2.RData")  # load data2
