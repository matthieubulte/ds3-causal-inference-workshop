
library(igraph) #comment out this line if you cannot install igraph 
library(CondIndTests) 
library(dHSIC)
source("./utils.R")

load(file = "./ComputeInterventionsData1.Rdata")

Adj <- rbind(c(0,0,0,1,0,0,0,0,0), c(0,0,1,1,0,0,0,0,0), c(0,0,0,0,0,0,0,1,0), c(0,0,0,0,1,1,0,0,0), 
           c(0,0,0,0,0,0,0,0,0), c(0,0,0,0,0,0,1,1,0), c(0,0,0,0,0,0,0,0,0), c(0,0,0,0,0,0,0,0,1), 
           c(0,0,0,0,0,0,0,0,0))
set.seed(1)
plotGraphfromAdj(Adj, labels = c("C", "A", "K", "X", "F", "D", "G", "Y", "H")) 
#comment out the above line if you cannot install igraph


set.seed(1); n <- 200
C <- rnorm(n)
A <- 0.8*rnorm(n)
K <- A + 0.1*rnorm(n)
X <- C - 2*A + 0.2*rnorm(n)
F <- 3*X + 0.8*rnorm(n)
D <- -2*X + 0.5*rnorm(n)
G <- D + 0.5*rnorm(n)
Y <- 2*K - D + 0.2*rnorm(n)
H <- 0.5*Y + 0.1*rnorm(n)
data.check <- cbind(C, A, K, X, F, D, G, Y, H)

head(data.loaded)
head(data.check)
