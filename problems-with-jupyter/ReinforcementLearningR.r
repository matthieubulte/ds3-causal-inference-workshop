
library(hash)

## The Game

game <- function(player1, player2, silent = FALSE){
  gameState <- rep(0,9)
  
  while(evaluateGameState(gameState)==42){
    movePlus1 <- do.call(player1, list(gameState))$move
    gameState[movePlus1] <- 1
    if(evaluateGameState(gameState)==42){
      moveMinus1 <- do.call(player2, list(-gameState))$move
      gameState[moveMinus1] <- -1
    }
  }
  evGS <- evaluateGameState(gameState)
  if(evGS==0){
    do.call(paste(player1, ".draw", sep = ""), list())
    do.call(paste(player2, ".draw", sep = ""), list())
    totalResults[totalGames] <<- 0 
    res <- 0 
    if(!silent){
      cat("Draw!")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  if(evGS==1){
    res <- 1 
    do.call(paste(player1, ".win", sep = ""), list())
    do.call(paste(player2, ".loss", sep = ""), list())
    totalResults[totalGames] <<- 1 
    if(!silent){
      print("The winner is player 1")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  if(evGS==-1){
    res <- -1 
    totalResults[totalGames] <<- -1 
    do.call(paste(player1, ".loss", sep = ""), list())
    do.call(paste(player2, ".win", sep = ""), list())
    if(!silent){
      print("The winner is player -1")
      cat("\n\n")
      cat("Result:")
      cat("\n\n")
      print(matrix(gameState,3,3,byrow=T))
    }
  }
  return(res)
}

## gamestate

evaluateGameState <- function(gameState, permut = (1:9)){
  gameState <- gameState[permut]
  gameStateMat <- matrix(gameState, 3, 3, byrow = T)
  rSums <- rowSums(gameStateMat)
  cSums <- colSums(gameStateMat)
  dSums <- c(sum(gameState[c(1,5,9)]), sum(gameState[c(3,5,7)]))
  sums  <- c(rSums, cSums, dSums)
  if(any(sums==3)) return(1)
  else if(any(sums==-3)) return(-1)
  else if(any(gameState==0)) return(42)
  else return(0)
}



## players
    
######################## playerRandom ########################
# chooses randomly among empty fields
playerRandom <- function(gameState){
  move <- sample((1:9)[which(gameState==0)],1) 
  return(list(move = move)) 
}
    
playerRandom.draw <- function(){
  historyRandom$n <<- historyRandom$n + 1 
  historyRandom$results[historyRandom$n] <<- 0 
}
playerRandom.win <- function(){
  historyRandom$n <<- historyRandom$n + 1 
  historyRandom$results[historyRandom$n] <<- 1 
}
playerRandom.loss <- function(){
  historyRandom$n <<- historyRandom$n + 1 
  historyRandom$results[historyRandom$n] <<- -1 
}


######################## playerLeft ########################
# chooses the leftmost empty field
playerLeft <- function(gameState){ 
  move <- which.min(abs(gameState)) 
  return(list(move = move)) 
}

playerLeft.draw <- function(){
  historyLeft$n <<- historyLeft$n + 1 
  historyLeft$results[historyLeft$n] <<- 0 
}
playerLeft.win <- function(){
  historyLeft$n <<- historyLeft$n + 1 
  historyLeft$results[historyLeft$n] <<- 1 
}
playerLeft.loss <- function(){
  historyLeft$n <<- historyLeft$n + 1 
  historyLeft$results[historyLeft$n] <<- -1 
}


historyRandom <- list(n = 0, results = NA, games = list())
historyLeft <- list(n = 0, results = NA, games = list())
totalGames <- 0
totalResults <- c()

#########################
## Play PlayerLeft against PlayerRandom
## fill in 
#########################

# updated playerRandom
playerRandom <- function(gameState){ 
  probmass <- rep(0,9) 
  probmass[gameState == 0] <- 1/sum(gameState == 0) 
  move <- sample(1:9, size=1, prob = probmass) 
  ngames <- historyRandom$n + 1 
  if(sum(gameState==0)>7){  
    # There is at most 1 occupied field. It is thus playerRandom's first turn.
    # We initialize entry number 'ngames' in historyRandom$games.
    # historyRandom$games contains information on the current gamestate, 
    # the move taken, and the probability with which it was taken.
    historyRandom$games[[ngames]] <<- c(gameState, move, probmass[move]) 
  }else{ 
    # We update entry number 'ngames' in historyRandom
    historyRandom$games[[ngames]] <<- rbind(historyRandom$games[[ngames]], c(gameState, move, probmass[move]))
  }
  return(list(move = move))
}

# 10000 games between PlayerRandom and PlayerLeft
historyRandom <- list(n = 0, results = NA, games = list())
set.seed(1)
numGames <- 10000
player1 <- "playerRandom" 
player2 <- "playerLeft" 

totalResults <- c()
totalGames <- 1

for(i in 1:numGames){
  game(player1, player2, silent = TRUE)  
  totalGames <- totalGames +1
}

# play 10000 games
plot((1:numGames) - cumsum(totalResults), xlim = c(0,numGames), 
     ylim = c(0,numGames), ylab = "games not won", type = "l")

# average reward
mean(historyRandom$results)

#### compute expected reward under strategy of playerLeft 

weights <- rep(1,historyRandom$n) # initializing weights
for(i in 1:historyRandom$n){
  currentgame <- historyRandom$games[[i]]
  # update weights, i.e., compute the fraction in the i'th term of the sum in (3)
  for(j in 1:(dim(currentgame)[1])){ 
    #########################
    ## fill in
    ######################### 
  }
}

# estimate for expected reward under the strategy of playerLeft
mean(weights*historyRandom$results)

######################## playerLearn ########################
## updates strategy based on experience
playerLearn <- function(gameState){ 
  if(is.null(strategyLearn[[toString(gameState)]])){ 
    # if gamestate has not been seen yet,
    # initialize strategy by uniform
    probmass <- rep(0,9) 
    probmass[gameState == 0] <- 1/sum(gameState == 0) 
    strategyLearn[[toString(gameState)]] <<- log(probmass) 
  } else { 
    # otherwise use current paramters in strategyLearn
    # and compute strategy from Equation (2)
    probmass <- exp(strategyLearn[[toString(gameState)]])/sum(exp(strategyLearn[[toString(gameState)]])) 
  }
  move <- sample(1:9, size=1, prob = probmass) 
  ngames <- historyLearn$n + 1 
  if(sum(gameState==0)>7){  
    historyLearn$games[[ngames]] <<- c(gameState, move, probmass[move]) 
  }else{ 
    historyLearn$games[[ngames]] <<- rbind(historyLearn$games[[ngames]], c(gameState, move, probmass[move]))
  }
  return(list(move = move))
}

playerLearn.draw <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- 0
  playerLearn.update()
}
playerLearn.win <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- 1
  playerLearn.update()
}
playerLearn.loss <- function(){
  historyLearn$n <<- historyLearn$n + 1
  historyLearn$results[historyLearn$n] <<- -1
  playerLearn.update()
}

playerLearn.update <- function(){
  stepsize <- lambda/historyLearn$n # decrease stepsize with time
  
  # compute gradient
  if((totalGames >  waitUntilStep) && ((totalGames %% doStepEvery) == doStepEvery-1)){ # update strategy
    gradientLearn <- copy(strategyLearn)
    .set(gradientLearn, keys(gradientLearn), rep(list(rep(0,9)),length(keys(gradientLearn))))
    for(i in 1:historyLearn$n){
      gamee <- historyLearn$games[[i]]
      gamee
      wup <- 1
      wdown <- 1
      
      for(j in 1:dim(gamee)[1]){
        gs <- gamee[j,1:9]
        
        # wup looks at current probabilities. 
        ac = gamee[j,10]
        wup <- wup * exp(strategyLearn[[toString(gs)]][ac])/sum(exp(strategyLearn[[toString(gs)]]))
        # wdown looks at probabilities, under which the action was decided. 
        wdown <- wdown * gamee[j,11]
      }
      if(wup > 0){ #if games have zero prob. they are disregarded.
        for(j in 1:dim(gamee)[1]){
          # get hashed game state
          gs <- gamee[j,1:9]
          ac <- gamee[j,10]
          gradientLearn[[toString(gs)]][ac] = gradientLearn[[toString(gs)]][ac] + 
            historyLearn$results[i] * wup/wdown * (1 - exp(strategyLearn[[toString(gs)]][ac])/sum(exp(strategyLearn[[toString(gs)]])))
          others <- setdiff(which(strategyLearn[[toString(gs)]] > -Inf), ac)
          gradientLearn[[toString(gs)]][others] = gradientLearn[[toString(gs)]][others] - 
            historyLearn$results[i] * wup/wdown * exp(strategyLearn[[toString(gs)]][others])/sum(exp(strategyLearn[[toString(gs)]]))
        }
      }
    }
      
    # gradient step 
    for(i in keys(strategyLearn)){
      # compute update of i'th entry in strategyLearn
      # store result in object 'newVector'
      
      ######################################
      ## change the following assignment
      newVector <- rep(1,9)
      ######################################
        
      newVector <- newVector - mean(newVector[newVector > -Inf])
      if(max(newVector) > 20){
        newVector <- newVector * 20 / max(newVector)
      }
      .set(strategyLearn, i, newVector)
    }
  }
  
  # clear history to keep only the last "keepMin" games
  if((totalGames > keepMin) && ((totalGames %% clearEvery) == (clearEvery - 1))){ #clear history
    historyLearn$games <<- historyLearn$games[(historyLearn$n - keepMin + 1):historyLearn$n]
    historyLearn$results <<- historyLearn$results[(historyLearn$n - keepMin + 1):historyLearn$n]
    historyLearn$n <<- keepMin
  }
}

strategyLearn <- hash()
historyLearn <- list(n = 0, results = NA, games = list())

waitUntilStep <- 500
clearEvery <- 500
keepMin <- 500
numGames <- 10000
player1 <- "playerLearn" 
player2 <- "playerRandom" 
totalResults <- c()
totalGames <- 1

########################################
# adjust values for lambda and doStepEvery
lambda <- 20
doStepEvery <- 1000
########################################

set.seed(20190623)
for(i in 1:numGames){
  game(player1, player2, silent = TRUE)  
  totalGames <- totalGames +1
}

# plot over performance
plot((1:numGames) - cumsum(totalResults), xlim = c(0,numGames), 
     ylim = c(0,numGames), ylab = "games not won", type = "l")

# percentage of last 1000 games that have been won
mean(tail(historyLearn$results,1000)==1)

strategyLearn[[toString(c(0,0,0,0,0,0,0,0,0))]] # largest entry 5
strategyLearn[[toString(c(-1,0,0,0,1,0,0,0,0))]] # largest entry 4
strategyLearn[[toString(c(-1,0,0,1,1,0,0,-1,0))]] # largest entry 6 -> WIN
