### prolegomena
Utterances <- c("every-not", "none", "some", "all") ### set of possible utterances

### quds and qud priors
QUDs <- c("how-many?", "all?", "none?")
QUDPriors <- c(1/3, 1/3, 1/3)
QUDPriors <- QUDPriors / sum(QUDPriors) ### normalisation
names(QUDPriors) <- QUDs

### worlds and world priors
Worlds <- c(0, 1, 2)
WorldPriors <- c(0.25, 0.25, 0.25)
WorldPriors <- WorldPriors / sum(WorldPriors) ### normalisation
names(WorldPriors) <- Worlds

### scopes and scope priors
Scopes <- c("surface", "inverse")
ScopePriors <- c(0.5, 0.5)
ScopePriors <- ScopePriors / sum(ScopePriors) ### normalisation
names(ScopePriors) <- Scopes

### alpha value reflecting optimality
alpha <- 1

### this function partitions based on the qud
QUDFunction <- function(qud, meaning) {
  Output <- meaning / sum(meaning)
  if(qud == "how-many?") {
    Output <- Output ### keep the meaning function
  } else if(qud == "all?") {
    Output <- c(sum(Output[1:2]), Output[3])
    names(Output) <- c("0,1", 2) ### partition all vs. not all
  } else if(qud == "none?") {
    Output <- c(Output[1], sum(Output[2:3])) 
    names(Output) <- c(0, "1,2") ### partition none vs. not none
  }
  return(Output)
}

Lexicon <- function(utterance, scope) {
  ### the null message is always true
  if(utterance == "null") {
    Meaning <- sapply(Worlds, function(x) TRUE)
  }
  else if (utterance == "every-not") {
    ### the surface scope is only true if world is 0
    if(scope == "surface") {
      Meaning <- sapply(Worlds, function(x) x == 0)
    ### the inverse scope is true if world is not 4
    } else if(scope == "inverse") {
      Meaning <- sapply(Worlds, function(x) x < max(Worlds))
    }
  }
  else if(utterance == "none") {
    Meaning <- sapply(Worlds, function(x) x == 0)
  }
  else if(utterance == "some") {
    Meaning <- sapply(Worlds, function(x) x > 0)
  }
  else if(utterance == "all") {
    Meaning <- sapply(Worlds, function(x) x == max(Worlds))
  }
  names(Meaning) <- Worlds
  return(Meaning)
}

LitListener <- function(utterance, scope, qud) {
  interpretation <- Lexicon(utterance, scope) ### get the meaning of the utterance
  output <- QUDFunction(qud, interpretation) ### partition depending on the qud
  output <- output / sum(output) ### normalise
  return(output)
}

Speaker <- function(world, scope, qud) {
  effects <- sapply(Utterances, function(x) LitListener(x, scope, qud)) ### get the listener's interpretation of each utterance
  effects <- effects[rownames(effects)[grepl(world, rownames(effects))],] ### select the row corresponding to the world state
  effects <- exp(alpha * log(effects)) ### softmax
  effects <- effects / sum(effects) ### normalise
  return(effects)
}

Listener <- function(utterance) {
  ### determine for each world, scope, and qud the probability that the speaker would produce that utterance
  worldOutput <- sapply(Worlds, function(x)
                  sapply(Scopes, function(y)
                    sapply(QUDs, function(z)
                      ### apparently consideration of priors only occurs here
                      Speaker(x, y, z)[utterance] * WorldPriors[as.character(x)] * ScopePriors[y] * QUDPriors[z])))

  ### sum over scopes and quds                                                      
  worldOutput <- colMeans(worldOutput)
  names(worldOutput) <- Worlds
  worldOutput <- worldOutput / sum(worldOutput) ### normalise
  
  ### determine for each world, scope, and qud the probability that the speaker would produce that utterance
  scopeOutput <- sapply(Scopes, function(y)
                  sapply(Worlds, function(x)
                    sapply(QUDs, function(z)
                      ### apparently consideration of priors only occurs here
                      Speaker(x, y, z)[utterance] * WorldPriors[as.character(x)] * ScopePriors[y] * QUDPriors[z])))
  
  ### sum over scopes and quds                                                      
  scopeOutput <- colMeans(scopeOutput)
  names(scopeOutput) <- Scopes
  scopeOutput <- scopeOutput / sum(scopeOutput) ### normalise
  
  return(worldOutput)
  
}

PragSpeaker <- function(world) {
  Output <- sapply(Utterances, function(x) Listener(x))[as.character(world),]
  Output <- Output / sum(Output)
  
  return(Output)
}

### the new speaker also takes an alpha argument specifying
### how optimally it behaves --- which is easier than setting the
### alpha outside of the function
NewSpeaker <- function(world, scope, qud, alpha) {
  effects <- sapply(Utterances, function(x) LitListener(x, scope, qud)) ### get the listener's interpretation of each utterance
  effects <- effects[rownames(effects)[grepl(world, rownames(effects))],] ### select the row corresponding to the world state
  effects <- exp(alpha * log(effects)) ### softmax
  effects <- effects / sum(effects) ### normalise
  return(effects)
}

### the new listener takes priors as its arguments
### which is easier than continuously specifying priors outside the function
NewListener <- function(utterance, wpriors, spriors, qpriors, alpha) {
  ScopePriors <- spriors
  ScopePriors <- ScopePriors / sum(ScopePriors)
  names(ScopePriors) <- Scopes
  
  WorldPriors <- wpriors
  WorldPriors <- WorldPriors / sum(WorldPriors)
  names(WorldPriors) <- Worlds
  
  QUDPriors <- qpriors
  QUDPriors <- QUDPriors / sum(QUDPriors)
  names(QUDPriors) <- QUDs
  
  ### determine for each world, scope, and qud the probability that the speaker would produce that utterance
  worldOutput <- sapply(Worlds, function(x)
                  sapply(Scopes, function(y)
                    sapply(QUDs, function(z)
                      ### apparently consideration of priors only occurs here
                      NewSpeaker(x, y, z, alpha)[utterance] * WorldPriors[as.character(x)] * ScopePriors[y] * QUDPriors[z])))

  ### sum over scopes and quds                                                      
  worldOutput <- colMeans(worldOutput)
  names(worldOutput) <- Worlds
  worldOutput <- worldOutput / sum(worldOutput) ### normalise
  
  ### determine for each world, scope, and qud the probability that the speaker would produce that utterance
  scopeOutput <- sapply(Scopes, function(y)
                  sapply(Worlds, function(x)
                    sapply(QUDs, function(z)
                      ### apparently consideration of priors only occurs here
                      Speaker(x, y, z)[utterance] * WorldPriors[as.character(x)] * ScopePriors[y] * QUDPriors[z])))
  
  ### sum over scopes and quds                                                      
  scopeOutput <- colMeans(scopeOutput)
  names(scopeOutput) <- Scopes
  scopeOutput <- scopeOutput / sum(scopeOutput) ### normalise
  
  return(worldOutput)
  
}

NewPragSpeaker <- function(world, wpriors, spriors, qpriors, alpha) {
  ScopePriors <- spriors
  ScopePriors <- ScopePriors / sum(ScopePriors)
  names(ScopePriors) <- Scopes
  
  WorldPriors <- wpriors
  WorldPriors <- WorldPriors / sum(WorldPriors)
  names(WorldPriors) <- Worlds
  
  QUDPriors <- qpriors
  QUDPriors <- QUDPriors / sum(QUDPriors)
  names(QUDPriors) <- QUDs

  Output <- sapply(Utterances, function(x) NewListener(x, wpriors, spriors, qpriors, alpha))[as.character(world),]
  Output <- Output / sum(Output)
  
  return(Output)
}