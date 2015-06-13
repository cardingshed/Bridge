###############################################################################
# Using R to calculate combinations                                           #
#                                                                             #
# Given a bridge hand a=c(s,h,d,c) calculates probability no fit of fit+cards #
# i.e. partner holds b=c(s2,h2,d2,c2) and a[i]+b[i] < fit,                    #
# fit should be >= max(hand[i]) and <= 13.                                    #
# helper function findshapes returns list of possible shapes opposite         #
# helper function handprob calculates probability of a given shape            #
# main function nofit sums all probabilities                                  #
###############################################################################
#  Examples:
#> nofit(c(2,2,4,5),9)
#70 patterns found
#[1] 0.6246847
#> nofit(c(2,2,4,5),8)
#10 patterns found
#[1] 0.1551474
#> nofit(c(3,3,4,3),7)
#0 patterns found
#[1] 0


findshapes <- function(hand,fit){
  # creates a list of all patterns opposite hand that have fit < fit cards
  # max(hand[i]) <= fit <= 13
  cards <- 0:13
  # compute range of all four suits (assumes fit >= max(hand[i]))
  s <- cards[cards<(fit - hand[1])]
  h <- cards[cards<(fit - hand[2])]
  d <- cards[cards<(fit - hand[3])]
  c <- cards[cards<(fit - hand[4])]
  # all combinations, return only hands with 13 cards
  combs <- expand.grid(s,h,d,c)
  shapes <- combs[rowSums(combs)==13,]
  # return list with each element a possible pattern
  shapelist <- as.list(as.data.frame(t(shapes)))
  message( paste(length(shapelist),"patterns found"))
  return (shapelist)
}
handprob <- function(shape,hand){
  # probability of shape c(s,d,h,c) opposite, given you hold hand c(s,d,h,c) 
  specified <- choose(13-hand[1],shape[1])*choose(13-hand[2],shape[2])*
    choose(13-hand[3],shape[3])*choose(13-hand[4],shape[4])
  allpossible <- choose(39,13)
  return (specified/allpossible)
}
nofit <- function(hand,fit){
  # input hand as a vector c(spades,hearts,diamonds,clubs) e.g. c(4,3,3,3)
  # calls findshapes to exhaustively list all possible shapes
  # uses lapply and handprob to find probability for each shape
  # returns sum of probabilities
  shapelist <- findshapes(hand,fit)
  cumulativeprob <- sum(unlist(lapply(shapelist,handprob,hand=hand)))
  return (cumulativeprob)
}

