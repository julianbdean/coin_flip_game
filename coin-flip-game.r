## simulate the coinflip problem 10000 times

# Flip a fair coin 100 times — it gives a sequence of heads (H) and tails (T). For each HH in the sequence of flips, Alice gets a point; for each HT, Bob does, so e.g. for the sequence THHHT Alice gets 2 points and Bob gets 1 point. Who is most likely to win?
# Source: https://twitter.com/littmath

## packages
library(tidyverse)

## first, create two tables - one for simulations, and one for scoring

simulations <- data.frame(matrix(nrow = 10000, ncol = 100))
scores_bob <- data.frame(matrix(nrow = 10000, ncol = 100))
scores_bob[,1] <- 0
scores_alice <- data.frame(matrix(nrow = 10000, ncol = 100))
scores_alice[,1] <- 0

### Run the simulations
set.seed(03182024)

for (i in 1:10000){
  simulations[i,] <- rbinom(100,size =1, prob = 0.5)
}

## H = 1, T = 0 arbitrarily

## Create the scores

#bob gets 1 point for HT, Alice gets 1 for HH

for (i in 1:10000){
  for (j in 2:100){
    
    scores_alice[i,j] <- ifelse(
      simulations[i,j] + simulations[i,j-1] == 2, 1, 0
    )
      
    scores_bob[i,j] <- ifelse(
      simulations[i,j] == 0 & simulations[i,j-1] == 1,
      1, 0
    )
  }
}

bob_totals <- rowSums(scores_bob)
alice_totals <- rowSums(scores_alice)

totals <- bind_cols(bob_totals, alice_totals)
colnames(totals) <- c("bob_totals", "alice_totals")


##Plot distribution of scores

totals %>% ggplot() +
  geom_density(aes(x = bob_totals)) + #bob
  geom_density(aes(x = alice_totals), linetype = "dashed") + #alice
  xlab("total points") + ggtitle("Score distribution Bob (solid) vs Alice (dashed)") +
  theme_classic()

## Who wins more?

bob_win_shares <- data_frame(matrix(nrow = 10000, ncol = 3))
bob_win_shares[,1] <- 1:10000
bob_win_shares[,2] <- NA
bob_win_shares[,3] <- NA
colnames(bob_win_shares) <- c("simulation", "bob_wins", "cumulative_share")

for (i in 1:10000){
  bob_win_shares[i,2] <- if_else(
    totals[i,1] > totals[i,2], 1, 0)
}

for (i in 1:10000){
  bob_win_shares[i,3] <- mean(bob_win_shares$bob_wins[1:i])
}

## alice
alice_win_shares <- data_frame(matrix(nrow = 10000, ncol = 3))
alice_win_shares[,1] <- 1:10000
alice_win_shares[,2] <- NA
alice_win_shares[,3] <- NA
colnames(alice_win_shares) <- c("simulation", "alice_wins", "cumulative_share")

for (i in 1:10000){
  alice_win_shares[i,2] <- if_else(
    totals[i,1] < totals[i,2], 1, 0)
}

for (i in 1:10000){
  alice_win_shares[i,3] <- mean(alice_win_shares$alice_wins[1:i])
}

win_shares <- bind_cols(bob_win_shares$cumulative_share, alice_win_shares$cumulative_share)
colnames(win_shares) <- c("bob_win_share", "alice_win_share")
win_shares$simulation <- 1:10000

## Plot number or % of wins by number of simulations

win_shares %>%
  ggplot() +
  geom_line(aes(x = simulation, y = bob_win_share)) +
  geom_line(aes(x = simulation, y = alice_win_share), linetype = 'dashed') +
  ylab("win_share") + ggtitle("Win shares bob (solid) vs Alice (dashed)") +
  theme_classic()


