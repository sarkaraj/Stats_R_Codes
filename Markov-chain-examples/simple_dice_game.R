# Simple Die game example.
#
# One repeatedly rolls a die,
# keeping a running total. Each time the total exceeds 10, we receive one dollar, and continue
# playing,resuming where we left of, mod 10.
#
# Result: Transition matrix for such an event.
#
p <- matrix(data = rep(0, 100), nrow = 10)
onesixth = 1/6

for (i in 1:10) {
  for (j in 1:6) {
    k <- i + j
    if (k > 10) {
      k <- k - 10
    }
    p[i,k] <- onesixth
  }
}

str(p)

# find stationary probabilities of the die game

# define function findpi()
# finds stationary probs of a Markov Chain using matrix multiplication
#______________________
findpi <- function(transition_matrix, iterations) {
  n_iters <- ceiling(log2(iterations))
  prd <- p
  for (i in n_iters) {
    prd <- prd %*% prd
  }
  colMeans(prd)
}

#______________________

dice_stationarity <- findpi(p, 10000)
dice_stationarity

# We get 1/10 for all the states which is obvious in this case since the likelihood of each state is symmetric.
#
# Visualizing the matrix p

dim <- ncol(p)
jpeg(filename = "./Markov-chain-examples/transition_matrix.jpg", width = 500, height = 500, units = "px", quality = "1000")
image(1:dim, 1:dim, p, xaxs = "i", yaxs = "i", xlab = "", ylab = "", col = heat.colors(2))
text(expand.grid(1:dim, 1:dim), sprintf("%0.3f", p))
dev.off()