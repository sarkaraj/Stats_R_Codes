# Author: Rajarshi Sarkar
# Topic: 3 heads in a row game.
#
# We keep tossing a coin until we get 3 heads consecutively. What is the expected value of tosses
# we need for such an event to occur.
#
# This is also modelled as a Markov Chain. It is assumed that the game is being played repeatedly.
# Meaning that as soon as we win, we immeadiately start a new game. In such a scenario, taking
# 3 states 0, 1 and 2 will suffice for modeling the Markov Chain (the state '3' can also be taken into account but then it will be an 'absorbing state' -- where the game basically ends.) The states here refer to the number of consecutive heads.
# The transition matrix is of the form:-
# [0.5  0.5   0]
# [0.5  0   0.5]
# [1    0     0]

p <- matrix(rep(0, 9), nrow = 3)
p[1, 1] = 0.5
p[1, 2] = 0.5
p[2, 3] = 0.5
p[2, 1] = 0.5
p[3, 1] = 1

# define function findpi()
# finds stationary probs of a Markov Chain using matrix solve()
# Won't work if the transition matrix is not invertible.
#______________________
findpi <- function(transition_matrix) {
  n <- nrow(transition_matrix)
  # find I - Matrix
  imp <- diag(n) - t(transition_matrix)
  # replace the last row of I-P
  imp[n, ] <- rep(1, n)
  # replace the corresponding element of the rhs by 1
  rhs <- c(rep(0, n - 1), 1)
  # solving equations
  solve(imp, rhs)
}

#______________________

pi_vector = findpi(p)
pi_vector

# [1] 0.5714286 0.2857143 0.1428571
#
# This means that 14.2% of the tosses will be made while in state 2 i.e., while we already have 2 consecutive heads.
# Therefore to answer our original question of 3 consecutive heads, 1/2 of the tosses in state '2' will result in successful state '3'.
# Therefore the stationary probability of state '3' will be 1/2 * 0.1428571 = 0.071
#
#
state_2 <- pi_vector[3]
state_3 <- state_2 * 0.50

state_3 * 100

# [1] 7.142857
#
# Therefore 7.14% of our total tosses will result in wins!
# Expected number of tosses = 1/p = 1/0.071 = 14.08 tosses