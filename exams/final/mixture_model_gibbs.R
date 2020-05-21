pacman::p_load(MCMCpack)

set.seed(1984)
G = 300
n = 20

true_theta_1 = 0.200
true_theta_2 = 0.325
true_rho = 0.3


x = sort(c(
  rbinom(G * true_rho, n, true_theta_1), 
  rbinom(G * (1 - true_rho), n, true_theta_2)
))

par(mfrow = c(1, 1))
hist(x, br = 80, main = "") 
#plot


#chains
S = 1e5
theta1s = array(NA, S)
theta2s = array(NA, S)
rhos = array(NA, S)
Is = matrix(NA, nrow = G, ncol = S)
#start positions
theta1s[1] = 0.5 #ensure one begins lower than other
theta2s[1] = 0.5 #ensure one begins lower than other
rhos[1] = 0.5
Is[1 : (G / 2), 1] = 1
Is[(G / 2 + 1) : G, 1] = 0

for (t in 2 : S){
  theta1 = theta1s[t - 1]
  theta2 = theta2s[t - 1]
  I = Is[, t - 1]
  
  sum_I = sum(I)
  sum_1_min_I = G - sum_I
  theta1s[t] = rbeta(1, sum(I * x), 1 + sum(I * (n - x)))
  theta2s[t] = rbeta(1, sum((1 - I) * x), 1 + sum((1 - I) * (n - x)))
  rhos[t] = rbeta(1, 1 + sum_I, 1 + sum_1_min_I)
  
  for (i in 1 : G){#now draw the Is
    a_i = rhos[t] * theta1s[t]^x[i] * (1 - theta1s[t])^(n - x[i])
    b_i = (1 - rhos[t]) * theta2s[t]^x[i] * (1 - theta2s[t])^(n - x[i])
    Is[i, t] = rbinom(1, 1, a_i / (a_i + b_i))
    # cat("a =", a, "b = ", b, "p =", a / (a + b), "I =", Is[i, t], "\n")
  }
  # cat("t =", t, "sum_I = ", sum_I, "sum_1_min_I =", sum_1_min_I, "theta1s[t] =", theta1s[t], "theta2s[t] =", theta1s[t], "rhos[t] =", rhos[t], "\n")
}



###assess convergence
#plot
###
par(mfrow = c(3, 1))
S0 = 850
plot(1 : S0, theta1s[1 : S0])
# abline(h = mean(theta1s[B : S0]), col = "blue")
# abline(h = true_theta_1, col = "red")
# abline(v = B, col = "grey")

plot(1 : S0, theta2s[1 : S0])
# abline(h = mean(theta2s[B : S0]), col = "blue")
# abline(h = true_theta_2, col = "red")
# abline(v = B, col = "grey")

plot(1 : S0, rhos[1 : S0])
# abline(h = mean(rhos[B : S0]), col = "blue")
# abline(h = sqrt(true_rho), col = "red")
# abline(v = B, col = "grey")
#plot


B = 250

##assess autocorrelation

par(mfrow = c(3, 1))
Kmax = 300
acf(theta1s[B : S], xlim = c(0, Kmax), lag.max = Kmax, ylim = c(0, 0.3))
acf(theta2s[B : S], xlim = c(0, Kmax), lag.max = Kmax, ylim = c(0, 0.3))
acf(rhos[B : S], xlim = c(0, Kmax), lag.max = Kmax, ylim = c(0, 0.3))
THIN = 280
#plot

#burn and thin
theta1s = theta1s[B : S]
theta1s = theta1s[seq(1, S - B, by = THIN)]
theta2s = theta2s[B : S]
theta2s = theta2s[seq(1, S - B, by = THIN)]
rhos = rhos[B : S]
rhos = rhos[seq(1, S - B, by = THIN)]
Is = Is[, B : S]
Is = Is[, seq(1, S - B, by = THIN)]
Is[1, ]
Is[n, ]


#look at posteriors with post-exp at 95% CI
par(mfrow = c(1, 1))
res = 200

hist(theta1s, br = res, main = "")
# abline(v = mean(theta1s), col = "blue", lwd = 3)
# abline(v = quantile(theta1s, 0.025), col = "grey", lwd = 3)
# abline(v = quantile(theta1s, 0.975), col = "grey", lwd = 3)
# abline(v = true_theta_1, col = "red", lwd = 3)

hist(theta2s, br = res)
# abline(v = mean(theta2s), col = "blue", lwd = 3)
# abline(v = quantile(theta2s, 0.025), col = "grey", lwd = 3)
# abline(v = quantile(theta2s, 0.975), col = "grey", lwd = 3)
# abline(v = true_theta_2, col = "red", lwd = 3)

hist(rhos, br = res)
# abline(v = mean(rhos), col = "blue", lwd = 3)
# abline(v = quantile(rhos, 0.025), col = "grey", lwd = 3)
# abline(v = quantile(rhos, 0.975), col = "grey", lwd = 3)
# abline(v = true_rho, col = "red", lwd = 3)
#plot

hist(theta2s - theta1s, br = res, main = "", xlab = "")
mean(theta2s - theta1s)

Is[1, ]
table(Is[1, ])
Is[n, ]
Is[n / 2, ]
mean(Is[n / 2, ])
table(Is[1, ])
