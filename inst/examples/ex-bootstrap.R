# ordinary non-parametric bootstrap
bootstrap(20, 5)

# Bayesian bootstrap
bootstrap(20, 5, bayes = TRUE)

# weighted bootstrap
w <- c(rep(5, 5), rep(1, 15))
bootstrap(20, 5, weights = w / sum(w))
