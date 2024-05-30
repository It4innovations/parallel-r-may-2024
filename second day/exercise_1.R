# Set parameters ----
x0 <- 0.0001
mu <- 3.9
iter <- 1000

# Define function ----
# https://en.wikipedia.org/wiki/Logistic_map
logistic_map <- function(x = 0.0001,
                         mu = 3.9,
                         iter = 1000) {
  # Initialize vector
  out <- rep(NA, iter)
  # Set first iteration
  out[1] <- x
  for (i in 2:iter) {
    x <- mu * x * (1 - x)
    out[i] <- x
  }
  return(out)
}

# Compute and plot ----

out <- logistic_map(x = x0,
                    mu = mu,
                    iter = iter)

plot(out, type = "l")

# C part ----

# Things to consider
# C is indexing from 0 instead of 1 as R.
# Always put ";" at the end of a statement.
# for cycle in C is defined as for (int i = 1; condition; increment){...}
# e.g. for (int i = 1; i < iter; i++)
# i++ stands for i = i + 1

Rcpp::cppFunction(
  "
  NumericVector logistic_map_c(double x, double mu, int iter){
    NumericVector out(iter);
    ...
    return out;
  }
  "
)

out_c <- logistic_map_c(x = x0,
                        mu = mu,
                        iter = iter)

# Check results and benchmark ----

all.equal(out, out_c)

microbenchmark::microbenchmark(r = logistic_map(x = x0,
                                                mu = mu,
                                                iter = iter),
                               c = logistic_map_c(x = x0,
                                                  mu = mu,
                                                  iter = iter))
