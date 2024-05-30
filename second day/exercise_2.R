# Set parameters ----
size = 1000

# Input data ----
input_mtx <- matrix(c(
  a = rnorm(size),
  b = rnorm(size, 5),
  c = rnorm(size, 10),
  d = rnorm(size, 15)
),
ncol = 4,
byrow = TRUE)

# Compute mean for each column ----
out <- apply(input_mtx,
             2,
             mean)

print(out)
# C part ----

# Things to consider
# C is indexing from 0 instead of 1 as R.
# Always put ";" at the end of a statement.
# for cycle in C is defined as for (int i = 1; condition; increment){...}
# e.g. for (int i = 1; i < iter; i++)
# i++ stands for i = i + 1
# You can get a ithg column of a matrix named m by calling m(_,i)

Rcpp::cppFunction(
  "
  NumericVector col_mean(NumericMatrix input){
    int ncol = input.cols();
    NumericVector out(ncol);
    ...
    return out;
  }
  "
)

out_c <- col_mean(input_mtx)

# Check results and benchmark ----

all.equal(out, out_c)

microbenchmark::microbenchmark(r = apply(input_mtx,
                                         2,
                                         mean),
                               c = col_mean(input_mtx))
