resolution <- as.integer(1000) # Resolution of output image. This increases the exponentially x^2.
max_iter <- as.integer(100)
cmin <- complex(real = -1.5, imaginary = -1)
cmax <- complex(real = 0.5, imaginary = 1)


Rcpp::cppFunction(
  "
IntegerMatrix Mandelbrot(
  int resolution,
  int max_iter,
  std::complex<double> cmin,
  std::complex<double> cmax )
{
  std::complex<double> dc = cmax - cmin;
  IntegerMatrix out( resolution );
  for (int i=0; i < resolution; i++){
    for(int j=0; j < resolution; j++){
      double helper = static_cast<double>(i);
      double helper2 = static_cast<double>(j);
      double fx = helper / resolution * real(dc);
      double fy = helper2 / resolution * imag(dc);
      std::complex<double> c(real(cmin) + fx, imag(cmin) + fy);
      std::complex<double> z = c;
      int iteration = 0;
      while(abs(z) < 2 && iteration < max_iter)
      {
        z = z*z + c;
        iteration++;
      }
      out(i, j) = iteration;
    }
  }
  return out;
}
"
)

res_c <- Mandelbrot(
  resolution = resolution,
  max_iter = max_iter,
  cmin = cmin,
  cmax = cmax
)

res_cuda <- R2CUDA::mandelbrot_CUDA(resolution, max_iter)

res_cuda <- matrix(res_cuda, nrow = resolution)

all.equal(res_c, res_cuda)
summary(as.integer((res_c - unlist(res_cuda))))

microbenchmark::microbenchmark(
  `c` = Mandelbrot(
    resolution = resolution,
    max_iter = max_iter,
    cmin = cmin,
    cmax = cmax
  ),
  cuda = R2CUDA::mandelbrot_CUDA(resolution, max_iter),
  times = 20
)
