# Wrapper function to call the CUDA implementation of
# the mandelbrot function
mandelbrot_CUDA <- function(res, iter) 
{
  .Call("cudaMandelbrot", res, iter)
}
