/** original code from https://developer.nvidia.com/blog/introduction-cuda-dynamic-parallelism/  */
/** @file histo-global.cu histogram with global memory atomics */
#include </apps/all/R/4.1.0-foss-2021a/lib/R/library/Rcpp/include/Rcpp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
using namespace Rcpp;

/** CUDA check macro */
#define cucheck(call)                                                         \
{                                                                             \
  cudaError_t res = (call);                                                   \
  if(res != cudaSuccess) {                                                    \
    const char* err_str = cudaGetErrorString(res);                            \
    fprintf(stderr, "%s (%d): %s in %s", __FILE__, __LINE__, err_str, #call);	\
    exit(-1);                                                                 \
  }                                                                           \
}

/** a useful function to compute the number of threads */
int divup(int x, int y) { return x / y + (x % y ? 1 : 0); }

/** a simple complex type */
struct complex {
  __host__ __device__ complex(float re, float im = 0) {
    this->re = re;
    this->im = im;
  }
  /** real and imaginary part */
  float re, im;
}; // struct complex

// operator overloads for complex numbers
inline __host__ __device__ complex operator+
(const complex &a, const complex &b) {
  return complex(a.re + b.re, a.im + b.im);
}
inline __host__ __device__ complex operator-
(const complex &a) { return complex(-a.re, -a.im); }
inline __host__ __device__ complex operator-
(const complex &a, const complex &b) {
  return complex(a.re - b.re, a.im - b.im);
}
inline __host__ __device__ complex operator*
(const complex &a, const complex &b) {
  return complex(a.re * b.re - a.im * b.im, a.im * b.re + a.re * b.im);
}
inline __host__ __device__ float abs2(const complex &a) {
  return a.re * a.re + a.im * a.im;
}
inline __host__ __device__ complex operator/
(const complex &a, const complex &b) {
  float invabs2 = 1 / abs2(b);
  return complex((a.re * b.re + a.im * b.im) * invabs2,
                 (a.im * b.re - b.im * a.re) * invabs2);
}  // operator/

#define MAX_DWELL 1024
#define BS 1024
/** computes the dwell for a single pixel */
__device__ int pixel_dwell
(int w, int h, complex cmin, complex cmax, int x, int y, int max_dwell) {
  complex dc = cmax - cmin;
  float fx = (float)x / w, fy = (float)y / h;
  complex c = cmin + complex(fx * dc.re, fy * dc.im);
  int dwell = 0;
  complex z = c;
  while(dwell < max_dwell && abs2(z) < 2 * 2) {
    z = z * z + c;
    dwell++;
  }
  return dwell;
}  // pixel_dwell

/** computes the dwells for Mandelbrot image 
 @param dwells the output array
 @param w the width of the output image
 @param h the height of the output image
 @param cmin the complex value associated with the left-bottom corner of the
 image
 @param cmax the complex value associated with the right-top corner of the
 image
 */
__global__ void mandelbrot_k
(int *dwells, int w, int h, complex cmin, complex cmax, int max_dwell) {
  // complex value to start iteration (c)
  int x = threadIdx.x + blockIdx.x * blockDim.x;
  int y = threadIdx.y + blockIdx.y * blockDim.y;
  int dwell = pixel_dwell(w, h, cmin, cmax, x, y, max_dwell);
  dwells[y * w + x] = dwell;
}  // mandelbrot_k


std::vector<int> mandelbrot_main
(int wh, int max_dwell) {
  // allocate memory
  int w = wh, h = wh;
  size_t dwell_sz = w * h * sizeof(int);
  int *h_dwells, *d_dwells;
  cucheck(cudaMalloc((void**)&d_dwells, dwell_sz));
  h_dwells = (int*)malloc(dwell_sz);
  
  // compute the dwells, copy them back
  dim3 bs(64, 4), grid(divup(w, bs.x), divup(h, bs.y));
  // Call the CUDA implementation of the mandelbrot function from Kernels.h with signature:
  mandelbrot_k<<<grid, bs>>>
    (d_dwells, w, h, complex(-1.5, -1), complex(0.5, 1), max_dwell);
  cucheck(cudaDeviceSynchronize());
  cucheck(cudaMemcpy(h_dwells, d_dwells, dwell_sz, cudaMemcpyDeviceToHost));
  
  // Convert the resultant array to a vector
  std::vector<int> res(h_dwells, h_dwells + (w * h));
  
  // free data
  cudaFree(d_dwells);
  free(h_dwells);
  return res;
}
