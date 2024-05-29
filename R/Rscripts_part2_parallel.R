#************************************************
#*prepare framework
#************************************************

myDir=getwd()
myDir_Data=paste(myDir,"data", sep="/")

ifelse(!dir.exists(myDir), dir.create(myDir), FALSE)
ifelse(!dir.exists(myDir_Data), dir.create(myDir_Data), FALSE)
setwd(myDir)
dir()


#************************************************
#*packages for parallel and rand
#************************************************

needed.packages <- c("foreach", "doParallel","parallel","tictoc","pracma")
new.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(foreach)
library(doParallel)
library(parallel)
library(tictoc)
library(pracma)


n_cores=parallel::detectCores() 

#************************************************
# for loop
#************************************************
N=1000
K=60
set.seed(2021)
sum_rand=rep(0,K-1);
tic()
time_for_sys=system.time({
for (i in c(1:K)){
  A=rand(N,N)
  sum_rand[i]=sum(A)
}
})
time_for=toc()


#************************************************
#  foreach do
#************************************************
set.seed(2021)
sum_rand=rep(0,K-1);
tic()
time_foreach_sys=system.time({
  foreach (i = c(1:K)) %do% {
  A=rand(N,N)
  sum_rand[i]=sum(A)
  }
})
time_foreach=toc()

#************************************************
#  foreach dopar - no cluster
#************************************************
set.seed(2021)
sum_rand=rep(0,K-1);
tic()
time_foreachdopar_sys=system.time({
print("for each-dopar (no cluster)")
foreach (i = c(1:K)) %dopar% {
  library(pracma)
  A= rand(N,N)
  sum_rand[i]=sum(A)
}}
)
time_foreach_dopar=toc()



#************************************************
#  foreach dopar - with cluster - option 1
#************************************************
set.seed(2021)
clust <- makeCluster(n_cores-1)  
registerDoParallel(clust)  # use multicore, set to the number of our cores - needed for foerach dopar
getDoParName()
sum_rand=rep(0,K-1);
tic()
time_foreachdopar_1_sys=system.time({
print("for each-dopar (cluster allocated)")
foreach (i = c(1:K)) %dopar% {
  library(pracma)
  A=rand(N)
  sum_rand[i]=sum(A)
}}
)
time_foreach_dopar_1=toc()
registerDoSEQ()


#************************************************
#  foreach dopar - with cluster - option 2
#************************************************
set.seed(2021)
registerDoParallel(n_cores-1)  # use multicore, set to the number of our cores - needed for foerach dopar
getDoParName()
sum_rand=rep(0,K-1);
tic()
time_foreachdopar_2_sys=system.time({
  print("for each-dopar (cluster allocated)")
  foreach (i = c(1:K)) %dopar% {
    library(pracma)
    A=rand(N)
    sum_rand[i]=sum(A)
  }}
)
time_foreach_dopar_1=toc()
registerDoSEQ()   #this registers sequential mode - equivalent

times_for_sys_1<-rbind(time_for_sys,time_foreach_sys,time_foreachdopar_sys,time_foreachdopar_1_sys,time_foreachdopar_2_sys) 




#************************************************
#  apply and parallel apply
#************************************************
mat_sum<-function(x){
  library(pracma)
  A=rand(x)
  return(sum(A))
}

time_lapply<-system.time({
  set.seed(2021)
  sum_rand_lapply=lapply(rep(N,K),FUN=mat_sum)
})

time_sapply<-system.time({
  set.seed(2021)
  sum_rand_sapply=sapply(rep(N,K),FUN=mat_sum)
})

# forking
time_mcLapply<-system.time({
  set.seed(2021)
#  sum_rand_mcLapply=mclapply(X=rep(N,K),FUN=mat_sum,mc.cores = 12)
  sum_rand_mcLapply=mclapply(X=rep(N,K),FUN=mat_sum,mc.cores = 1)
})

#forking with foreach dopar
#library(doMC) # should be included in doParallel, but is not
#time_foreach_dopar_fork<-system.time({registerDoMC(cores = 12) # make a fork cluster
#sum_rand=c()
#foreach (i=1:20, .combine = 'c') %dopar% {
#            A=rand(N,N)
#            sum_rand[i]=sum(A)}
#registerDoSEQ()
#}
#) # time the fork cluster



# socketing
clust <- makeCluster(n_cores, type="PSOCK")  
time_parLapply<-system.time({
  set.seed(2021)
  sum_rand_parLapply=parLapply(clust,rep(N,K),fun=mat_sum)
})
stopCluster(clust)



clust <- makeCluster(n_cores, type="PSOCK")  
time_parSapply<-system.time({
  set.seed(2021)
  sum_rand_parSapply=parSapply(clust,rep(N,K),FUN=mat_sum)
})
stopCluster(clust)


times_apply<-rbind(time_lapply,time_sapply,time_parLapply,time_parSapply,time_mcLapply) 
#times_apply<-rbind(time_lapply,time_sapply,time_parLapply,time_parSapply,time_mcLapply,time_foreach_dopar_fork) 
print(times_apply[,1:3])



# simple very parallel
library(parallel)
library(tictoc)

f <- function(...) {
  Sys.sleep(1) 
  "DONE"
}

tic()
res <- lapply(1:25, f)
t1=toc()
#> 5.025 sec elapsed

tic()
res <- mclapply(1:25, f, mc.cores = 25)
t2=toc()
#> 1.019 sec elapsed
