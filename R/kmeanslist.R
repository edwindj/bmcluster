D2 <- function(a,b){
  (a-b)*(a-b)
}

#' bregmann clustering with a list of objects
#'
#'
bmclust.list <- function(l, n, divFUN=D2, meanFUN=mean, K=10){
  eps <- 0.1
  div <- match.fun(divFUN)
  mean <- match.fun(meanFUN)
  
  N <- length(l)
  prevcl <- integer(N)
  
  #seed
  centers <- l[sample(N, n)]
  
  # D2 seeding
  centers <- list()
  prob <- rep(1, N)
  seedsd <- numeric(N)
  for (i in 1:N){
    seed <- l[[sample(N, 1, prob=prob)]]
    
    d  <- d/sum(d)
    print(d)
    centers <- c(centers, seed)
  }
  
  # assign elements to center
  cluster <- function(i){
    d <- sapply(centers, div, i)
    which(d == min(d))[1]
  }
  
  cl <- sapply(l, cluster)

  #iterate
  for (k in 1:K){  
     centers <- as.list(tapply(l, cl, FUN=function(ll){mean(unlist(ll))}))
     cl <- sapply(l, cluster)
     if (all(diff(prevcl-cl) == 0)){
       break
     }
     prevcl <- cl
     print(k)
     #print(cl)
  }
  print(cl)
  print(centers)
}


l <- list(1,2,5,6,8,9)
l <- as.list(runif(100))
n <- 3
bmclust.list(l, n)