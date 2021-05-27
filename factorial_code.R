## Author: Zhuojue Wang
## Date: 05/26/2021



# Four function and test

Factorial_loop <- function(x){
  res <- 1

  if(x<0){
    print("Number need to be greater or equal to 0")}
  else if(x==0){
    return(res)
  }
  else{
    for(i in 1:x){
      res <- res*i
    }
    return(res)
  }
}

Factorial_loop(10)

library(tidyverse)

Factorial_reduce <- function(x){
  if(x<0){
    print("Number need to be greater or equal to 0")}
  else if(x==0){
    return(1)
  }
  res <- 1:x %>% reduce(`*`)
  return(res)
}

Factorial_reduce(10)

Factorial_func <- function(x){
  if(x<0){
    print("Number need to be greater or equal to 0")}
  else if(x==0){
    res <- 1
  }
  else{
    res <- x*Factorial_func(x-1)
  }
  return(res)
}

Factorial_func(4)




Factorial_mem <- local({
  memory <- list()
  function(x) {
    if(x<0){
      print("Number need to be greater or equal to 0")}
    else{
      valueName <- as.character(x)
      if (!is.null(memory[[valueName]])) return(memory[[valueName]])
      if (x == 0) return(1)
      if (x == 1) return(1)
      res <- x*Recall(x - 1)
      memory[[valueName]] <<- res # store results
    }
    return(res)
  }
})

Factorial_mem(4)




## Benchmark and Graph

library(microbenchmark)
library(tidyverse)

factorial_output <- microbenchmark(a <- Factorial_loop(10),
                                   b <- Factorial_reduce(10),
                                   c <- Factorial_func(10),
                                   d <- Factorial_mem(10))
autoplot(factorial_output)


factorial_output1 <- microbenchmark(a <- Factorial_loop(100),
                                    b <- Factorial_reduce(100),
                                    c <- Factorial_func(100),
                                    d <- Factorial_mem(100))
autoplot(factorial_output1)
