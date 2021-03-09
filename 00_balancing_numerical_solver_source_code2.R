####START OF SOLVER FXN 
#separate from other fxns 

## The expit (or inverse logit) funtion.
expit <- function(x){
  exp(x)/(1+exp(x))
}


determine.beta <- function(upper = upper.bound, lower = lower.bound)
{
  t.beta <- ((upper + lower) / 2)
  return(t.beta)
}
##########
##########


generate <- function(covariate.gen.function.list, covmatrix = NULL, cormatrix = NULL, samplesize = 1000)
{
  numcon <- 0
  numbin <- 0
  na.vector <- vector()
  for(i in 1:length(covariate.gen.function.list))
  {  
    if(covariate.gen.function.list[[i]][[1]] == "continuous")
    {
      numcon <- numcon + 1
      na.vector <- c(na.vector, 'n')
    } else if(covariate.gen.function.list[[i]][[1]] == "binary")
    {
      numbin <- numbin + 1
      if(!is.null(cormatrix))
      {
        na.vector <- c(na.vector, 'b')
      }
    }
    
  }
  if(is.null(covmatrix))
  {
    covmatrix <- diag(numcon)
  }
  
  simul <- data.frame(x1 = rep(1, samplesize))
  for(i in 2:(1 + length(covariate.gen.function.list)))
  {
    simul[, i] <- rep(NA, samplesize)
  }
  continuous.means <- vector()
  binary.means <- vector()
  
  for(i in 1:length(covariate.gen.function.list))
  {
    if(covariate.gen.function.list[[i]][[1]] == "continuous")
    {
      continuous.means <- c(continuous.means, covariate.gen.function.list[[i]][[2]])
    } else if(covariate.gen.function.list[[i]][[1]] == "binary")
    {
      if(is.null(cormatrix))
      {
        simul[, i + 1] <- rbinom(samplesize, 1, covariate.gen.function.list[[i]][[2]])
        
      } else
      {
        binary.means <- c(binary.means, covariate.gen.function.list[[i]][[2]])
      }
    } else if(covariate.gen.function.list[[i]][[1]] == "gamma")
    {
      simul[, i + 1] <- rgamma(samplesize, shape = covariate.gen.function.list[[i]][[2]], rate = covariate.gen.function.list[[i]][[3]]) 
    }
   else if(covariate.gen.function.list[[i]][[1]] == "cauchy")
  {
    simul[, i + 1] <- rcauchy(samplesize, location = 0, scale = 1)
   }else if(covariate.gen.function.list[[i]][[1]] == "uniform")
   {
     simul[, i + 1] <- runif(samplesize, covariate.gen.function.list[[i]][[2]], covariate.gen.function.list[[i]][[3]])
   }
    c.name <- capture.output(cat("x", i + 1, sep = ""))
    colnames(simul)[i + 1] <- c.name
  }
  if(length(continuous.means > 0))
  {
    continuous.values <- mvrnorm(n = samplesize, mu = continuous.means, Sigma = covmatrix)
  }
  if(length(binary.means > 0) && is.null(cormatrix) == FALSE)
  {
    binary.values <- rmvbin(n = samplesize, margprob = binary.means, bincorr = cormatrix)
  }
  initial.n = 1
  initial.b = 1
  count = 1
  for(i in which(is.na(simul[1, ])))
  {
    if(na.vector[count] == 'n')
    {
      simul[, i] <- continuous.values[, initial.n]
      initial.n <- initial.n + 1
    } else if(na.vector[count] == 'b')
    {
      simul[, i] <- binary.values[, initial.b]
      initial.b <- initial.b + 1
    }
    count <- count + 1
  }
  return(simul)
}




####################
determine.intercept <- function(intercept = NULL, beta_vec = beta_vec, prev = 0.5, sample.size = 200000, tolerance = .001, covariate.gen.function.list)
{
  if(is.null(intercept))
  {
    beta_vec <- c(0, beta_vec)
    temp.dataset <- generate(covariate.gen.function.list, samplesize = sample.size)
    print(str(temp.dataset))
    mean.oc <- 0
    upper.bound <- 10 
    lower.bound <- -10
    count <- 1
    while(abs(mean.oc - prev) > tolerance && upper.bound - lower.bound != 0)
    {
      print(count)
      print(beta_vec)
      linearformula=paste(beta_vec, colnames(temp.dataset), sep="*")
      linearformula2=paste(linearformula, collapse=" + ")
      xai <- with(temp.dataset, eval(parse(text = linearformula2)))
      print(range(xai))
      mean.oc<-mean(expit(xai))
      cat("Outcome prevalence:", mean.oc, "\n")
      if(mean.oc > prev)
      {
        upper.bound <- beta_vec[1]
      } else if(mean.oc < prev)
      {
        lower.bound <- beta_vec[1]
      }
      temp.coefficients <- beta_vec
      beta_vec[1] <- determine.beta(upper.bound, lower.bound)
      #(upper.bound + lower.bound) / 2
      count <- count + 1      
    }
  } else {
    
    beta_vec <- c(intercept, beta_vec)
    temp.coefficients <- beta_vec
  }
  print(temp.coefficients[1])
  return(data.frame(intercept=temp.coefficients[1], prevalence=mean.oc))
}



