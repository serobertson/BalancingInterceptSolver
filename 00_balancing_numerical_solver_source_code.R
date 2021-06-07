#Source code for the numerical algorithm 

# The expit (or inverse logit) funtion.
expit <- function(x){
  exp(x)/(1+exp(x))
}


determine.beta <- function(upper = upper.bound, lower = lower.bound)
{
  t.beta <- ((upper + lower) / 2)
  return(t.beta)
}


####################
determine.intercept <- function(intercept = NULL, beta_vec = beta_vec, marg = 0.5, 
                               tolerance = .001, 
                               lower.bound=-10, upper.bound=10, external.dataset=NA)
{
  if(is.null(intercept))
  {
    
    temp.dataset<-external.dataset
    beta_vec <- c((upper.bound+lower.bound)/2, beta_vec)
    count <- 1
    
    #calculate initial p-tilde
    linearformula=paste(beta_vec, colnames(temp.dataset), sep="*")
    linearformula2=paste(linearformula, collapse=" + ")
    lp <- with(temp.dataset, eval(parse(text = linearformula2)))
    mean.oc<-mean(expit(lp))
    
    while(abs(mean.oc - marg) > tolerance && upper.bound>lower.bound && count<=99)
    {
      print(paste("a:",lower.bound, "b:", upper.bound))

      if(mean.oc > marg)
      {
        upper.bound <- beta_vec[1]
      } else if(mean.oc < marg)
      {
        lower.bound <- beta_vec[1]
      }
      temp.coefficients <- beta_vec
      beta_vec[1] <- determine.beta(upper.bound, lower.bound)
      print(paste("iteration:",count))
      print(paste("intercept value:", beta_vec[1]))
      linearformula=paste(beta_vec, colnames(temp.dataset), sep="*")
      linearformula2=paste(linearformula, collapse=" + ")
      lp <- with(temp.dataset, eval(parse(text = linearformula2)))
      mean.oc<-mean(expit(lp))
      cat("Outcome marginal expectation:", mean.oc, "\n")
      count <- count + 1      
    }
  } else {
    
    beta_vec <- c(intercept, beta_vec)
    temp.coefficients <- beta_vec
   
      temp.dataset<-external.dataset
      sample.size=nrow(temp.dataset)
    
    linearformula=paste(beta_vec, colnames(temp.dataset), sep="*")
    linearformula2=paste(linearformula, collapse=" + ")
    lp <- with(temp.dataset, eval(parse(text = linearformula2)))
    #use p-hat to evaluate:
    Ysim <- rbinom(sample.size, 1, expit(lp))
    mean.oc<-mean(Ysim)
    cat("Outcome marginal expectation:", mean.oc, "\n")
    count=0 #no solving steps
  }
  #only print this warning when solving for an intercept
  if(abs(mean.oc - marg) > tolerance & is.null(intercept)==T){warning('Algorithm did not converge within tolerance.')}
  return(data.frame(intercept=beta_vec[1], marginal.expectation=mean.oc, iterations=count, 
                    a=lower.bound, b=upper.bound))
  
}




