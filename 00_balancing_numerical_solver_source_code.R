#Source code for the numerical algorithm 

# The expit (or inverse logit) funtion.
expit <- function(x){
  exp(x)/(1+exp(x))
}


determine_beta <- function(upper = upper_bound, lower = lower_bound)
{
  t_beta <- ((upper + lower) / 2)
  return(t_beta)
}


####################
determine_intercept <- function(intercept = NULL, beta_vec = beta_vec, marg = 0.5, 
                               tolerance = .001, 
                               lower_bound=-10, upper_bound=10, external_dataset=NA)
{
  if(is.null(intercept))
  {
    
    temp_dataset<-external_dataset
    beta_vec <- c((upper_bound+lower_bound)/2, beta_vec)
    count <- 1
    
    #calculate initial p-tilde
    linearformula=paste(beta_vec, colnames(temp_dataset), sep="*")
    linearformula2=paste(linearformula, collapse=" + ")
    lp <- with(temp_dataset, eval(parse(text = linearformula2)))
    mean.oc<-mean(expit(lp))
    
    while(abs(mean.oc - marg) > tolerance && upper_bound>lower_bound && count<=99)
    {
      print(paste("a:",lower_bound, "b:", upper_bound))

      if(mean.oc > marg)
      {
        upper_bound <- beta_vec[1]
      } else if(mean.oc < marg)
      {
        lower_bound <- beta_vec[1]
      }
      temp.coefficients <- beta_vec
      beta_vec[1] <- determine_beta(upper_bound, lower_bound)
      print(paste("iteration:",count))
      print(paste("intercept value:", beta_vec[1]))
      linearformula=paste(beta_vec, colnames(temp_dataset), sep="*")
      linearformula2=paste(linearformula, collapse=" + ")
      lp <- with(temp_dataset, eval(parse(text = linearformula2)))
      mean.oc<-mean(expit(lp))
      cat("Outcome marginal expectation:", mean.oc, "\n")
      count <- count + 1      
    }
  } else {
    
    beta_vec <- c(intercept, beta_vec)
    temp.coefficients <- beta_vec
   
    temp_dataset<-external_dataset
    sample_size=nrow(temp_dataset)
    
    linearformula=paste(beta_vec, colnames(temp_dataset), sep="*")
    linearformula2=paste(linearformula, collapse=" + ")
    lp <- with(temp_dataset, eval(parse(text = linearformula2)))
    #use p-hat to evaluate:
    Ysim <- rbinom(sample_size, 1, expit(lp))
    mean.oc<-mean(Ysim)
    cat("Outcome marginal expectation:", mean.oc, "\n")
    count=0 #no solving steps
  }
  #only print this warning when solving for an intercept
  if(abs(mean.oc - marg) > tolerance & is.null(intercept)==T){warning('Algorithm did not converge within tolerance.')}
  return(data.frame(intercept=beta_vec[1], marginal_expectation=mean.oc, iterations=count, 
                    a=lower_bound, b=upper_bound))
  
}



