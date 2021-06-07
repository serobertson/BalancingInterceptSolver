
source('00_balancing_numerical_solver_source_code.R')

simulate_solver_intercept<-function(c, marg.expectation, Xdistn, evaluate_error=T){
  
  start_timer <- proc.time() 
  
  set.seed(123)
  n=10^6
  
  X1 <- rbinom(n, 1, 0.5)
  
if(Xdistn=="binary"){
  X2 <- rbinom(n, 1, 0.8)
} else if (Xdistn=="uniform"){
  X2 <- runif(n,-1,3) 
} else if (Xdistn=="normal"){
  X2 <- rnorm(n,0,1) 
} else if (Xdistn=="gamma"){
  X2 <- rgamma(n,shape=1,rate=0.5) 
}

  int=rep(1, n)  
  DF=data.frame(int, X1, X2)

coefficients_marg.expectation_solver<- determine.intercept(tolerance = .0001, marg = marg.expectation, 
                                            intercept = NULL, beta_vec =c( log(c*2),log(c*1.5)),
                                            external.dataset=DF)

end_timer=proc.time() - start_timer #time how long takes to solve 

coefficients_marg.expectation_solver$time=end_timer["elapsed"]

#browser()

if(evaluate_error==T){
#evaluate solver solution on larger dataset
set.seed(1234)
  n=10^8
  
  X1 <- rbinom(n, 1, 0.5)
  
  if(Xdistn=="binary"){
    X2 <- rbinom(n, 1, 0.8)
  } else if (Xdistn=="uniform"){
    X2 <- runif(n,-1,3) 
  } else if (Xdistn=="normal"){
    X2 <- rnorm(n,0,1) 
  } else if (Xdistn=="gamma"){
    X2 <- rgamma(n,shape=1,rate=0.5) 
  }
  
  int=rep(1, n)  
  DF.evaluate=data.frame(int, X1, X2)
  
coefficients_marg.expectation<- determine.intercept(tolerance = .0001, marg = marg.expectation, 
                                                 intercept = coefficients_marg.expectation_solver$intercept,
                                                 beta_vec =c( log(c*2),log(c*1.5)),
                                                 external.dataset=DF.evaluate)
#save time and iterations from the solver
coefficients_marg.expectation$time=end_timer["elapsed"] 
coefficients_marg.expectation$iterations=coefficients_marg.expectation_solver$iterations
coefficients_marg.expectation$a=coefficients_marg.expectation_solver$a
coefficients_marg.expectation$b=coefficients_marg.expectation_solver$b

return(coefficients_marg.expectation)
} else {return(coefficients_marg.expectation_solver)}

}


for (marg_target in c(0.10,0.20, 0.30,0.40, 0.50,0.60, 0.70,0.80,0.90 )) {
  
  evaluate=T #set to false to only run the numerical solver, without the evaluation step (much faster)
  
  #case with binary
  sim1=simulate_solver_intercept(c=1, marg.expectation=marg_target,Xdistn="binary",evaluate_error=evaluate)
  sim2=simulate_solver_intercept(c=1.5, marg.expectation=marg_target,Xdistn="binary",evaluate_error=evaluate)
  sim3=simulate_solver_intercept(c=2, marg.expectation=marg_target,Xdistn="binary",evaluate_error=evaluate)
  sim4=simulate_solver_intercept(c=2.5, marg.expectation=marg_target,Xdistn="binary",evaluate_error=evaluate)
  sim5=simulate_solver_intercept(c=3, marg.expectation=marg_target,Xdistn="binary",evaluate_error=evaluate)
  
  sim_binary<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_binary$marg_target=marg_target
  sim_binary$c=c(1,1.5,2,2.5,3)
  sim_binary$distn="binary_080"
  
  #case with uniform
  sim1=simulate_solver_intercept(c=1, marg.expectation=marg_target,Xdistn="uniform",evaluate_error=evaluate)
  sim2=simulate_solver_intercept(c=1.5, marg.expectation=marg_target,Xdistn="uniform",evaluate_error=evaluate)
  sim3=simulate_solver_intercept(c=2, marg.expectation=marg_target,Xdistn="uniform",evaluate_error=evaluate)
  sim4=simulate_solver_intercept(c=2.5, marg.expectation=marg_target,Xdistn="uniform",evaluate_error=evaluate)
  sim5=simulate_solver_intercept(c=3, marg.expectation=marg_target,Xdistn="uniform",evaluate_error=evaluate)
 
  sim_uniform<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_uniform$marg_target=marg_target
  sim_uniform$c=c(1,1.5,2,2.5,3)
  sim_uniform$distn="unif_n13"
  
  #case with normal
  sim1=simulate_solver_intercept(c=1, marg.expectation=marg_target,Xdistn="normal", evaluate_error=evaluate)
  sim2=simulate_solver_intercept(c=1.5, marg.expectation=marg_target,Xdistn="normal", evaluate_error=evaluate)
  sim3=simulate_solver_intercept(c=2, marg.expectation=marg_target,Xdistn="normal", evaluate_error=evaluate)
  sim4=simulate_solver_intercept(c=2.5, marg.expectation=marg_target,Xdistn="normal",evaluate_error=evaluate)
  sim5=simulate_solver_intercept(c=3, marg.expectation=marg_target,Xdistn="normal",evaluate_error=evaluate)
  
  sim_normal<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_normal$marg_target=marg_target
  sim_normal$c=c(1,1.5,2,2.5,3)
  sim_normal$distn="norm_01"
  
  #case with gamma
  sim1=simulate_solver_intercept(c=1, marg.expectation=marg_target,Xdistn="gamma",evaluate_error=evaluate)
  sim2=simulate_solver_intercept(c=1.5, marg.expectation=marg_target,Xdistn="gamma",evaluate_error=evaluate)
  sim3=simulate_solver_intercept(c=2, marg.expectation=marg_target,Xdistn="gamma",evaluate_error=evaluate)
  sim4=simulate_solver_intercept(c=2.5, marg.expectation=marg_target,Xdistn="gamma",evaluate_error=evaluate)
  sim5=simulate_solver_intercept(c=3, marg.expectation=marg_target,Xdistn="gamma",evaluate_error=evaluate)
  
  sim_gamma<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_gamma$marg_target=marg_target
  sim_gamma$c=c(1,1.5,2,2.5,3)
  sim_gamma$distn="gamma1_5"
  
  
  
  assign(paste0("DF", marg_target), data.frame(rbind(sim_binary, sim_uniform, sim_normal,sim_gamma)))
}

DFall<-data.frame(rbind(DF0.1,DF0.2,DF0.3,DF0.4,DF0.5,DF0.6,DF0.7,DF0.8,DF0.9))
head(DFall)

write.csv(DFall, "results_numerical_approximation.csv")
