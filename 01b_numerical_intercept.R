
source('~/Dropbox (Personal)/Setting the intercept -- Issa, Sarah/00_balancing_numerical_solver_source_code2.R')

simulate_solver_intercept<-function(c, prevalence, Ldistn){
if(Ldistn=="binary"){
  function.list <- list(  list("binary", 0.5),
                          list("binary", 0.8)) 
} else if (Ldistn=="uniform"){
  function.list <- list(list("binary", 0.5),
                        list("uniform", -1,3))  
} else if (Ldistn=="normal"){
  function.list <- list(list("binary", 0.5),
                        list("continuous", 0))  
} else if (Ldistn=="gamma"){
  #p.l <- 1/0.75 #1.333
  #L3 <- rgamma(n,shape=1,rate=0.75)
  function.list <- list(list("binary", 0.5),
                        list("gamma", 1,0.50))  
  
}


set.seed(123)
n=10^7
tx.coefficients_prevalence<- determine.intercept(sample.size = n, tolerance = .0001, prev = prevalence, 
                                            intercept = NULL, beta_vec =c( log(c*2),log(c*1.5)),
                                            covariate.gen.function.list = function.list)

return(tx.coefficients_prevalence)


}

ptm <- proc.time() 
for (prev_target in c(0.10,0.20, 0.30,0.40, 0.50,0.60, 0.70,0.80,0.90 )) {
  
  #case with binary
  sim1=simulate_solver_intercept(c=1, prevalence=prev_target,Ldistn="binary")
  sim2=simulate_solver_intercept(c=1.5, prevalence=prev_target,Ldistn="binary")
  sim3=simulate_solver_intercept(c=2, prevalence=prev_target,Ldistn="binary")
  sim4=simulate_solver_intercept(c=2.5, prevalence=prev_target,Ldistn="binary")
  sim5=simulate_solver_intercept(c=3, prevalence=prev_target,Ldistn="binary")
  
  sim_binary<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_binary$prev_target=prev_target
  sim_binary$c=c(1,1.5,2,2.5,3)
  sim_binary$distn="binary_080"
  
  #case with uniform
  sim1=simulate_solver_intercept(c=1, prevalence=prev_target,Ldistn="uniform")
  sim2=simulate_solver_intercept(c=1.5, prevalence=prev_target,Ldistn="uniform")
  sim3=simulate_solver_intercept(c=2, prevalence=prev_target,Ldistn="uniform")
  sim4=simulate_solver_intercept(c=2.5, prevalence=prev_target,Ldistn="uniform")
  sim5=simulate_solver_intercept(c=3, prevalence=prev_target,Ldistn="uniform")
 
  sim_uniform<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_uniform$prev_target=prev_target
  sim_uniform$c=c(1,1.5,2,2.5,3)
  sim_uniform$distn="unif_n13"
  
  #case with normal
  sim1=simulate_solver_intercept(c=1, prevalence=prev_target,Ldistn="normal")
  sim2=simulate_solver_intercept(c=1.5, prevalence=prev_target,Ldistn="normal")
  sim3=simulate_solver_intercept(c=2, prevalence=prev_target,Ldistn="normal")
  sim4=simulate_solver_intercept(c=2.5, prevalence=prev_target,Ldistn="normal")
  sim5=simulate_solver_intercept(c=3, prevalence=prev_target,Ldistn="normal")
  
  sim_normal<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_normal$prev_target=prev_target
  sim_normal$c=c(1,1.5,2,2.5,3)
  sim_normal$distn="norm_01"
  
  #case with gamma
  sim1=simulate_solver_intercept(c=1, prevalence=prev_target,Ldistn="gamma")
  sim2=simulate_solver_intercept(c=1.5, prevalence=prev_target,Ldistn="gamma")
  sim3=simulate_solver_intercept(c=2, prevalence=prev_target,Ldistn="gamma")
  sim4=simulate_solver_intercept(c=2.5, prevalence=prev_target,Ldistn="gamma")
  sim5=simulate_solver_intercept(c=3, prevalence=prev_target,Ldistn="gamma")
  
  sim_gamma<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
  sim_gamma$prev_target=prev_target
  sim_gamma$c=c(1,1.5,2,2.5,3)
  sim_gamma$distn="gamma1_5"
  
  
  
  assign(paste0("DF", prev_target), data.frame(rbind(sim_binary, sim_uniform, sim_normal,sim_gamma)))
}
#head(DF0.3)
DFall<-data.frame(rbind(DF0.1,DF0.2,DF0.3,DF0.4,DF0.5,DF0.6,DF0.7,DF0.8,DF0.9))
head(DFall)

write.csv(DFall, "simulate_solver_intercept_all_10million_final.csv")
proc.time() - ptm
