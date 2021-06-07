
# The expit (or inverse logit) funtion.
expit <- function(x){
  exp(x)/(1+exp(x))
}


simulate_balance_intercept<-function(c, marg.expectation, Xdistn){
  
#Code in lines 50-51 in this function "simulate_balance_intercept" 
#have been slightly modified from code provided by
#Rudolph et al. (American Journal of Epidemiology,2021)
#paper on "Simulation in Practice: The Balancing Intercept.
  
start_timer <- proc.time() 
  
set.seed(123)

#n <- 10^6
n <- 10^8
#Marginal probabilities of each variable
p.y <- marg.expectation; c


p.x1<- 0.5
X1 <- rbinom(n, 1, p.x1)

if(Xdistn=="binary"){
  p.x2 <- 0.8
 X2 <- rbinom(n, 1, p.x2)
} else if (Xdistn=="uniform"){
  p.x2 <- 1
 X2 <- runif(n,-1,3)
} else if (Xdistn=="normal"){
  p.x2 <- 0
  X2 <- rnorm(n,0,1)
} else if (Xdistn=="gamma"){
  p.x2 <- 1/0.50 #2
  X2 <- rgamma(n,shape=1,rate=0.5)
  
}
#-------------------------------------------

#intercept value
gamma0=qlogis(p.y)- log(c*2)*p.x1  - log(c*1.5)*p.x2
lp=(gamma0 + log(c*2)*X1+log(c*1.5)*X2)
Y_simbalance <- rbinom(n, 1, expit(lp))
mean(Y_simbalance)

end_timer=proc.time() - start_timer #time how long takes to solve 


return(data.frame(intercept=gamma0, marg.expectation=mean(Y_simbalance), time=end_timer["elapsed"]))

}


#original case with some modification to strength of c= coefficient 
for (marg_target in c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70,0.80,0.90 )) {
sim1=simulate_balance_intercept(c=1, marg.expectation=marg_target,Xdistn="binary")
sim2=simulate_balance_intercept(c=1.5, marg.expectation=marg_target,Xdistn="binary")
sim3=simulate_balance_intercept(c=2, marg.expectation=marg_target,Xdistn="binary")
sim4=simulate_balance_intercept(c=2.5, marg.expectation=marg_target,Xdistn="binary")
sim5=simulate_balance_intercept(c=3, marg.expectation=marg_target,Xdistn="binary")

sim_binary<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_binary$marg_target=marg_target
sim_binary$c=c(1,1.5,2,2.5,3)
sim_binary$distn="binary_080"

#case with uniform
sim1=simulate_balance_intercept(c=1, marg.expectation=marg_target,Xdistn="uniform")
sim2=simulate_balance_intercept(c=1.5, marg.expectation=marg_target,Xdistn="uniform")
sim3=simulate_balance_intercept(c=2, marg.expectation=marg_target,Xdistn="uniform")
sim4=simulate_balance_intercept(c=2.5, marg.expectation=marg_target,Xdistn="uniform")
sim5=simulate_balance_intercept(c=3, marg.expectation=marg_target,Xdistn="uniform")

sim_uniform<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_uniform$marg_target=marg_target
sim_uniform$c=c(1,1.5,2,2.5,3)
sim_uniform$distn="unif_n13"

#case with normal
sim1=simulate_balance_intercept(c=1, marg.expectation=marg_target,Xdistn="normal")
sim2=simulate_balance_intercept(c=1.5, marg.expectation=marg_target,Xdistn="normal")
sim3=simulate_balance_intercept(c=2, marg.expectation=marg_target,Xdistn="normal")
sim4=simulate_balance_intercept(c=2.5, marg.expectation=marg_target,Xdistn="normal")
sim5=simulate_balance_intercept(c=3, marg.expectation=marg_target,Xdistn="normal")

sim_normal<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_normal$marg_target=marg_target
sim_normal$c=c(1,1.5,2,2.5,3)
sim_normal$distn="norm_01"

#case with gamma
sim1=simulate_balance_intercept(c=1, marg.expectation=marg_target,Xdistn="gamma")
sim2=simulate_balance_intercept(c=1.5, marg.expectation=marg_target,Xdistn="gamma")
sim3=simulate_balance_intercept(c=2, marg.expectation=marg_target,Xdistn="gamma")
sim4=simulate_balance_intercept(c=2.5, marg.expectation=marg_target,Xdistn="gamma")
sim5=simulate_balance_intercept(c=3, marg.expectation=marg_target,Xdistn="gamma")

sim_gamma<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_gamma$marg_target=marg_target
sim_gamma$c=c(1,1.5,2,2.5,3)
sim_gamma$distn="gamma1_5"



assign(paste0("DF", marg_target), data.frame(rbind(sim_binary, sim_uniform, sim_normal,sim_gamma)))
}


DFall<-data.frame(rbind(DF0.1,DF0.2,DF0.3,DF0.4,DF0.5,DF0.6,DF0.7,DF0.8,DF0.9))
head(DFall)

write.csv(DFall, "results_analytical_approximation.csv") 

