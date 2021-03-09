setwd("~/Dropbox (Personal)/Setting the intercept -- Issa, Sarah")

simulate_balance_intercept<-function(c, prevalence, Ldistn){
set.seed(123)
#n <- 10000
#n <- 10^6
n <- 10^7
#Marginal probabilities of each variable
#b0,b1,b2
p.y <- prevalence; p.x <- 0.5

#; p.l <- 0.8

# Generate L, X, and Y -------------------

if(Ldistn=="binary"){
  p.l <- 0.8
L3 <- rbinom(n, 1, p.l)
} else if (Ldistn=="uniform"){
  p.l <- 1
L3 <- runif(n,-1,3) #nonsymmetric
} else if (Ldistn=="normal"){
  p.l <- 0
  L3 <- rnorm(n,0,1)
} else if (Ldistn=="gamma"){
  #p.l <- 1/0.75 #1.333
  #L3 <- rgamma(n,shape=1,rate=0.75)
  p.l <- 1/0.50 #2
  L3 <- rgamma(n,shape=1,rate=0.5)
  
}

#L3 <- rnorm(n,p.l)

#P(X=1|L)
p.x3b <- 1/(1 + exp(-(-log(1/p.x - 1) + log(c*1.5)*L3 - log(c*1.5)*p.l)))
X3b <- rbinom(n, 1, p.x3b)


#P(Y=1|X,L) with balancing intercept
#\gamma1= log(2)
#\gamma2= log(1.5)
p.y3b <- 1/(1 + exp(-(-log(1/p.y - 1) + log(c*2)*X3b - log(c*2)*p.x 
                      + log(c*1.5)*L3 - log(c*1.5)*p.l)))
Y3b <- rbinom(n, 1, p.y3b)


#
gamma0=-log(1/p.y - 1)- log(c*2)*p.x  - log(c*1.5)*p.l

#SR: check marginal
mean(L3) #0.8051
mean(X3b)  #0.5037
mean(Y3b) #0.311

return(data.frame(intercept=gamma0, prevalence=mean(Y3b)))

}


#original case with some modification to strength of c= coefficient 
for (prev_target in c(0.10,0.20, 0.30,0.40, 0.50,0.60, 0.70,0.80,0.90 )) {
#prev_target=0.30
sim1=simulate_balance_intercept(c=1, prevalence=prev_target,Ldistn="binary")
sim2=simulate_balance_intercept(c=1.5, prevalence=prev_target,Ldistn="binary")
sim3=simulate_balance_intercept(c=2, prevalence=prev_target,Ldistn="binary")
sim4=simulate_balance_intercept(c=2.5, prevalence=prev_target,Ldistn="binary")
sim5=simulate_balance_intercept(c=3, prevalence=prev_target,Ldistn="binary")

sim_binary<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_binary$prev_target=prev_target
sim_binary$c=c(1,1.5,2,2.5,3)
sim_binary$distn="binary_080"

#case with uniform
sim1=simulate_balance_intercept(c=1, prevalence=prev_target,Ldistn="uniform")
sim2=simulate_balance_intercept(c=1.5, prevalence=prev_target,Ldistn="uniform")
sim3=simulate_balance_intercept(c=2, prevalence=prev_target,Ldistn="uniform")
sim4=simulate_balance_intercept(c=2.5, prevalence=prev_target,Ldistn="uniform")
sim5=simulate_balance_intercept(c=3, prevalence=prev_target,Ldistn="uniform")

sim_uniform<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_uniform$prev_target=prev_target
sim_uniform$c=c(1,1.5,2,2.5,3)
sim_uniform$distn="unif_n13"

#case with normal
sim1=simulate_balance_intercept(c=1, prevalence=prev_target,Ldistn="normal")
sim2=simulate_balance_intercept(c=1.5, prevalence=prev_target,Ldistn="normal")
sim3=simulate_balance_intercept(c=2, prevalence=prev_target,Ldistn="normal")
sim4=simulate_balance_intercept(c=2.5, prevalence=prev_target,Ldistn="normal")
sim5=simulate_balance_intercept(c=3, prevalence=prev_target,Ldistn="normal")

sim_normal<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_normal$prev_target=prev_target
sim_normal$c=c(1,1.5,2,2.5,3)
sim_normal$distn="norm_01"

#case with gamma
sim1=simulate_balance_intercept(c=1, prevalence=prev_target,Ldistn="gamma")
sim2=simulate_balance_intercept(c=1.5, prevalence=prev_target,Ldistn="gamma")
sim3=simulate_balance_intercept(c=2, prevalence=prev_target,Ldistn="gamma")
sim4=simulate_balance_intercept(c=2.5, prevalence=prev_target,Ldistn="gamma")
sim5=simulate_balance_intercept(c=3, prevalence=prev_target,Ldistn="gamma")

sim_gamma<-data.frame(rbind(sim1, sim2,sim3, sim4,sim5))
sim_gamma$prev_target=prev_target
sim_gamma$c=c(1,1.5,2,2.5,3)
sim_gamma$distn="gamma1_5"



assign(paste0("DF", prev_target), data.frame(rbind(sim_binary, sim_uniform, sim_normal,sim_gamma)))
}

#head(DF0.3)
DFall<-data.frame(rbind(DF0.1,DF0.2,DF0.3,DF0.4,DF0.5,DF0.6,DF0.7,DF0.8,DF0.9))
head(DFall)

write.csv(DFall, "simulate_balance_intercept_all_10million.csv") #10 million
#write.csv(DFall, "simulate_balance_intercept_all.csv")
