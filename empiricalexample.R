# packages  
library(ergm) # make sure you downloaded ergm from the source!
library(ergMargins) 

# load in data
load(paste("best_friend_adjmat2172wave4.RData", sep="")) # adjacency matrix including all strong tie nominations
load(paste("friend_adjmat2172wave4.RData", sep="")) # adjacency matrix including all weak tie nominations
load(paste("race2172wave4.RData", sep="")) # actor-level race data
load(paste("gender2172wave4.RData", sep="")) # actor-level gender data
load(paste("lunch2172wave4.RData", sep="")) # actor-level lunch data

# make weighted adjmat
adjmat_bf = adjmat_bf*2
adjmat = adjmat_bf+adjmat_f
adjmat[adjmat == 3] <- 2 # a couple kids nominate same people for best and regular, make those best

netw<-as.network(adjmat, directed =T,
                 matrix.type="a",ignore.eval=FALSE,
                 names.eval="nominations")

netw %v% "gender" <- gender
netw %v% "race" <- race
netw %v% "lunch" <- lunch

# Valued ERGM with standard nodematch terms
mod1 <- ergm(netw ~ nonzero +sum 
             +nodematch("race", form="nonzero")
             +nodematch("gender", form="nonzero")
             +nodematch("lunch", form="nonzero")
             +mutual("min")
             +transitiveweights("min","max","min")
             ,response="nominations", reference=~Binomial(2), control = control.ergm(MCMLE.maxit = 200))
summary(mod1)

# check diagnostics
mcmc.diagnostics(mod1)
vif.ergm(mod1)


# Valued ERGM with nodematchhigh and nodematchlow terms for race
mod2 <- ergm(netw ~ 
               nonzero
             +sum 
             +nodematchhigh(cut =2, attrname = "race", form="nonzero")
             +nodematchlow(cut =2, attrname = "race", form="nonzero")
             +nodematch("gender", form="nonzero")
             +nodematch("lunch", form="nonzero")
             +mutual("min")
             +transitiveweights("min","max","min") 
             ,response="nominations", reference=~Binomial(2), control = control.ergm(MCMLE.maxit = 200, seed = 110291))

# check diagnostics
mcmc.diagnostics(mod1)
vif.ergm(mod1)


# Valued ERGM with nodematchhigh and nodematchlow terms for gender
mod3 <- ergm(netw ~ 
               nonzero +sum 
             +nodematch("race", form="nonzero")
             +nodematchhigh(cut =2, attrname = "gender", form="nonzero")
             +nodematchlow(cut =2, attrname = "gender", form="nonzero")
             +nodematch("lunch", form="nonzero")
             +mutual("min")
             +transitiveweights("min","max","min") 
             ,response="nominations", reference=~Binomial(2), control = control.ergm(MCMLE.maxit = 5000, seed = 1102))
summary(mod3)

# check diagnostics
mcmc.diagnostics(mod1)
vif.ergm(mod1)
