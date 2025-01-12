#############################################
# Example: 2 state process (from CTMC file) #
#############################################

### First, we found the solution by directly solving the 
### forward and backward equations and we obtained (check!)
P_t.1 <- function(t, mu, lambda){
  matrix(c(lambda+mu*exp(-(mu+lambda)*t),
           mu-mu*exp(-(mu+lambda)*t),
           lambda-lambda*exp(-(mu+lambda)*t),
           mu+lambda*exp(-(mu+lambda)*t))
           ,nrow=2,byrow=T)/(lambda+mu)
}
# For instance, for mu=2, lambda=1 and t=3, we get
P_t.1(3,2,1)

### Then, we saw that the same solution can be obtained 
### from the matrix exponential

# Note: if you haven't done so, you need to install 
# the "expm" package:
# install.packages("expm")

# Then load the package
library("expm")

# Define the generator (as a function of mu and lambda)
G <- function(mu,lambda){
  matrix(c(-mu, mu, lambda, -lambda),nrow=2,byrow=T)
}
# For instance, for mu=2 and lambda=1, we get
G(2,1)

# And define the transition semigrup as a function 
# of t, and G
P_t.2 <- function(t, G){
  expm(t*G)
}
# For mu=2, lambda=1 and t=3, we get the same matrix as
# before (check!)
P_t.2(3,G(2,1))


### Finally, we can do it by diagonalization
# Eigenvalues
ev<-eigen(G(2,1))
D<-diag(ev$values)
V<-ev$vectors
inv_V<-solve(V)
# Check:
V%*%D%*%inv_V

P_t.3 <- V%*%diag(exp(3*ev$values))%*%solve(V)

##### EXERCIZE: Define the necessary functions to calculate P_t.3 as a function of t, mu and lambda

