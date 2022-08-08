# In this brief script I investigate the effect of causal structure on multiple linear regression estimates
# in simple case of "case-control bias" (https://youtu.be/NSuTaeW6Orc?t=3652)

# Inspired by the assertion of Wysocki et al. (2022, https://dx.doi.org/10.1177/25152459221095823) that they
# attribute to Cohen et al. (2003, Applied multiple regression/correlation analysis for the behavioral sciences)
# that "Controlling for a variable that shares variance with the outcome but not the predictor will decrease
# the amount of residual variance in the outcome, which, in turn, lowers the standard error of the estimated
# regression coefficient and increases power (Cohen et al., 20003)." (Wysocki et al. 2022, p. 3)

# load rethinking to mcreplicate and density plotting
library("rethinking")

# prepare a function to generate synthetic data from one of the three causal models:
# 1) case-control bias ("cc"): X -> Y -> Z
# 2) multiple causes ("mc"): X -> Y <- Z
# 3) common variance ("cv"): X -> Y <- u -> Z
f <- function( n = 100, bXY = 1, bZY = 1, buY = 1, buZ = 1, model = "cc" ) {
  
  # prepare the data
  # X (predictor) and u (unobserved 'confounder') has no in-model causes, so just generate them
  X <- rnorm( n )
  u <- rnorm( n )
  # depending on the causal model, generate Z (covariate) and Y (outcome)
  if( model == "cc" ) {
    Y <- rnorm( n , bXY*X )
    Z <- rnorm( n , bZY*Y )
  } else if( model == "mc" ) {
    Z <- rnorm( n )
    Y <- rnorm( n , bXY*X + bZY*Z )
  } else if( model == "cv" ) {
    Z <- rnorm( n , buZ*u )
    Y <- rnorm( n , bXY*X + buY*u )
  }
  
  # calculate regressions
  bX <- summary( lm(Y ~ X) )[['coefficients']][ 'X' , c('Estimate','Std. Error') ]
  bXZ <- summary( lm(Y ~ X + Z) )[['coefficients']][ 'X' , c('Estimate','Std. Error') ]
  
  # return the results
  return( cbind(bX,bXZ) )

}

# simulate data for each of the causal model
sim <- list()
N <- 100 # number of simulated observations

# do the simulations
sim$cc <- mcreplicate( 1e4 , f(n = N, model="cc"), mc.cores = 4)
sim$mc <- mcreplicate( 1e4 , f(n = N, model="mc"), mc.cores = 4) 
sim$cv <- mcreplicate( 1e4 , f(n = N, model="cv"), mc.cores = 4) 

# prepare environment for plotting
par( mfrow = c(3,3) )

# plot dags for each simulation
plot( dagitty::dagitty( "dag { X -> Y -> Z }" ) )
title( "Case-control bias")
plot( dagitty::dagitty( "dag { X -> Y <- Z }" ) )
title( "Multiple causes" )
plot( dagitty::dagitty( "dag { X -> Y <- u -> Z }" ) )
title( "Common variance" )

# plot mean estimates
for ( i in c("cc","mc","cv") ) {
  dens( sim[[i]][ "Estimate", "bXZ", ] , lwd = 3 , col = 2 , xlab = "bX (mean)" ,
        # set all x-axes according to min-max across simulations
        xlim = c( min( c( min( sim$cc["Estimate", , ] ), min( sim$mc["Estimate", , ] ), min( sim$cv["Estimate", , ] ) ) ),
                  max( c( max( sim$cc["Estimate", , ] ), max( sim$mc["Estimate", , ] ), max( sim$cv["Estimate", , ] ) ) ) )
        )
  dens( sim[[i]][ "Estimate", "bX", ] , lwd = 3 , add = T )
}

# plot standard errors
for ( i in c("cc","mc","cv") ) {
  dens( sim[[i]][ "Std. Error", "bXZ", ] , lwd = 3 , col = 2 , xlab = "bX (SE)" ,
        # set all x-axes according to min-max across simulations
        xlim = c( min( c( min( sim$cc["Std. Error", , ] ), min( sim$mc["Std. Error", , ] ), min( sim$cv["Std. Error", , ] ) ) ),
                  max( c( min( sim$cc["Std. Error", , ] ), max( sim$mc["Std. Error", , ] ), max( sim$cv["Std. Error", , ] ) ) ) )
  )
  dens( sim[[i]][ "Std. Error", "bX", ] , lwd = 3 , add = T )
}

