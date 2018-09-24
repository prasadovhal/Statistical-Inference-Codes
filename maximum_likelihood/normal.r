# -----------------------------------------------------------------------------------------
# Disclaimer: Shared as_is for non-commercial purposes with the hope that it may be useful,
# but without any claim or guarantee about performance, correctness, or documentation.
#
# Author: Mihir Arjunwadkar <mihir.arjunwadkar@gmail.com>
# -----------------------------------------------------------------------------------------

# log likelihood function for a normal sample
#
 log.likelihood.normal <- function( mu, sigma, X )
  {
   n <- length( X )

   S2   <- ( n - 1 ) * var( X ) / n
   Xbar <- mean( X )

   sapply( sigma, function( sigma ) { -n * log( sigma ) - 0.5 * n * ( S2 + ( Xbar - mu )^2 ) / sigma^2 } )
  }

# The "Truth"
#
 mu    <- 3
 sigma <- 2

# visualization
#
 grid.size <- 501 # finer grid ==> smoother contours, slower draw, larger storage

 mu.grid <- seq( floor( 0.5 * mu    ), ceiling( 1.5 * mu    ), length = grid.size ) # arbitrary choice
 se.grid <- seq( floor( 0.5 * sigma ), ceiling( 1.5 * sigma ), length = grid.size ) # arbitrary choice

 op <- par( mfrow = c( 2, 3 ) )

 for ( N in c( 5, 10, 15, 30, 100, 1000 ) ) # loop over sample sizes
  {
   X      <- rnorm( N, mu, sigma )            # data sample
   mu.hat <- mean( X )                        # MLE of mu
   se.hat <- sqrt( ( N - 1 ) * var( X ) / N ) # MLE of sigma; != sd( X ). this is the MLE for sigma.

   ll <- log.likelihood.normal( mu.grid, se.grid, X )

   contour( mu.grid, se.grid, ll, nlevels = 21, xlim = range( mu.grid, mu, mu.hat ), ylim = range( se.grid, sigma, se.hat ), asp = 1, col = 'blue', bty = 'n' )
   title( xlab = expression( mu ), ylab = expression( sigma ), main = paste( 'log(likelihood) for N(mu,sigma) | N =', N ) )

   points( mu, sigma,  pch = '+', cex = 2, col = 'red' )
   text( mu, sigma, 'TRUTH', pos = if ( mu < mu.hat ) 2 else 4, cex = 0.75, col = 'red' )

   points( mu.hat, se.hat, pch = '+', cex = 2, col = 'blue' )
   text( mu.hat, se.hat, 'MLE', pos = if ( mu < mu.hat ) 4 else 2, cex = 0.75, col = 'blue' )
  }

 par( op )
