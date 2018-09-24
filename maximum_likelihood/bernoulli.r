# -----------------------------------------------------------------------------------------
# Disclaimer: Shared as_is for non-commercial purposes with the hope that it may be useful,
# but without any claim or guarantee about performance, correctness, or documentation.
#
# Author: Mihir Arjunwadkar <mihir.arjunwadkar@gmail.com>
# -----------------------------------------------------------------------------------------

# log likelihood function for a Bernoulli(p) sample
#
 log.likelihood.bernoulli <- function( data, p = seq( 0, 1, by = 0.1 ) )
  {
   N <- length( data )
   S <- sum( data )

   L    <- p^S * ( 1 - p )^( N - S )
   logL <- S * log( p ) + ( N - S ) * log( 1 - p )

   cbind( p, L, logL )
  }

# The "Truth"
#
 coin.bias <- 0.1 # p = 0.5 + coin.bias; -0.5 <= coin.bias <= 0.5

# visualization
#
 op <- par( mfrow = c( 2, 3 ) )

 for ( N in c( 5, 10, 15, 30, 100, 1000 ) ) # loop over sample sizes
  {
   coin.tosses <- sample( c( 1, 0 ), N, replace = T, prob = 0.5 + c( 1, -1 ) * coin.bias ) # data sample

   # cat( coin.tosses, '\n' )
   # cat( sum( coin.tosses ), 'heads,', N - sum( coin.tosses ), 'tails\n\n' )

   ll  <- log.likelihood.bernoulli( coin.tosses, p = seq( 0, 1, by = 0.01 ) )
   mle <- sum( coin.tosses ) / length( coin.tosses )

   plot( ll[,'p'], ll[,'L'], type = 'l', xlab = 'p', ylab = 'log( likelihood )', main = paste( N, 'Tosses,', sum( coin.tosses ), 'Heads' ), bty = 'n' )
   abline( v = mle, col = 'blue', lty = 2 )
   abline( v = 0.5 + coin.bias, col = 'red' )
   mtext( 'TRUTH', 3, at = 0.5 + coin.bias, line = 0.25, cex = 0.5, col = 'red' )
   text( mle, 1.01 * max( ll[,'L'] ), 'MLE', cex = 0.75, col = 'blue', adj = c( 0.5, 0 ) )
  }

 par( op )
