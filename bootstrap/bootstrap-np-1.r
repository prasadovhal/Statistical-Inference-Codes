# estimator for the quantity of interest
#
# this should be an R function; you can replace this with any estimator of your interest
#
 estimator <- median
 estimator <- mean

# data size
#
 N <- 25

# artificial/simulated data from Uniform(0,1)
#
# you can replace this with any distribution of your interest
#
 X <- runif( N )

# estimate of the quantity of interest from the data
#
 T.hat <- estimator( X )

# some visualization
#
 op <- par( mfrow = c( 2, 1 ) )

 # plot the empirical CDF of the data together with data values on the x-axis:
 # illustration that ecdf() puts mass of 1/n at each data point;
 # this will look good only for sufficiently small N, say 10-30.
 #
 plot( ecdf( X ), xlim = c( 0, 1 ), main = 'ECDF( X )' )
 rug( X, lwd = 1.5 )
 abline( h = ( 1:(N-1) ) / N, col = 'grey', lty = 3 )

 # location of T.hat on the ECDF plot
 #
 abline( v = T.hat, col = 'red', lty = 2 )
 axis( 3, at = T.hat, lab = parse( text = paste( 'widehat( T ) ==', T.hat ) ), col = 'red', col.axis = 'red', cex.axis = 0.65, line = -0.5 )

 # true CDF of Uniform(0,1) over [0,1], and the true value (0.5) of the mean/median of Uniform(0,1)
 # turn this off if the data distribution is not Uniform(0,1)
 #
 lines( c( 0, 1 ), c( 0, 1 ), col = 'green', lty = 1 )
 abline( v = 0.5, col = 'green', lty = 2 )

# bootstrap loop
#
 B      <- 1000                          # number of bootstrap (re)samples
 T.star <- rep( NA, B )                  # array to hold bootstrap estimates
 for ( i in 1:B )                        # the bootstrap loop
  {
   # X.star    <- sample( X, replace = T ) # generate a bootstrap sample by resampling values from the data
   # T.star[i] <- estimator( X.star )      # estimate of the quantity of interest for X.star

   T.star[i] <- estimator( sample( X, replace = T ) ) # same as above; there is no need to store the X.star (re)sample
  }

 T.star.mean <- mean( T.star )           # sample average over all bootstrap samples
 T.star.se   <- sd( T.star )             # bootstrap estimated standard error for quantity of interest
                                         # (compare with sd( X ) / sqrt( N ) when estimator == mean)

# bootstrap confidence intervals
#
 alpha <- 0.05 # confidence level

 ci.normal     <- T.hat + c( -1 , 1 ) * qnorm( 1 - 0.5 * alpha ) * T.star.se                            # normal-based CI
 ci.pivot      <- 2 * T.hat - c( quantile( T.star, 1 - 0.5 * alpha ), quantile( T.star, 0.5 * alpha ) ) # pivot-based CI
 ci.percentile <- c( quantile( T.star, 0.5 * alpha ), quantile( T.star, 1 - 0.5 * alpha ) )             # percentile-based CI

 # arrows( ci.percentile[1],  0, ci.percentile[2],  0, length = 0.2, code = 3, col = 'black' ) # percentile-based CI
 # arrows( ci.pivot[1],       0, ci.pivot[2],       0, length = 0.2, code = 3, col = 'blue'  ) # pivot-based CI
 # arrows( ci.normal[1],      0, ci.normal[2],      0, length = 0.2, code = 3, col = 'red'   ) # normal-based CI

 h <- hist( T.star, nclass = nclass.FD, xlim = c( 0, 1 ), # histogram of the bootstrap sample T.star
       main = 'Histogram of Bootstrap-Resampled Estimates', col = 'grey', border = 'white' )
 rug( T.star, col = 'grey' )
 
 abline( v = T.hat, col = 'red', lty = 2 ) # location of T.hat
 axis( 3, at = T.hat, lab = parse( text = paste( 'widehat( T ) ==', T.hat ) ), col = 'red', col.axis = 'red', cex.axis = 0.65, line = -0.5 )

 # abline( v = T.star.mean, col = 'blue', lty = 2 )   # location of T.star.mean
 abline( v = 0.5, col = 'green', lty = 2 ) # true value of the mean/median of Uniform(0,1):
                                           # turn this off if the data distribution is not Uniform(0,1)

 arrows( ci.percentile[1],  0, ci.percentile[2],  0, length = 0.2, code = 3, col = 'black' ) # percentile-based CI
 arrows( ci.pivot[1],       0, ci.pivot[2],       0, length = 0.2, code = 3, col = 'blue'  ) # pivot-based CI
 arrows( ci.normal[1],      0, ci.normal[2],      0, length = 0.2, code = 3, col = 'red'   ) # normal-based CI

 legend( "topleft", c( 'Confidence Intervals', 'Normal-Based', 'Percentile', 'Pivot-Based' ), col = c( 'black', 'red', 'black', 'blue' ), lty = c( 0, 1, 1, 1 ), bty = 'n', cex = 0.65 )
 legend( "left", parse( text = paste( 'widehat( SE( widehat( T ) ) ) ==', format( T.star.se ) ) ), cex = 0.65, col = 'red', bty = 'n' )

 par( op )
