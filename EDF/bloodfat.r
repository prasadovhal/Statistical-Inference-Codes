#cholesterol is high that doesn't mean person has heart disease,corelation between cholesterol and heart disease is strong. (heart diease and heart attack are not same,heart attack is a part of heart disease)
#corelation doesn't imply causesation

# Example 7.15 in All of Statistics (Springer, 2004) by Larry Wasserman
#
# Exercise
# -- Visualize the data in as many ways as possible.
# -- Verify the computated numbers in the analysis, and interprete your results.
# -- Explore if (and how far) the lower end of the CI on theta can be pushed close to 0 by changing the confidence level.

# read data
x <- matrix( scan( 'bloodfat.dat', comment.char = '#' ), ncol = 2, byrow = TRUE, dimnames = list( NULL, c( 'Cholesterol', 'Triglycerides' ) ) )

nhd <- 1:51                        # no evidence for heart disease
noa <- setdiff( 1:nrow( x ), nhd ) # narrowing of arteries

# visualize
op <- par( mfcol = c( 2, 3 ) )
 for ( log in c( FALSE, TRUE ) )
  {
   plot.new()
   plot.window( xlim = range( x[,1] ), ylim = range( x[,2] ), log = if ( log ) 'xy' else '' )

   rug( x[noa,1], side = 1, col = 'darkorange',    ticksize = +0.02 )
   rug( x[noa,2], side = 2, col = 'darkorange',    ticksize = +0.02 )
   rug( x[nhd,1], side = 1, col = 'springgreen', ticksize = -0.02 )
   rug( x[nhd,2], side = 2, col = 'springgreen', ticksize = -0.02 )

   axis( 1 ); axis( 2 )
   title( xlab = if ( log ) paste( 'log(', colnames( x )[1], ')', sep = '' ) else colnames( x )[1],
          ylab = if ( log ) paste( 'log(', colnames( x )[2], ')', sep = '' ) else colnames( x )[2],
          main = if ( log ) 'Scott 1978 Log(Data)' else 'Scott 1978 Data' )

   points( x[noa,], pch = 21, bg = 'darkorange'    )
   points( x[nhd,], pch = 21, bg = 'springgreen' )

   fit.nhd <- lm( Triglycerides ~ Cholesterol, data = as.data.frame( if ( log ) log10( x ) else x ), subset = nhd )
   fit.noa <- lm( Triglycerides ~ Cholesterol, data = as.data.frame( if ( log ) log10( x ) else x ), subset = noa )

   abline( coef( fit.nhd ), col = 'springgreen', lty = 2 )
   abline( coef( fit.noa ), col = 'darkorange',  lty = 2 )
  }

 for ( i in 1:ncol( x ) )
  {
   hist( x[nhd,i], 'FD', border = 'white', col = 'gray', xlim = range( x[,i] ),
         main = paste( colnames( x )[i], ', No Evidence for Heart Disease', sep = '' ), xlab = colnames( x )[i] )
   rug( x[nhd,i], side = 1, ticksize = 0.02, col = 'springgreen' )
   axis( 1 )

   hist( x[noa,i], 'FD', border = 'white', col = 'gray', xlim = range( x[,i] ),
         main = paste( colnames( x )[i], ', Narrowing of Arteries', sep = '' ), xlab = colnames( x )[i] )
   rug( x[noa,i], side = 1, ticksize = 0.02, col = 'darkorange' )
   axis( 1 )
  }
par( op )

# Analysis from Example 7.15

source( '~/Public/R/Z_ab2.r' )

ms <- function( x, nhd, noa )
 {
  # mean and se for individual groups
  m <- c( mean( x[nhd,'Cholesterol'] ), mean( x[noa,'Cholesterol'] ) )
  s <- c( sd( x[nhd,'Cholesterol'] ) / sqrt( length( nhd ) ), sd( x[noa,'Cholesterol'] ) / sqrt( length( noa ) ) )

  ms <- cbind( m, s )
  colnames( ms ) <- c( 'mean', 'se' )
  rownames( ms ) <- c( 'nhd', 'noa' )

  # difference in the means
  ms <- rbind( ms, c( diff( m ), sqrt( sum( s^2 ) ) ) )
  rownames( ms)[3] <- 'diff'

  ms
 }

ci <- function( ms, alpha = 0.05 )
 {
  stopifnot( is.matrix( ms ), ncol( ms ) == 2 )

  #zab2 <- Z_ab2( alpha )
  zab2 <- qnorm(1 - 0.5* alpha )
  print(zab2)

  ci <- t( sapply( 1:nrow( ms ), function( i ) { ms[i,1] + c( -1, +1 ) * zab2 * ms[i,2] } ) )

  colnames( ci ) <- c( 'lower', 'upper' )
  rownames( ci ) <- rownames( ms )

  ci
 }

# mean and se
ms <- ms( x, nhd, noa )

# CI on difference in the means
alu <- NULL
for ( alpha in 0.4 / 2^( 0:15 ) ) alu <- rbind( alu, c( 100*(1-alpha), ci( matrix( ms[3,], nrow = 1 ), alpha ) ) )
colnames( alu ) <- c( '100*(1-alpha)', 'lower', 'upper' )

cat( '\n' ); print( ms )
cat( '\n' ); print( alu )
