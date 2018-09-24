theta <- 1
n     <- 10
u     <- runif( n, 0, theta )

curve( x^(-n), from = max( u ), to = 1.5 * max( u ), lwd = 2, bty = 'n',
       xlab = expression( theta ), ylab = 'Likelihood', xlim = c( 0, 1.5 * max( u ) ) )
lines( c( 0, max( u ) ), c( 0, 0 ), lwd = 2 )
abline( v = max( u ), lty = 2 )
rug( u, col = 'red', lwd = 1.5 )
axis( 3, at = max( u ), label = expression( widehat( theta )[n] ) )
