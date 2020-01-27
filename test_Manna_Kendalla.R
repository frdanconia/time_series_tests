###############################################################################
#		Test z aproksymacj¹ normaln¹ statystyki testowej			#
###############################################################################

# Patametr stats okreœla czy wyœwietliæ statystyki testu, czy tylko decyzjê stetystycz¹ na poziomie istotnoœci size
# size --- poziom istotnoœci testu (u¿ywany tylko przy podejmowaniu decyzji statystycznej)

Mann.Kendall.test <- function(x,stats=TRUE,size=0.05){
	n <- length(x)
	T <- 0
	for(k in 2:n) T <- T + sum(sign(x[k]-x[1:(k-1)]))
	sigma <- sqrt(n*(n-1)*(2*n+5)/18)
	p <- ifelse(n<30,2*sum(mannkendalldist(n)[-(1:abs(T))]),2*pnorm(-abs(T/sigma)))
	wynik <- list(method='Mann--Kendall test against trend', statistic=c(T=T),p.value=p,data.name=deparse(substitute(x)),alternative='Trend in sample')
  class(wynik) <- 'htest'
  if(stats)  return(wynik)
	else return(as.numeric(p>size))
}

######### Prawdziwy rozk³ad #############
mannkendalldist <- function(n){
	mmax <- n*(n-1)/2+1
	P <- matrix(0,n-1,mmax)
	P[1,2] <- 0.5
	for (k in 2:(n-1))
	for (m in 1:mmax){
	x <- abs(m+k-2*(1:(k+1))+1)
	P[k,m] <- sum(P[k-1,x[x<mmax]+1])/(k+1)
	}
	p <- P[n-1,]
	names(p) <- as.character(0:(mmax-1))
	return(p)
}