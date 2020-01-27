########################### Generator serii ##############################
generujserie <- function(x){
  med <- median(x)
  y <- x
  y[x<=med] <- 1
  y[x>med] <- 0
  return(y)
}

############################ Licznik serii ###############################
ileserii <- function(x){
  y <- numeric(length(x))
  y[1] <- 1
  for(i in 2:length(x)) y[i] <- ifelse(x[i-1]==x[i],0,1)
  return(sum(y))
}

#################### Rozk³ad liczby serii ################################
rozklserii <- function(n0,n1){
  P <- numeric(n0+n1)
  if(n1*n0==0) P[1]=1 
  else{
    for(k in 2:(n0+n1)){
      if(k%%2==0) P[k] <- 2*choose(n0-1,n0-k/2)*choose(n1-1,n1-k/2)
      else P[k] <- choose(n0-1,n0-(k+1)/2)*choose(n1-1,n1-(k-1)/2) + choose(n1-1,n1-(k+1)/2)*choose(n0-1,n0-(k-1)/2)
    }
  }
  return(P/choose(n0+n1,n0))
}

################ Test serii Walda--Wolfowitza ###########################

# alternative --- ('left','right','twosided')alternatywa: lewostronny, prawostronny lub symetryczny obszar krytyczny
# asymptotic --- (TRUE/FALSE) czy stosowaæ rozk³ad asymptotyczny statystyki testowej
# stats --- (TRUE/FALSE) wyœwietliæ statystyki testu czy tylko decyzjê statystyczn¹ na poziomie istotnoœci size
# size --- poziom istotnoœci testu (u¿ywany tylko przy podejmowaniu decyzji statystycznej)

runs.test <- function(x,alternative='twosided',asymptotic=(length(x)>100),stats=TRUE,size=0.05){
  n <- length(x)
  y <- generujserie(x)
  K <- ileserii(y)
  n1 <- sum(y)
  n0 <- n-n1
  w.oczekiwana=(n+2*n1*n0)/n  # asymptotyka
  wariancja=(2*n1*n0*(2*n1*n0-n))/(n^2*(n-1)) # asymptotyka
  Knorm=(K-w.oczekiwana)/sqrt(wariancja) # asymptotyka
  if(alternative=='left')
    p.value <- ifelse(asymptotic, pnorm(Knorm),sum(rozklserii(n0,n1)[1:K]))
  if(alternative=='right')
    p.value <- ifelse(asymptotic, 1-pnorm(Knorm),sum(rozklserii(n0,n1)[K:n]))
  if(alternative=='twosided'){
    if(asymptotic==FALSE) P <- rozklserii(n0,n1)
    p.value <- ifelse(asymptotic, 2*pnorm(-abs(Knorm)),2*min(sum(P[1:K]),sum(P[K:n])))
  }
  wynik <- list(method='Wald--Wolfowitz Runs Test',statistic=c(K=K),perameter=c(Expectation=w.oczekiwana,Variance=wariancja),p.value=p.value,data.name=deparse(substitute(x)),alternative=alternative)  
  class(wynik) <- 'htest'
  if(stats) return(wynik)
  else return(as.numeric(p.value>size))
}