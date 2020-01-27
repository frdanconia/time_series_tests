# Wycena europejskiej opcji kupna o cenie wykupu K na rynku o parametrach a,b,r,N

wycena_CRR <- function(a,b,r,N,S0,K){
  q <- (r-a)/(b-a) # Miara martyngalowa
  k <- 0:N
  f <- sapply(S0*(1+b)^k*(1+a)^(N-k)-K,max,0)*choose(N,k)*q^k*(1-q)^(N-k) # Wektor F
  C <- (1+r)^(-N)*sum(f) # Cena sprawiedliwa
  return(C)
}
