# Wycena europejskiej opcji kupna wzorem Blacka-Scholesa

wycena_BS <- function(K,S0=1,sigma=1,r=0,T=1){
  y1 <- (log(S0/K)+T*(r+sigma^2/2))/(sigma*sqrt(T))
  y2 <- (log(S0/K)+T*(r-sigma^2/2))/(sigma*sqrt(T))
  C <- S0*pnorm(y1) - K*exp(-r*T)*pnorm(y2)
  return(C)
}

# Cena sprawiedliwa w zaleznoaci od r,sigma,K.
rate <- seq(-0.95,1,by=0.05) # stopy procentowe r
sigma <- seq(-2,2,by=0.05); sigma <- sigma[sigma != 0] # zmienności rynku
wykup <- seq(0,2,by=0.05) # ceny wykupu

# Trojwymiarowa macierz cen
cena <- array(0,c(length(rate),length(sigma),length(wykup)), c("r","sigma","K"))
for(i in 1:length(rate))
  for(j in 1:length(sigma))
    for(k in 1:length(wykup))
      cena[i,j,k] <- wycena_BS(wykup[k],sigma=sigma[j],r=rate[i])

# Wykres 3D
library(rgl)
rysuj <- function(x,y,z,...,labs=c("sigma","K","Cena")){
 # kolorowanie wykresu
  h <- (z-min(z))/(max(z)-min(z)) 
  r.prop <- h
  g.prop <- 0
  b.prop <- 1 - h
  color  <- rgb(r.prop, g.prop, b.prop, maxColorValue=1)
  persp3d(x,y,z,col=color,...,xlab=labs[1],ylab=labs[2],zlab=labs[3]) # wykres
}
