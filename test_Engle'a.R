########################################################################
#                  Test Engle'a --- iid VS ARCH
########################################################################

# p --- liczba regresor?w (parametr modelu ARCH)
# stats --- (TRUE/FALSE) Je?li FALSE wynikiem jest tylko decyzja statystyczna na poziomie istotno?ci size

# Je?li stats=TRUE, wynikiem jest obiekt klasy 'htest' (statystyka testowa, parametry)
# Je?li stats=FALSE, wynikiem jest decyzja statystyczna (0-'Odrzu? H_0'; 1-'Nie ma podstaw do odrzucenia H_0') na poziomie istotno?ci size

engle.test <- function(x,p=2,stats=TRUE,size=0.05){
  n <- length(x)
  
  y <- x[(p+1):n]^2
  X <- matrix(1,n-p,p+1)
  for(i in 1:p){
    X[,i+1] <- x[i:(i+n-p-1)]^2
  }
  
  my <- mean(y)
  fy <- lm(y~X)$fitted.values
  nR2 <- n*sum((fy-my)^2)/sum((y-my)^2)
  p.value <- 1-pchisq(nR2,df=p)
  
  if(stats){
    wynik <- list(method='Engle test for ARCH effect',parameter=c(df=p),statistic=c(NR2=nR2),p.value=p.value,data.name=deparse(substitute(x)))
    class(wynik) <- 'htest'
  }
  else wynik <- as.numeric(p.value>0.05)
      
  return(wynik)
}
