# tvARCH--------------------------------------------

arch_data<-function(seed,n){
  set.seed(seed)
  Ti <- n
  t <- 0:Ti/Ti
  mut0 <- 10*exp(-(t-0.5)^2/0.1)
  at10 <- 0.4*(t-0.15)^2 + 0.1
  datavect <- rep(0, Ti+1)
  datavect[1] <- rnorm(1, 0, sqrt(mut0[1]))
  datavect[2] <- rnorm(1, 0, sqrt(mut0[2]+at10[2]*datavect[1]^2))
  for(i in 3:Ti){
    datavect[i] <- rnorm(1, 0, sqrt(mut0[i]+at10[i]*datavect[i-1]^2))
  }
  
  data <- datavect[c(1:n)]
  return(data)
}

# tvGARCH ------------------------------------------------------

garch_data<-function(seed,n){
  
  set.seed(seed)
  
  resolution=n #means we compute at every 1/n
  
  g=(0:resolution)/resolution;
  #a1=a1fun(g);a0=a0fun(g);b1=b1fun(g);
  
  
  a0= 1-0.8*sin(0.5*pi*g)
  a1 = -1*(g-0.3)^2 + 0.5
  b1 = 0.4-0.5*(g-0.4)^2
  
  e=rnorm(n);x=e;sigma2=array(0,n)
  sigma20=a0[1]/(1-b1[1]);x0=rnorm(1, 0, sqrt(sigma20));
  sigma2[1]=a0[1]+a1[1]*x0^2+b1[1]*sigma20;x[1]=rnorm(1, 0, sqrt(sigma2[1]))
  
  for (i in 2:n)
  {
    sigma2[i]=a0[i]+a1[i]*x[i-1]^2+b1[i]*sigma2[i-1]
    x[i]= rnorm(1, 0, sqrt(sigma2[i]))
  }  
  #data <- as.numeric(c(x0,x))
  data=x[c(1:n)]
  
  return(data)
  
}

# tviGarch -------------------------------------------------

igarch_data<-function(seed,n){
  set.seed(seed)
  resolution=n #means we compute at every 1/n
  
  g=(1:resolution)/resolution;
  
  cilmu=numeric(0);cila1=numeric(0);cilb1=numeric(0)
  accumu=numeric(0);accua1=numeric(0);accub1=numeric(0)
  
  a0 <- exp(-(g-0.5)^2/0.1)
  a1 <- 0.4*(g-1)^2+0.1
  b1 <- 1-a1 
  
  e=rnorm(n);x=e;sigma2=array(0,n)
  sigma20=a0[1]/(1-b1[1]);x0=rnorm(1, 0, sqrt(sigma20));
  sigma2[1]=a0[1]+a1[1]*x0^2+b1[1]*sigma20;x[1]=rnorm(1, 0, sqrt(sigma2[1]))
  
  for (i in 2:n)
  {
    sigma2[i]=a0[i]+a1[i]*x[i-1]^2+b1[i]*sigma2[i-1]
    x[i]= rnorm(1, 0, sqrt(sigma2[i]))
  }  
  #data <- as.numeric(c(x0,x))
  data=x[c(1:n)]
  
  return(data)
  
}  
