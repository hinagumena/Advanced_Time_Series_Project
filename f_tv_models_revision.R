
# kernel model ------------------------------------------ 
epa<-function(x){ifelse(abs(x)<=1,3/4*(1-x^2),0)}

kernel_value <-function(t,data,bw){
  n<-length(data)
  weight<-sapply((t-(1:n)/n)/bw,epa)
  return(weight)}

# tvARCH(1,1) -----------------------------------------------------

arch_Recursion <- function(omega, alpha, data){
  # conditional variance matrix:
  Sigma2 <- NULL
  # Store "arch value" for each data:
  for (i in 1:length(data)){
    # Initial variance -> unconditional variance/long run variance/sample variance
    if (i == 1){
      Sigma2[i] <- var(data)
    } 
    else
      # arch(1,1) model:
      Sigma2[i] <- omega + alpha*(data[i-1])^2 
  }
  return(Sigma2)
}

arch_Loglikelihood <- function(arch_parameters, data,t,bw){
  # Create conditional variance matrix using garch recursion function:
  omega <- arch_parameters[1]
  alpha <- arch_parameters[2]
  
  conditional_variance <- arch_Recursion(omega, alpha,data)
  
  #Loglikelihood function estimation
  Loglikelihood_value <- -0.5*(log(2*pi) + log(conditional_variance) + data^2/conditional_variance )
  
  weight<-kernel_value(t,data,bw)
  objective<-sum(weight[-1]*Loglikelihood_value[-1])
  
  return(-objective)# (-) because optim minimizes by default
} 

ARCH<-function(arch_parameters, data,t,bw){
  
  # Maximize loglike by calling arch_loglike in optim function
  MLE <- optim(arch_parameters, 
               arch_Loglikelihood, 
               gr = NULL, 
               data,
               t,
               bw,
               lower = c(0, 0),      # 각 파라미터의 하한 설정
               upper = c(Inf, 1))
  
  return(MLE$par)}

arch_param <-function(arch_parameters,data,bw){
  n<-length(data)
  t(sapply((1:n)/n,
         function(x)ARCH(arch_parameters,data,t=x,bw))) -> param_vector
  return(param_vector)
}

arch_estim<-function(param,data){
  n<-nrow(param)
  estimated <-numeric(n)
  mu <- param[,1]
  a <- param[,2]
  
  estimated[1]<-var(data)
  for(i in 2:n){
    estimated[i]<-mu[i]+a[i]*data[i-1]^2}
  
  return(estimated)}

# tvGARCH(1,1)--------------------------------------------------------

# Create function that stores the "Garch value" for each data:
Garch_Recursion <- function(omega, alpha, beta, data){
  # conditional variance matrix:
  Sigma2 <- NULL
  # Store "garch value" for each data:
  for (i in 1:length(data)){
    # Initial variance -> unconditional variance/long run variance/sample variance
    if (i == 1){
      Sigma2[i] <- var(data)
    } 
    else
      # garch(1,1) model:
      Sigma2[i] <- omega + alpha*(data[i-1])^2 + beta*Sigma2[i-1]
  }
  return(Sigma2)
}


# Garch loglikelihood function
Garch_Loglikelihood <- function(garch_parameters, data,t,bw){
  # Create conditional variance matrix using garch recursion function:
  omega <- garch_parameters[1]
  alpha <- garch_parameters[2]
  beta <- garch_parameters[3]
  
  conditional_variance <- Garch_Recursion(omega, alpha, beta, data)
  
  #Loglikelihood function estimation
  Loglikelihood_value <- -0.5*(log(2*pi) + log(conditional_variance) + data^2/conditional_variance )
  
  weight<-kernel_value(t,data,bw)
  objective<-sum(weight[-1]*Loglikelihood_value[-1])
  
  return(-objective)# (-) because optim minimizes by default
} 


GARCH<-function(garch_parameters, data,t,bw){
  
  # Maximize loglike by calling garch_loglike in optim function
  MLE <- optim(garch_parameters, 
               Garch_Loglikelihood, 
               gr = NULL, 
               data,
               t,
               bw,
               lower = c(0, 0,0),      # 각 파라미터의 하한 설정
               upper = c(Inf, 1,1)
               )
  
  return(MLE$par)}

garch_param <-function(garch_parameters,data,bw){
  n<-length(data)
  t(sapply((1:n)/n,
           function(x)GARCH(garch_parameters,data,t=x,bw))) -> param_vector
  return(param_vector)
}

garch_estim<-function(param,data){
  n<-nrow(param)
  estimated <-numeric(n)
  mu <- param[,1]
  a <- param[,2]
  b <- param[,3]
  
  estimated[1]<-var(data)
  for(i in 2:n){
    estimated[i]<-mu[i]+a[i]*data[i-1]^2+b[i]*estimated[i-1]}
  
  return(estimated)}

# tvIGARCH (1,1)--------------------------------------------
IGarch_Recursion <- function(omega, alpha, beta, data){
  # conditional variance matrix:
  Sigma2 <- NULL
  # Store "garch value" for each data:
  for (i in 1:length(data)){
    # Initial variance -> unconditional variance/long run variance/sample variance
    if (i == 1){
      Sigma2[i] <- var(data)
    } 
    else
      # garch(1,1) model:
      Sigma2[i] <- omega + alpha*(data[i-1])^2 + beta*Sigma2[i-1]
  }
  return(Sigma2)
}

IGarch_Loglikelihood <- function(igarch_parameters, data,t,bw){
  # Create conditional variance matrix using garch recursion function:
  omega <- igarch_parameters[1]
  alpha <- igarch_parameters[2]
  beta <- 1-alpha
  
  conditional_variance <- IGarch_Recursion(omega, alpha, beta, data)
  
  #Loglikelihood function estimation
  Loglikelihood_value <- -0.5*(log(2*pi) + log(conditional_variance) + data^2/conditional_variance )
  
  weight<-kernel_value(t,data,bw)
  objective<-sum(weight[-1]*Loglikelihood_value[-1])
  
  return(-objective)# (-) because optim minimizes by default
} 

IGARCH<-function(igarch_parameters, data,t,bw){
  
  # Maximize loglike by calling garch_loglike in optim function
  MLE <- optim(igarch_parameters, 
               IGarch_Loglikelihood, 
               gr = NULL, 
               data,
               t,
               bw,
               lower = c(0, 0),      # 각 파라미터의 하한 설정
               upper = c(Inf, 1)
               )
  
  return(MLE$par)}

igarch_param <-function(igarch_parameters,data,bw){
  n<-length(data)
  t(sapply((1:n)/n,
           function(x)IGARCH(igarch_parameters,data,t=x,bw))) -> param_vector
  return(param_vector)
}

igarch_estim<-function(param,data){
  n<-nrow(param)
  estimated <-numeric(n)
  mu <- param[,1]
  a <- param[,2]
  b <- 1-a
  
  estimated[1]<-var(data)
  for(i in 2:n){
    estimated[i]<-mu[i]+a[i]*data[i-1]^2+b[i]*estimated[i-1]}
  
  return(estimated)} 