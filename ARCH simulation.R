 
require(magrittr)
require(fGarch)
require(tseries)
require(rugarch)
require(tictoc)
source("f_tv_models_revision.R")
source("true_value_function.R")
 

Simulation 

#  n = 200 

  
n200<-lapply(c(1:5),function(x)arch_data(seed=x,n=200))
 

## 1. seed = 1 

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_1 <- ugarchfit(spec, data = n200[[1]] )
coef(tc_200_1)[c(2,3)] %>% as.vector -> coefs; coefs
 
  
arch_Recursion(0.0201843,0.6258778,n200[[1]])->c_arch
data<-n200[[1]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[1]],bw=0.4)-> param 
toc()
 
  
data<-n200[[1]]
arch_estim(param,n200[[1]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 


