
require(magrittr)
require(fGarch)
require(tseries)
require(rugarch)
require(tictoc)
source("f_tv_models_revision.R")
source("true_value_function.R")


Simulation 

#  n = 200 


n<-lapply(c(1:5),function(x)garch_data(seed=x,n=1000))


## 1. seed = 1 


options(scipen = 999)
# time constant model 
x<-n[[5]]

spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_1 <- ugarchfit(spec, data = x )
coef(tc_200_1)[c(2,3,4)] %>% as.vector -> coefs; coefs



Garch_Recursion(coefs[1],coefs[2],coefs[3],x)->c_garch
data<-x
(data[-1]^2-c_garch[-1])^2 %>% mean 



# time varying model 

tic('파라미터 추정')
# c(0.00001, 0.01, 0.2)
#coefs
garch_param(c(0.00001, 0.01, 0.01),x,bw=0.4)-> param 
toc()



garch_estim(param,x) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
