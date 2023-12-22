
require(magrittr)
require(fGarch)
require(tseries)
require(rugarch)
require(tictoc)
source("f_tv_models_revision.R")
source("true_value_function.R")


Simulation 



n<-lapply(c(1:5),function(x)igarch_data(seed=x,n=200))


## 1. seed = 1 


options(scipen = 999)
# time constant model 
x<-n[[5]]

spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)))
fit <- ugarchfit(spec, data = x )
coef(fit)[c(2,3,4)] %>% as.vector -> coefs; coefs



IGarch_Recursion(coefs[1],coefs[2],coefs[3],x)->c_garch
data<-x
(data[-1]^2-c_garch[-1])^2 %>% mean %>% log


# time varying model 

tic('파라미터 추정')
# c(0.00001, 0.0001)
#coefs[1],coefs[2]
igarch_param(c(0.0001, 0.0001),x,bw=0.4)-> param 
toc()



igarch_estim(param,x) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean %>% log
