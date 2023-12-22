 
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
 
 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[1]],bw=0.4)-> param 
toc()
 
  
data<-n200[[1]]
arch_estim(param,n200[[1]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 
## 2. seed = 2

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_2 <- ugarchfit(spec, data = n200[[2]] )
coef(tc_200_2)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n200[[2]])->c_arch
data<-n200[[2]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[2]],bw=0.4)-> param 
toc()
 

  
data<-n200[[2]]
arch_estim(param,n200[[2]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 
## 3. seed =3 

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_3 <- ugarchfit(spec, data = n200[[3]] )
coef(tc_200_3)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n200[[3]])->c_arch
data<-n200[[3]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[3]],bw=0.5)-> param 
toc()
 

  
data<-n200[[3]]
arch_estim(param,n200[[3]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 




## 4. seed = 4 

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_4 <- ugarchfit(spec, data = n200[[4]] )
coef(tc_200_4)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n200[[4]])->c_arch
data<-n200[[4]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[4]],bw=0.4)-> param 
toc()
 

  
data<-n200[[4]]
arch_estim(param,n200[[4]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
  
## 5. seed = 5


  
# time constant model 
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_200_5 <- ugarchfit(spec, data = n200[[5]] )
coef(tc_200_5)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n200[[5]])->c_arch
data<-n200[[5]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n200[[5]],bw=0.4)-> param 
toc()
 

  
data<-n200[[5]]
arch_estim(param,n200[[5]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 







# n = 500 

  
n500<-lapply(c(1:5),function(x)arch_data(seed=x,n=500))
 

## seed = 1

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_500_1 <- ugarchfit(spec, data = n500[[1]] )
coef(tc_500_1)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n500[[1]])->c_arch
data<-n500[[1]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n500[[1]],bw=0.5)-> param 
toc()
 

  
data<-n500[[1]]
arch_estim(param,n500[[1]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 








## seed = 2
  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_500_2 <- ugarchfit(spec, data = n500[[2]] )
coef(tc_500_2)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n500[[2]])->c_arch
data<-n500[[2]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n500[[2]],bw=0.4)-> param 
toc()
 

  
data<-n500[[2]]
arch_estim(param,n500[[2]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean

 
## seed = 3

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_500_3 <- ugarchfit(spec, data = n500[[3]] )
coef(tc_500_3)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n500[[3]])->c_arch
data<-n500[[3]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n500[[3]],bw=0.5)-> param 
toc()
 

  
data<-n500[[3]]
arch_estim(param,n500[[3]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 


## seed = 4

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_500_4 <- ugarchfit(spec, data = n500[[4]] )
coef(tc_500_4)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n500[[4]])->c_arch
data<-n500[[4]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n500[[4]],bw=0.4)-> param 
toc()
 

  
data<-n500[[4]]
arch_estim(param,n500[[4]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
  



## seed = 5


  
# time constant model 
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_500_5 <- ugarchfit(spec, data = n500[[5]] )
coef(tc_500_5)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n500[[5]])->c_arch
data<-n500[[5]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n500[[5]],bw=0.4)-> param 
toc()
 

  
data<-n500[[5]]
arch_estim(param,n500[[5]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 






# n = 1000

  
n1000<-lapply(c(1:5),function(x)arch_data(seed=x,n=1000))
 


## seed = 1

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_1000_1 <- ugarchfit(spec, data = n1000[[1]] )
coef(tc_1000_1)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n1000[[1]])->c_arch
data<-n1000[[1]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n1000[[1]],bw=0.5)-> param 
toc()
 

  
data<-n1000[[1]]
arch_estim(param,n1000[[1]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 
## seed =2 

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_1000_2 <- ugarchfit(spec, data = n1000[[2]] )
coef(tc_1000_2)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n1000[[2]])->c_arch
data<-n1000[[2]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n1000[[2]],bw=0.4)-> param 
toc()
 

  
data<-n1000[[2]]
arch_estim(param,n1000[[2]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 
## seed = 3

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_1000_3 <- ugarchfit(spec, data = n1000[[3]] )
coef(tc_1000_3)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n1000[[3]])->c_arch
data<-n1000[[3]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n1000[[3]],bw=0.4)-> param 
toc()

 

  
data<-n1000[[3]]
arch_estim(param,n1000[[3]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 

## seed = 4

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_1000_4 <- ugarchfit(spec, data = n1000[[4]] )
coef(tc_1000_4)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n1000[[4]])->c_arch
data<-n1000[[4]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n1000[[4]],bw=0.4)-> param 
toc()

 

  
data<-n1000[[4]]
arch_estim(param,n1000[[4]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 



## seed = 5

  
# time constant model 

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                   mean.model = list(armaOrder = c(0, 0)))
tc_1000_5 <- ugarchfit(spec, data = n1000[[5]] )
coef(tc_1000_5)[c(2,3)] %>% as.vector -> coefs; coefs
 

  
arch_Recursion(coefs[1],coefs[2],n1000[[5]])->c_arch
data<-n1000[[5]]
(data[-1]^2-c_arch[-1])^2 %>% mean 
 

 {r warning = FALSE, message = FALSE}
# time varying model 

tic('파라미터 추정')
arch_param(coefs,n1000[[5]],bw=0.5)-> param 
toc()

 

  
data<-n1000[[5]]
arch_estim(param,n1000[[5]]) -> estimated
(data[-1]^2-estimated[-1])^2 %>% mean
 