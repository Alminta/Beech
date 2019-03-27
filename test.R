# ML-project.R
# adapt to the folder where the data are stored on your machine
RDataPath.n<-'.\\Data'
# if needed to handle chron objects: install.packages('chron')
require(chron)

list.files(RDataPath.n,'D')

load(paste(RDataPath.n,'\\Soroe_fluxAndMetData_1996_2018.RData',sep=''))

t.df = Soroe_fluxAndMetData_1996_2018.dfN

#pdf((paste(RDataPath.n,'\\tmp\\Soroe_tmp.pdf',sep='')))
#for (v in names(t.df)[-1])
#  plot(t.df$DT,t.df[,v],type='l',ylab=v,xlab='time')
#dev.off()

t0.df = Soroe_fluxAndMetData_1996_2018.df
t0.df[is.na(t0.df)] = 0


require(glmnet)

# NOT RUN {
# Gaussian

tmp = t.df[complete.cases(t.df[,2:6]),]

x = data.matrix(tmp[,2:5])
y = data.matrix(tmp[,6])



#x=matrix(rnorm(100*20),100,20)
#y=rnorm(100)
fit1=glmnet(x,)?re?
print(fit1)
coef(fit1,s=0.01) # extract coefficients at a single value of lambda
predict(fit1,newx=x[1:10,],s=c(0.01,0.005)) # make predictions

#multivariate gaussian
y=matrix(rnorm(100*3),100,3)
fit1m=glmnet(x,y,family="mgaussian")
plot(fit1m,type.coef="2norm")

#binomial
g2=sample(1:2,100,replace=TRUE)
fit2=glmnet(x,g2,family="binomial")

#multinomial
g4=sample(1:4,100,replace=TRUE)
fit3=glmnet(x,g4,family="multinomial")
fit3a=glmnet(x,g4,family="multinomial",type.multinomial="grouped")
#poisson
N=500; p=20
nzc=5
x=matrix(rnorm(N*p),N,p)
beta=rnorm(nzc)
f = x[,seq(nzc)]%*%beta
mu=exp(f)
y=rpois(N,mu)
fit=glmnet(x,y,family="poisson")
plot(fit)
pfit = predict(fit,x,s=0.001,type="response")
plot(pfit,y)

#Cox
set.seed(10101)
N=1000;p=30
nzc=p/3
x=matrix(rnorm(N*p),N,p)
beta=rnorm(nzc)
fx=x[,seq(nzc)]%*%beta/3
hx=exp(fx)
ty=rexp(N,hx)
tcens=rbinom(n=N,prob=.3,size=1)# censoring indicator
y=cbind(time=ty,status=1-tcens) # y=Surv(ty,1-tcens) with library(survival)
fit=glmnet(x,y,family="cox")
plot(fit)

# Sparse
n=10000;p=200
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
iz=sample(1:(n*p),size=n*p*.85,replace=FALSE)
x[iz]=0
sx=Matrix(x,sparse=TRUE)
inherits(sx,"sparseMatrix")#confirm that it is sparse
beta=rnorm(nzc)
fx=x[,seq(nzc)]%*%beta
eps=rnorm(n)
y=fx+eps
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
system.time(fit1<-glmnet(sx,y))
system.time(fit2n<-glmnet(x,y))
# }

print("hello")
