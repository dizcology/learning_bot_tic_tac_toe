#testing0:

n_data=10
nnds=c(2,1)
y0=1
x0=c(1,0)
y1=matrix(y0+rnorm(n_data*length(y0),mean=0,sd=0.1),nnds[length(nnds)])
x1=matrix(x0+rnorm(n_data*length(x0),mean=0,sd=0.1),nnds[1])
newnn0=nn.initialize(nnds,mean=0,sd=1)
newnn0
nn.forward(newnn0,x0)$y
newnn0=nn.learn(newnn0,x1,y1,lambda=default_lambda)
newnn0
nn.forward(newnn0,x0)$y

#testing: 

yand=t(as.matrix(c(1,0,0,0)))
yor=t(as.matrix(c(1,1,1,0)))
yxor=t(as.matrix(c(0,1,1,0)))
ynxor=t(as.matrix(c(1,0,0,1)))


n_data=20
nnds=c(2,2,1)
y=yxor
x=rbind(c(1,1,0,0),c(1,0,1,0))
yy=matrix(as.numeric(y)+rnorm(n_data*length(y),mean=0,sd=0.01),nnds[length(nnds)])
xx=matrix(as.numeric(x)+rnorm(n_data*length(x),mean=0,sd=0.01),nnds[1])
newnn=nn.initialize(nnds,mean=0,sd=1)
newnn
for (i in 1:dim(x)[2]){print(nn.forward(newnn,x[,i])$y)}
newnn=nn.learn(newnn,xx,yy,lambda=default_lambda)
newnn
for (i in 1:dim(x)[2]){print(nn.forward(newnn,x[,i])$y)}
