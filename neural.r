library(ggplot2)

search_interval=c(-300,300)
default_threshold=0.001

default_lambda=0

nn.initialize = function(n_nodes=c(1,1),mean=0,sd=0.1,fct=1){
  n_layers=length(n_nodes)
  theta=rep(list(NA),n_layers-1)

  for (i in 1:(n_layers-1)){
    theta[[i]]=fct*matrix(rnorm(n_nodes[i+1]*(n_nodes[i]+1),mean=mean,sd=sd),n_nodes[i+1],n_nodes[i]+1)
  }
  
  
  return(list(n_nodes=n_nodes, theta=theta))
}

nn.sigmoid = function(z){
  return(1/(1+exp(-z)))
}

nn.forward = function(nn,x,f=nn.sigmoid){
  n_layers=length(nn$n_nodes)
  if (length(x)!=nn$n_nodes[1]){
    print("input size incorrect")
    return(nn)
  }
  
  z=rep(list(NA),n_layers)
  a=rep(list(NA),n_layers)
  z[[1]]=x
  a[[1]]=x #f(x) or x
  for (i in 2:n_layers){
    z[[i]]=nn$theta[[i-1]]%*%as.matrix(c(a[[i-1]],1))
    a[[i]]=f(z[[i]])
  }
  
  return(list(z=z,a=a,x=x,y=a[[n_layers]]))
}

nn.backward = function(nn,x,y,f=nn.sigmoid){
  n_layers=length(nn$n_nodes)
  if (length(y)!=nn$n_nodes[n_layers]){
    print("output size incorrect")
    return(nn)
  }
  
  nnf=nn.forward(nn,x,f)
  
  del=rep(list(NA),n_layers)
  del[[n_layers]]=(nnf$y-y)*nnf$y*(1-nnf$y) ## Only for the sigmoid
  
  grd=rep(list(NA),n_layers)
  
  for (i in (n_layers-1):1){
    del[[i]]=(t(nn$theta[[i]])%*%del[[i+1]])[1:nn$n_nodes[i]]*nnf$a[[i]]*(1-nnf$a[[i]])  ## Only for the sigmoid
    grd[[i]]=as.matrix(del[[i+1]])%*%t(as.matrix(c(nnf$a[[i]],1)))
  }
  
  return(grd)
}

nn.learn = function(nn,xx,yy,lambda=default_lambda,f=nn.sigmoid,method="batch",bsize=10, visualize=FALSE){

  n_layers=length(nn$n_nodes)
  
  x=as.matrix(xx)
  y=as.matrix(yy)
  if (dim(x)[2]!=dim(y)[2] || dim(x)[1]!=nn$n_nodes[1] || dim(y)[1]!=nn$n_nodes[n_layers]){
    print ("incorrect input/output dimensions")
    return(nn)
  }
   
  if (method != "stochastic" && method != "batch" && method !="minibatch"){
    print ("unknown method")
    return(nn)
  }

  if (method=="stochastic"){
    bsize=1
    th=NA
  } else if (method =="minibatch"){
    th=NA
  } else if (method =="batch"){
    bsize=dim(x)[2]
    th=default_threshold
  }
  
  mm=grad.desc(nn,x,y,lambdagd=lambda,f=nn.sigmoid,threshold=th,batch_size=bsize,vis=visualize)
  
  return(mm)
}

grad.desc = function(nn,x,y,lambdagd=default_lambda,f=nn.sigmoid,threshold=default_threshold,batch_size=10,vis=FALSE){  

  mm=nn
  mm_temp=mm
  
  cost=cost(nn,x,y,lambdagd)
  print(paste("initial cost:",cost))
  cost_temp=0
  count=0
  n_layers=length(nn$n_nodes)
  n_data=dim(x)[2]
  
  if (is.na(threshold)){ #TODO: minibatch or stochastic
    for (i in 1:n_data){  #TODO:this is only stochastic, namely batch_size=1
      
      grd=mm.backward(mm,x[,i],y[,i],f)
      
      for (i in 1:(n_layers-1)){
        mm$theta[[i]] = mm$theta[[i]]-alphagd*grd[[i]]-lambdagd*(mm$theta[[i]]%*%diag(c(rep(1,mm$n_nodes[i]),0)))
      
      }

    }
  
  } else {    
    while (cost_temp<=cost && abs(cost-cost_temp)>=threshold*cost) {
    
      mm_d=nn.initialize(n_nodes=nn$n_nodes,fct=0)

      mm=mm_temp
      count=count+1
      cost=cost(mm,x,y,lambdagd)
      
      for (i in 1:n_data){
        
        grd=nn.backward(mm,x[,i],y[,i],f)
        
        for (i in 1:(n_layers-1)){
          mm_d$theta[[i]]=mm_d$theta[[i]]+grd[[i]]/n_data
          
        }

      }
      
      for (i in 1:(n_layers-1)){
        mm_d$theta[[i]]=mm_d$theta[[i]]+lambdagd*(mm$theta[[i]]%*%diag(c(rep(1,mm$n_nodes[i]),0)))
      }
       
      obj = function(t){
        return(cost(add(mm,mult(mm_d,t)),x,y,lambdagd))
      }
      
      alpha=optimize(f=obj,interval=search_interval)$minimum
      mm_temp=add(mm,mult(mm_d,alpha))
      
      if (vis){
        print(visualize(mm_temp))
      }
      
      cost_temp=cost(mm_temp,x,y,lambdagd)
      #print(cost_temp)
    }
 
  }
  
  print(paste("->final cost:", cost))
  print(paste("iterations:",count))
  #print(paste("number of iterations:",count))
  return(mm)

}

mean_size = function(m){

  d=0
  len=0
  for (i in 1:length(m$theta)){
    d=d+sum(abs(m$theta[[i]]))
    len=len+length(m$theta[[i]])
  }
  d=d/len
  return(d)
}

mean_distance = function(n,m){
  if (!all(m$n_nodes==n$n_nodes)){
    print ("incompatible neural networks")
  }
  d=0
  len=0
  for (i in 1:length(m$theta)){
    d=d+abs(m$theta[[i]]-n$theta[[i]])
    len=len+length(m$theta[[i]])
  }
  d=d/len
  return(d)
}


add = function(n,m){
  if (!all(m$n_nodes==n$n_nodes)){
    print ("incompatible neural networks")
  }
  
  s=m
  
  for (i in 1:length(m$theta)){
    s$theta[[i]]=m$theta[[i]]+n$theta[[i]]
  }
  return(s)
}

mult = function(n,r){
  m=n
  for (i in 1:length(n$theta)){
    m$theta[[i]]=r*m$theta[[i]]
  }
  return(m)
}

cost = function(nn,x,y,lambda=default_lambda){
  c=0
  s=0
  n_data=dim(x)[2]
  for (i in 1:n_data){
    c=c+sum((nn.forward(nn,x[,i])$y-y[,i])^2)/2
  }
  for (t in nn$theta){
    s=s+sum((as.numeric(t[,1:(dim(t)[2]-1)]))^2)
  }
  c=c/n_data
  s=lambda*s/2
  return(c+s)
}
 
visualize = function(nn,z=0.5,rng=c(-1,2),step=0.1, output=1){ #when nn has exactly two input nodes; by default shows the first output node

  if (nn$n_nodes[1]!=2){
    print("Error: Number of input nodes not equal to 2.")
    return(NULL)
  }
  
  v=seq(rng[1],rng[2],step)
  pairs=expand.grid(v,v)
  f = function(w){
    nn.forward(nn,w)$y[output]
  }
  triples=cbind(pairs,apply(pairs,1,f))
  ddf=as.data.frame(triples)
  colnames(ddf)=c("x","y","z")
  ggplot(ddf,aes(x,y,fill=z))+geom_tile()+scale_fill_gradient(low="white",high="black")
  
}
