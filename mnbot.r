h=c(50)
n_nodes=c(9,h,9)
lamb=0

botmn=NULL
for (i in 1:9){
  botmn=c(botmn,nn.initialize(n_nodes=n_nodes,mean=0,sd=0.1))
}

slice = function(mn,k){ #k is taken+1
  nn=list(n_nodes=mn[[2*k-1]],theta=mn[[2*k]])
  return(nn)
}

mntrain = function(n=100, mn, players=c("s","s")){
  reset()
  win_count=rep(0,3)
  names(win_count)=c(paste0(players,1:2),"tie")
  
  mm=mn
  xa=rep(list(NULL),9)
  ya=rep(list(NULL),9)
  
  for(i in 1:n){

    game=generate(show=FALSE, players=players)
    winner=game$winner
    
    if (winner==0){
      win_count[3]=win_count[3]+1
    } else {
      win_count[(3-winner)/2]=win_count[(3-winner)/2]+1
    }
    
    nsh=nshape(game)
    for (i in 1:9){
      xa[[i]]=cbind(xa[[i]],(nsh$x)[[i]])
      ya[[i]]=cbind(ya[[i]],(nsh$y)[[i]])
    }
  }
  #xa=xa+rnorm(length(xa),sd=0.1)
  #ya=ya+rnorm(length(xa),sd=0.1)
  
  for (i in 1:9){
    nn=slice(mm,i)
    temp=nlearn(nnl=nn, x=xa, y=ya, lambd=lamb)
    mm[[2*i]]=temp[[2]]
  }
  print(win_count)
  reset()
  return(mm)
}

mnshape = function(g){  #TODO
  records=g$records
  winner=g$winner
  id=diag(rep(1,9))
  
  x1=matrix(unlist(records[,1]),9)
  y1=matrix(0.5,9,dim(records)[1]) # entirely arbitrary choice of weight for moves that result in a tie
  
  for (i in 1:dim(y1)[2]){
    if (winner*(-1)*(-1)^i==1){ #means the i-th move is a winning move
      y1[records[,2][[i]],i]=1
    } else if (winner*(-1)*(-1)^i==-1){
      y1[records[,2][[i]],i]=0
    }
  }
  
  y1[,which(winner*(-1)*(-1)^(1:dim(rcd)[1])==-1)]=0

  return(list(x=x1,y=y1))
}

nlearn = function(nnl,x,y,lambd=lamb){ 
  
  mm=nn.learn(nnl,xx=x,yy=y,lambda=lambd)
  return(mm)
}

mnbotmove = function(conf=conf, show=TRUE){
  w=1-abs(conf)
  k=taken(conf)+1
  nn=slice(botmn,k)
  v=nn.forward(nn,conf)$y*w
  m=ssample(which(v==max(v)))
  if (show==TRUE){
    print (search.mnlm())
  }
  return(m)
}

search.mnlm = function(c=conf){
  k=taken(c)+1
  nn=slice(botmn,k)
  m=matrix(nn.forward(nn,c)$y,3,3)
  rownames(m)=1:3
  colnames(m)=1:3
  
  return(as.table(m))
}
