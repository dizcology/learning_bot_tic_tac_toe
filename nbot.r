h=c(9)
n_nodes=c(9,h,9)
botnn=nn.initialize(n_nodes=n_nodes, mean=0, sd=0.01)
lamb=0



ntrain = function(n=100,nn, players=c("s","s")){
  reset()
  win_count=rep(0,3)
  names(win_count)=c(paste0(players,1:2),"tie")
  mm=nn
  xa=NULL
  ya=NULL
  
  for(i in 1:n){

    game=generate(show=FALSE, players=players)
    winner=game$winner
    
    if (winner==0){
      win_count[3]=win_count[3]+1
    } else {
      win_count[(3-winner)/2]=win_count[(3-winner)/2]+1
    }
    
    nsh=nshape(game)
    xa=cbind(xa,nsh$x)
    ya=cbind(ya,nsh$y)
  }
  xa=xa+rnorm(length(xa),sd=0.1)
  ya=ya+rnorm(length(xa),sd=0.1)
  mm=nlearn(nnl=nn, x=xa, y=ya, lambd=lamb)
  print(win_count)
  reset()
  return(mm)
}

nshape = function(g){
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
  reset()

  return(mm)
}

nbotmove = function(show=TRUE){
  w=1-abs(conf)
  v=nn.forward(botnn,conf)$y*w
  m=ssample(which(v==max(v)))
  if (show==TRUE){
    print (search.nlm())
  }
  return(m)
}

search.nlm = function(c=conf){
  m=matrix(nn.forward(botnn,c)$y,3,3)
  rownames(m)=1:3
  colnames(m)=1:3
  
  return(as.table(m))
}

write.nlm = function(lmat=nlearn_matrix,file="nlearn_matrix.dat"){  #TODO
  write.table(t(apply(lmat,1,unlist)),file=file,row.names=F,col.names=F)
}

read.nlm = function(file="nlearn_matrix.dat"){  #TODO
  lmat=NULL
  tmp=read.table(file=file)
  colnames(tmp)=c(1:9,1:9)
  for (i in 1:dim(tmp)[1]){
    rw=c(list(conf=as.numeric(tmp[i,1:9])),as.list(tmp[i,10:18]))
    lmat=rbind(lmat,rw)
  }
  
  colnames(lmat)=c("conf",as.character(1:9))
  rownames(lmat)=NULL
  return(lmat)
}

stat.nlm = function(lmat=nlearn_matrix){  #TODO
  print (paste0("Configurations observed: ",dim(lmat)[1]))
  mm=unlist(as.matrix(lmat[1:dim(lmat)[1],2:10]))
  idx=sum(abs(mm))
  
  print (paste0("Learn index: ",idx))
  
  
}
