

train = function(n=100,mat=learn_matrix, players=c("s","s")){
  count=0 #to avoid unending loop!
  lmat=mat
  
  for(i in 1:n){
    count=count+1
    if (count>1000){
      print ("1000 game played")
      print (paste(i,"games without tie"))
      return(lmat)
    }
    
    winner=0
    while (winner==0){
      game=generate(show=FALSE, players=players)
      winner=game$winner
    }
    
    lmat=learn(game=game, mat=lmat)
  }
  reset()
  return(lmat)
}

learn = function(game=list(records=rcd,winner=judge()$winner), mat=learn_matrix, wt=1){
  lmat=mat
  temp=NULL
  
  records=game$records
  winner=game$winner
  
  for (k in 1:dim(records)[1]){
      
    match=0  
    for (t in 1:dim(lmat)[1]){
      if (all(records[k,]$conf==lmat[t,]$conf)){
        m=as.character(records[k,]$move)
        lmat[[t,m]] <- lmat[[t,m]]+wt*((-1)^(k+1))*winner
        match=1
        break
      } 
      
    }
    
    if (match==0){
      temp <- rbind(temp,matrix(c(list(conf=records[k,]$conf),rep(list(0),9)),1,10))
      colnames(temp)=c("conf",as.character(1:9))
      m=as.character(records[k,]$move)
      s=dim(temp)[1]
      temp[[s,m]] <- temp[[s,m]]+wt*((-1)^(k+1))*winner      
    }
    
  }
  

  reset()

  return(rbind(lmat,temp))
}

botmove = function(show=TRUE){
  m=NA
  n_guess=0
  
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,]$conf)){
      v=unlist(learn_matrix[t,2:10])[which(conf==0)]
      mm=as.numeric(names(v))
      if (length(which(v==max(v)))==0) {
        break
      } else if (length(which(v==max(v)))==1){
        m=mm[which(v==max(v))]
      } else{
        m=mm[sample(which(v==max(v)),1)]
      }
      break
    } 
  }
  
  if(is.na(m)){
    if (length(which(conf==0))==1){
      m=which(conf==0)
    } else{
      n_guess=n_guess+1
      m=sample(which(conf==0),1)
    }
  }
  if (n_guess>0){
    print(paste0("guess count=",n_guess))
  }
  if (show==TRUE){
    print(lm_search())
  }
  return(m)
}

lm_search = function(c=conf){
  r=NA
  for (t in 1:dim(learn_matrix)[1]){
    if (all(conf==learn_matrix[t,]$conf)){
      r=matrix(unlist(learn_matrix[t,2:10]),3,3)
      rownames(r)=1:3
      colnames(r)=1:3
      break
    } 
  }
  
  return(r)
}

