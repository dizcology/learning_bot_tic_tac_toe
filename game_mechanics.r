
play = function(m,show=TRUE){
  if(!(m%in%strat) || conf[m]!=0){
    print("illegal move")
    if (show==TRUE){
      print(status())
    }
    return(FALSE)
  }
  
  if (judge()$finished==TRUE){
    print("game already finished")
    if (show==TRUE){
      print(status())
    }
    return(FALSE)
  }
  
  rcd <<- rbind(rcd,list(conf=conf,move=m))
  
  conf[m] <<- conf[m]+turn
  turn <<- (-1)*turn
  if (show==TRUE){
    print(status())
  }
  return(TRUE)
}



judge = function(){
  m=matrix(conf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  winner=0
  finished=NA
  
  if (max(abs(chk))==3){
    
    finished=TRUE
    if (max(chk)==3){
      winner=1
    } else {
      winner=-1
    }
  } else {
    finished=FALSE
  }
  
  return(list(winner=winner,finished=finished))
}

reset = function(){
  conf <<- rep(0,9)
  turn <<- 1
  rcd <<- NULL

}


generate = function(show=TRUE, show_each=FALSE, players=c("s","s")){
  reset()
  #records=NULL
  winner=0
  fnshed=FALSE
  
  v=list()
  for (i in 1:length(players)){
    if (players[i]=="m"){
      v[[i]]=botmove
    } else {
      v[[i]]=sbotmove
    }
  }
  
  while ((fnshed==FALSE) && (sum(abs(conf))<9)){
    j=(3-turn)/2
    
    m=v[[j]](show=FALSE)
    
    #records=rbind(records,list(conf=conf,move=m))
    play(m, show=show_each)
    fnshed=judge()$finished
    winner=judge()$winner
  }
  
  if (show==TRUE){
    print(status())
  }
  return(list(records=rcd,winner=winner))
}





