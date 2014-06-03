

find_check = function(){
  check=NULL
  m=matrix(conf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  if (max(abs(chk))==2){
    p=which(abs(chk)==2)
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(conf[v]==0)])
        next
      }
    }
  }
  
  return(check)
}

find_chance = function(){
  check=NULL
  m=matrix(conf,3,3)
  v=apply(m,1,sum)
  w=apply(m,2,sum)
  s=m[1,1]+m[2,2]+m[3,3]
  t=m[1,3]+m[2,2]+m[3,1]
  chk=c(v,w,s,t)
  
  ma=matrix(abs(conf),3,3)
  va=apply(ma,1,sum)
  wa=apply(ma,2,sum)
  sa=ma[1,1]+ma[2,2]+ma[3,3]
  ta=ma[1,3]+ma[2,2]+ma[3,1]
  chka=c(va,wa,sa,ta)
  
  if (length(which(chka==1))>0){
    p=which((chka==1) && (chk==turn))
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(conf[v]==0)])
        next
      }
    }
  }
  
  if ((length(which(chka==1))>0)){
    p=which(chka==1)
    for (pp in p){
      if (pp<=3){
        v=c(1,4,7)+(pp-1)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if ((pp<=6) && (pp>=4)){
        v=c(1,2,3)+3*(pp-4)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==7){
        v=c(1,5,9)
        check=c(check,v[which(conf[v]==0)])
        next
      }
      if (pp==8){
        v=c(3,5,7)
        check=c(check,v[which(conf[v]==0)])
        next
      }
    }
  }
  
  
  return(unique(check))
}


sbotmove = function(show=TRUE){
  m=NA
  ck=NULL
  ch=NULL
  if (length(which(conf==0))==1){
    m=which(conf==0)
  } else if (length(find_check())>0) {
    ck=find_check()
    if (length(ck)==1){
      m=ck
    } else {
      m=sample(ck,1)
    }
  
  } else if (length(find_chance())>0) {
    ch=find_chance()
    if (length(ch)==1){
      m=ch 
    } else {
      m=sample(ch,1)
    }
  } else{
    m=sample(which(conf==0),1)
  }
  if (show==TRUE){
    print(paste(ck,",",ch))
  }
  return(m)
}

 