
hplay = function(r,c){
  play(r+(c-1)*3)
}


ox = function(v){
  w=rep(NA,length(v))
  for (i in 1:length(v)){
    if (v[i]==1){
      w[i]="O"
    } else if (v[i]==-1){
      w[i]="X"
    } else {
      w[i]=" "
    }
    
  }

  return(w)
}

board = function(){
    
  m=matrix(ox(conf),3,3)
  rownames(m)=1:3
  colnames(m)=1:3
  return(as.table(m))

}

status = function(){
  print(paste("current turn:",ox(turn)))
  return(board())
}

