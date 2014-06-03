source("game_mechanics.r")
source("interface.r")
source("sbot.r")
source("bot.r")

conf=rep(0,9)
turn=1
strat=1:9
rcd=NULL


learn_matrix=matrix(c(list(conf=rep(0,9)),rep(list(0),9)),1,10)
colnames(learn_matrix)=c("conf",as.character(1:9))



###############
###############


learn_matrix=train(n=100, players=c("s","s"))

learn_matrix=train(n=100,players=c("m","s"))

learn_matrix=train(n=100,players=c("s","m"))

learn_matrix=train(n=200,players=c("m","m"))


range(learn_matrix[2:dim(learn_matrix)[1],2:10])
dim(learn_matrix)

##  after match with human learn with higher weight:
##  learn_matrix=learn(wt=2)

