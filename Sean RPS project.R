### Sean D RPS project ###

#lets start by trying to initialize the R and Q matrices! lets adapt some older code from a previous HW
## Strick copying here ###
maze <- as.matrix(read_table('maze.txt', col_names = F))
l <- nrow(maze)
reward <- t(matrix(rep(as.vector(maze),l), nrow=l^2, ncol=l^2))
index <- expand.grid(1:l, 1:l)
dMat <- as.matrix(dist(index, upper=T))
#how does this do what we want?
reward[dMat >= 2] <- -1
#add our rewards
reward[132,144]<-50

#lets try to run it in a loop for 1000 iterations?
Q_mat<-matrix(0,nrow=l^2, ncol=l^2)
gamz<-0.9
learn<-0.2

what_cell<-sample(1:nrow(Q_mat), 1)
#initial sample?
for(i in 1:100000){
  blep<-reward[what_cell,]
  prop_move<-sample(which(blep>0),1)
  which.is.max(reward[prop_move,])
  chek<-which(reward[prop_move,]>0)
  Max_Q<-max(Q_mat[prop_move,chek])
  Q_mat[what_cell,prop_move]<-learn*(reward[what_cell,prop_move]+gamz*Max_Q) 
  what_cell<-prop_move
}
#we tried this w/ random reseed each time, what's diff b/w that
#and having our 'init' state being our prev. sampled move?

#when adding the learn rate and gamz set to .9 vs no learn rate and gamz = .8 and 10,000 iter
#it seems like we dont actually find the correct 'final square'
#ok bump the iter to 100,000 and it works juuuust fine!

maze_pos<-list(NA)

maze_pos[1]<-which.max(Q_mat[1,])

for(i in 2:20){
  next_step<-maze_pos[[i-1]]
  maze_pos[[i]]<-which.max(Q_mat[next_step,])
}

#converges in 14 steps
#1, 14, 25, 38, 50, 63, 76, 77, 90, 103, 104, 117,130,143,144

#alright, lets make a for loop that does it v nicely
row_pos<-list(NA)
col_pos<-list(NA)
for(i in 1:length(maze_pos)){
  row_pos[[i]]<-ceiling(maze_pos[[i]]/12)
  col_pos[[i]]<-maze_pos[[i]]%%12
  if(col_pos[[i]]==0){col_pos[[i]]<-12}
}
maze_dir<-cbind(unlist(maze_pos),unlist(row_pos),unlist(col_pos))
maze_dir<-as.data.frame(maze_dir)
colnames(maze_dir)<-c("Pos","x","y")
maze_dir<-rbind(c(1,1,1),maze_dir)
maze_dir<-maze_dir[1:15,]
maze_dir<-as.data.frame(maze_dir)

mazeTidy <- data.frame(y=1:12, maze)
names(mazeTidy)[-1] <- 1:12
mazeTidy <- mazeTidy %>% gather("x", "Value", 2:13) %>% mutate(x=as.numeric(x))

ggplot(mazeTidy, aes(x=x,y=y))+
  
  geom_raster(aes(fill=factor(Value))) +
  
  scale_y_reverse() +
  
  geom_point(data=maze_dir) +
  
  geom_line(data=maze_dir) +
  
  scale_fill_discrete("Cell Reward") +
  
  ggtitle("Maze Solution using Q-learning")

###
library(nnet)
#the first thing to do is create 3 R matrices?
#robo rock
# we make 3 r matricies, and sample from them as needed to design Q?

r_rock<-matrix(c(10,10,10,100,100,100,0,0,0),nrow=3)

r_paper<-matrix(c(0,0,0,10,10,10,100,100,100),nrow=3)

r_scissors<-matrix(c(100,100,100,0,0,0,10,10,10),nrow=3)

#rock_list?
mat_list<-list(r_rock,r_rock,r_paper,r_scissors)
#pulls our sample, save sample to rward?
sample(mat_list,1)

#build q_mat
Q_mat<-matrix(0,nrow=3, ncol=3)
gamz<-0.9
learn<-0.2

what_cell<-sample(1:nrow(Q_mat), 1)
#initial sample?
for(i in 1:100){
  reward<-sample(mat_list,1)
  reward<-matrix(unlist(reward), nrow=3)
  blep<-reward[what_cell,]
  prop_move<-sample(which(blep>0),1)
  which.is.max(reward[prop_move,])
  chek<-which(reward[prop_move,]>0)
  Max_Q<-max(Q_mat[prop_move,chek])
  Q_mat[what_cell,prop_move]<-learn*(reward[what_cell,prop_move]+gamz*Max_Q) 
  what_cell<-prop_move
}


