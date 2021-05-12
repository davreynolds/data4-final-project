## ----setup--------------------------------------------------------------------
# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed, include = FALSE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----pkgload, include = FALSE-------------------------------------------------
#PUT YOUR LIBRARY PACKAGES HERE WHEN YOU ARE READY

#sean's pkgs
library(lme4)
library(ggplot2)
library(papaja)
library(sjPlot)
library(tidyverse)
library(nnet)

## ----davidsetup, include = FALSE----------------------------------------------



## ----method_david, echo=FALSE, message=FALSE, warning=FALSE-------------------



## ----results_david, echo=FALSE, message=FALSE, warning=FALSE------------------



## ----seansetup, include = FALSE-----------------------------------------------
set.seed(1)
library(lme4)
library(ggplot2)
library(papaja)
library(sjPlot)
library(tidyverse)
library(nnet)

r_rock<-matrix(c(10,10,10,100,100,100,0,0,0),nrow=3)
r_paper<-matrix(c(0,0,0,10,10,10,100,100,100),nrow=3)
r_scissors<-matrix(c(100,100,100,0,0,0,10,10,10),nrow=3)
mat_list<-list(r_rock,r_rock,r_paper,r_scissors)

Q_mat<-matrix(0,nrow=3, ncol=3)
gamz<-0.9
learn<-0.2
what_cell<-sample(1:nrow(Q_mat), 1)

#code to display the matrices
write_matex <- function(x) {
  begin <- "$$\\begin{bmatrix}"
  end <- "\\end{bmatrix}$$"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c(begin, X, end))
}


## ----m1_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(r_rock)

## ----m2_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(r_paper)

## ----m3_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(r_scissors)

## ----method_sean, echo=FALSE, message=FALSE, warning=FALSE--------------------
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

## ----m4_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(Q_mat)

## ----results_sean, echo=FALSE, message=FALSE, warning=FALSE-------------------

mat_list<-list(r_rock,r_rock,r_rock,r_rock,r_paper,r_scissors)
Q_mat<-matrix(0,nrow=3, ncol=3)
gamz<-0.9
learn<-0.2
what_cell<-sample(1:nrow(Q_mat), 1)

for(i in 1:1000){
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

## ----m5_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(Q_mat)

## ----last_sean, echo=FALSE, message=FALSE, warning=FALSE----------------------

r_rock<-matrix(c(1,1,1,200,200,200,-1,-1,-1),nrow=3)
r_paper<-matrix(c(-1,-1,-1,1,1,1,200,200,200),nrow=3)
r_scissors<-matrix(c(200,200,200,-1,-1,-1,1,1,1),nrow=3)


mat_list<-list(r_rock,r_rock,r_rock,r_rock,r_paper,r_scissors)
Q_mat<-matrix(0,nrow=3, ncol=3)
gamz<-0.9
learn<-0.2
what_cell<-sample(1:nrow(Q_mat), 1)

for(i in 1:1000){
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

## ----m6_sean, echo=FALSE, message=FALSE, warning=FALSE, results='asis'--------
write_matex(Q_mat)

