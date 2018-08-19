library(MDPtoolbox)

#Define Probability Transition Matrices for Action Space

FOP<-matrix(rbind(c(0.1167,0,0,0.8833),c(0,0.1434,0,0.8566),c(0,0,0.127,0),c(0,0,0,1)),nrow = 4,ncol = 4)

CRU<-matrix(rbind(c(0.1335,0,0,0.8665),c(0,0.0985,0,0.9015),c(0,0,0.1164,0.8836),c(0,0,0,1)),nrow = 4,ncol = 4)

ONS<-matrix(rbind(c(0.2769,0,0,0.7231),c(0,0.2746,0,0.7254),c(0,0,0.2724,0.7276),c(0,0,0,1)),nrow = 4,ncol = 4)

L3 <-matrix(rbind(c(0,0,0,1),c(0,0,0,1),c(0,0,0,1),c(0,0,0,1)),nrow = 4,ncol = 4)


#Corresponding Rewards Matrices

a<--0.5
b<--1.475
c<--2.028
d<--10

RFOP<-matrix(rbind(c(a,0,0,a),c(0,a,0,a),c(0,0,a,a),c(0,0,0,a)),nrow = 4,ncol = 4)

RCRU<-matrix(rbind(c(b,0,0,b),c(0,b,0,b),c(0,0,b,b),c(0,0,0,b)),nrow = 4,ncol = 4)


RONS<-matrix(rbind(c(c,0,0,c),c(0,c,0,c),c(0,0,c,c),c(0,0,c)),nrow = 4,ncol = 4)

RL3<-matrix(rbind(c(0,0,0,d),c(0,0,0,d),c(0,0,0,d),c(0,0,0,d)),nrow = 4,ncol = 4)


#Set Up MDP Array

#Probability Transition Arrays
S<-list()
S[[1]]<-FOP
S[[2]]<-CRU
S[[3]]<-ONS
S[[4]]<-L3

#Reward Arrays

K<-list()
K[[1]]<-RFOP
K[[2]]<-RCRU
K[[3]]<-RONS
K[[4]]<-RL3

#Initial Set Up
Policy<-c(1,1,1,1)
V0<-c(0,0,0,0)
epsilon<-0.02
mdp_check(S,K)

#Policy Iterations
mdp_policy_iteration_modified(S,K,0.9)
