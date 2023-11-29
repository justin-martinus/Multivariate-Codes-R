library(Hmisc)
library(matlib)
library(Matrix)
library(expm)
library(matrixcalc)
library(ellipsis)
library(Hotelling)
library(dplyr)
library(pracma)
library(psych)
B <- matrix(c(15,17,24,14,17,15,32,26,15,14,29,23,13,12,10,16,20,17,26,28,15,21,26,21,15,13,26,22,13,5,22,22,14,7,30,17,17,15,30,27,17,17,26,20,17,20,28,24,15,15,29,24,18,19,32,28,18,18,31,27,15,14,26,21,18,17,33,26,10,14,19,17,18,21,30,29,18,21,34,26,13,17,30,24,16,16,16,16,11,15,25,23,16,13,26,16,16,13,23,21,18,18,34,24,16,15,28,27,15,16,29,24,18,19,32,23,18,16,33,23,17,20,21,21,19,19,30,28),nrow=4, byrow=TRUE)
B <- t(B)
B

C <- matrix(c(13,14,12,21,14,12,14,26,12,19,21,21,12,13,10,16,11,20,16,16,12,9,14,18,10,13,18,24,10,8,13,23,12,20,19,23,11,10,11,27,12,18,25,25,14,18,13,26,14,10,25,28,13,16,8,14,14,8,13,25,13,16,23,28,16,21,26,26,14,17,14,14,16,16,15,23,13,16,23,24,2,6,16,21,14,16,22,26,17,17,22,28,16,13,16,14,15,14,20,26,12,10,12,9,14,17,24,23,13,15,18,20,11,16,18,28,7,7,19,18,12,15,7,28,6,5,6,13),nrow=4, byrow=TRUE)
C <- t(C)
C


rataB <- matrix(colMeans(B), ncol = 1)
rataB

rataC <- matrix(colMeans(C), ncol = 1)
rataC

sb <- cov(B)
sb

sc <- cov(C)
sc

rows1 <- nrow(B)
rows2 <- nrow(C)
spl <- 1/(rows1 + rows2 - 2) * ((rows1 - 1) * sb + (rows2 - 1) * sc); spl

  
  
  
invspl <- inv(spl)
invspl


T2 <- (rows1 * rows2)/(rows1 + rows2)*t(rataB-rataC)%*%inv(spl)%*%(rataB-rataC); T2

#Fungsi Diskriminan

a <- inv(spl) %*% (rataB - rataC); a
