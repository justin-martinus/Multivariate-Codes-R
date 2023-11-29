packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

#import data
df <- IsiData
#df <- as.matrix(df)
df
df$V1 <- as.factor(df$V1)
y <- cbind(df$V2,df$V3,df$V4,df$V5)
y
totalmeans <- colMeans(y); totalmeans

#grouping

df.group <- split(df[,2:5], df$V1)

n <- dim(df)[1] / length(unique(df$V1))


#rata-rata

df.means <- sapply(df.group, function(x) {
  apply(x, 2, mean)
}, simplify = 'data.frame')

df.means

#Matriks H

H = matrix(data = 0, nrow = 4, ncol = 4)
for (i in 1:dim(H)[1]) {
  for (j in 1:i) {
    H[i,j] <- n * sum((df.means[i,] - totalmeans[i]) * (df.means[j,] - totalmeans[j]))
    H[j,i] <- n * sum((df.means[j,] - totalmeans[j]) * (df.means[i,] - totalmeans[i]))
  }
}
H

#Matriks E

E = matrix(data = 0, nrow = 4, ncol = 4)
for (i in 1:dim(E)[1]) {
  for (j in 1:i) {
    b <- c() 
    for (k in df.group) {
      a <- sum((k[,i] - mean(k[,i])) * (k[,j] - mean(k[,j])))
      b <- append(b, a)
    }
    E[i,j] <- sum(b)
    E[j,i] <- sum(b)
  }
}
E


#Wilks Lambda Test

Lambda <- det(E)/det(E + H); Lambda
summary(manova(y ~ df$V1), 'Wilks')$stats[,2][1]

df.manova <- summary(manova(y ~ df$V1)); df.manova


#Roy's Test

EinvH.eigen <- eigen(inv(E) %*% H)
roy.stat <- EinvH.eigen$values[1]; roy.stat
summary(manova(y ~ df$V1), 'Roy')$stats[,2][1]
theta <- roy.stat/(1+roy.stat); theta

#Pillai Test

pillai.stat <- sum(diag(inv(E + H) %*% H)); pillai.stat
sum(EinvH.eigen$values / (1 + EinvH.eigen$values))
summary(manova(y ~ df$V1), 'Pillai')$stats[,2][1]

#Lawley Hotelling Test

lawhot.stat <- sum(diag(inv(E) %*% H)); lawhot.stat
sum(EinvH.eigen$values)
summary(manova(y ~ df$V1), 'Hotelling-Lawley')$stats[,2][1]