packages <- c("car", "carData", "MVN", "StepReg", "MVLM", "lmtest")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

data <- c(41.5, 33.8, 27.7, 21.7, 19.9, 15.0, 12.2, 4.3, 19.3, 6.4, 37.6, 18.0, 26.3, 9.9, 25.0, 14.1, 15.2, 15.9, 19.6 , 45.9, 53.3, 57.5, 58.8, 60.6, 58.0, 58.6, 52.4, 56.9, 55.4, 46.9, 57.3, 55.0, 58.9, 50.3, 61.1, 62.9, 60.0, 60.6 , 11.2, 11.2, 12.7, 16.0, 16.2, 22.6, 24.5, 38.0, 21.3, 30.8, 14.7, 22.2, 18.3, 28.0, 22.1, 23.0, 20.7, 22.1, 19.3 , 162, 162, 162, 162, 172, 172, 172, 172, 167, 177, 157, 167, 167, 167, 167, 177, 177, 160, 160, 23, 23, 30, 30, 25, 25, 30, 30, 27.5, 27.5, 27.5, 32.5, 22.5, 27.5, 27.5, 20, 20, 34, 34, 3, 8, 5, 8, 5, 8, 5, 8, 6.5, 6.5, 6.5, 6.5, 6.5, 9.5, 3.5, 6.5, 6.5, 7.5, 7.5)

df <- matrix(data, ncol=6)

colnames(df) <- c("y1", "y2", "y3", "x1", "x2", "x3")
df <- as.data.frame(df)

df

mvlm <- mvlm(cbind(y1, y2, y3)~x1+x2+x3, data=df)

summary(mvlm)

ytotal <- cbind(df$y1, df$y2, df$y3)
fit <- lm(ytotal ~ x1 + x2 + x3, data=df)
summary(fit)

coef(fit)

residual <- mvn(resid(fit))
residual

bptest(fit)

coef(fit <- lm(ytotal ~ x1 + x2 + x3, data = df))
