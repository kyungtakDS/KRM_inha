install.packages("caret")
library(caret)
preproc1 <- preProcess(dat[,c(1:4,6)], method=c("center", "scale"))
norm1 <- predict(preproc1, dat[,c(1:4,6)])
summary(norm1)


dat_scaled <- as.data.frame(scale(dat[,c(1:4,6)]))
summary(dat_scaled$Income)



summary(dat$Income)
logincome = log(dat$Income)
summary(logincome)