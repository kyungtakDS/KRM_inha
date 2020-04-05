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



#' lattice test
regVar <- c("X16_ex_str", "X17_ex_str")
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = DB[, regVar], 
            y = DB$SGG, 
            plot = "scatter", 
            layout = c(2, 1))




library(caret)  
pre <- preProcess(DB[,4:6], method=c("range"))  #Min-max scaling
pred <- predict(pre, DB[,4:6])
colnames(pred) <- c("X16_hazard", "X17_hazard", "X18_hazard")
result <- cbind(DB[,1:3], pred)



DB_e_dif[1:10,]
DB_e_dif[152:161,]





