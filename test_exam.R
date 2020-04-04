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

