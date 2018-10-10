# load the data
# (it's from the subject shown in fig. 1 of Lisi & Cavanagh, 2015)
d <- read.table("example_data", header=T, sep="\t")

# fit multivariate linear model
mXY <- lm(cbind(sacXamp,sacYamp) ~ (tarPosSacOnX + tarPosSacOnY)*drift, data=d)

# note you can see already from multivaraite statistics that 
# the distribution of landing position do not vary across conditions
# (all terms involving 'drift' are very close to zero)
summary(mXY)

# make new dataset for prediction & plotting
# alpha is the tilt from vertical (positive = clockwise)
alphaTable <- tapply(d$alpha,d$tilt,mean)
l <- 3    # path length
ecc <- 10 # mean eccentricity
sequence_l <- seq(-l/2, l/2, length.out=2)

tarPosSacOnX <- {}; tarPosSacOnY <- {}; alpha <- {}

for (tlt in 1:length(alphaTable)) {
  # generate target positions along the path
  tarX <- ecc + sequence_l * sin(alphaTable[tlt] / 180 * pi)
  tarY <- sequence_l * cos(alphaTable[tlt]/ 180 * pi)
  tarPosSacOnX <- c(tarPosSacOnX, tarX)
  tarPosSacOnY <- c(tarPosSacOnY, tarY)
  alpha <- c(alpha, rep(alphaTable[tlt], length(sequence_l)))
}

drift <- c(rep("control",4),rep("double-drift",4))
nd <- data.frame(rep(tarPosSacOnX,2),rep(tarPosSacOnY,2),drift,rep(alpha,2))
colnames(nd) <- c("tarPosSacOnX","tarPosSacOnY","drift","alpha")

# mean of predicted landing position for the two extremes
nd$fitted_mCX_mv <- predict(mXY, newdata=nd)[,1]
nd$fitted_mCY_mv <- predict(mXY, newdata=nd)[,2]
nd$tilt <- as.factor(ifelse(sign(nd$alpha)==-1,"left","right"))

# plot
# in grey are the target positions at the moment of saccade onset
# red lines are the predictions of the multivariate model
library(ggplot2)
ggplot(d,aes(x=sacXamp,y=sacYamp))+theme_bw()+geom_point(data=d,aes(x=tarPosSacOnX,y=tarPosSacOnY),color="dark grey")+geom_point()+facet_grid(tilt~drift)+geom_line(data=nd,aes(x=fitted_mCX_mv ,y=fitted_mCY_mv),lwd=2,color="red")


# the angle of the path targeted by saccades is computed from the 
# tilt of the predictions of the multivariate model, i.e. is the slant of the 
# predicted values of the landing locations
nd$path_angle <- {}
nd$path_angle_abs <- {}
for(i in unique(nd$tilt)){
  for(j in unique(nd$drift)){
    # comput the slope of the y-x line
    path_slope <- abs(diff(nd$fitted_mCY_mv[nd$tilt==i & nd$drift==j]))/abs(diff(nd$fitted_mCX_mv[nd$tilt==i & nd$drift==j]))
    
    # transform to deviation from vertical
    nd$path_angle_abs[nd$tilt==i & nd$drift==j] <- atan(1/(path_slope))/pi*180
    nd$path_angle[nd$tilt==i & nd$drift==j] <- sign(nd$alpha[nd$tilt==i & nd$drift==j]) * atan(1/(path_slope))/pi*180
  }
}

# a measure of the effect of the internal motion on saccade landing is then the difference in 
# absolute tilt between double-drift and control; in this case is less than 1 deg.
mean(nd$path_angle_abs[nd$drift=="double-drift"]-nd$path_angle_abs[nd$drift=="control"])
