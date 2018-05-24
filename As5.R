
# question a

cardata=read.table("~/prob-stats-assignments/carmpgdat_new.txt",header=TRUE)
cardata=cardata[,c(4,2,3,5,6)]

# question b:

pairs(cardata1)

cardata2 = cardata[,c(2,3,4,5)]
pairs(cardata2)

# question c
# 
# Fit a multiple linear regression model to predict MPG from the other variables.
# Report the fitted regression equation, the estimated σ 2 , and the determination
# coefficient R 2 . Are any of the explanatory variables insignificant?
#   Hint: σ̂ is Residual standard error .

carlm = lm(MPG~VOL+HP+SP+WT, data=cardata)
summary(carlm)

# question d
carfulllm=lm(MPG~VOL+HP+SP+WT,data=cardata1)
# confint(carfulllm)
# 
# confint(carfulllm, level = 0.99)

summary(carfulllm)

lm1A=lm(MPG~VOL,data=cardata1)
summary(lm1A)

lm1B=lm(MPG~HP,data=cardata1)
summary(lm1B)

lm1C=lm(MPG~SP,data=cardata1)
summary(lm1C)

lm1D=lm(MPG~WT,data=cardata1)
summary(lm1D)

# we choose WT first because it had the highest R^2 value
lm2A = lm(MPG~WT+HP, data=cardata1)
summary(lm2A) #insignificant

lm2B = lm(MPG~WT+SP,data=cardata1)
summary(lm2B) #significant at 0.05

lm2C = lm(MPG~WT+VOL,data=cardata1)
summary(lm2C) # insignificant

# because combination of WT and SP was significant, we carry on with another step
lm3A=lm(MPG~WT+SP+HP, data=cardata1)
summary(lm3A) #significant

lm3B = lm(MPG~WT+SP+VOL,data=cardata1)
summary(lm3B) #insignificant => we stop here

# question e
car_AIC_step_down = step(carlm)
summary(car_AIC_step_down)

# question f
pairs(log(cardata))

# question g
#
# The model formula used to fit a multiple linear regression model can be
# easily adapted to logarithmic transformations. For instance
# > lm(log(MPG)~̃log(VOL)+log(WT),data=cardata)
# fits a model where the logarithm of MPG is explained/predicted by the loga-
#   rithm of VOL and the logarithm of WT .
# Select a suitable model using the step-down testing method using log( MPG )
# as the response and log( VOL ), log( HP ), log( SP ), and log( WT ) as explanatory
# variables. Report the fitted regression equation, the estimated σ 2 , and the
# determination coefficient R 2 .

carlm_log = lm(log(MPG)~log(VOL)+log(HP)+log(SP)+log(WT),data=cardata)
carlm_log_SP = lm(log(MPG)~log(VOL)+log(HP)+log(WT),data=cardata)
carlm_log_VOL = lm(log(MPG)~log(HP)+log(WT),data=cardata)

summary(carlm_log_VOL)

# question h: diagnostics

diagnose_by_plots = function(model) {
  par(mfrow=c(1,2))
  par(pty="s")
  qqnorm(residuals(model))
  plot(fitted(model),residuals(model),xlab="fitted values",ylab="residuals")
  abline(0,0,lty=2)
} 
# model in c: full lm
diagnose_by_plots(carlm)

# model in d: lm3A
diagnose_by_plots(lm3A)

# model in e:
diagnose_by_plots(car_AIC_step_down)

# model in g:
diagnose_by_plots(carlm_log_VOL)