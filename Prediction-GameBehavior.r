##merge dataset

playersData <- read.csv("~/R exercises/merged_data.csv")
View(playersData)
names(playersData)
playerList <- cbind(playersData$player_id, playersData$gamer,playersData$sex,playersData$age,playersData$O,
                    playersData$C,playersData$E,playersData$A,playersData$N,playersData$Seq0)
View(playerList)
dimnames(playerList)[[2]] <- c('player_id', 'Gamer', 'Sex', 'Age', 'O','C','E','A','N','Seq0')

playerList <- as.data.frame(playerList)
##Factoring the Sex Variable

playerList$Sex[playerList$Sex == -1] <- 0

##Factoring the Gamer variable
playerList$Gamer[playerList$Gamer == -1] <- 0

####scale OCEAN values

require(stats)
scale.O <- scale(playerList$O)
View(scale.O)
scale.C <- scale(playerList$C)
scale.E <- scale(playerList$E)
scale.A <- scale(playerList$A)
scale.N <- scale(playerList$N)

Scaled.playerData <- cbind(playerList$player_id, playerList$Gamer,playerList$Sex,playerList$Age,scale.O,scale.C,scale.E,
                           scale.A,scale.N,playerList$Seq0)
View(Scaled.playerData)
dimnames(Scaled.playerData)[[2]] <- c('player_id', 'Gamer', 'Sex', 'Age', 'O','C','E','A','N','Seq0')
Scaled.playerData <- as.data.frame(Scaled.playerData)
Scaled.playerData <- Scaled.playerData[-1]
###checking for multicollinearity
pairs(Scaled.playerData)
round(cor(Scaled.playerData[,-9]), digits=1)
##dataset with the interaction variables
playerList.interactData <- cbind(Scaled.playerData$player_id, Scaled.playerData$Gamer,Scaled.playerData$Sex,
                                 Scaled.playerData$Age,Scaled.playerData$O,Scaled.playerData$C,Scaled.playerData$E,
                                 Scaled.playerData$A,Scaled.playerData$N,Scaled.playerData$O*Scaled.playerData$C,
                                 Scaled.playerData$O*Scaled.playerData$E,Scaled.playerData$O*Scaled.playerData$A,
                                 Scaled.playerData$O*Scaled.playerData$N,Scaled.playerData$C*Scaled.playerData$E,
                                 Scaled.playerData$C*Scaled.playerData$A,Scaled.playerData$C*Scaled.playerData$N,
                                 Scaled.playerData$E*Scaled.playerData$A,Scaled.playerData$E*Scaled.playerData$N,
                                 Scaled.playerData$A*Scaled.playerData$N,Scaled.playerData$Age*Scaled.playerData$O,
                                 Scaled.playerData$Age*Scaled.playerData$C,Scaled.playerData$Age*Scaled.playerData$E,
                                 Scaled.playerData$Age*Scaled.playerData$A,Scaled.playerData$Age*Scaled.playerData$N,
                                 Scaled.playerData$Sex*Scaled.playerData$O,Scaled.playerData$Sex*Scaled.playerData$C,
                                 Scaled.playerData$Sex*Scaled.playerData$E, Scaled.playerData$Sex*Scaled.playerData$A, 
                                 Scaled.playerData$Sex*Scaled.playerData$N, Scaled.playerData$Gamer*Scaled.playerData$O, 
                                 Scaled.playerData$Gamer*Scaled.playerData$C, Scaled.playerData$Gamer*Scaled.playerData$E, 
                                 Scaled.playerData$Gamer*Scaled.playerData$A, Scaled.playerData$Gamer*Scaled.playerData$N, 
                                 Scaled.playerData$Seq0)
View(playerList.interactData)
dimnames(playerList.interactData)[[2]] <- c('player_id', 'Gamer', 'Sex', 'Age', 'O','C','E','A','N','O*C','O*E','O*A','O*N','C*E','C*A','C*N','E*A','E*N','A*N','Age*O', 'Age*C', 'Age*E', 'Age*A', 'Age*N', 'Sex*O', 'Sex*C', 'Sex*E', 'Sex*A', 'Sex*N', 'Gamer*O', 'Gamer*C', 'Gamer*E', 'Gamer*A', 'Gamer*N', 'Seq0')
playerList.interactData <- as.data.frame(playerList.interactData)
##Data Split
set.seed(1)
train = sample(1:nrow(Scaled.playerData), 2*nrow(Scaled.playerData)/3)
test =(-train)

##Likelihood ratio test

player.reducedmodel <- glm(Seq0 ~., family=binomial, data=Scaled.playerData)
summary(player.reducedmodel)

player.fullmodel <- glm(Seq0 ~ O+C+E+A+N+O*C+O*E+O*A+O*N+C*E+C*A+C*N+E*A+E*N+A*N+Age+Age*O+Age*C+Age*E+Age*A+Age*N+Sex+Sex*O+Sex*C+Sex*E+Sex*A+Sex*N+Gamer+Gamer*O+Gamer*C+Gamer*E+Gamer*A+Gamer*N+Gamer*Sex+Gamer*Age+Sex*Age, family=binomial, data=Scaled.playerData)
install.packages("lmtest")
require(lmtest)
lrtest(player.fullmodel, player.reducedmodel)
anova(player.reducedmodel, player.fullmodel, test="F")
##Lasso variable selection method

x=model.matrix(Seq0~O+C+E+A+N+O*C+O*E+O*A+O*N+C*E+C*A+C*N+E*A+E*N+A*N+Age+Age*O+Age*C+Age*E+Age*A+Age*N+Sex+Sex*O+Sex*C+Sex*E+Sex*A+Sex*N+Gamer+Gamer*O+Gamer*C+Gamer*E+Gamer*A+Gamer*N+Gamer*Sex+Gamer*Age+Sex*Age,Scaled.playerData)[,-10]
y=Scaled.playerData$Seq0
set.seed(1)
train = sample(1:nrow(x), 2*nrow(x)/3)
test =(-train)
y.test=y[test]

library(glmnet)
grid =10^ seq (10,-2, length =100)
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)


set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)
out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:33 ,]
lasso.coef
lasso.coef[lasso.coef !=0]

####StepAIC
stepAIC <- step(player.fullmodel, k=2, trace=F)
stepAIC$anova



####Step BIC
stepBIC <- step(player.fullmodel, k=log(nrow(Scaled.playerData[train,])), trace=F)
stepBIC$anova

library(DAAG)
install.packages("boot")
library(boot)
#Lassomodel <- glm(Seq0 ~ O+A+N+Age+Sex+Gamer+O*E+O*N+C*E+C*A+C*N+E*A+E*N+C*Age+E*Age+O*Sex+C*Sex+E*Sex+A*Sex+N*Sex+O*Gamer+E*Gamer+A*Gamer, family=binomial, data=Scaled.playerData)
Lassomodel1 <- glm(Seq0 ~ Gamer+C*A+E*N+E*Sex, family=binomial, data=Scaled.playerData)

##There are 2 pacakages which does cross validation with binary response
##Method1
cv.binary(Lassomodel1)

##Method2
cv.glm(Scaled.playerData, Lassomodel1, K=5)$delta

###Cross validation estimate for the AIC Model

AICmodel1 <- glm(Seq0 ~ O + C + E + A + N + Age + Sex + Gamer + O*C + O*A + C*A + C*Age + N*Age + E*Sex + C*Gamer,family=binomial, data=Scaled.playerData)

##Method1
cv.binary(AICmodel1)

##Method2
cv.glm(Scaled.playerData, AICmodel1, K=5)$delta

###Cross validation estimate for the BIC Model
BICmodel1 <- glm(Seq0 ~ C + E + A + N + Age + Sex + C*Age + E*Age + A*Age + C*Sex,family=binomial, data=Scaled.playerData)

##Method1
cv.binary(BICmodel1)

##Method2
cv.glm(Scaled.playerData[train,], BICmodel1, K=5)$delta

##deviance of goodness of fit
AICmodel.fit <- glm(Seq0 ~ O + C + E + A + N + Age + Sex + Gamer + C*Age + N*Age + A*Sex,family=binomial, data=Scaled.playerData)
install.packages("ResourceSelection")
library(ResourceSelection)
hl <- hoslem.test(Scaled.playerData[train,]$Seq0, fitted(Lassomodel1), g=10)
##conclusion H0


# -------------------------Residual diagnostics-------------------------
#Automated residuaaal plot
par(mfrow=c(2,2))
for(i in 1:4)
  plot(Lassomodel1, which = i)


View(Scaled.playerData[train,])

##removing the cases
remove <- rownames(Scaled.playerData[train,])[21]
train.playerdata <- Scaled.playerData[-which(rownames(Scaled.playerData) %in% remove), ]

View(train.playerdata)
##again fit the glm on the new train data
lassomodel.new <- glm(Seq0 ~ Gamer+C*A+E*N+E*Sex, family=binomial, data=train.playerdata)


View(train.playerdata)


##################################################################################
# ROC with cross validation k=10
####################################################################################

library(pROC)
test.df <- as.data.frame(best.playerdata)
k <- 10
n <- dim(test.df)[1]
set.seed(1)
indices <- sample(rep(1:k, ceiling(n/k))[1:n])

all.response <- all.predictor <- aucs <- c()
for (i in 1:k) {
  roctest = test.df[indices==i,]
  learn = test.df[indices!=i,]
  model.pred <- predict(best.newplayermod, newdata=roctest)
  aucs <- c(aucs, roc(roctest$Seq0, model.pred)$auc)
  all.response <- c(all.response, roctest$Seq0)
  all.predictor <- c(all.predictor, model.pred)
}
roc(all.response, all.predictor)
mean(aucs)


##################################################################################
# Effect of Gamer on predicted probilities of Seq0
####################################################################################
newplayerdata_G <- data.frame(Gamer= seq(from=0, to=1, length=10), C=rep(median(best.playerdata$C), times=10),
                              A=rep(median(best.playerdata$A), times=10), E=rep(median(best.playerdata$E), times=10),
                              N=rep(median(best.playerdata$N), times=10), Sex=rep(1, times=10))

plot(best.playerdata$Gamer, best.newplayermod$fitted.values, xlab="Gamer",ylab="Predicted probability")
Seq.predict_Gamer <- predict(best.newplayermod, newdata=newplayerdata_G, se.fit=T, type="response")
lines(newplayerdata_G$Gamer, Seq.predict_Gamer$fit, col="blue")
legend("topright", lty=c(1), col=c("blue"), c("Gamer"))


?effects
install.packages("effects")
library(effects)
with(train.playerdata, mean(A)-sd(A))
with(train.playerdata, mean(A)+sd(A))

plot(effect("C*A", lassomodel.new,xlevels=list(C=seq(from = -1.7, to=2, length=10), A=c(-1.18,0.62))),multiline = TRUE)

##################################################################################
# Effect of C on predicted probabilities of Seq0 at different values of A
####################################################################################
newplayerdata_C <- data.frame(Gamer= rep(1, times=10), C=seq(from=-1.7, to=2, length=10),
                              A=rep(mean(best.playerdata$A)-sd(best.playerdata$A), times=10), 
                              E=rep(median(best.playerdata$E), times=10),
                              N=rep(median(best.playerdata$N), times=10), Sex=rep(1, times=10))

newplayerdata_C1 <- data.frame(Gamer= rep(1, times=10), C=seq(from=-1.7, to=2, length=10),
                               A=rep(mean(best.playerdata$A)+sd(best.playerdata$A), times=10),
                               E=rep(median(best.playerdata$E), times=10),
                               N=rep(median(best.playerdata$N), times=10), Sex=rep(1, times=10))

plot(best.playerdata$C, best.newplayermod$fitted.values, xlab="C*A",ylab="Predicted probability")
seq.predict_CA <- predict(best.newplayermod, newdata=newplayerdata_C, se.fit=T, type="response")
lines(newplayerdata_C$C, seq.predict_CA$fit, col="blue")
seq.predict_CA1 <- predict(best.newplayermod, newdata=newplayerdata_C1, se.fit=T, type="response")
lines(newplayerdata_C1$C, seq.predict_CA1$fit, col="red")
legend("topright", lty=c(1,1), col=c("blue", "red"), c("A=-1.18", "A=0.62"))

##################################################################################
# Effect of E on predicted probabilities of Seq0 at different values of N
####################################################################################
newplayerdata_E <- data.frame(Gamer= rep(1, times=10), C=rep(median(best.playerdata$C), times=10),
                              A=rep(median(best.playerdata$A), times=10), E=seq(from=-2.5, to=1.9, length=10),
                              N=rep(mean(best.playerdata$N)-sd(best.playerdata$N), times=10), Sex=rep(1, times=10))

newplayerdata_E1 <- data.frame(Gamer= rep(1, times=10), C=rep(median(best.playerdata$C), times=10),
                               
                               A=rep(median(best.playerdata$A), times=10), E=seq(from=-2.5, to=1.9, length=10),
                               N=rep(mean(best.playerdata$N)+sd(best.playerdata$N), times=10), Sex=rep(1, times=10))

plot(best.playerdata$E, best.newplayermod$fitted.values, xlab="E*N",ylab="Predicted probability")
seq.predict_EN <- predict(best.newplayermod, newdata=newplayerdata_E, se.fit=T, type="response")
lines(newplayerdata_E$E, seq.predict_EN$fit, col="blue")
seq.predict_EN1 <- predict(best.newplayermod, newdata=newplayerdata_E1, se.fit=T, type="response")
lines(newplayerdata_E1$E, seq.predict_EN1$fit, col="red")
legend("topright", lty=c(1,1), col=c("blue", "red"), c("N=-1.09", "N=0.68"))
##################################################################################
# Effect of E on predicted probabilities of Seq0 at when player is Male and Female
##################################################################################
newplayerdata_ES <- data.frame(Gamer= rep(1, times=10), C=rep(median(best.playerdata$C), times=10),
                               A=rep(median(best.playerdata$A), times=10), E=seq(from=-2.5, to=1.8, length=10),
                               N=rep(median(best.playerdata$N), times=10), Sex=rep(1, times=10))

newplayerdata_ES1 <- data.frame(Gamer= rep(1, times=10), C=rep(median(best.playerdata$C), times=10),
                                A=rep(median(best.playerdata$A), times=10), E=seq(from=-2.5, to=1.8, length=10),
                                N=rep(median(best.playerdata$N), times=10), Sex=rep(0, times=10))

plot(best.playerdata$E, best.newplayermod$fitted.values, xlab="E*Sex",ylab="Predicted probability")
action.predict <- predict(best.newplayermod, newdata=newplayerdata_ES, se.fit=T, type="response")
lines(newplayerdata_ES$E, action.predict$fit, col="blue")
action.predict1 <- predict(best.newplayermod, newdata=newplayerdata_ES1, se.fit=T, type="response")
lines(newplayerdata_ES1$E, action.predict1$fit, col="red")
legend("topright", lty=c(1,1), col=c("blue", "red"), c("Sex=M", "Sex=F"))