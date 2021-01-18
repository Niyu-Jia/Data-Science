library(faraway)
data(chredlin)
library(MASS)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(broom) 
library(car)
library(AUC)
library(caret)
library(PRROC)
library(heplots)
library(klaR)
variable<- colnames(chredlin)

# variance check
boxm <- heplots::boxM(chredlin[, c(1:5, 6)], chredlin$side)
boxm
plot(boxm)

#par(mfrow=c(2,3))
#boxplot(race~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="race")
#boxplot(fire~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="fire")
#boxplot(age~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="age")
#boxplot(theft~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="theft")
#boxplot(income~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="income")
#boxplot(involact~side,data=chredlin,col=c("blue","red"),main="Boxplot",xlab="side",ylab="involact")

#All variance elipses for LDA equal variance assumption checking heplots::covEllipses(chredlin[,1:6],
heplots::covEllipses(chredlin[,1:6],
                     chredlin$side,
                     fill = TRUE,
                     pooled = FALSE,
                     col = c("blue", "red"), variables = c(1:5, 6), fill.alpha = 0.05)
# covariance check samples

ggplot(chredlin, aes(x = age, y = race, col = side)) + 
  geom_point() +
  stat_ellipse() + 
  scale_color_manual(values = c("blue", "red"))

ggplot(chredlin, aes(x = income, y = theft, col = side)) + 
  geom_point() +
  stat_ellipse() + 
  scale_color_manual(values = c("blue", "red"))

ggplot(chredlin, aes(x = involact, y = fire, col = side)) + 
  geom_point() +
  stat_ellipse() + 
  scale_color_manual(values = c("blue", "red"))

#Homogeneity of Variance Test

leveneTest(race~side, chredlin)
leveneTest(fire~side, chredlin)
leveneTest(theft~side, chredlin)
leveneTest(age~side, chredlin)
leveneTest(involact~side, chredlin)
leveneTest(income~side, chredlin)
#None of the leventests yield a significant F value (<0.05) so variances are approx. equal


#var_list=c()
#for (i in 1:6)
#{
#  test<-leveneTest( chredlin[,i]~ side, chredlin)
#  p_value<-test$`Pr(>F)`[1]
#  if (p_value>0.05) {var_list<-append(var_list,"Equal Variance")}
#  else {var_list<-append(var_list,"Unequal Variance")}
#}
#var_df<-data.frame(variable[1:6],var_list)
#None of the leventests yield a significant F value (<0.05) so variances are approx. equal

#Normality Checking
south <- subset(chredlin, side == "s") 
north <- subset(chredlin, side == "n")


par(mfrow = c(2, 3)) 
for(i in variable[1:6]) { 
  qqnorm(south[[i]]); qqline(south[[i]], col = 2) 
}

par(mfrow = c(2, 3)) 
for(i in variable[1:6]) { 
  qqnorm(north[[i]]); qqline(north[[i]], col = 2) 
}

#distribution check
plot<-list()
for(i in 1:6) { 
  plot[[i]] <- ggplot(chredlin, aes_string(x = chredlin[,i], y = "..density..", col = "side")) + 
    geom_density(aes(y = ..density..)) + 
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.position="bottom") 
} 
do.call(grid.arrange, c(plot, nrow = 3))


#LDA algorithm
# Running LDA Histogram between N and S
LDA <- MASS::lda(side ~., data = chredlin) 
predction <- predict(LDA) 
head(predction$posterior)
LDA

# Running Discriminant Function plot for LDA 
plot(LDA)

# Running confusionMatrix with respect to variable 'side'
predict_data <- data.frame(chredlin, predicted = predction$class) 

confusionMatrix(predict_data$predicted,predict_data$side)

# prediction accuracy 
accuracy<-round((21+17)/(47), 4)
# For running partimat graphs
partimat(side ~ race + fire + theft + age + involact + income, data = chredlin, method = "lda")

#ROC curve
predict_data$side <- ifelse(predict_data$side == "n", 1, 0)
predict_data$predicted <- ifelse(predict_data$predicted == "n", 1, 0)
PRROC_obj <- roc.curve(scores.class0 = predict_data$predicted, weights.class0=predict_data$side,curve=TRUE)
plot(PRROC_obj,main="LDA ROC Curve")


#QDA algorithm
QDA <- MASS::qda(side ~., data = chredlin) 
predction_2 <- predict(QDA) 
head(predction_2$posterior)
QDA


predict_data_2 <- data.frame(chredlin, predicted = predction_2$class) 
confusionMatrix(predict_data_2$predicted,predict_data_2$side)

# prediction accuracy 
accuracy_2<-round((22+20)/(47), 4)


#ROC curve
predict_data_2$side <- ifelse(predict_data_2$side == "n", 1, 0)
predict_data_2$predicted <- ifelse(predict_data_2$predicted == "n", 1, 0)
PRROC_obj_2 <- roc.curve(scores.class0 = predict_data_2$predicted, weights.class0=predict_data_2$side,curve=TRUE)
plot(PRROC_obj_2,main="QDA ROC Curve")



