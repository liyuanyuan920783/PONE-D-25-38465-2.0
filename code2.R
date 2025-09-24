library(tidyverse)
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(e1071)
library(pROC)
library(caret)




###将CEF这三个变量重新进行构建glm逻辑回归模型
total_data <- read.csv("C:/Users/lenovo/Desktop/total.csv")
model <- glm(status ~ Sex + Age + smoking + Drink+ BMI + U + FBG, 
             family = binomial(link = "logit"),
             data = total_data)
summary(model)
coefficients(model)
exp(confint(model))

predvalue1 <- predict(model, type = "response")
##利用训练好的逻辑回归模型mylogxunlian2，对训练集中的数据进行预测，“response”代表返回的是预测的概率值
##将生成的概率值储存在predvalue1中
ROCxunlian <- roc(total_data$status, predvalue1)
##对真实标签和预测概率值做ROC曲线
plot(ROCxunlian,
     print.auc=TRUE,
     print.auc.x=0.4,print.auc.y=0.5,
     print.thres=TRUE, 
     main="ROC of trainset", 
     xlab = " ",
     col="#0000CD", 
     legacy.axes= TRUE, 
     print.ci=TRUE, 
     lty=1,lwd=3)
##绘制ROC曲线
ROCxunlian$auc  ##计算AUC值
ci(ROCxunlian)  ##计算AUC的置信区间 
coords(ROCxunlian, x="best", ret="all", transpose = FALSE) ##查看模型所有的统计指标

##构建验证集中的ROC曲线
ceshi<-read.csv("C:/Users/lenovo/Desktop/total验证.csv",row.names = 1)
predvalue2 <- predict(model, type = "response", newdata = ceshi)
#####一定注意，我们是利用训练集构建好的模型，而不是重新对验证集建模
ROCyanzheng <- roc(ceshi$status,predvalue2)  
plot(ROCyanzheng,
     print.auc=TRUE,
     print.auc.x=0.4,print.auc.y=0.5,
     print.thres=TRUE,
     main="ROC of testset",
     xlab = " ",
     col="red",
     legacy.axes= FALSE,
     print.ci=TRUE)
ROCyanzheng$auc
ci(ROCyanzheng)
coords(ROCyanzheng, x="best", ret="all", transpose = FALSE)


library(dplyr)
library(rms)
CHD.pred<-lrm(status ~ Sex + Age + smoking + Drink+ BMI + U + FBG,data=total_data,x=T,y=T)
Cal.CHD<-calibrate(CHD.pred,method="boot",B=1000)
plot(Cal.CHD,xlab="Predicted Probability",ylab="Observed Probability")
lines(Cal.CHD[,c("predy","calibrated.orig")],type="l",lwd=2,pch=20,col="red")
lines(Cal.CHD[,c("predy","calibrated.corrected")],type="l",lwd=3,pch=20,col="blue")
abline(0, 1, lwd = 3, lty = 2, col = "black")#绘制对角线#绘制对角线
legend("bottomright",c("Apparent","Bias-corrected","Ideal"),col=c("red","blue","black"),lty=1,lwd=2,bty = "n",cex=0.8)

library(ResourceSelection)
h1<-hoslem.test(total_data$status,predvalue1,g=10)
h1#训练集HL检验
library(ResourceSelection)
h2<-hoslem.test(ceshi$status,predvalue2,g=10)
h2#测试集HL检验
######HL检验需要大于0.05才能被认为拟合得较好

