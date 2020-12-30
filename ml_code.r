library(ggplot2)
library(gplots)
library(lattice)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(grid)
library(pROC)
library(mice)
library(RANN)
library(DMwR)
library(carData)
library(car)
citation("pROC")
library(corrplot)

library(MASS)
setwd("C:/Users/a/Desktop")
wuran<-read.table('alf2.csv',header=T,sep=',')
summary(wuran)
names(wuran) 
wuran[, names(wuran) %in% c("Weight")]
x<-wuran[, names(wuran) %in% c("Weight","Height","Age","Body.Mass.Index","Waist","Maximum.Blood.Pressure","Minimum.Blood.Pressure","Good.Cholesterol","Bad.Cholesterol" ,"Total.Cholesterol")]
b<-cor(x)
corrplot(b,add=TRUE, type="lower", method="number",diag=TRUE,tl.pos="n", cl.pos="n")
corrplot(b)

set.seed(12345)
getwd()
setwd("C:/Users/a/Desktop")
wuran<-read.table('wuran.csv',header=T,sep=',')
names(wuran)
dim(wuran)
summary(wuran)
s<-vector(mode='numeric',length=0)
k=1
yeaterday_pm<-vector(mode='numeric',length=0)
ye_pm2.5<-vector(mode='numeric',length=0)
ye_wendu<-vector(mode='numeric',length=0)
ye_shidu<-vector(mode='numeric',length=0)
ye_fengsu<-vector(mode='numeric',length=0)
ye_zhengfa<-vector(mode='numeric',length=0)
ye_jiangshui<-vector(mode='numeric',length=0)
ye_qiya<-vector(mode='numeric',length=0)
ye_rizhao<-vector(mode='numeric',length=0)
ye_pm10<-vector(mode='numeric',length=0)
ye_SO2<-vector(mode='numeric',length=0)
ye_CO<-vector(mode='numeric',length=0)
ye_NO2<-vector(mode='numeric',length=0)
ye_O3<-vector(mode='numeric',length=0)
ye_aqi<-vector(mode='numeric',length=0)
ye_luoyang_pm<-vector(mode='numeric',length=0)
ye_xuchang_pm<-vector(mode='numeric',length=0)
ye_jiaozuo_pm<-vector(mode='numeric',length=0)
ye_xinxiang_pm<-vector(mode='numeric',length=0)
ye_kaifeng_pm<-vector(mode='numeric',length=0)
ye_pingding_pm<-vector(mode='numeric',length=0)

ye_luoyang_aqi<-vector(mode='numeric',length=0)
ye_xuchang_aqi<-vector(mode='numeric',length=0)
ye_jiaozuo_aqi<-vector(mode='numeric',length=0)
ye_xinxiang_aqi<-vector(mode='numeric',length=0)
ye_kaifeng_aqi<-vector(mode='numeric',length=0)
ye_pingding_aqi<-vector(mode='numeric',length=0)
ye_dengji<-"l"
as.data.frame(ye_dengji)

#删除因变量为无的观测空值填补
for(i in 2:dim(wuran)[1]){
  if(wuran$质量等级[i]==''){
    s[k]=i
    k=k+1
  }
  ye_pm2.5[i]=wuran$PM2.5[i-1]
  ye_wendu[i]=wuran$平均气温...[i-1]
  ye_shidu[i]=wuran$平均相对湿度...[i-1]
  ye_fengsu[i]=wuran$平均风速.m.s.[i-1]
  ye_zhengfa[i]=wuran$小型蒸发量.mm.[i-1]
  ye_jiangshui[i]=wuran$当日累计降水量.mm.[i-1]
  ye_qiya[i]=wuran$平均本站气压.hPa.[i-1]
  ye_rizhao[i]=wuran$日照时数.h.[i-1]
  ye_pm10[i]=wuran$PM10[i-1]
  ye_SO2[i]=wuran$SO2[i-1]
  ye_CO[i]=wuran$CO[i-1]
  ye_NO2[i]=wuran$NO2[i-1]
  ye_O3[i]=wuran$O3[i-1]
  ye_luoyang_pm[i]=wuran$PM2.5_洛阳[i-1]
  ye_xuchang_pm[i]=wuran$PM2.5_许昌[i-1]
  ye_jiaozuo_pm[i]=wuran$PM2.5_焦作[i-1]
  ye_xinxiang_pm[i]=wuran$PM2.5_新乡[i-1]
  ye_kaifeng_pm[i]=wuran$PM2.5_开封[i-1]
  ye_pingding_pm[i]=wuran$PM2.5_平顶山[i-1]
  ye_aqi[i]=wuran$AQI[i-1]
  
  ye_luoyang_aqi[i]=wuran$AQI_洛阳[i-1]
  ye_xuchang_aqi[i]=wuran$AQI_许昌[i-1]
  ye_jiaozuo_aqi[i]=wuran$AQI_焦作[i-1]
  ye_xinxiang_aqi[i]=wuran$AQI_新乡[i-1]
  ye_kaifeng_aqi[i]=wuran$AQI_开封[i-1]
  ye_pingding_aqi[i]=wuran$AQI_平顶山[i-1]
  ye_dengji[i]=as.character(wuran$质量等级[i-1])
}
k
union_delete<-union(s,s+1)



wuran2<-cbind(wuran[, names(wuran) %in% c("年份","月份","日","PM2.5","质量等级","AQI")],ye_pm2.5,ye_aqi,ye_wendu,ye_shidu,  ye_fengsu,  ye_zhengfa,ye_jiangshui, ye_qiya,ye_rizhao,ye_pm10,ye_SO2,  ye_CO,  ye_NO2,  ye_O3,  ye_luoyang_pm,  ye_xuchang_pm,  ye_jiaozuo_pm, ye_xinxiang_pm,  ye_kaifeng_pm,  ye_pingding_pm,  ye_luoyang_aqi,  ye_xuchang_aqi,  ye_jiaozuo_aqi,  ye_xinxiang_aqi,  ye_kaifeng_aqi,  ye_pingding_aqi,ye_dengji)
wuran3<-wuran2[-union_delete,]
wuran4<-wuran3[-1,]
summary(wuran4)

#knn近邻算法填补其他城市AQI和PM2.5缺失值
pp<-wuran4
md.pattern(pp[,21:33])
wuran5 <- cbind(knnImputation(pp[, !names(pp) %in% c("PM2.5","质量等级","AQI","ye_dengji")]),pp[, names(pp) %in% c("PM2.5","质量等级","AQI","ye_dengji")])
anyNA(wuran5)
md.pattern(wuran5)


#划分污染等级类别
t<-vector(mode='numeric',length=0)
ye_t<-vector(mode='numeric',length=0)
for(i in 1:dim(wuran5)[1]){
  if(wuran5$质量等级[i] %in% c('优','良')){
    t[i]='good'
  }
  else{
    t[i]='bad'
  }
  
  if(wuran5$ye_dengji[i] %in% c('优','良')){
    ye_t[i]='good'
  }
  else{
    ye_t[i]='bad'
  }
}
wuran6<-cbind(wuran5,t,ye_t)
summary(wuran6)
ggplot(wuran6,aes(x=rep(1:dim(a)[1],1),y=ye_pm2.5))+geom_point(color='#F8766D')+geom_smooth(method = 'loess')+xlab("2014年")+ylab("PM2.5")

plot(wuran6[(wuran6$年份=="2016"),names(wuran) %in% c("PM2.5")],type='b')
#探索性数据分析

#PM2.5浓度时间序列图
a<-wuran6[which(wuran6$年份=="2014"),]
p1<-ggplot(a,aes(x=rep(1:dim(a)[1],1),y=ye_pm2.5))+geom_point(color='#F8766D')+geom_smooth(method = 'loess')+xlab("2014年")+ylab("PM2.5")

b<-wuran6[which(wuran6$年份=="2015"),]
p2<-ggplot(b,aes(x=rep(1:dim(b)[1],1),y=ye_pm2.5))+geom_point(color='#7CAE00')+geom_smooth(method = 'loess')+xlab("2015年")+ylab("PM2.5")

c<-wuran6[which(wuran6$年份=="2016"),]
p3<-ggplot(c,aes(x=rep(1:dim(c)[1],1),y=ye_pm2.5))+geom_point(color='#00BFC4')+geom_smooth(method = 'loess')+xlab("2016年")+ylab("PM2.5")

d<-wuran6[which(wuran6$年份=="2017"),]
p4<-ggplot(d,aes(x=rep(1:dim(d)[1],1),y=ye_pm2.5))+geom_point(color='#C77CFF')+geom_smooth(method = 'loess')+xlab("2017年")+ylab("PM2.5")

#ggplot(wuran6,aes(x=rep(1:dim(wuran6)[1],1),y=ye_pm2.5))+geom_point()+geom_smooth()

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
output <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = output(1, 1))
print(p2, vp = output(1, 2))
print(p3, vp = output(2, 1))
print(p4, vp = output(2, 2))

#按分类目标表变量不同颜色的直方图
p1<-ggplot(wuran6,aes(x=ye_wendu,fill=t))+geom_histogram(binwidth = 1)
p2<-ggplot(wuran6,aes(x=ye_shidu,fill=t))+geom_histogram(binwidth = 2)
p3<-ggplot(wuran6,aes(x=ye_fengsu,fill=t))+geom_histogram(binwidth = 0.1)
p4<-ggplot(wuran6,aes(x=ye_pm2.5,fill=t))+geom_histogram(binwidth = 10)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
output <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = output(1, 1))
print(p2, vp = output(1, 2))
print(p3, vp = output(2, 1))
print(p4, vp = output(2, 2))

p1<-ggplot(wuran6,aes(x=ye_kaifeng_pm,fill=t))+geom_histogram(binwidth = 8)
p2<-ggplot(wuran6,aes(x=ye_pm10,fill=t))+geom_histogram(binwidth =10)
p3<-ggplot(wuran6,aes(x=ye_SO2,fill=t))+geom_histogram(binwidth = 5)
p5<-ggplot(wuran6,aes(x=ye_NO2,fill=t))+geom_histogram(binwidth = 3.5)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
output <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = output(1, 1))
print(p2, vp = output(1, 2))
print(p3, vp = output(2, 1))
print(p5, vp = output(2, 2))


p1<-ggplot(wuran6,aes(x=wuran6$ye_luoyang_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)
p2<-ggplot(wuran6,aes(x=wuran6$ye_kaifeng_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)
p3<-ggplot(wuran6,aes(x=wuran6$ye_xuchang_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)
p4<-ggplot(wuran6,aes(x=wuran6$ye_jiaozuo_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)
p5<-ggplot(wuran6,aes(x=wuran6$ye_xinxiang_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)
p6<-ggplot(wuran6,aes(x=wuran6$ye_pingding_aqi,fill=wuran6$t))+geom_histogram(binwidth = 8)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3)))
output <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(p1, vp = output(1, 1))
print(p2, vp = output(1, 2))
print(p3, vp = output(1, 3))
print(p4, vp = output(2, 1))
print(p5, vp = output(2, 2))
print(p6, vp = output(2, 3))

#箱线图离群值点检测
boxplot(apply(wuran6[,3:17], 2, FUN=function(x){(x-mean(x))/sd(x)}), 
        horizontal=F,col=heat.colors(15) ,names=names(wuran6[,3:17]),outline=TRUE)
title(main="Boxplots of wuran",xlab =  "variables", ylab= "value")

boxplot(apply(wuran6[,18:30], 2, FUN=function(x){(x-mean(x))/sd(x)}), 
        horizontal=F,col=heat.colors(12) ,names=names(wuran6[,18:30]),outline=TRUE)
title(main="Boxplots of wuran",xlab =  "variables", ylab= "value")

boxplot(apply(wuran6[,names(wuran6) %in% c("ye_jiangshui","ye_fengsu","ye_luoyang_pm","PM2.5","AQI","ye_SO2")], 2, FUN=function(x){(x-mean(x))/sd(x)}), 
        horizontal=F,col=heat.colors(7) ,names= c("风速","降水","二氧化硫","洛阳PM2.5","郑州AQI","郑州PM2.5"),outline=TRUE)
title(main="Boxplots of wuran",xlab =  "variables", ylab= "value")

##画2017年污染日历图
library(openair)

data(mydata)
data <- seq.Date(from = as.Date("2017/01/01",format = "%Y/%m/%d"), to= as.Date("2017/12/31",format = "%Y/%m/%d"),by = "day")
data<- format(data, format ="%Y/%m/%d")
data<-as.data.frame(data)
riqi<-cbind(data,wuran$质量等级[which(wuran$年份=="2017")])
cname<-c("data","pm25")
colnames(riqi)=cname
for(i in 1:365){
  mydata[i,1]=riqi[i,1]
  mydata[i,10]=riqi[i,2]
}
calendarPlot(mydata,pollutant = "pm25",year=2017)

shapiro.test(wuran6$PM2.5)
ggplot(wuran6,aes(x=PM2.5))+geom_histogram(binwidth = 8)


#############多元线性回归
#去掉部分离群值点来调整正态性
wuran_norm<-wuran6
wuran_norm<-wuran_norm[order(wuran6$PM2.5),]
quantile(wuran_norm$PM2.5,probs=seq(0,1,by= 0.002))
wuran_norm<-wuran_norm[3:1365,]
wuran_norm$PM2.5[1365:1375]
wuran_norm2<-wuran_norm[order(wuran_norm$ye_pm2.5),]
wuran_norm2$ye_pm2.5[1358:1364]
wuran_norm2$ye_pm2.5[1:2]
wuran_norm3<-wuran_norm2[3:1357,]
dim(wuran_norm3)

k2<-cbind(wuran_norm3[, !names(wuran_norm3) %in% c("ye_dengji","ye_t","年份","日","月份","质量等级","AQI","ye_zhengfa",'t','ye_aqi',"ye_pingding_aqi","ye_luoyang_aqi","ye_jiaozuo_aqi","ye_xuchang_aqi","ye_xinxiang_aqi","ye_kaifeng_aqi")])
k3<-cbind(wuran6[, !names(wuran6) %in% c("ye_dengji","ye_t","年份","日","月份","质量等级","AQI","ye_zhengfa",'t','ye_aqi',"ye_pingding_aqi","ye_luoyang_aqi","ye_jiaozuo_aqi","ye_xuchang_aqi","ye_xinxiang_aqi","ye_kaifeng_aqi")])

test20<-sample(nrow(k2),20)
train_data<-k2[-test20,]
test_data<-k2[test20,]
test_data1<-test_data
train_data1<-k3[!row.names(k3)%in%row.names(test_data1),]

#box-cox变换
m0<-lm(PM2.5~., data=train_data)
boxcox(m0, lambda=seq(0, 1, by=0.01))
library(moments)
lambda<- 0.14
train_data$PM2.5<- ((train_data$PM2.5)^lambda-1)/lambda
test_data$PM2.5<- ((test_data$PM2.5)^lambda-1)/lambda
ggplot(train_data,aes(x=PM2.5))+geom_histogram(binwidth = 0.1)
skewness(train_data$PM2.5)


train_data2<-train_data
rownames(train_data2)<-NULL
dim(train_data2)
#多元线性回归
m2 <- lm(PM2.5~., data=train_data)
summary(m2)
opar<-par(no.readonly=T)
par(mfrow = c(2, 2))
plot(m2)
par(opar)
detach(opar)
qqPlot(m2, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")
#误差的独立性
durbinWatsonTest(m2)
#线性相关性
crPlots(step2, ask = FALSE)
vif(m2)
#检查异常值
library(car)
g<-outlierTest(m2 )

#逐步回归
step2<-step(m2)
l3<-(lambda*predict(step2,test_data)+1)^(1/lambda)
#plot((lambda*test_data$PM2.5+1)^(1/lambda),type='o')
#lines(l3,col='blue')
vif(step2)
par(mfrow = c(2, 2))
plot(step2)
par(opar)
detach(opar)
summary(step2)


qqPlot(step2, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")
#误差的独立性
durbinWatsonTest(step2)
#线性相关性
library(car)
crPlots(m2, ask = FALSE)
crPlots(step2, ask = FALSE)
vif(step2)
linearHypothesis(step2)
gqtest(step2)

train_data4<-cbind(train_data,log(train_data$ye_pm2.5),train_data$ye_luoyang_pm^2,train_data$ye_xuchang_pm^2,train_data$ye_jiaozuo_pm^2,train_data$ye_xinxiang_pm^2,train_data$ye_kaifeng_pm^2,train_data$ye_pingding_pm^2)
m3<-lm(PM2.5~.,data=train_data4)
step3<-step(m3)
summary(m3)
summary(step3)
crPlots(step3, ask = FALSE)
library(lmtest)
bptest(step2,studentize=T)
vif(step2)
par(mfrow = c(2, 2))
plot(step3)
par(opar)
detach(opar)
summary(step2)
vif(step3)
##############################################################################
##随机森林模型

library(randomForest)
rf<-randomForest(PM2.5~.,data=train_data1,mtry=6,ntree=600,importance=TRUE,proximity=TRUE)
plot(rf)    #绘制模型误差与决策树数量关系图  
print(rf)
varImpPlot(rf)
l=predict(rf,test_data1)

#画出线性回归与随机森林回归测试集拟合效果图
par(mfrow = c(1, 1))
plot(seq(1,20,1),test_data1$PM2.5,type='l',col=1,ylab="PM2.5",xlab="时间")
lines(seq(1,20,1),l3,col=2)
lines(seq(1,20,1),l,col=4)
legend('topleft',legend=c('样本值','线性回归预测','随机森林预测'),col=c(1,2,4),lty=1,lwd=2)
title("预测效果")
#计算rmse和mae
mean(abs(l3-test_data1$PM2.5))
mean(abs(l-test_data1$PM2.5))
sqrt(mean(abs(l3-test_data1$PM2.5)^2))
sqrt(mean(abs(l-test_data1$PM2.5)^2))

##SVM
set.seed(1234)
library(kernlab)
wuran7<-wuran6[, !names(wuran6) %in% c("年份","日","月份","质量等级","AQI",'PM2.5')]
names(wuran7)
num=sample(nrow(wuran7),dim(wuran7)[1]*3/10)
train_svm<-wuran7[-num,]
test_svm<-wuran7[num,]

#参数C和sigma的选择
index=1
costs=1
gammas=1
rate2=1
for (i in c(0.1,1,10,50,100)){
  for (j in seq(0.005,0.3,0.004)){
    costs[index]=i
    gammas[index]=j
    wumai.ksvm <- ksvm(t~.,data=train_svm, type = "C-svc", kernel = "rbfdot", cost=i, prob.model=T,kpar = list(sigma =j))
    wuran.prob <- predict(wumai.ksvm, test_svm, type = "probabilities")
    roc1 <- roc(ifelse(test_svm$t=="good",1,0),wuran.prob[,1])
    rate2[index]=auc(roc1)
    index=index+1
  }
}
op<-cbind(costs,gammas,rate)
op<-as.data.frame(op)

#svm模型实施
wumai.ksvm <- ksvm(t~.,data=train_svm, type = "C-svc", kernel = "rbfdot", cost=10, prob.model=T,kpar = list(sigma =0.07))
wuran.prob <- predict(wumai.ksvm, test_svm, type = "probabilities")

roc1 <- roc(ifelse(test_svm$t=="good",1,0),wuran.prob[,1])
plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC曲线')

#根据ROC曲线最佳阈值，设置阈值计算精确度
t<-table(ifelse(wuran.prob>0.518,"bad","good")[,1],test_svm$t,dnn=c("预测值","真实值"))
sum(diag(t))/sum(t)




#svm十折交叉验证
wuran8<-wuran6[, !names(wuran6) %in% c("年份","日","月份","质量等级","AQI",'PM2.5')]

folds<-createFolds(y=wuran8$t,k=10)

accu_svm<-vector(mode='numeric',length=0)
auc_svm<-vector(mode='numeric',length=0)
attach(mtcars)
opar<-par(no.readonly=T)
par(mfrow=c(5,2))

for(i in 1:10){
  #logistic 
  fold_test<-wuran8[folds[[i]],]
  fold_train<-wuran8[-folds[[i]],]
  wumai.ksvm <- ksvm(t~.,data=fold_train, type = "C-svc", kernel = "rbfdot", cost=10, prob.model=T,kpar = list(sigma =0.3))
  wuran.prob1 <- predict(wumai.ksvm, fold_test, type = "probabilities")
  wuran.prob2 <- predict(wumai.ksvm, fold_test, type = "response")
  t<-table(wuran.prob2,fold_test$t)
  accu_svm[i]<-sum(diag(t))/sum(t)

  roc1 <- roc(ifelse(fold_test$t=="good",1,0),wuran.prob1[,1])
  plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC曲线')
  auc_svm[i]<-auc(roc1)
}
par(opar)
detach(mtcars)
mean(accu_svm)
mean(auc_svm)


##随机森林
library(randomForest)

#两个参数的调整
n<-length(names(train_svm))     #计算数据集中自变量个数
rate=1     #设置模型误判率向量初始值
for(i in 1:(n-1)){
  set.seed(1234)
  rf<-randomForest(as.factor(t)~.,data=train_svm,mtry=i,ntree=1000)
  rate[i]<-mean(rf$err.rate)   #计算基于OOB数据的模型误判率均值
}
rate     
plot(rate,type='l',xlab="mtry")

##随机森林预测与训练集结果展示、变量重要性展示
rf<-randomForest(as.factor(t)~.,data=train_svm,mtry=14,ntree=4000)
plot(rf)    
rf<-randomForest(t~.,data=train_svm,mtry=14,ntree=1100,importance=TRUE,proximity=TRUE)
varImpPlot(x=rf,sort=TRUE,n.var=nrow(rf$importance),main="输入变量重要性测度散点图")
MDSplot(rf,train_svm$t,palette=c(2,4),pch=as.numeric(train_svm$t))    

##随机森林测试集预测
t<-predict(rf,test_svm,type="prob")
roc1 <- roc(ifelse(test_svm$t=="good",1,0),t[,1])
plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC曲线')
ta<-table(ifelse(t>0.523,"bad","good")[,1],test_svm$t,dnn=c("预测值","真实值"))
sum(diag(ta))/sum(ta)
