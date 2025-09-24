####### 诺如图R
library(rms)
lowbirth <- read.csv("C:/Users/lenovo/Desktop/Cardiovascular.csv")
dim(lowbirth)
str(lowbirth) 
library(dplyr)
dd <- datadist(lowbirth)
options(datadist="dd")
fit1 <- lrm(status ~ Sex + Age + smoking +Drink,data = lowbirth,x=T,y=T)
nom2 <- nomogram(fit1, fun=plogis,
                 fun.at=c(0.001,0.01,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.99),
                 lp=T, 
                 maxscale = 100, # ?????÷???
                 conf.int = F, # ???????????䣬???ѿ??????Բ?Ҫ
                 funlabel="Risk of UTI")  
plot(nom2,
     col.grid=c("tomato","grey")
     #conf.space = c(0.1,1000) # ????????λ??
) 
# 打开一个宽度更大的 PDF 文件
pdf("output.pdf", width = 30, height =15)
install.packages("regplot")
library(regplot)
# Generate the regplot with margins set using par function
regplot(fit1,
        plots = c("violin","spikes") ,##指定了用于绘制的小提琴图和尖峰图" plot" "density" "boxes" "ecdf" "bars" "boxplot" "violin" "bean" "spikes"????????��????״????ѡ"no plot" "boxes" "bars" "spikes"
        observation = lowbirth[2,], #指定需要将哪一行数据绘制到图表中
        center = T, # 将中心点放在回归拟合的平均值处
        subticks = T,#在坐标轴上添加子标记以增加可读性
        droplines = T,#展示回归直线上每个观察值的垂直线，以便于了解拟合线和实际数据之间的差异。
        title = "Nomogram of 30-day readmission Risk",#设置图表标题为 “Preoperative DVT的Nomogram”。
        points = T,# 显示数据点的位置
        odds = F ,# 不显示odds值。
        showP = T ,# 显示每一个变量的显著性水平，可以评估模型的整体拟合度。
        rank = "sd" ,# 根据标准差排序预测变量的重要性
        interval="confidence" ,#显示95％置信区间，表示模型中参数的可靠程度?
        clickable = F, #设置图表不能交互，所以不能捕捉和保存坐标轴上的点
        
)        
