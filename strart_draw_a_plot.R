#how to draw a plot
#yang 20240701
rm(list=ls())
data("iris")#as an example

library(ggplot2)

## topice one
##绘图的维度
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width))
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width,shape=Species))#一般绘图主要展示五维度信息

ggplot(NULL)+geom_point(aes(1:10,1:10,size=rnorm((10))))

## topice two
##如何拓展信息
###拆分
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width,shape=Species))
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width))+facet_wrap(.~Species)
###合并
ggplot(iris) +
  geom_line(aes(x = Sepal.Length, y = Sepal.Width, group = 1), color = "red", size = 1) + 
  geom_point(aes(x = Sepal.Length, y = Petal.Width), color = "blue", size = 2)
ggplot(iris) +
  geom_line(aes(x = Sepal.Length, y = Sepal.Width, group = 1), color = "red", size = 1) + 
  geom_point(aes(x = Sepal.Length, y = Petal.Width*1.2+1), color = "blue", size = 2) + 
  scale_y_continuous(
    name = "Value 1",
    sec.axis = sec_axis(~./1.2-1/1.2, name = "Petal.Width")
  )

##topic three
##绘图类型和用法
###表现数值高低
####AI CODE
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Sepal Length for Iris Species",
       x = "Species",
       y = "Sepal Length") +
  theme_minimal()
####my example
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_boxplot()+geom_jitter()#箱线图
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#小提琴图

test=aggregate(iris$Petal.Width,by=list(iris$Species),mean)
names(test)=c("Species","Petal.Width")
ggplot(test,aes(x=Species,y=Petal.Width))+geom_bar(stat = "identity")#柱状图
#####radarchart {fmsb} 雷达图
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+coord_flip ()#横置
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+coord_polar()#极坐标
#####添加显著性标记
library(ggsignif)
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+geom_signif(comparisons = list(c("setosa","virginica"),c("virginica", "versicolor"),c("setosa","versicolor")),map_signif_level =c("***"=0.001, "**"=0.01, "*"=0.05),test= "wilcox.test",y_position = c(4,3,2))#标记
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+annotate("text",x=1:3,y=2.6,label=c("c","b","a"))#字母

###比例和对应关系
ggplot(iris, aes(y=Sepal.Length, x=Species,fill=Sepal.Length)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(iris, aes(y=Sepal.Length,x=1,fill=Species)) + 
  geom_bar(position="dodge", stat="identity")+coord_polar(theta="y")

#library(circlize)#复杂弦图
#networkD3 d3network sankey riverplot#桑基图
#VennDiagram ggvenn UpsetR#韦恩图

###相关性或大量数据
#pheatmap ComplexHeatmap corrplot ggcorrplot#热图
ggplot(iris,aes(x=Sepal.Width,y=Petal.Width))+geom_point()+geom_smooth(method="lm")

data(mtcars)
corr <- round(cor(mtcars), 1)
data2=reshape2::melt(corr)
ggplot(data2,aes(x=Var1,y=Var2,colour=value))+geom_point(size=12)#散点图画热图

####树图
#ggtree
#参见https://github.com/XingshengYang/plot_tools

####网络图
#igraph ggClusterNet
#软件Cytoscape Gephi

## topice FOUR
##修改一张图
theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border =element_rect(fill = "transparent",size=2),axis.line = element_line(colour = "black"),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"),legend.text = element_text(colour = 'black', size = 20,  family  = "serif",face = 'bold'),legend.title = element_text(colour = 'black', size = 20,  family  = "serif",face = 'bold'),axis.title.x= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.title.y= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.ticks=element_line(colour = "black",size=1.2))
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#底图
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))#上色
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08)#加入箱线图
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)#箱线图调色
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_minimal()#默认主题1
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()#默认主题2
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+coord_polar()#极坐标
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none")#改注释
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "bottom")
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"))#调整线框
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"),axis.title= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"))#调整字体

## topice five
##拼图
#library(aplot)
library(patchwork)
p1=ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#底图
p2=ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"),axis.title= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"))#调整字体
p1+p2#拼图示例

##topic 6
##输出
#ggsave()
pdf("plot.pdf",width = 25,height=25)#pdf便于修改
p2
dev.off()
library(export)
graph2ppt(x=p2,"plot.pptx", width=7,height=7) #ppt便于修改

