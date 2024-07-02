#how to draw a plot
#yang 20240701
rm(list=ls())
data("iris")#as an example

library(ggplot2)

## topice one
##��ͼ��ά��
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width))
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width,shape=Species))#һ���ͼ��Ҫչʾ��ά����Ϣ

ggplot(NULL)+geom_point(aes(1:10,1:10,size=rnorm((10))))

## topice two
##�����չ��Ϣ
###���
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width,shape=Species))
ggplot(iris)+geom_point(aes(Sepal.Length,Sepal.Width,size=Petal.Length,fill=Petal.Width))+facet_wrap(.~Species)
###�ϲ�
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
##��ͼ���ͺ��÷�
###������ֵ�ߵ�
####AI CODE
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Sepal Length for Iris Species",
       x = "Species",
       y = "Sepal Length") +
  theme_minimal()
####my example
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_boxplot()+geom_jitter()#����ͼ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#С����ͼ

test=aggregate(iris$Petal.Width,by=list(iris$Species),mean)
names(test)=c("Species","Petal.Width")
ggplot(test,aes(x=Species,y=Petal.Width))+geom_bar(stat = "identity")#��״ͼ
#####radarchart {fmsb} �״�ͼ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+coord_flip ()#����
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+coord_polar()#������
#####���������Ա��
library(ggsignif)
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+geom_signif(comparisons = list(c("setosa","virginica"),c("virginica", "versicolor"),c("setosa","versicolor")),map_signif_level =c("***"=0.001, "**"=0.01, "*"=0.05),test= "wilcox.test",y_position = c(4,3,2))#���
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()+annotate("text",x=1:3,y=2.6,label=c("c","b","a"))#��ĸ

###�����Ͷ�Ӧ��ϵ
ggplot(iris, aes(y=Sepal.Length, x=Species,fill=Sepal.Length)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(iris, aes(y=Sepal.Length,x=1,fill=Species)) + 
  geom_bar(position="dodge", stat="identity")+coord_polar(theta="y")

#library(circlize)#������ͼ
#networkD3 d3network sankey riverplot#ɣ��ͼ
#VennDiagram ggvenn UpsetR#Τ��ͼ

###����Ի��������
#pheatmap ComplexHeatmap corrplot ggcorrplot#��ͼ
ggplot(iris,aes(x=Sepal.Width,y=Petal.Width))+geom_point()+geom_smooth(method="lm")

data(mtcars)
corr <- round(cor(mtcars), 1)
data2=reshape2::melt(corr)
ggplot(data2,aes(x=Var1,y=Var2,colour=value))+geom_point(size=12)#ɢ��ͼ����ͼ

####��ͼ
#ggtree
#�μ�https://github.com/XingshengYang/plot_tools

####����ͼ
#igraph ggClusterNet
#����Cytoscape Gephi

## topice FOUR
##�޸�һ��ͼ
theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border =element_rect(fill = "transparent",size=2),axis.line = element_line(colour = "black"),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"),legend.text = element_text(colour = 'black', size = 20,  family  = "serif",face = 'bold'),legend.title = element_text(colour = 'black', size = 20,  family  = "serif",face = 'bold'),axis.title.x= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.title.y= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.ticks=element_line(colour = "black",size=1.2))
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#��ͼ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))#��ɫ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08)#��������ͼ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)#����ͼ��ɫ
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_minimal()#Ĭ������1
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()#Ĭ������2
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+coord_polar()#������
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none")#��ע��
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "bottom")
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"))#�����߿�
ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"),axis.title= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"))#��������

## topice five
##ƴͼ
#library(aplot)
library(patchwork)
p1=ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin()#��ͼ
p2=ggplot(iris,aes(x=Species,y=Petal.Width))+geom_violin(aes(fill=Species))+geom_boxplot(width=0.08,alpha=0.5)+theme_bw()+theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0,colour = "black"),panel.border =element_rect(fill = "transparent",size=1.2),axis.ticks = element_line(size=1.2,colour = "black"),axis.title= element_text(colour = 'black', size = 28,  family  = "serif",face = 'bold'),axis.text= element_text(size=25, color="black", family  = "serif", face= "bold"))#��������
p1+p2#ƴͼʾ��

##topic 6
##���
#ggsave()
pdf("plot.pdf",width = 25,height=25)#pdf�����޸�
p2
dev.off()
library(export)
graph2ppt(x=p2,"plot.pptx", width=7,height=7) #ppt�����޸�
