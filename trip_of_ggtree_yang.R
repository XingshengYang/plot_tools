# a trip of ggtree by yang 20230911

##intall the packages
#install.packages(c("ggplot2","ggtree","ape","dplyr","tidytree","treeio","ggtreeExtra","vegan","patchwork","export","ggnewscale"))
library(ggplot2)
library(ggtree)
library(treeio)
library(patchwork)
library(vegan)
library(dplyr)
## structure a simulated data
simulated_data=data.frame(row.names = paste0("ASV",1:1000),value=rnorm(1000))
original_treedata=ape::as.phylo(hclust(vegan::vegdist(simulated_data, "euclidean"), "average"))
#original_treedata2=rtree(1000)#you can get a tree use rtree in treeio
#original_treedata2$tip.label=paste0("ASV",1:1000)
###try to plot the tree
ggtree(original_treedata2)

##function for select element in tree data
select_f_tree= function (treedata1,label_list=NULL,sample0=length(treedata1$tip.label)) {
  treedata2=treedata1
  if(!is.null(label_list)){
    o_label=treedata2$tip.label
    d_label=o_label[is.na(match(o_label,label_list))]
    treedata2=drop.tip(treedata2,d_label)
  }
  if (sample0<length(treedata1$tip.label)){
    if (sample0>length(treedata2$tip.label)) stop ("the sample0 is larger than label_list")
    else {
      r_label=treedata2$tip.label
      d_label2=sample(r_label,(length(treedata2$tip.label)-sample0))
      treedata2=drop.tip(treedata2,d_label2)
    }
  }
  return(treedata2)
}

##draw tree with 100,200,300,500 edges
tree_list1=list()
tree_list2=list()
testlist=c(100,200,300,500)
for (ii in 1:length(testlist)) {
  thetree=select_f_tree(original_treedata,sample0=testlist[ii])
  tree_list1[[ii]]=ggtree(thetree)
  tree_list2[[ii]]=ggtree(thetree,layout = "fan")
}
(tree_list1[[1]]/tree_list1[[2]])|(tree_list1[[3]]/tree_list1[[4]])
(tree_list2[[1]]/tree_list2[[2]])|(tree_list2[[3]]/tree_list2[[4]])

##draw tree with different layout
tree_list3=list()
testlist=c( 'rectangular', 'dendrogram', 'slanted', 'ellipse', 'roundrect', 'fan', 'circular', 'inward_circular', 'radial', 'equal_angle', 'daylight' , 'ape')
thetree=select_f_tree(original_treedata,sample0=50)
for (ii in 1:length(testlist)) {
  tree_list3[[ii]]=ggtree(thetree,layout = testlist[ii])
}
(tree_list3[[1]]/tree_list3[[2]])|(tree_list3[[3]]/tree_list3[[4]])|(tree_list3[[5]]/tree_list3[[6]])|(tree_list3[[7]]/tree_list3[[8]])|(tree_list3[[9]]/tree_list3[[10]])|(tree_list3[[11]]/tree_list3[[12]])

## open angle
ggtree(thetree,layout = 'fan',open.angle = 90,right = T,colour="red")
ggtree(thetree,layout = 'fan',open.angle = 30,colour="green")

## add labels in figure
ggtree(thetree,layout = 'fan',open.angle = 90,right = T,colour="red")+geom_tiplab(colour="red")

##adding information
###simulated information
information=data.frame(label=paste0("ASV",1:100),group=rep(c("A","B"),each=50),phylum=rep(paste0("phylum",1:10),each=10),order=rep(paste0("order",1:20),each=5),sample_1=rnorm(100),sample_2=rnorm(100))
row.names(information)=information$label
###get subtree
subtree=select_f_tree(original_treedata,information$label)
ggtree(subtree,layout = 'fan',open.angle = 15)+geom_tiplab()
###match information
subtree2=full_join(as.phylo(subtree),information[c(1,2)],by="label")
gg1=ggtree(subtree2,layout = 'fan',open.angle = 50)+geom_tiplab(aes(colour=group),size=1)

##plot tree with information
###method 1 geom_facet
####use the firt column to match
ggtree(subtree2)+geom_facet(data=information,panel = "sample1",mapping = aes(x=sample_1,colour=phylum),geom=geom_col,orientation="y",width=.5)+geom_facet(data=information,panel = "sample2",mapping = aes(x=sample_2,colour=phylum),geom=geom_point,orientation="y")

###method 2 gheatmap
library(ggnewscale)
####use rowname to match
gg2=gheatmap(gg1,information[3:4],offset = 0.2,width = 0.3,colnames_offset_y = 0.3,colnames_angle = 90,hjust=1)+scale_fill_viridis_d(option = "D",name="classification")
gg3=gg2+new_scale_fill()
gg4=gheatmap(gg3,information[5:6],offset = 0.7,width = 0.3,colnames_offset_y = 0.3,colnames_angle = 90,hjust=1)+scale_fill_viridis_c(option = "A",name="abundance")

###method 3 geom_fruit
library(ggtreeExtra)
###use "y" to match
pl2=gg1+geom_fruit(data=information,geom = geom_tile,mapping = aes(y=label,fill=phylum),offset = 0.2,pwidth =0.1)+scale_fill_viridis_d(option = "D",name="phylum",begin = 0,end=0.5,guide=guide_legend(ncol=2))
pl3=pl2+new_scale_fill()
pl4=pl3+geom_fruit(data=information,geom = geom_tile,mapping = aes(y=label,fill=order),offset = 0.08,pwidth =0.1)+scale_fill_viridis_d(option = "D",name="order",begin = 0.6,end=1,guide=guide_legend(ncol=2))
pl5=pl4+geom_fruit(data=information,geom = geom_bar,mapping = aes(y=label,x=abs(sample_1)),orientation="y",stat="identity",offset = 0.08,fill="#696969")+geom_fruit(data=information,geom = geom_bar,mapping = aes(y=label,x=abs(sample_2)),orientation="y",stat="identity",offset = 0.02,fill="#006400")

##output
### type1 PDF
pdf("tree.pdf",width = 25,height=25)
pl4
dev.off()
####using AI to modification

### type1 ppt
library(export)
graph2ppt(pl5,"tree.pptx", width=12, aspectr=2, append = TRUE)
###using ppt to modification

##
print("Have a good trip!")
