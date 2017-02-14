
getwd()
setwd("d:/Dropbox/R/BGI-R-training/script")


a <- read.table("D:/Dropbox/R/BGI-R-training/data/mcp.M113.035600-2.txt", ##数据文件名
                head=TRUE, ##第一行为表头
                sep="\t", ##制表符("\t")分割
                ##为TRUE时进行列名检查，如果有空格，会用"."代替,
                ##为FALSE时，不进行列名检查
                check.names = FALSE) 

##第一列为基因ID，去掉第一列，把剩余的列保存到x里
x <- a[,-1]

##转置
x <- t(x)
x <- log2(x+1)
setwd( "G:/Dropbox/R/WangJinghua")
a =read.table("../data/WangJinghua.txt",head=T)
x <- a[4:11]

##加载包，利用这个包里的pca函数进行PCA分析
library(Rcpp)
library(pcaMethods)

##进行PCA分析
pres <- pca(x,scale = "pareto",center = TRUE)

##提取主成分的贡献率
xlab <- paste("PC1",sprintf("(%.2f%%)",100*pres@R2[1]))
ylab <- paste("PC2",sprintf("(%.2f%%)",100*pres@R2[2]))

##绘制PCA的得分图
pdf("pca.pdf",width = 4,height = 4)
par(mar=c(3,3,1,1),mgp=c(1.6,0.6,0))
plot(pres@scores[,1],pres@scores[,2],
     xlim=c(-150,150),
     xlab=xlab,ylab=ylab,
     pch=16,col="green")
#points(pres@scores[,1], pres@scores[,2], col=NULL, bg=rgb(1, 0, 0, alpha=0.1), pch=21, cex=2)
abline(v=0,h=0,lty=2,col="gray",lwd=2)
text(pres@scores[,1],pres@scores[,2],labels = names(a)[-1],pos = 4,cex=0.5)
dev.off()

