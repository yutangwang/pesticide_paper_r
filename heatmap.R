
a <- read.table("D:/Dropbox/R/BGI-R-training/data/mcp.M113.035600-2.txt", ##数据文件名
                head=TRUE, ##第一行为表头
                sep="\t", ##制表符("\t")分割
                ##为TRUE时进行列名检查，如果有空格，会用"."代替,
                ##为FALSE时，不进行列名检查
                check.names = FALSE) 

##第一列为基因ID，去掉第一列，把剩余的列保存到x里

x <- a[,-1]

##对x进行log转换
x <- log2(x+1)



##计算相关系数
corX <- cor(x)


######## 制相关性热图
library(plyr)
library(pheatmap)
## border_color=NA 表示去掉边框颜色
pheatmap(corx,border_color = NA,legend_labels="Correlation", fontsize = 4.5) 

###
a <- profile_1
a <- a[,-1]
x <- t(a)
x <- as.data.frame(x)
names(x) <- profile_1$db_id

x <- x[,(colSums(x^2) != 0)]

write.csv(x,'x.csv')

###建立数据

corx <- biosimimportR
row.names(corx) <- corx$cid
corx <- corx[,-1]
corx <- as.matrix(corx)



###删掉全部为0的行
a <- profile_1
a <- a[,-1]
x <- t(a)
x <- as.data.frame(x)
names(x) <- profile_1$db_id

x <- x[,(colSums(x^2) != 0)]
a <- t(x)
db_id <- paste("db",gsub('-','_',row.names(a)), sep = "")

a <- data.frame(db_Id = db_id, a)


write.csv(a,'profile_972.csv', row.names = FALSE)
##删掉零的行之后，进行biosim计算后倒入的。

corx <- BioSim_importR_data

row.names(corx) <- corx$CID
corx <- corx[,-1]

drop_name <- c('X')

corx <- corx[,!(names(corx) %in% drop_name)]

corx_all <- distance_matrix_dbid[,-1]

row.names(corx_all) <- distance_matrix_dbid$dbid
corx_500 <- corx_all[0:50,0:50]
corx <- as.matrix(d)
c <- corx
corx <- (1+c)^0.25

row.names(corx_all) <- names(corx_all) 
pheatmap(corx,border_color = NA,legend_labels="Correlation", fontsize = 2.5, color = colorRampPalette(c("firebrick3", "white", "navy"))(50)) 

