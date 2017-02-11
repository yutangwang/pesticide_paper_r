setwd('F:/ytwang/Dropbox/USLab/pesticides/')


library(mongolite)
library(grid)
library(futile.logger)
library(VennDiagram)


cndb <- mongo(collection= "CNPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

eudb <- mongo(collection= "EUPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

usdb <- mongo(collection= "USPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

jpdb <- mongo(collection= "JPPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

pe_pro1 <- mongo(collection= "pesticides_profile1", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

pe_pro1$insert(profile_1)
  
  
venn.data <- list()
venn.data$China_1199<- cndb$find()$cactus_smiles            #1
venn.data$EU_914 <- eudb$find()$cactus_smiles               #2
venn.data$USA_669 <- usdb$find()$cactus_smiles               #3
venn.data$Japan_401 <- jpdb$find()$cactus_smiles            #4


##绘图函数
##4个集合的文氏图
grob.list <- venn.diagram(x=venn.data,filename=NULL,
                          cex=1.8,cat.cex=1.8,
                          cat.dist=0.04,##类别名与边的距离
                          label.col="white",##区域类数字的颜色
                          fontface=2,##区域类数字的字体
                          fill=2:5,
                          category.names = names(venn.data)
)

grid.draw(grob.list)

###提取overlap区域
ev.getven<-get.venn.partitions(x=
                                 list(
                                   "China"=venn.data$China_1199,
                                   "EU"=venn.data$EU_914,
                                   "USA"=venn.data$USA_669,
                                   "Japan"=venn.data$Japan_401
                                 )
)

### 把生成的交集插入mongodb
newdb <- mongo(collection= "Pesticides_4countriesClean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)
newdb$insert(ev.getven)


## write a function
## n-a number or numerical vector
dlm.extract.bb <- function(n) {
  for(i in 1:length(n)){
    aim<-ev.getven[ev.getven$..count..==n[i],]
    aim1<-aim$..values.. 
    aim1<-as.data.frame(aim1)
    names(aim1)<-aim$..set..
    filename<-paste0(substr(names(aim1[1]),2,6),".csv")
    write.csv(aim1,filename)
  }
  print("Job done!")
}


dlm.df <- function(){
  dfa <- data.frame()
  for (s in ev.getven$..set..){
    aim <- ev.getven[ev.getven$..set.. == s,]
    df <- data.frame(set = rep(s,length(aim$..values..)))
    df <- cbind(df,X1= aim$..values..)
    dfa <- rbind(dfa,df)
  }
  return dfa
}

# 提取并保存为csv文件
dfa <- data.frame()
i = 1

for (s in ev.getven$..set..){
  aim <- ev.getven[ev.getven$..set.. == s,]
  df <- data.frame(set = rep(s,length(aim$..values..)), aim$..values..)
  names(df) <- c("set", "smiles")
  #df$set = i
  #i = i +1
  dfa <- rbind(dfa,df)
}

write.csv(dfa, 'setoverlap_pesticides.csv', row.names = FALSE)



#把重叠区域和非重叠区域区分开
(China)\(EU<U+222A>Japan<U+222A>USA)
(EU)\(China<U+222A>Japan<U+222A>USA)
(Japan)\(China<U+222A>EU<U+222A>USA)
(USA)\(China<U+222A>EU<U+222A>Japan)





for (s in ev.getven$..set..){
  aim <- ev.getven[ev.getven$..set.. == s,]
  a <- aim$..values..
  for smiles in a{
    print(smiles)
  }
}
b <- as.vector(a)


## Example
a<-c(469,41,233,7,35,35)
dlm.extract.bb(a)
dlm.df
#####
library(mongolite)
cndb <- mongo(collection= "CNPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

eudb <- mongo(collection= "EUPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

usdb <- mongo(collection= "USPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)

jpdb <- mongo(collection= "JPPesticides_clean", db = "learnDB", 
              url = "mongodb://yw518:tmp2016@ciipro.rutgers.edu:27017/learnDB?authSource=admin",
              verbose = TRUE)


cndb$count()
c <- cndb$find()


venn.data$China_1257 <- unique(China_Pesticides$stdinchikey) #1
venn.data$EU_1017 <- unique(EU_Pesticides$stdinchikey)   #2
venn.data$Japan_881 <- unique(JP_Pesticides$stdinchikey)               #3
venn.data$USA_1542 <- unique(USA_Pesticides$stdinchike)   

csv_2_mongodbcn('cn.txt', 'CNPesticides_clean')
csv_2_mongodb('eu.txt', 'EUPesticides_clean')
csv_2_mongodb('us.txt', 'USPesticides_clean')
csv_2_mongodb('jp.txt', 'JPPesticides_clean')

cndb$find()$CASRN

cndb
data(diamonds, package="ggplot2")
diamonds
typeof(diamonds)
