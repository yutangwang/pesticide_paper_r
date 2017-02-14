

a <- sim

b <- apply(a,2,max)
c <- data.frame(id = names(a), max_value = b)


ns <- new_sim
ns_b <- apply(ns,2,max)

ns_c <- data.frame(id = names(ns), max_value = ns_b)

ns_c_sort <- ns_c[order(ns_c$max_value),] 






############
b <- apply(a,2,min)


p <- profile_1
p_id <- gsub('-', '_', p$db_id)
p_db <- paste('db',p_id, sep = "")
p$db_id <- p_db
write.csv(p, 'profile_dbid.csv', row.names = FALSE)

###

###Finding the column number and value the of second highest value in a row

df <- ns
# a function that returns the position of n-th largest
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
df <- ns[c("CID","db15_314", "db15_4", "db15_384", "db15_441", "db15_416", "db15_71", "db15_246", "db15_203","db15_127", "db15_204", "db15_455")]

# value of the largest
max1_value <- apply(df, 2, function(x)x[maxn(1)(x)])
max1_value_frame <- as.data.frame(max1_value)
## value of the largest
apply(df,2,max)
#position of the largest
max1_position <-  apply(df, 2, maxn(1))
max1_position_frame <- as.data.frame(max1_position)


# position of the 2nd largest
max2_position <- apply(df, 2, maxn(2))
max2_position_frame <- as.data.frame(max2_position)

# value of the 2nd largest
max2_value <- apply(df, 2, function(x)x[maxn(2)(x)])
max2_value_frame <- as.data.frame(max2_value)


#search the neighbor dbid

df$db15_314[176]
df$CID[176]

df$db15_4
df$CID[176]
#
target <- c("CID","db15_314", "db15_4", "db15_384", "db15_441", "db15_416", "db15_71", "db15_246", "db15_203","db15_127", "db15_204", "db15_455")




#select the first three neighbors

se3 <- function(dbid){
  df1 <- df[c("CID",dbid)]
  #position of the largest
  max1_position <-  apply(df1, 2, maxn(1))  
  print(df1$CID[max1_position[2]])
  max2_position <- apply(df1,2, maxn(2))
  print(df1$CID[max2_position[2]])
  max3_position <- apply(df1,2, maxn(3))
  print(df1$CID[max3_position[2]])
  
}

se3("db15_314")

#print max3_position in order to check if it is correct.
for (m in max3_position){
  print(df$CID[m])
}




# value of the 3nd largest
max3_value <- apply(df, 2, function(x)x[maxn(3)(x)])
max3_value_frame <- as.data.frame(max3_value)

# position of the 2nd largest
max3_position <- apply(df, 2, maxn(3))
max3_position_frame <- as.data.frame(max3_position)









# position of the 2nd largest
max2_position <- apply(df, 2, maxn(2))
# value of the 2nd largest
apply(df, 2, function(x)x[maxn(2)(x)])


x.sub <- subset(df, car == 2)

apply(df, 1, maxn(2))



####
# a function that returns the position of n-th largest
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

this is a closure, so you can use like this:
  
  > # position of the largest
  > apply(df, 2, maxn(1))


> # position of the 2nd largest
  > apply(df, 1, maxn(2))
[1] 2 3 1 2 1
> 
  > # value of the largest
  > apply(df, 1, function(x)x[maxn(1)(x)])
[1] 2 4 2 1 1
> # value of the 2nd largest
  > apply(df, 1, function(x)x[maxn(2)(x)])
[1] 0 3 1 1 0

Updated

Why using closure here?

One reason is that you can define a function such as:
  
max2 <- maxn(2)
max3 <- maxn(3)

then, use it

> apply(df, 1, max2)
[1] 2 3 1 2 1
> apply(df, 1, max3)
[1] 3 2 2 3 2

###
x.sub <- subset(df, df$db15_314 == 0.1071429)
