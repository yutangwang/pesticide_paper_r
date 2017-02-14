sumcount <-function(df){
  scv <- c()
  for (i in c(1:nrow(df))){
    #print(df[i,])
    sc <- sum(df[i,] == 1)
    scv <- append(scv,sc)
  }
  return(scv)
}