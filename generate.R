setwd('F:/ytwang/Dropbox/USLab/pesticides/BioSim')

#generating some data frame
neighbor2_4 <- profile_1[(profile_1$db_id == '2-4'| profile_1$db_id == "5-17"),]

#remove the column that value is zero
db_id <- neighbor2_4$db_id
neighbor2_4 <- neighbor2_4[,colSums(neighbor2_4[,-1]^2) == 2]

neighbor2_4 <- neighbor2_4[,colSums(neighbor2_4^2) == 2]

neighbor2_4 <- cbind(db_id = db_id, neighbor2_4)


write.csv(neighbor2_4, 'neighbor2_4.csv')


#generating some data frame for 2-5 neighbor
neighbor2_5 <- profile_1[(profile_1$db_id == '2-5'| profile_1$db_id == "15-219" | profile_1$db_id == "14-99" | profile_1$db_id == "15-376"),]

#remove the column that value is zero
db_id <- neighbor2_5$db_id
neighbor2_5 <- neighbor2_5[,colSums(neighbor2_5[,-1]^2) == 4]

neighbor2_5 <- neighbor2_5[,colSums(neighbor2_5^2) == 4]

neighbor2_5 <- cbind(db_id = db_id, neighbor2_5)

write.csv(neighbor2_5, 'neighbor2_5.csv')


#generating some data frame for 5-15 neighbor
neighbor5_15 <- profile_1[(profile_1$db_id == '5-15'| profile_1$db_id == "5-108" | profile_1$db_id == "5-112" | profile_1$db_id == "9-68"),]

#remove the column that value is zero
db_id <- neighbor5_15$db_id
neighbor5_15 <- neighbor5_15[,colSums(neighbor5_15[,-1]^2) == 4]

neighbor5_15 <- neighbor5_15[,colSums(neighbor5_15^2) == 4]

neighbor5_15 <- cbind(db_id = db_id, neighbor5_15)

write.csv(neighbor5_15, 'neighbor5_15.csv')


#generating some data frame for 15-452 neighbor
neighbor15_452 <- profile_1[(profile_1$db_id == '15-452'| profile_1$db_id == "5-108" | profile_1$db_id == "9-68"),]

#remove the column that value is zero
db_id <- neighbor15_452$db_id
neighbor15_452 <- neighbor15_452[,colSums(neighbor15_452[,-1]^2) == 3]

neighbor15_452 <- neighbor15_452[,colSums(neighbor15_452^2) == 3]

neighbor15_452 <- cbind(db_id = db_id, neighbor15_452)

write.csv(neighbor15_452, 'neighbor15_452.csv')



