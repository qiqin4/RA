

#FISS
#Remove Israel
FISSitems_Israel_removed <- FISSitems[FISSitems$cntry!=60,]
FISSitems_clean <- FISSitems_Israel_removed
FISSitems_clean$nna <- NA
for (j in 1:nrow(FISSitems_clean)) {
  FISSitems_clean$nna[j] <- table(is.na(FISSitems_clean[j,4:43]))["TRUE"]
}
#Remove the cases with complete NAs
FISSitems_clean$nna <- as.numeric(FISSitems_clean$nna)
FISSitems_clean$nna <- ifelse(is.na(FISSitems_clean$nna),0,FISSitems_clean$nna)
FISSitems_clean <- FISSitems_clean[FISSitems_clean$nna!=40,]
FISSitems_clean$sum <- rowSums(FISSitems_clean[,4:43],na.rm = T)

#SISS
#Remove the cases with complete NAs
SISSitems_clean <- SISSitems
SISSitems_clean$nna <- NA

for (j in 1:nrow(SISSitems)) {
  SISSitems_clean$nna[j] <- table(is.na(SISSitems_clean[j,4:27]))["TRUE"]
}

SISSitems_clean$nna <- ifelse(is.na(SISSitems_clean$nna),0,SISSitems_clean$nna)
SISSitems_clean <- SISSitems_clean[SISSitems_clean$nna!=24,]
table(SISSitems_clean$nna)
table(SISSitems$nna)

SISSitems_clean$sum <- rowSums(SISSitems_clean[,4:27],na.rm = T)

#-----------------------------------
#point-biserial correlation coefficient 
####FISS####

###in general by item
#cor(FISSitems_clean[complete.cases(FISSitems_clean[,-c(1:3,44)]),-c(1:3,44)])
FISScor <- cor(FISSitems_clean[,4:43],use = "pairwise.complete.obs")
FISSsumcor <- cor(FISSitems_clean[,4:43],FISSitems_clean$sum,use = "pairwise.complete.obs")
#FISSb <- psych::biserial(FISSitems_clean[,4:43],FISSitems_clean$sum)

###in each educational system
View(FISSitems_clean)
FISSctrylist <- split(FISSitems_clean,FISSitems_clean$cntry)
FISScorlist <- list()
for (i in 1:length(FISSctrylist)) {
  FISScorlist[[i]] <- cor(FISSctrylist[[i]][,4:43],use = "pairwise.complete.obs")
  
}

FISSbis <- data.frame(matrix(nrow=40,ncol=16))
for (i in 1:length(FISSctrylist)) {
  FISSbis[,i] <- cor(FISSctrylist[[i]][,4:43],FISSctrylist[[i]][45],use = "pairwise.complete.obs")
}

#FISSbislist <- list()
# for (i in 1:length(FISSctrylist)) {
#   FISSbislist[[i]] <- cor(FISSctrylist[[i]][,4:43],FISSctrylist[[i]][45],use = "pairwise.complete.obs")
#   
# }


#save to excel
FISScnt <- as.numeric(rownames(table(FISSitems_clean$cntry)))
FISScntnames <- names(attributes(FISSitems_clean$cntry)$labels[attributes(FISSitems_clean$cntry)$labels%in%FISScnt])
names1 <- list("BELGIUM, FLEMISH" = FISScorlist[[1]], 
              "BELGIUM, FRENCH"= FISScorlist[[2]],  
              "CHILE" = FISScorlist[[3]],           
              "ENGLAND-WALES" = FISScorlist[[4]],  
              "FED REP GERMANY" = FISScorlist[[5]],
              "FINLAND"   = FISScorlist[[6]],
              "HUNGARY"     = FISScorlist[[7]],
              "INDIA"    = FISScorlist[[8]],       
              "IRAN"   = FISScorlist[[9]],
              "ITALY"   = FISScorlist[[10]],
              "JAPAN"   = FISScorlist[[11]],
              "NETHERLANDS"  = FISScorlist[[12]],   
              "SCOTLAND"  = FISScorlist[[13]],
              "SWEDEN"   = FISScorlist[[14]],
              "THAILAND"  = FISScorlist[[15]],
              "USA" = FISScorlist[[16]])

openxlsx::write.xlsx(names1, file = 'FISScor.xlsx')

names(FISSbis) <- FISScntnames
xlsx::write.xlsx(FISSbis,"FISSbiser.xlsx")

# names2 <- list("BELGIUM, FLEMISH" = FISSbislist[[1]], 
#                "BELGIUM, FRENCH"= FISSbislist[[2]],  
#                "CHILE" = FISSbislist[[3]],           
#                "ENGLAND-WALES" = FISSbislist[[4]],  
#                "FED REP GERMANY" = FISSbislist[[5]],
#                "FINLAND"   = FISSbislist[[6]],
#                "HUNGARY"     = FISSbislist[[7]],
#                "INDIA"    = FISSbislist[[8]],       
#                "IRAN"   = FISSbislist[[9]],
#                "ITALY"   = FISSbislist[[10]],
#                "JAPAN"   = FISSbislist[[11]],
#                "NETHERLANDS"  = FISSbislist[[12]],   
#                "SCOTLAND"  = FISSbislist[[13]],
#                "SWEDEN"   = FISSbislist[[14]],
#                "THAILAND"  = FISSbislist[[15]],
#                "USA" = FISSbislist[[16]])

# openxlsx::write.xlsx(names2, file = 'FISSbis.xlsx')




####SISS####
###in general by item
SISScor <- cor(SISSitems_clean[,4:27],use = "pairwise.complete.obs")
SISSsumcor <- cor(SISSitems_clean[,4:27],SISSitems_clean$sum,use = "pairwise.complete.obs")
SISSb <- psych::biserial(SISSitems_clean[,4:27],SISSitems_clean$sum)

###in each educational system
View(SISSitems_clean)
SISSctrylist <- split(SISSitems_clean,SISSitems_clean$CNTRYNUM)
SISScorlist <- list()
for (i in 1:length(SISSctrylist)) {
  SISScorlist[[i]] <- cor(SISSctrylist[[i]][,4:27],use = "pairwise.complete.obs")
  
}

SISSbis <- data.frame(matrix(nrow=24,ncol=18))
for (i in 1:length(SISSctrylist)) {
  SISSbis[,i] <- cor(SISSctrylist[[i]][,4:27],SISSctrylist[[i]][29],use = "pairwise.complete.obs")
}
#SISSbislist <- list()
# for (i in 1:length(SISSctrylist)) {
#   SISSbislist[[i]] <- cor(SISSctrylist[[i]][,4:27],SISSctrylist[[i]][29],use = "pairwise.complete.obs")
#   
# }



#save to excel
SISScnt <- as.numeric(rownames(table(SISSitems_clean$CNTRYNUM)))
SISScntnames <- names(attributes(SISSitems_clean$CNTRYNUM)$labels[attributes(SISSitems_clean$CNTRYNUM)$labels%in%SISScnt])

names3 <- list("Australia" = SISScorlist[[1]], 
               "Canada (French)"= SISScorlist[[2]],  
               "Canada (English)" = SISScorlist[[3]],           
               "UK (England)" = SISScorlist[[4]],  
               "Finland" = SISScorlist[[5]],
               "Hong Kong"   = SISScorlist[[6]],
               "Hungary"     = SISScorlist[[7]],
               "Israel"    = SISScorlist[[8]],       
               "Italy"   = SISScorlist[[9]],
               "Japan"   = SISScorlist[[10]],
               "Korea"   = SISScorlist[[11]],
               "Nigeria"  = SISScorlist[[12]],   
               "Norway"  = SISScorlist[[13]],
               "Philippines"   = SISScorlist[[14]],
               "Poland"  = SISScorlist[[15]],
               "Singapore" = SISScorlist[[16]],
               "Sweden (A,B)" = SISScorlist[[17]],
               "United States (A,B)" = SISScorlist[[18]]
               )
openxlsx::write.xlsx(names3, file = 'SISScor.xlsx')

names(SISSbis) <- SISScntnames
xlsx::write.xlsx(SISSbis,"SISSbiser.xlsx")


# names4 <- list("Australia" = SISSbislist[[1]], 
#                "Canada (French)"= SISSbislist[[2]],  
#                "Canada (English)" = SISSbislist[[3]],           
#                "UK (England)" = SISSbislist[[4]],  
#                "Finland" = SISSbislist[[5]],
#                "Hong Kong"   = SISSbislist[[6]],
#                "Hungary"     = SISSbislist[[7]],
#                "Israel"    = SISSbislist[[8]],       
#                "Italy"   = SISSbislist[[9]],
#                "Japan"   = SISSbislist[[10]],
#                "Korea"   = SISSbislist[[11]],
#                "Nigeria"  = SISSbislist[[12]],   
#                "Norway"  = SISSbislist[[13]],
#                "Philippines"   = SISSbislist[[14]],
#                "Poland"  = SISSbislist[[15]],
#                "Singapore" = SISSbislist[[16]],
#                "Sweden (A,B)" = SISSbislist[[17]],
#                "United States (A,B)" = SISSbislist[[18]]
# )
# openxlsx::write.xlsx(names4, file = 'SISSbis.xlsx')





#tetrachoric correlation matrix
#FISS
#general
FISSt <- psych::tetrachoric(FISSitems_clean[,4:43])
corrplot::corrplot(FISSt$rho)
mtext("FISS-tetrachoric-Allcountries", line=2.7, cex=1.3)

#in each education system
FISStlist <- list()
for (i in 1:length(FISSctrylist)) {
  FISStlist[[i]] <- psych::tetrachoric(FISSctrylist[[i]][,4:43])
}


for (i in 1:length(FISSctrylist)) {
  corrplot::corrplot(FISStlist[[i]]$rho)
  mtext(paste0("FISS-Tetrachoric-",FISScntnames[i]), line=2.7, cex=1.3)
}

#SISS
#general
SISSt <- psych::tetrachoric(SISSitems_clean[,4:27])
corrplot::corrplot(SISSt$rho)
mtext("SISS-tetrachoric-Allcountries", line=2.7, cex=1.3)

#in each education system
SISStlist <- list()
for (i in 1:length(SISSctrylist)) {
  SISStlist[[i]] <- psych::tetrachoric(SISSctrylist[[i]][,4:27])
}


for (i in 1:length(SISSctrylist)) {
  corrplot::corrplot(SISStlist[[i]]$rho)
  mtext(paste0("SISS-Tetrachoric-",SISScntnames[i]), line=2.7, cex=1.3)
}

#save to excel
names5 <- list("BELGIUM, FLEMISH" = FISStlist[[1]]$rho, 
               "BELGIUM, FRENCH"= FISStlist[[2]]$rho,  
               "CHILE" = FISStlist[[3]]$rho,           
               "ENGLAND-WALES" = FISStlist[[4]]$rho,  
               "FED REP GERMANY" = FISStlist[[5]]$rho,
               "FINLAND"   = FISStlist[[6]]$rho,
               "HUNGARY"     = FISStlist[[7]]$rho,
               "INDIA"    = FISStlist[[8]]$rho,       
               "IRAN"   = FISStlist[[9]]$rho,
               "ITALY"   = FISStlist[[10]]$rho,
               "JAPAN"   = FISStlist[[11]]$rho,
               "NETHERLANDS"  = FISStlist[[12]]$rho,   
               "SCOTLAND"  = FISStlist[[13]]$rho,
               "SWEDEN"   = FISStlist[[14]]$rho,
               "THAILAND"  = FISStlist[[15]]$rho,
               "USA" = FISStlist[[16]]$rho)
names6 <- list("Australia" = SISStlist[[1]]$rho, 
               "Canada (French)"= SISStlist[[2]]$rho,  
               "Canada (English)" = SISStlist[[3]]$rho,           
               "UK (England)" = SISStlist[[4]]$rho,  
               "Finland" = SISStlist[[5]]$rho,
               "Hong Kong"   = SISStlist[[6]]$rho,
               "Hungary"     = SISStlist[[7]]$rho,
               "Israel"    = SISStlist[[8]]$rho,       
               "Italy"   = SISStlist[[9]]$rho,
               "Japan"   = SISStlist[[10]]$rho,
               "Korea"   = SISStlist[[11]]$rho,
               "Nigeria"  = SISStlist[[12]]$rho,   
               "Norway"  = SISStlist[[13]]$rho,
               "Philippines"   = SISStlist[[14]]$rho,
               "Poland"  = SISStlist[[15]]$rho,
               "Singapore" = SISStlist[[16]]$rho,
               "Sweden (A,B)" = SISStlist[[17]]$rho,
               "United States (A,B)" = SISStlist[[18]]$rho
)
openxlsx::write.xlsx(names5, file = 'FISStetr.xlsx')
openxlsx::write.xlsx(names6, file = 'SISStetr.xlsx')



