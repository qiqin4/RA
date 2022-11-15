load("~/Desktop/RA-2022Oct2023Mar/FISS_SISS/SISS.RData")
load("~/Desktop/RA-2022Oct2023Mar/FISS_SISS/FISS.RData")
load("~/Desktop/RA-2022Oct2023Mar/FISS_SISS/all_FISS_SISS.RData")


# 1. Comparing the functions to calculate point biserial
# * Methods to calculate the point-biserial correlation:
#   * ltm::biserial.cor() 
#   * Cor.test()
#   * cor() 

# FISS
ltm::biserial.cor() 

####FISS####
###in each educational system
FISSscores <- FISSsave
FISSscores$sumscore <- rowSums(FISSscores[4:43])
FISSctrylist <- split(FISSscores,FISSscores$cntry)
 
#cor()
FISSbis1 <- data.frame(matrix(nrow=40,ncol=16))
colnames(FISSbis1) <- names(attributes(FISSscores$cntry)$labels[attributes(FISSscores$cntry)$labels%in%names(FISSctrylist)])
for (i in 1:length(FISSctrylist)) {
  FISSbis1[,i] <- cor(FISSctrylist[[i]][,4:43],FISSctrylist[[i]][44],use = "complete.obs")
}

#cor.test()
FISSbis2 <- data.frame(matrix(nrow=40,ncol=16))
colnames(FISSbis2) <- names(attributes(FISSscores$cntry)$labels[attributes(FISSscores$cntry)$labels%in%names(FISSctrylist)])

for (i in 1:length(FISSctrylist)) {
  for (j in 1:40) {
    FISSbis2[j,i] <- cor.test(unlist(FISSctrylist[[i]][,j+3]),as.numeric(unlist(FISSctrylist[[i]][44])),use = "complete.obs")["estimate"]
    
  }
}

#ltm package
FISSbis3 <- data.frame(matrix(nrow=40,ncol=16))
colnames(FISSbis3) <- names(attributes(FISSscores$cntry)$labels[attributes(FISSscores$cntry)$labels%in%names(FISSctrylist)])

for (i in 1:length(FISSctrylist)) {
  for (j in 1:40) {
    FISSbis3[j,i] <- ltm::biserial.cor(as.numeric(unlist(FISSctrylist[[i]][44])),unlist(FISSctrylist[[i]][,j+3]),use = "complete.obs",level=2)
    
  }
}

####SISS####
###in each educational system
SISSscores <- SISSsave
SISSscores$sumscore <- rowSums(SISSscores[4:27])
SISSctrylist <- split(SISSscores,SISSscores$CNTRYNUM)

#cor()
SISSbis1 <- data.frame(matrix(nrow=24,ncol=18))
colnames(SISSbis1) <- names(attributes(SISSscores$CNTRYNUM)$labels[attributes(SISSscores$CNTRYNUM)$labels%in%names(SISSctrylist)])
for (i in 1:length(SISSctrylist)) {
  SISSbis1[,i] <- cor(SISSctrylist[[i]][,4:27],SISSctrylist[[i]][28],use = "complete.obs")
}

#cor.test()
SISSbis2 <- data.frame(matrix(nrow=24,ncol=18))
colnames(SISSbis2) <- names(attributes(SISSscores$CNTRYNUM)$labels[attributes(SISSscores$CNTRYNUM)$labels%in%names(SISSctrylist)])

for (i in 1:length(SISSctrylist)) {
  for (j in 1:24) {
    SISSbis2[j,i] <- cor.test(unlist(SISSctrylist[[i]][,j+3]),as.numeric(unlist(SISSctrylist[[i]][28])),use = "complete.obs")["estimate"]
    
  }
}

#ltm package
SISSbis3 <- data.frame(matrix(nrow=24,ncol=18))
colnames(SISSbis3) <- names(attributes(SISSscores$CNTRYNUM)$labels[attributes(SISSscores$CNTRYNUM)$labels%in%names(SISSctrylist)])

for (i in 1:length(SISSctrylist)) {
  for (j in 1:24) {
    SISSbis3[j,i] <- ltm::biserial.cor(as.numeric(unlist(SISSctrylist[[i]][28])),unlist(SISSctrylist[[i]][,j+3]),use = "complete.obs",level=2)
    
  }
}
#save to excel
FISSbis3$Item <- colnames(FISSsave)[-c(1:3)]
SISSbis3$Item <- colnames(SISSsave)[-c(1:3)]

# openxlsx::write.xlsx(FISSbis3, file = 'FISSbis.xlsx')
# openxlsx::write.xlsx(SISSbis3, file = 'SISSbis.xlsx')


#how many of the items are flagged
#FISS
FISSflags <- data.frame(matrix(nrow=16,ncol=1))
FISSflags$cntry <- names(attributes(FISSscores$cntry)$labels[attributes(FISSscores$cntry)$labels%in%names(FISSctrylist)])
colnames(FISSflags)[1] <- "n_flag"
for (i in 1:(ncol(FISSbis3)-1)) {
  FISSflags[i,1] <- sum(FISSbis3[,i]<0.3)
}
FISSflags$proportion <- FISSflags$n_flag/40
FISSflags <- FISSflags[,c(2,1,3)]

#SISS
SISSflags <- data.frame(matrix(nrow=18,ncol=1))
SISSflags$cntry <- names(attributes(SISSscores$CNTRYNUM)$labels[attributes(SISSscores$CNTRYNUM)$labels%in%names(SISSctrylist)])
SISSflags$cntry <- dplyr::recode(SISSflags$cntry,
                                          "UK (England)"="ENGLAND-WALES",
                                          "Finland"="FINLAND",
                                          "Hungary"="HUNGARY",
                                          "Italy"="ITALY",
                                          "Japan"="JAPAN",
                                          "Sweden (A,B)"="SWEDEN",
                                          "United States (A,B)"="USA"
)
colnames(SISSflags)[1] <- "n_flag"
for (i in 1:(ncol(SISSbis3)-1)) {
  SISSflags[i,1] <- sum(SISSbis3[,i]<0.3)
}
SISSflags$proportion <- SISSflags$n_flag/24
SISSflags <- SISSflags[,c(2,1,3)]
#mergeing the data sets
flags_all <- dplyr::full_join(FISSflags,SISSflags,by="cntry")
# openxlsx::write.xlsx(flags_all, file = 'flags_all.xlsx')


#Flag using the common items
#FISS
commonitems <- c("e1sa01", "e1sb02", "e1sb03", "e1sa04", "e1sa05"  , 
                 "e1sa06", "e1sb07", "e1sb06", "e1sb08", "e1sa08", "e1sa07", "e1sb09", "e1sb10"   ,
                 "e1sb04", "e1sa12", "e1sa14", "e1sa15", "e1sa16", "e1sa17"  , 
                 "e1sb18", "e1sb19", "e1sb20")

FISSbis_common <- FISSbis3[FISSbis3$Item%in%commonitems,]
FISSflags_common <- data.frame(matrix(nrow=16,ncol=1))
FISSflags_common$cntry <- names(attributes(FISSscores$cntry)$labels[attributes(FISSscores$cntry)$labels%in%names(FISSctrylist)])
colnames(FISSflags_common)[1] <- "n_flag"
for (i in 1:(ncol(FISSbis_common)-1)) {
  FISSflags_common[i,1] <- sum(FISSbis_common[,i]<0.3)
}
FISSflags_common$proportion <- FISSflags_common$n_flag/22
FISSflags_common <- FISSflags_common[,c(2,1,3)]

#SISS
SISSbis_common <- SISSbis3[!SISSbis3$Item==c("P1M15"),]
SISSbis_common <- SISSbis_common[!SISSbis_common$Item==c("P1M17"),]
SISSbis_common$Item <- commonitems

SISSflags_common <- data.frame(matrix(nrow=18,ncol=1))
SISSflags_common$cntry <- names(attributes(SISSscores$CNTRYNUM)$labels[attributes(SISSscores$CNTRYNUM)$labels%in%names(SISSctrylist)])
SISSflags_common$cntry <- dplyr::recode(SISSflags$cntry,
                                 "UK (England)"="ENGLAND-WALES",
                                 "Finland"="FINLAND",
                                 "Hungary"="HUNGARY",
                                 "Italy"="ITALY",
                                 "Japan"="JAPAN",
                                 "Sweden (A,B)"="SWEDEN",
                                 "United States (A,B)"="USA"
)
colnames(SISSflags_common)[1] <- "n_flag"
colnames(SISSbis_common)[1:18] <- SISSflags_common$cntry
for (i in 1:(ncol(SISSbis_common)-1)) {
  SISSflags_common[i,1] <- sum(SISSbis_common[,i]<0.3)
}
SISSflags_common$proportion <- SISSflags_common$n_flag/22
SISSflags_common <- SISSflags_common[,c(2,1,3)]

#mering the data sets
#mergeing the data sets
flags_all_common <- dplyr::full_join(FISSflags_common,SISSflags_common,by="cntry")
# openxlsx::write.xlsx(flags_all_common, file = 'flags_all_common.xlsx')
# openxlsx::write.xlsx(FISSbis_common, file = 'FISSbis_common.xlsx')
bis_common <- dplyr::full_join(FISSbis_common,SISSbis_common, by = "Item")
#openxlsx::write.xlsx(bis_common, file = 'bis_common.xlsx')

#counting the number of <0.3 for each item
bis_common_count <- data.frame(matrix(nrow=22,ncol=1))
for (i in 1:nrow(bis_common)) {
  bis_common_count[i,1] <- sum(bis_common[i,-17]<0.3)
}

bis_common_count$Item <- bis_common$Item
colnames(bis_common_count)[1] <- "n.flag"
bis_common_count <- bis_common_count[,c(2,1)]
bis_common_count$proportion <- bis_common_count$n.flag/34
# openxlsx::write.xlsx(bis_common_count, file = 'bis_common_peritem.xlsx')








