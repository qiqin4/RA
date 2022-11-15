library(haven)
#Importing data sets
dbm2001_ALL_SC_RL_ <- read_sav("dbm2001_ALL_SC_RL_.sav")
View(dbm2001_ALL_SC_RL_)
sc21alls <- read_sav("sc21alls.sav")
View(sc21alls)
FISS <- dbm2001_ALL_SC_RL_
SISS <- sc21alls

############  Bullet point 1 & 2  ##############
#Scoring the FISS 40 items------------------------------------------------------ 
colnames(FISS)
FISSitems <- subset(FISS,select = c(cntry,sch,stud,e1sa01:e1sb20))

FISScr <- as.data.frame(c(1,2,5,2,1,4,4,2,2,4,5,3,5,1,2,4,1,3,4,4,4,3,3,3,5,4,2,3,5,2,5,1,2,3,3,3,5,3,1,1))
colnames(FISScr) <- "cr"


for (i in 1:40) {
    incr <- c(1,2,3,4,5)[!(c(1,2,3,4,5)%in%FISScr[i,])]
    FISSitems[,i+3] <- ifelse(FISSitems[,i+3]!=FISScr[i,],ifelse(FISSitems[,i+3]==incr[1]|incr[2]|incr[3]|incr[4],0,NA),1)
}

#Comparing them with the coding from the database-------------------------------
FISScoding <- subset(FISS,select = c(cntry,sch,stud,DE1SA01:DE1SB20))
FISScoding <- as.data.frame(FISScoding)
for (i in 1:40) {
  FISScoding[,i+3] <- as.numeric(FISScoding[,i+3])
}
table(FISSitems$e1sa01,useNA = "always")
table(FISScoding$DE1SA01,useNA = "always")

all.equal(FISSitems[,4:43],FISScoding[4:43]) #inconsistencies in numbers of NA

#Scoring the SISS 24 items------------------------------------------------------
colnames(SISS)
SISSitems <- subset(SISS,select = c(CNTRYNUM,SCHOOL,STUDENT,P1M01:P1M24))
table(SISSitems$P1M01,useNA = "always")

SISScr <- as.data.frame(c(1,3,3,2,1,4,2,4,3,2,4,5,2,3,2,5,3,1,2,4,1,3,1,1))
colnames(SISScr) <- "cr"

for (i in 1:24) {
  incr <- c(1,2,3,4,5)[!(c(1,2,3,4,5)%in%SISScr[i,])]
  SISSitems[,i+3] <- ifelse(SISSitems[,i+3]!=SISScr[i,],ifelse(FISSitems[,i+3]==incr[1]|incr[2]|incr[3]|incr[4],0,NA),1)
}


############  Bullet point 3  ##############
##Calculate the amount of NA
##proportion of correct answers
#at item level 
#at student level 
#at country level 

#FISS---------------------------------------------------------------------------

FISSlist <- list(NAbyItem = data.frame(Item = 1:40,               
                                        Count = rep(NA,40)),
                 NAbyStud = data.frame(Student = 1:nrow(FISSitems),
                                        Count = rep(NA, nrow(FISSitems))),
                 NAbycnt = data.frame(matrix(NA, ncol = 41, nrow = length(as.numeric(rownames(table(FISSitems$cntry))))))
                 )
#NA by item                
for (i in 1:40) {
  FISSlist$NAbyItem$Count[i] <- table(is.na(FISSitems[,i+3]))["TRUE"]
}

#NA by student
for (j in 1:nrow(FISSitems)) {
  FISSlist$NAbyStud$Count[j] <- table(is.na(FISSitems[j,4:ncol(FISSitems)]))["TRUE"]
}

FISSlist$NAbyItem$Count <- ifelse(is.na(FISSlist$NAbyItem$Count),0,FISSlist$NAbyItem$Count)
FISSlist$NAbyStud$Count <- ifelse(is.na(FISSlist$NAbyStud$Count),0,FISSlist$NAbyStud$Count)

#NA within educational systems---------------------------------------------------------------------
colnames(FISSlist$NAbycnt) <- c("Country",colnames(FISSitems[-c(1:3)]))
FISScnt <- as.numeric(rownames(table(FISSitems$cntry)))
FISScntlist <- list()
FISScntnames <- names(attributes(FISSitems$cntry)$labels[attributes(FISSitems$cntry)$labels%in%FISScnt])
FISSlist$NAbycnt$Country <- FISScntnames
names(FISScntlist) <- FISScntnames
for (i in 1:nrow(table(FISSitems$cntry))) {
  FISScntlist[[i]] <- as.data.frame(FISSitems[FISSitems$cntry==FISScnt[i],])
}

#by item
  for (i in 1:40) {
    for (j in 1:nrow(table(FISSitems$cntry))) {
    FISSlist$NAbycnt[j,i+1] <- table(is.na(FISScntlist[[j]][,i+3]))["TRUE"]
  }
  
  }
#in total
FISSlist$NAbycnt$Sum <- rowSums(FISSlist$NAbycnt[,-1])



#SISS---------------------------------------------------------------------------

SISSlist <- list(NAbyItem = data.frame(Item = 1:24,               
                                       Count = rep(NA,24)),
                 NAbyStud = data.frame(Student = 1:nrow(SISSitems),
                                       Count = rep(NA, nrow(SISSitems))),
                 NAbycnt = data.frame(matrix(NA, ncol = 25, nrow = length(as.numeric(rownames(table(SISSitems$CNTRYNUM))))))
                 )
#NA by item                
for (i in 1:24) {
  SISSlist$NAbyItem$Count[i] <- table(is.na(SISSitems[,i+3]))["TRUE"]
}

#NA by student
for (j in 1:nrow(SISSitems)) {
  SISSlist$NAbyStud$Count[j] <- table(is.na(SISSitems[j,4:ncol(SISSitems)]))["TRUE"]
}

SISSlist$NAbyItem$Count <- ifelse(is.na(SISSlist$NAbyItem$Count),0,SISSlist$NAbyItem$Count)
SISSlist$NAbyStud$Count <- ifelse(is.na(SISSlist$NAbyStud$Count),0,SISSlist$NAbyStud$Count)

#NA within educational systems
colnames(SISSlist$NAbycnt) <- c("Country",colnames(SISSitems[-c(1:3)]))
SISScnt <- as.numeric(rownames(table(SISSitems$CNTRYNUM)))
SISScntlist <- list()
SISScntnames <- names(attributes(SISSitems$CNTRYNUM)$labels[attributes(SISSitems$CNTRYNUM)$labels%in%SISScnt])
SISSlist$NAbycnt$Country <- SISScntnames
for (i in 1:nrow(table(SISSitems$CNTRYNUM))) {
  SISScntlist[[i]] <- as.data.frame(SISSitems[SISSitems$CNTRYNUM==SISScnt[i],])
}
names(SISScntlist) <- SISScntnames

#by item
for (i in 1:24) {
  for (j in 1:nrow(table(SISSitems$CNTRYNUM))) {
    SISSlist$NAbycnt[j,i+1] <- table(is.na(SISScntlist[[j]][,i+3]))["TRUE"]
  }
  
}
#in total
#for (i in 2:25) {
#  SISSlist$NAbycnt[,i] <- ifelse(is.na(SISSlist$NAbycnt[,i]),0,SISSlist$NAbycnt[,i])
#}
SISSlist$NAbycnt$Sum <- rowSums(SISSlist$NAbycnt[,-1],na.rm = T)

#######################Proportion of correct answers#############################
#FISS---------------------------------------------------------------------------
#by item
FISSlist$CRbyItem <- FISSlist$NAbyItem
for (i in 1:40) {
  FISSlist$CRbyItem[i,2] <- table(FISSitems[,i+3]==1,useNA = "always")["TRUE"]/nrow(FISSitems)

}

#by student
FISSlist$CRbyStud <- FISSlist$NAbyStud
for (i in 1:nrow(FISSitems)) {
  FISSlist$CRbyStud[i,2] <- table(FISSitems[i,4:43]==1,useNA = "always")["TRUE"]/40
  
}

#by country
for (j in 1:17) {
  for (i in 1:40) {
    FISScntlist[[j]][i+3] <- ifelse(is.na(FISScntlist[[j]][i+3]),0,unlist(FISScntlist[[j]][i+3]))
  }
}

FISSlist$CRbycnt <- FISSlist$NAbycnt
for (j in 1:17) {
  FISSlist$CRbycnt[j,2:41] <- t(colMeans(FISScntlist[[j]][-c(1:3)]))
}

FISSlist$CRbycnt <- FISSlist$CRbycnt[,-42]

#SISS---------------------------------------------------------------------------
SISSlist$CRbyItem <- SISSlist$NAbyItem
for (i in 1:24) {
  SISSlist$CRbyItem[i,2] <- table(SISSitems[,i+3]==1,useNA = "always")["TRUE"]/nrow(SISSitems)
  
}

#by student
SISSlist$CRbyStud <- SISSlist$NAbyStud
for (i in 1:nrow(SISSitems)) {
  SISSlist$CRbyStud[i,2] <- table(SISSitems[i,4:27]==1,useNA = "always")["TRUE"]/24
  
}

#by country
for (j in 1:18) {
  for (i in 1:24) {
    SISScntlist[[j]][i+3] <- ifelse(is.na(SISScntlist[[j]][i+3]),0,unlist(SISScntlist[[j]][i+3]))
  }
}

SISSlist$CRbycnt <- SISSlist$NAbycnt
for (j in 1:18) {
  SISSlist$CRbycnt[j,2:25] <- t(colMeans(SISScntlist[[j]][-c(1:3)]))
}
SISSlist$CRbycnt <- SISSlist$CRbycnt[,-26]



