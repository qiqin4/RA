# 1. About functions to calculate point biserial
# * Methods to calculate the point-biserial correlation:
#   * Ltm package: https://github.com/drizopoulos/ltm/blob/master/R/biserial.cor.R 
# * Cor.test()
# * cor() 
# 2. Create the new dataset FISS + SISS (only with groups in common)
# 3.  Calculate the proportion of correct answers in the items in common by each educational system
# 


library(haven)
#Importing data sets
dbm2001_ALL_SC_RL_ <- read_sav("dbm2001_ALL_SC_RL_.sav")
View(dbm2001_ALL_SC_RL_)
sc21alls <- read_sav("sc21alls.sav")
View(sc21alls)
FISS <- dbm2001_ALL_SC_RL_
SISS <- sc21alls

############  recode 0,6,7,8 to NA  ##############
#Scoring the FISS 40 items------------------------------------------------------
colnames(FISS)
FISSitems <- subset(FISS,select = c(cntry,sch,stud,e1sa01:e1sb20))

FISScr <- as.data.frame(c(1,2,5,2,1,4,4,2,2,4,5,3,5,1,2,4,1,3,4,4,4,3,3,3,5,4,2,3,5,2,5,1,2,3,3,3,5,3,1,1))
colnames(FISScr) <- "cr"

#recode 0,6,7,8 to NA
FISSitems[FISSitems==0] <- NA
FISSitems[FISSitems==6] <- NA
FISSitems[FISSitems==7] <- NA
FISSitems[FISSitems==8] <- NA
#recode 0/1
for (i in 1:40) {
  FISSitems[,i+3] <- ifelse(FISSitems[,i+3]==FISScr[i,],1,0)
}


#Scoring the SISS 24 items------------------------------------------------------
colnames(SISS)
SISSitems <- subset(SISS,select = c(CNTRYNUM,SCHOOL,STUDENT,P1M01:P1M24))

for (i in 1:24) {
  print(table(SISSitems[,i+3],useNA = "always"))
}


SISScr <- as.data.frame(c(1,3,3,2,1,4,2,4,3,2,4,5,2,3,2,5,3,1,2,4,1,3,1,1))
colnames(SISScr) <- "cr"

#recode 0/1
for (i in 1:24) {
  SISSitems[,i+3] <- ifelse(SISSitems[,i+3]==SISScr[i,],1,0)
}



##Delete the students with full na and/or Israel##
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
FISSsave <- FISSitems_clean[,-44]
#SISS
#Remove the cases with complete NAs
SISSitems_clean <- SISSitems
SISSitems_clean$nna <- NA

for (j in 1:nrow(SISSitems)) {
  SISSitems_clean$nna[j] <- table(is.na(SISSitems_clean[j,4:27]))["TRUE"]
}

SISSitems_clean$nna <- ifelse(is.na(SISSitems_clean$nna),0,SISSitems_clean$nna)
SISSitems_clean <- SISSitems_clean[SISSitems_clean$nna!=24,]
SISSsave <- SISSitems_clean[,-28]

# save(FISSsave, file = "FISS.RData")
# save(SISSsave, file = "SISS.RData")

######Create the new dataset FISS + SISS (only with groups in common)######
#Identify the countries in common
FISSitems_clean$new_cnt <- FISSitems_clean$cntry
FISSitems_clean$new_cnt <- haven::as_factor(FISSitems_clean$new_cnt)
table(FISSitems_clean$new_cnt)


SISSitems_clean$new_cnt <- SISSitems_clean$CNTRYNUM
SISSitems_clean$new_cnt <- haven::as_factor(SISSitems_clean$new_cnt)
table(haven::as_factor(SISSitems$CNTRYNUM))

SISSitems_clean$new_cnt2 <- dplyr::recode(SISSitems_clean$new_cnt,
                                          "UK (England)"="ENGLAND-WALES",
                                          "Finland"="FINLAND",
                                          "Hungary"="HUNGARY",
                                          "Italy"="ITALY",
                                          "Japan"="JAPAN",
                                          "Sweden (A,B)"="SWEDEN",
                                          "United States (A,B)"="USA"
)

table(FISSitems_clean$new_cnt)
table(SISSitems_clean$new_cnt2)


#Identify the items in common between FISS and SISS--------------------------------------
#SISS 1:14,16,18:24

#3. Items only in FISS | Items only in SISS | Items in common (rename the items using FISS as the reference)
#Items only in SISS: P1M15, P1M17
#Items only in FISS: "e1sa02","e1sa03","e1sa09","e1sa10",  "e1sa11","e1sa13","e1sa18", "e1sa19"  ,"e1sa20"
#"e1sb01" ,"e1sb05","e1sb11" , "e1sb12" , "e1sb13" ,"e1sb14" , "e1sb15" , "e1sb16" , "e1sb17" 

#Items in common
# "e1sa01"     "e1sa04"  "e1sa05"  "e1sa06" 
# "e1sa07"  "e1sa08"      "e1sa12"    "e1sa14"  "e1sa15" 
# "e1sa16"  "e1sa17"  
# "e1sb02"  "e1sb03"  "e1sb04" 
# "e1sb06"  "e1sb07"  "e1sb08"  "e1sb09"  "e1sb10"  
#   "e1sb18"  "e1sb19"  "e1sb20" 

SISS_NewItem <- SISSitems_clean
colnames(SISS_NewItem) <- c("CNTRYNUM", "SCHOOL", "STUDENT", "e1sa01", "e1sb02", "e1sb03", "e1sa04", "e1sa05"  , 
                            "e1sa06", "e1sb07", "e1sb06", "e1sb08", "e1sa08", "e1sa07", "e1sb09", "e1sb10"   ,
                            "e1sb04", "P1M15", "e1sa12", "P1M17", "e1sa14", "e1sa15", "e1sa16", "e1sa17"  , 
                            "e1sb18", "e1sb19", "e1sb20", "nna"  ,    "new_cnt_1", "new_cnt")

#4. Create an id for time: FISS and SISS
FISSitems_clean$TestTime <- "FISS"
SISS_NewItem$TestTime <- "SISS"


FISSnew <- FISSitems_clean[,c(4:43,45,46)]
SISSnew <- SISS_NewItem[,c(4:27,30,31)]

all <- dplyr::bind_rows(FISSnew,SISSnew)
all <- all[,c(
  "P1M15","P1M17",
  "e1sa02","e1sa03","e1sa09","e1sa10",  "e1sa11","e1sa13","e1sa18","e1sa19"  ,"e1sa20",
  "e1sb01" ,"e1sb05","e1sb11" , "e1sb12" , "e1sb13" ,"e1sb14" , "e1sb15" , "e1sb16" , "e1sb17",
  "e1sa01"  ,   "e1sa04" , "e1sa05" , "e1sa06",
  "e1sa07" , "e1sa08"  ,    "e1sa12"  ,  "e1sa14" , "e1sa15",
  "e1sa16" , "e1sa17",  
  "e1sb02" , "e1sb03" , "e1sb04",
  "e1sb06" , "e1sb07" , "e1sb08" , "e1sb09" , "e1sb10",
  "e1sb18"  ,"e1sb19" , "e1sb20",
  "new_cnt","TestTime")]


# save(all, file = "all_FISS_SISS.RData")



######Comparison of the proportion correction answers in each country that are in common (items& countries)######
#correct proportion for each students
all$p_correct_commoni <- rowMeans(all[,21:42])

#common country
#"ENGLAND-WALES"
# "FINLAND",
# "HUNGARY",
# "ITALY",
# "JAPAN",
# "SWEDEN",
# "USA"

#correct proportion for each item in each educational system for each time
item1<- aggregate(e1sa01 ~ new_cnt + TestTime, data = all[c(20+1,43:45)], mean)
item2<- aggregate(e1sa04 ~ new_cnt + TestTime, data = all[c(20+2,43:45)], mean)
item3<- aggregate(e1sa05 ~ new_cnt + TestTime, data = all[c(20+3,43:45)], mean)
item4<- aggregate(e1sa06 ~ new_cnt + TestTime, data = all[c(20+4,43:45)], mean)
item5<- aggregate(e1sa07 ~ new_cnt + TestTime, data = all[c(20+5,43:45)], mean)
item6<- aggregate(e1sa08 ~ new_cnt + TestTime, data = all[c(20+6,43:45)], mean)
item7<- aggregate(e1sa12 ~ new_cnt + TestTime, data = all[c(20+7,43:45)], mean)
item8<- aggregate(e1sa14 ~ new_cnt + TestTime, data = all[c(20+8,43:45)], mean)
item9<- aggregate(e1sa15 ~ new_cnt + TestTime, data = all[c(20+9,43:45)], mean)
item10<- aggregate(e1sa16 ~ new_cnt + TestTime, data = all[c(20+10,43:45)], mean)
item11<- aggregate(e1sa17 ~ new_cnt + TestTime, data = all[c(20+11,43:45)], mean)
item12<- aggregate(e1sb02 ~ new_cnt + TestTime, data = all[c(20+12,43:45)], mean)
item13<- aggregate(e1sb03 ~ new_cnt + TestTime, data = all[c(20+13,43:45)], mean)
item14<- aggregate(e1sb04 ~ new_cnt + TestTime, data = all[c(20+14,43:45)], mean)
item15<- aggregate(e1sb06 ~ new_cnt + TestTime, data = all[c(20+15,43:45)], mean)
item16<- aggregate(e1sb07 ~ new_cnt + TestTime, data = all[c(20+16,43:45)], mean)
item17<- aggregate(e1sb08 ~ new_cnt + TestTime, data = all[c(20+17,43:45)], mean)
item18<- aggregate(e1sb09 ~ new_cnt + TestTime, data = all[c(20+18,43:45)], mean)
item19<- aggregate(e1sb10 ~ new_cnt + TestTime, data = all[c(20+19,43:45)], mean)
item20<- aggregate(e1sb18 ~ new_cnt + TestTime, data = all[c(20+20,43:45)], mean)
item21<- aggregate(e1sb19 ~ new_cnt + TestTime, data = all[c(20+21,43:45)], mean)
item22<- aggregate(e1sb20 ~ new_cnt + TestTime, data = all[c(20+22,43:45)], mean)

#combine them to a list
itemlist <- list(item1,item2,item3,item4,item5,
                 item6,item7,item8,item9,item10,
                 item11,item12,item13,item14,item15,
                 item16,item17,item18,item19,item20,
                 item21,item22)

# filter the countries
for (i in 1:22) {
  itemlist[[i]] <- itemlist[[i]][itemlist[[i]]$new_cnt %in% c("FINLAND",
                                                              "HUNGARY",
                                                              "ITALY",
                                                              "JAPAN",
                                                              "SWEDEN",
                                                              "USA",
                                                              "ENGLAND-WALES"),]
}

#combine the results to one data frame
itemlistdf <- as.data.frame(c(itemlist[[1]][1:3],itemlist[[2]][3],itemlist[[3]][3],itemlist[[4]][3],itemlist[[5]][3],itemlist[[6]][3],itemlist[[7]][3],itemlist[[8]][3],itemlist[[9]][3],itemlist[[10]][3],
                              itemlist[[11]][3],itemlist[[12]][3],itemlist[[13]][3],itemlist[[14]][3],itemlist[[15]][3],itemlist[[16]][3],itemlist[[17]][3],itemlist[[18]][3],itemlist[[19]][3],itemlist[[20]][3],
                              itemlist[[21]][3],itemlist[[22]][3]))



#save to excel
# openxlsx::write.xlsx(itemalistdf, file = 'cr_proportion.xlsx')


