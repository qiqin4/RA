list_qi<- split(FISSitemsNIS,FISSitemsNIS$cntry)
list_original<- split(FISScodingNIS,FISScodingNIS$cntry)

# first group
for(i in 1:16){
  print(i)
  print(all.equal(list_qi[[i]][,42],list_original[[i]][,42])) #inconsistencies in numbers of NA
}

length(list_qi)
length(list_original)

all.equal(list_qi[[1]][,43],list_original[[1]][,43]) #inconsistencies in numbers of NA

head(list[[1]][,43])


for (i in 1:24) {
  print(table(SISSitems[,i+3],useNA = "always"))
  
}

FISSitems[which(FISSitems[,43]==0),]


table(FISSitems$cntry,useNA = "always")











