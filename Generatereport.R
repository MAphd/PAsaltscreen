
#setwd()
setwd(paste0(getwd(),"/run2"))

library(readr)

Data <- read_csv("pyphe-analyse_data_report.csv", col_types = cols(...1 = col_skip()))


#Generate mean colony sizes for LB+Salt and LB

Colsize1m <- mean(subset(Data$Colony_size,Data$Condition=="LB"),na.rm = TRUE)
Colsize2m <- mean(subset(Data$Colony_size,Data$Condition=="LB+Salt"),na.rm = TRUE)

Colsize2sdcut <- 2*sd(subset(Data$Colony_size,Data$Condition=="LB+Salt"),na.rm = TRUE)



Datafix <- Data
#fix plates where some rows or columns weren't detected and add NA rows (and columns)
for(i in 1:length(unique(Data$Plate))){
  
  Temp1 <- subset(Data,Data$Plate == unique(Data$Plate)[i])
  
  if( !identical(dim(Temp1)[1],as.integer(96))  ){
    
    #Column check
    if( any(!c(rep(1:12))%in%unique(Temp1$Column)) ){
      #index which columns are missing
      a <- which(!c(rep(1:12))%in%unique(Temp1$Column)) 
      for(j in 1:length(a)){
        #add columns
        
        tempdf <-data.frame(a[j], Row = rep(1:8), Colony_size = NA,NA,NA,unique(Temp1$Data_path),unique(Temp1$Image_path),unique(Temp1$Condition),unique(Temp1$mPlate),(unique(Temp1$Processing)),unique(Temp1$Plate))
        colnames(tempdf) <- colnames(Data)
        
        Datafix <- rbind(Datafix, tempdf)
        
      }
      
    }
    
    #Row check
    if( any(!(c(rep(1:8)))%in%unique(Temp1$Row))){
      
      #index which rows are missing
      a <- which(!(c(rep(1:8)))%in%unique(Temp1$Row))
      
      for(j in 1:length(a)){
        #add rows
        
        tempdf <-data.frame(Column = rep(1:12), Row = a[j], Colony_size = NA,NA,NA,unique(Temp1$Data_path),unique(Temp1$Image_path),unique(Temp1$Condition),unique(Temp1$mPlate),(unique(Temp1$Processing)),unique(Temp1$Plate))
        colnames(tempdf) <- colnames(Data)
        
        Datafix <- rbind(Datafix, tempdf)
        
      }
      
    }
    #cols: 
    # (rep(1:12,times=1,each= 8))
    #rows:
    # (rep(1:8,times=12,each= 1))
    
    
  }
}



Data <- Datafix
#Reorder data appropriately


Dataorder <- Data

Dataorder <- Dataorder[order(as.numeric(as.character(Dataorder$Row))) ,]
Dataorder <- Dataorder[order(as.numeric(as.character(Dataorder$Column))),]
Dataorder <- Dataorder[order(as.numeric(unlist(strsplit((Dataorder$mPlate),"_"))[c(FALSE,TRUE)])),]


Data <- Dataorder


Data$Well <- paste0(factor(Data$Row, levels = seq(1,8,1),labels = c("A","B","C","D","E","F","G","H")),Data$Column)

Data <- Data[-which(duplicated(Data)),]


t1 <- c("p1_EXP","p5_EXP","p13_EXP","p15_EXP","p20_EXP","p23_CTRL","p23_CTRL","p23_EXP","p24_EXP","p26_EXP",
        "p27_EXP","p30_EXP","p37_CTRL","p44_EXP","p46_EXP","p47_EXP")

t2 <- c("A11","E3","A11","A11","A11","A9","A10","E3","D12","E3","E3","H7","H1","E3","E3","A11")
#Set these colony size to NA

#Colonies misidentified
Tempfix <- data.frame(Plate = t1, Well = t2)[c(1,2,3,4,5,6,7,8,10,11,12,14,15,16),]

for(i in 1:dim(Tempfix)[1]){
  #Set colony parameters to NA
  Data[Data$Plate==Tempfix$Plate[i] & Data$Well == Tempfix$Well[i],c(2,3,4)] <- NA
  
  
  
}

#Colonies missed
Tempfix2 <- data.frame(Plate = t1, Well = t2)[c(13),]


for(i in 1:dim(Tempfix2)[1]){
  #Set colony size to mean colony size
  Data[Data$Plate==Tempfix2$Plate[i] & Data$Well == Tempfix2$Well[i],3] <- Colsize1m
  
  
  
}


# factor(Data$Row, levels = seq(1,8,1),labels = c("A","B","C","D","E","F","G","H"),)
#plate by plate comparisons:
Report <- data.frame()
for(i in 1:length(unique(Data$mPlate))){
  
  Temp1 <- subset(Data,Data$mPlate == unique(Data$mPlate)[i])
  # hey1 <- subset(Temp1, Temp1$Condition == "LB")
  # hey2 <- subset(Temp1, Temp1$Condition == "LB+Salt")
  #Get NA index
  A1na <- is.na(Temp1$Colony_size[Temp1$Condition=="LB"])
  A2na <- is.na(Temp1$Colony_size[Temp1$Condition=="LB+Salt"])
  # print(paste("A1:",length(A1na),". A2:", length(A2na) ) )
  
  if( !identical(length(A1na),length(A2na)) ){
    #Ensure all rows (A-H) and columns (1-12) are present even if they were not detected, and add these. 
    stop("hey")
  }
  
  Diffwells <- NA
  Nogrowth <- NA
  
  #Growth/nogrowth check
  if(any(!(A1na==A2na)) ){
    #Index of differentially growing colonies
    #wells
    Diffwells <- Temp1$Well[Temp1$Condition=="LB"][which(!(A1na==A2na))]
    
    
    
    
    Nogrowth <- c()
    for(j in 1:length(Diffwells)){
      #Determine type of difference: is a colony growing on LB and not LB+Salt, or the opposite?
      B <- which(is.na(Temp1$Colony_size[Temp1$Well == Diffwells[j]]))
      
      #Salt = 1, LB = 0
      Nogrowth[j] <- identical(as.character(Temp1[Temp1$Well == Diffwells[j],][B,8]),"LB+Salt")*1
      
      
      
    }
    
  }
  
  Bigcol <- NA
  Smallcol <- NA
  #Colony size check
  #Do any colonies differ in size from mean of all LB+Salt colonies by more than +/- 2*sd?
  if(    any(Temp1$Colony_size[Temp1$Condition=="LB+Salt"][!A2na]>Colsize2m+Colsize2sdcut) | any(Temp1$Colony_size[Temp1$Condition=="LB+Salt"][!A2na]<Colsize2m-Colsize2sdcut) ){
    if( any(Temp1$Colony_size[Temp1$Condition=="LB+Salt"][!A2na]>Colsize2m+Colsize2sdcut) ){
      # stop("col size bigger")
      B1 <- which(Temp1$Colony_size[Temp1$Condition=="LB+Salt"]>Colsize2m+Colsize2sdcut)
      Bigcol <- Temp1$Well[Temp1$Condition=="LB+Salt"][c(B1)]
    }
    
    if( any(Temp1$Colony_size[Temp1$Condition=="LB+Salt"][!A2na]<Colsize2m-Colsize2sdcut) ){
      
      B2 <- which(Temp1$Colony_size[Temp1$Condition=="LB+Salt"]<Colsize2m-Colsize2sdcut)
      Smallcol <- Temp1$Well[Temp1$Condition=="LB+Salt"][c(B2)]
    }
    
    
  }

  #If nothing is interesting about the plate, skip it
  if( any(is.na(Diffwells)) & any(is.na(Bigcol)) & any(is.na(Smallcol)) ){
    next
  }
  #Write report
  Report <- rbind(Report, c(unique(Data$mPlate)[i], paste(Diffwells,collapse = " "), paste(Nogrowth,collapse = " "), paste(Bigcol,collapse = " "), paste(Smallcol,collapse = " ") ) )
  

  
}
colnames(Report) <- c("Plate","Diffwells","Nogrowth","Bigcol","Smallcol")
# 
# (unlist(strsplit(paste(Report$Diffwells[-c(which(Report$Diffwells == "NA"))], collapse = " ")," ")))
# 
# as.logical(as.numeric((unlist(strsplit(paste(Report$Nogrowth[-c(which(Report$Nogrowth == "NA"))], collapse = " ")," ")))))




AS1 <- (unlist(strsplit(paste(Report$Diffwells[-c(which(Report$Diffwells == "NA"))], collapse = " ")," ")))[as.logical(as.numeric((unlist(strsplit(paste(Report$Nogrowth[-c(which(Report$Nogrowth == "NA"))], collapse = " ")," ")))))]
AS2 <- (unlist(strsplit(paste(Report$Diffwells[-c(which(Report$Diffwells == "NA"))], collapse = " ")," ")))[!as.logical(as.numeric((unlist(strsplit(paste(Report$Nogrowth[-c(which(Report$Nogrowth == "NA"))], collapse = " ")," ")))))]


#Statistics 
#outcome 1 and 2 colonies
length(unlist(strsplit(paste(Report$Diffwells[-c(which(Report$Diffwells == "NA"))], collapse = " ")," ")))
#small colonies
length(unlist(strsplit(paste(Report$Smallcol[-c(which(Report$Smallcol == "NA"))], collapse = " ")," ")))


#Total number of colonies growing on LBA
length(subset(Data$Colony_size,Data$Condition=="LB")[!is.na(subset(Data$Colony_size,Data$Condition=="LB"))])

#Total number of colonies growing on LBA+Salt
length(subset(Data$Colony_size,Data$Condition=="LB+Salt")[!is.na(subset(Data$Colony_size,Data$Condition=="LB+Salt"))])




#Generate reports
repo1 <- data.frame(Plate = Report$Plate[-c(which(Report$Diffwells == "NA"))]
           , Well = Report$Diffwells[-c(which(Report$Diffwells == "NA"))], Nogrowth = Report$Nogrowth[-c(which(Report$Nogrowth == "NA"))])

#Outcome1 table
repo2 <- data.frame()
for(i in 1:dim(repo1)[1]){
  
  if( any(strsplit(repo1$Nogrowth[i]," ")[[1]]=="0") ){
    if( all(strsplit(repo1$Nogrowth[i]," ")[[1]]=="0") ) {
      next
    } 
    Well2 <- paste(strsplit(repo1$Well[i]," ")[[1]][(as.logical(as.numeric((strsplit(repo1$Nogrowth[i]," ")[[1]]))))],collapse = " ")
    
  } else {
    Well2 <- repo1$Well[i]
  }
  
  repo2 <- rbind(repo2, c(repo1$Plate[i],Well2 ))
  
}
colnames(repo2) <- c("Plate","Wells")

#Outcome2 table
repo3 <- data.frame()
for(i in 1:dim(repo1)[1]){
  
  if( any(strsplit(repo1$Nogrowth[i]," ")[[1]]=="0") ){
    
    Well2 <- paste(strsplit(repo1$Well[i]," ")[[1]][!(as.logical(as.numeric((strsplit(repo1$Nogrowth[i]," ")[[1]]))))],collapse = " ")
    
  } else {
    next
  }
  
  repo3 <- rbind(repo3, c(repo1$Plate[i],Well2 ))
  
}
colnames(repo3) <- c("Plate","Wells")



write_tsv(repo2,"Reportdiffcoloutcome1.txt")

write_tsv(repo3,"Reportdiffcoloutcome2.txt")

write_tsv(Report[-which(Report$Smallcol=="NA"),c(1,5)],"Reportsmallcol.txt")
