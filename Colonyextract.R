

#image dir

library(magick)
library(readr)
#set right working directory
Usedir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(Usedir)

Imgs <- paste0("images/",list.files("images/"))

#Load outcome1 colonies report
Data <- suppressMessages(read_delim("Reportdiffcoloutcome1.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)) 


#Initialize functions
source(paste0(Usedir,"/Saltyfunctions.R"))


# test <- c()
#condition can be Salt or anything, we only check if the string "Salt" is there for selecting plates

# example: this line plots the colony in H12 for plate 13, which is LBA
# plot(Colextract("H12",13,"sadasde"))

# example 2: this line plots the colony in A12 for plate 2, which is LBA+Salt
# plot(Colextract("A12",2,"Salt"))


########
# Reportd <- c()
# for(i in 1:dim(Data)[1] ){
# 
#   Reportd <- c(Reportd, paste0(gsub("_","",Data$Plate)[i],"_", strsplit(Data$Wells," ")[[i]]))
# 
# }
#rough estimate of array dimensions for plotting
# ceiling( sqrt( length(Reportd) ))*floor( sqrt(length(Reportd)) )
#
#plots all colonies from outcome1 colonies, no labels.
#
# par(mfrow = c(ceiling( sqrt( length(Reportd) )),floor( sqrt( length(Reportd) ))),mar = c(0.1,0.1,0.1,0.1))
# 
# for(i in 1:length(Reportd)){
#   
#   temp <- strsplit(Reportd[i],"_")[[1]]
#   
#   plot(Colextract(temp[2],as.integer(strsplit(temp[1],"[:alpha:]")[[1]][2]),"sadasde"))
#   
#   
# }
#############


#For each plate type deal

dir.exists("Colonyextract")


#This code plots and saves all colonies from the outcome1 report in separate PNGs within the working directory. 
#Labels are not always pretty but it works
for(i in 1:dim(Data)[1]){
  
  temp1 <-paste0(gsub("_","",Data$Plate)[i],"_", strsplit(Data$Wells," ")[[i]])
  
  
  
  if( length(temp1) > 2){
    png(paste0("Colonyextract/",Data$Plate[i],"colonies.png"), width=118*ceiling(sqrt(length(temp1))), height = 118*ceiling(sqrt(length(temp1))), units = "px" )
    par(mfrow = c(ceiling(sqrt(length(temp1))),ceiling(sqrt(length(temp1)))),mar = c(1,0.1,2,0.1))
  } else {
    png(paste0("Colonyextract/",Data$Plate[i],"colonies.png"), width=(118*length(temp1)), height = (118*length(temp1)) )
    par(mfrow = c(1,length(temp1)),mar = c(1,0.1,1,0.1))
    
  }
  
  for(j in 1:length(temp1)){
    
    temp <- strsplit(temp1,"_")[[j]]
    
    plot(Colextract(temp[2],as.integer(strsplit(temp[1],"[:alpha:]")[[1]][2]),"sadasde"))

    mtext(temp1[[j]],line= 0)
  }
  dev.off()
  
  
}

# dev.off()
