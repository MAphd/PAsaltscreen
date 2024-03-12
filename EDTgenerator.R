
#Generate plate condition metadata file for pyphe

getwd()


# Imglist <- list.files("./images")


#Column 1

EDTdf <- data.frame()
#name, data_path, image_path, condition, layout, processing date
# index of bad plates (runny or otherwise unreadable)
badI <- c(16,22,28,29,45,50,51)

#30 partial? 
for( i in 1:52){
  
  
  if( any(badI %in% i) ){
    next
  }
  
  if( i<10  ){
    #add trailing 0s
    EDTdf <- rbind(EDTdf, c(paste0("p",i,"_CTRL" ),paste0("pyphe_quant/NewProject-Run-2-Plate-00",i,"-Ctrl.jpg.csv"), paste0("images/NewProject-Run-2-Plate-00",i,"-Ctrl.jpg"), "LB",paste0("p_",i ) ) )
    EDTdf <- rbind(EDTdf, c(paste0("p",i,"_EXP" ),paste0("pyphe_quant/NewProject-Run-2-Plate-00",i,"-Exp.jpg.csv"), paste0("images/NewProject-Run-2-Plate-00",i,"-Exp.jpg"), "LB+Salt",paste0("p_",i )  ) )

  } else {

    EDTdf <- rbind(EDTdf, c(paste0("p",i,"_CTRL" ), paste0("pyphe_quant/NewProject-Run-2-Plate-0",i,"-Ctrl.jpg.csv"), paste0("images/NewProject-Run-2-Plate-0",i,"-Ctrl.jpg"), "LB",paste0("p_",i ) ) )
    EDTdf <- rbind(EDTdf, c(paste0("p",i,"_EXP" ), paste0("pyphe_quant/NewProject-Run-2-Plate-0",i,"-Exp.jpg.csv"), paste0("images/NewProject-Run-2-Plate-0",i,"-Exp.jpg"), "LB+Salt",paste0("p_",i )  ) )

  }
  
  
  # paste0("p",i, c("CTRL","EXP")[((i+1)%%2)+1])
}
EDTdf$processing <- Sys.time()

colnames(EDTdf) <- c("","Data_path","Image_path","Condition","mPlate","Processing")


write.csv(EDTdf,"EDT.csv", quote = FALSE, row.names = FALSE)
