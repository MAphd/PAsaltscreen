## test <- as.raster(image_read(Imgs[plate]))
## dim(test)[1] = 473
## dim(test)[2] = 725


xcoord1 <- function(x,y){
  
  as.integer(5+x*50+x*5.5-50+(x^2)/2-(x^2/10)-(0.5/x)*8  )
  
}
xcoord2 <- function(x,y){
  
  as.integer(55+x*50+x*5.5-50+(x^2)/2-(x^2/10)-(0.5/x)*8 )
  
  
}
xcoord3 <- function(x,y){
  if(xcoord2(x,y) > 725){
    725
  } else {
    xcoord2(x,y)
  }
  
}
xcoordf <- function(x,y){
  
  if( identical(length(xcoord1(x,y):xcoord3(x,y)),as.integer(51)) ){
    xcoord1(x,y):xcoord3(x,y)
  } else {
    
    ((xcoord3(x,y)-50):xcoord3(x,y))
  }
  
}


ycoord1 <- function(x,y){
  as.integer( y*51-50+3+(y)^2.1 ) 
  
}
ycoord2 <- function(x,y){
  as.integer( y*51+3+(y)^2.1 ) 
  
}
ycoord3 <- function(x,y){
  if( ycoord2(x,y) > 473 ){
    473
  } else {
    ycoord2(x,y)
  }
  
}
ycoordf <- function(x,y){
  
  if( identical(length(ycoord1(x,y):ycoord3(x,y)),as.integer(51)) ){
    ycoord1(x,y):ycoord3(x,y)
  } else {
    
    ((ycoord3(x,y)-50):ycoord3(x,y))
  }
  
}

Colextract <- function(well,plate,condition){
  
  #shitty workaround time
  
  plate <- plate+(plate-1)
  
  
  if(condition == "Salt" ){
    #shitty workaround, we could check filenames for "Exp" or "Ctrl", but this is easier.
    plate <- plate + 1 
    
  }
  # test <- as.raster(image_read(Imgs[plate]))
  
  #Convert well to x and y coordinates
  
  #Well should be like examples "A1" or "H12", etc - and only one well at a time!
  
  hey <- unlist(strsplit(well, ""))
  if( any((c("A","B","C","D","E","F","G","H")%in%hey[1])) ){
    if( length(which((c("A","B","C","D","E","F","G","H")%in%hey[1])))>1 ){
      stop("Well must be one letter (A-H) followed by a number (1-12)")
    }
    
    y <- which((c("A","B","C","D","E","F","G","H")%in%hey[1]))
  } else {
    
    stop("Well must be one letter (A-H) followed by a number (1-12)")
  }
  #regex doesnt understand H is a letter so we cant use [:alpha:] for whatever reason
  hey <- unlist(strsplit(well, "(?=[A-Z])",perl = TRUE))
  if( any((c(seq(1,12,1))%in%hey[2])) ){
    if( length( which((c(seq(1,12,1))%in%hey[2])) )>1 ){
      stop("Well must be one letter (A-H) followed by a number (1-12)")
    }
    
    x <-  which((c(seq(1,12,1))%in%hey[2])) 
  } else {
    
    stop("Well must be one letter (A-H) followed by a number (1-12)")
  }
  
  
  (as.raster(image_read(Imgs[plate])))[ycoordf(x,y),xcoordf(x,y)]
  
}
