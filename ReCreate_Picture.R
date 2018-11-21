setwd("~/../Desktop/Github Push Stuff/MultiPic/")

if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}

recreate_picture <- function(smalls_source, original, target = NULL){
  if(is.null(target)){
    target = gsub("\\.png", "_combined.png", original)
  }
  c_Dir <- getwd()
  tryCatch({Source <<- readPNG(original)},error = function(x){stop("Error reading source picture")})
  tryCatch({setwd(smalls_source)},error = function(x){stop("Error getting to smalls")})
  
  # at this point we are in the smalls directory and have the original picture
  
  tryCatch({test <- readPNG(dir()[1])}, error = function(x){stop("Failed to read a picture from the smalls Directory")})
  Test_small = readPNG(dir()[1])
  D = dim(Test_small)
  SD = dim(Source)
  
  #Time to answer the question, what we doing here
  # Black and white edition
  BW = F
  if(min(length(D), length(SD))==2){
    # Black and white
    BW = T
    if(length(SD)==3){
      if(SD[3] >=3){ # RGB with or without alpha layer
        Source <- (Source[,,1]+Source[,,2]+Source[,,3])/3
      } else { # Monotone + alpha layer
        Source <- Source[,,1]
      }
    } else if (length(D) == 3){ 
      stop("Smalls Dimensions exceed 3rd layer of Source")
    }
  } else if (min(length(D), length(SD)) < 2){
    stop("Bad Dimensions")
  }
  
  num_row = SD[1]
  num_col = SD[2]
  row_size = D[1]
  col_size = D[2]
  
 
  
  
  
  # Logic only set up for square replacements now so we crop source
  if(BW){
    Source <- Source[1:(num_row-(num_row%%D[1])), 1:(num_col-(num_col%%D[2]))]
  } else {
    Source <- Source[1:(num_row-(num_row%%D[1])), 1:(num_col-(num_col%%D[2])),]
  }
  Row_Elements = dim(Source)[1]/D[1]
  Col_Elements = dim(Source)[2]/D[2]
  
  # Create picture slices 
  row = 1
  col = 1
  Picture_Slices <- list(Source[(1+(row-1)*row_size):(row*row_size), (1+(col-1)*col_size):(col*col_size)])
  index = 1
  for(row in 1:Row_Elements){
    for(col in 1:Col_Elements){
      if(BW){
        Picture_Slices[[index]] <- Source[(1+(row-1)*row_size):(row*row_size), (1+(col-1)*col_size):(col*col_size)]
      } else {
        Picture_Slices[[index]] <- Source[(1+(row-1)*row_size):(row*row_size), (1+(col-1)*col_size):(col*col_size),]
      }
      index = index+1
    }
  }
  
  # Creates a clone to use for holding best slices and best norms
  Clone_To_Replace <- Picture_Slices
  for(slice in 1:length(Picture_Slices)){
    Clone_To_Replace[[slice]] <- Test_small
  }
  Best_Norms <- c()
  for(index in 1:length(Clone_To_Replace)){
    Best_Norms <- c(Best_Norms, sqrt(sum(abs(Picture_Slices[[index]]-Clone_To_Replace[[index]])^2)))
  }
  
  #Now we iterate through the directory of smalls, for each element we calculate the Norm. 
  #Wherever the norm wins we replace the clone picture list with that matrix
  for(Small in 1:length(dir())){
    # Reads the new small
    small <- readPNG(dir()[Small])
    for(index in 1:length(Clone_To_Replace)){
      Norm <- sqrt(sum(abs(Picture_Slices[[index]]-small)^2))
      if(Norm < Best_Norms[index]){
        Best_Norms[index] <- Norm
        Clone_To_Replace[[index]] <- small
      }
    }
    print(paste(round(Small/length(dir()),4), "% Done"))
  }
  
  # Now we rebuild 
  To_Save <- Source
  index = 1
  for(row in 1:Row_Elements){
    for(col in 1:Col_Elements){
      if(BW){
        To_Save[(1+(row-1)*row_size):(row*row_size), (1+(col-1)*col_size):(col*col_size)] <- Clone_To_Replace[[index]]
      } else {
        To_Save[(1+(row-1)*row_size):(row*row_size), (1+(col-1)*col_size):(col*col_size),] <- Clone_To_Replace[[index]]
      }
      index = index+1
    }
  }
  # And save
  setwd(c_Dir)
  writePNG(image = To_Save, target = target)
}

