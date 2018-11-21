setwd("~/../Desktop/Github Push Stuff/MultiPic/")
MAX_SAMPLE_SIZE = 700
if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}

# Creates <number> pictures of <size>x<size> in directory "Smalls_of_<size>" from directory <use_Pictures_dir> 
# If bw only use black and white 
create_smalls <- function(number = 10, size = 10, use_Pictures = c(), use_Pictures_dir = "", bw = T){
  c_Dir <- getwd()
  tryCatch({dir.create("sampledir")},error=function(x){stop("Failed to create tempDir")})
  num_done = 0
  tryCatch({dir.create(paste("Smalls_of_", size, sep = ""))}, error = function(x){stop("Failed to create smalls directory")})
  setwd(paste("Smalls_of_", size, sep = ""))
  Save_Dir <- getwd()
  setwd(c_Dir)
  setwd("sampledir")
  Sample_Dir <- getwd()
  
  if(length(use_Pictures) > 0){ # break up pictures
    
    
  } else { # No Pictures to break up, use randomly generated pictures
    times <- c()
    names <- c()
    # Generate names now to be faster later
    print("Generating names...")
    for(index in 1:number){
      time <- Sys.time()
      new = Generate_Name()
      while(!(new %in% names)){
        names <- c(names, new)
      }
      times <- c(times, Sys.time()-time)
    }
    
    # This is where I would put magic to make sure each generated picture is vastly different
    index = 1
    while(index < number){
      # Create a picture randomly
      if(bw){
        Matrix <- matrix(floor(runif(size*size)*256)/256,nrow=size,ncol=size)
      } else {
        Matrix <- abind(matrix(floor(runif(size*size)*256)/256,nrow=size,ncol=size),
                        matrix(floor(runif(size*size)*256)/256,nrow=size,ncol=size),
                        matrix(floor(runif(size*size)*256)/256,nrow=size,ncol=size),
                        along = 3)
      }
      writePNG(image = Matrix, target = names[index])
      index = index + 1
      if(index%%100==0){print(paste(round(index/number, 4), "% Done Generating Smalls"))}
      # To speed up saving the small slices, every 1500 slices it will copy everything into the final directory
      # Dont forget to copy the final batch over
      if(index%%MAX_SAMPLE_SIZE==0){ 
        print("Transfering...")
        Success <- file.copy(dir(),Save_Dir)
        if(FALSE %in% Success){stop("Error transfering a slice")}
        Success <- file.remove(dir())
        if(FALSE %in% Success){stop("Error deleting a slice")}
      }
    }
    # Cleanup
    if(length(dir())>0){
      print("Transfering...")
      Success <- file.copy(dir(),Save_Dir)
      if(FALSE %in% Success){stop("Error transfering a slice")}
      Success <- file.remove(dir())
      if(FALSE %in% Success){stop("Error deleting a slice")}
    }
    # Removed the sample directory
    setwd(c_Dir)
    print("Cleaning up sampledir...")
    unlink("sampledir", recursive = T)
  }
  setwd(c_Dir)
  closeAllConnections()
}



Generate_Name <- function(length = 10){
  name = ""
  for(each in 1:length){
    name = paste(name, letters[sample(x = 1:26, size = 1)], sep = "")
  }
  return(paste(name, ".png", sep = ""))
}
