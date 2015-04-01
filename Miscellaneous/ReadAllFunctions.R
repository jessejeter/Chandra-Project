# Replace file.path with your file path

#file.path <- YOUR_FILE_PATH
sapply(list.files(paste(file.path, "/GitHub/Chandra-Project/Main Functions", sep=""), full.names=TRUE), source)
sapply(list.files(paste(file.path, "/GitHub/Chandra-Project/Background Functions", sep=""), full.names=TRUE), source)
