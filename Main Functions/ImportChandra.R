ImportChandra <- function(file.path, file.name = NA, ID.num = NA, 
                          sections = FALSE) {

# Import Chandra data.
if(is.na(file.name) == FALSE) {
  chandra.data <- read.table(paste(file.path, file.name, sep = ""), 
                             header=FALSE, quote="\"", sep=",")
} else if(is.na(ID.num) == FALSE) {
  chandra.data <- read.table(paste(file.path, "acisf", ID.num,
                                   "_repro_evt2_500-8000eV_t1.txt", sep = ""),
                             header=FALSE, quote="\"", sep=",")
}

X.orig <- chandra.data[, 11]
Y.orig <- chandra.data[, 12]

# Shift, rotate and rescale Chandra data.
rotated.points <- SquareRot(X.orig, Y.orig)$"points"

X.rot <- rotated.points[,1]
Y.rot <- rotated.points[,2]

final.object <- cbind(X.orig, Y.orig, X.rot, Y.rot)
names(final.object) <- c('X.orig', 'Y.orig', 'X.rot', 'Y.rot')
return(final.object)

}
