
RemoveNewline <- function(text){
  print("-- Start to remove \r \n \f \t")
  text <- gsub("\r|\n|\f|\t", " ", text)
}
