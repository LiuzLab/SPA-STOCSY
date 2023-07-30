PkinReg <- function(pk_list, reg){
  record <- 0
  for(i in 1: length(pk_list)){
    if(pk_list[i] < reg[2] & pk_list[i] > reg[1]){
      record <- record + 1
    }
    else{record <- record}
  }
  return(red = record)
}
