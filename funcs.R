
selectYears = function(data,selectedYears){
  result = integer(dim(data)[1])

  for (i in selectedYears){
    result = (years(data[,1]) == i) | result
  }
  
  return(result)
}


selectMonths = function(data,selectedMonths){
  result = integer(dim(data)[1])
  
  referenceMonths = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  for (i in selectedMonths){
    result = (months(data[,1]) == referenceMonths[i]) | result
  }
  
  return(result)
}





