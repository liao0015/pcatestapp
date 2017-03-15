#' missing value filtering function
#' @export
#' @param data_matrix data_matrix as input
#' @param threshold the value for threshold is set to 1as default 

missingvalue_filtering<-function(data.matrix, threshold = 1){ 
  data.matrix[is.infinite(as.matrix(data.matrix))]<-NA # the is.infinite function does not work on data.frame, 
  # in case there are infinte values there
  
  if(threshold < 0|threshold > ncol(data.matrix) ){
    print ("Threshold cannont be smaller than 0 or bigger than the number of columns, please check you threshold setting and rerun!!!")
  }else{

    if(threshold<=1){ # concet the q value to the real missing value number
      threshold <- ceiling(ncol(data.matrix)*threshold)
    }
    
    data_qualified<-data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x)))<=(ncol(data.matrix)-threshold))),]
    data_not.qualified<-data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x)))>(ncol(data.matrix)-threshold))),]
    return(list(data_qualified=data_qualified, 
                filtering_summary = list(data_not.qualified=data_not.qualified,
                number.qualified=nrow(data_qualified), 
                number.not.qualified=nrow(data_not.qualified))
    ))
    
  }
}