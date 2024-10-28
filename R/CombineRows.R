CombineRows=rbind_fill=function(...,na.rm=FALSE){
  
  inputs <- list(...)
  if(length(inputs)==0) return(NULL)
  
  PatternMatrix = as.matrix(inputs[[1]])
  if(dim(PatternMatrix)[2] == 1){
    PatternMatrix = t(PatternMatrix)
  }
  
  
  if (is.null(colnames(PatternMatrix))) {
    colnames(PatternMatrix) = paste0("C", 1:ncol(PatternMatrix))
  }
  ind_store = colnames(PatternMatrix)
  
  for (i in 2:length(inputs)) {
    if (is.vector(inputs[[i]])) {
      CurrentMatrixToAdd = matrix(inputs[[i]], nrow = 1)
    } else{
      CurrentMatrixToAdd = inputs[[i]]
    }
    if (is.null(colnames(CurrentMatrixToAdd))) {
      colnames(CurrentMatrixToAdd) = colnames(PatternMatrix)[1:ncol(CurrentMatrixToAdd)]
    }
    n_tofill=ncol(PatternMatrix) - ncol(CurrentMatrixToAdd)
    if(n_tofill>0){
      if (isFALSE(na.rm)) {
        extra_cols = matrix(
          NaN,
          nrow = nrow(CurrentMatrixToAdd),
          ncol = n_tofill
        )
      } else{
        extra_cols = matrix(
          0,
          nrow = nrow(CurrentMatrixToAdd),
          ncol = n_tofill
        )
      }
    }else{
      extra_cols=NULL
    }

    if (length(extra_cols) > 0) {
      colnames(extra_cols) = colnames(PatternMatrix)[which(match( colnames(PatternMatrix),
                                                                  colnames(CurrentMatrixToAdd),nomatch = 0 ) == 0)]
      fill_up = cbind(extra_cols, as.matrix(CurrentMatrixToAdd))
    } else{
      fill_up = as.matrix(CurrentMatrixToAdd)
    }

    fill_up = fill_up[, order(colnames(fill_up), decreasing = F)]
    PatternMatrix = PatternMatrix[, order(colnames(PatternMatrix), decreasing = F)]
    
    PatternMatrix = rbind(PatternMatrix, fill_up[,1:ncol(PatternMatrix)])
  }
  
  PatternMatrixExtended = PatternMatrix[, match(colnames(PatternMatrix), ind_store)]
  
  return(PatternMatrixExtended)
}
