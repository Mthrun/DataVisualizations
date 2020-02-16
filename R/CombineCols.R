CombineCols=function(...){
  
  buffer <- function(x,
                     length.out = len(x),
                     fill = NULL,
                     preserveClass = FALSE){
    #author: Craig Varrichio
    xclass <- class(x)
    input <- lapply(vert(x), unlist)
    results <- as.data.frame(lapply(input, rep, length.out = length.out))
    if (length.out > len(x) && !is.null(fill)){
      results <- t(results)
      results[(length(unlist(x)) + 1):length(unlist(results))] <- fill
      results <- t(results)
    }
    #if (preserveClass)
    #  results <- as2(results, xclass)
    return(results)
  }
  
  len <- function(data){
    #author: Craig Varrichio
    result <- ifelse(is.null(nrow(data)), length(data), nrow(data))
    return(result)
  }
  
  vert <- function(object){
    #author: Craig Varrichio
    if (is.list(object))
      object <- cbind(object)
    object <- data.frame(object)
    return(object)
  }
  
  cbind_fill <- function(..., fill = NULL){
    #author: Craig Varrichio
    inputs <- list(...)
    inputs <- lapply(inputs, vert)
    maxlength <- max(unlist(lapply(inputs, len)))
    bufferedInputs <-
      lapply(inputs,
             buffer,
             length.out = maxlength,
             fill,
             preserveClass = FALSE)
    return(Reduce(cbind.data.frame, bufferedInputs))
  }
  return(cbind_fill(...,fill = NaN))
}