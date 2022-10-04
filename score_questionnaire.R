score_questionnaire <- function(x, cols, rev_score_cols = NULL, min = NULL, max = NULL){
  if(!inherits(x = x, what = 'data.frame')){
    stop(sprintf('Input data x must be of class `data.frame`, but is %s', class(x)))
  }
  if(any(dim(x) == 0)){
    stop('Input data x is empty')
  }
  if(!inherits(cols, 'character') | 
     (!is.null(rev_score_cols) & !inherits(cols, 'character'))){
    stop('cols or rev_score_cols are not character vectors')
  }
  
  all_cols <- unique(c(cols, rev_score_cols))
  if(!all(all_cols %in% names(x))){
    stop('At least one column name in cols or rev_score_cols is not found in x')
  }
  
  ncols <- length(all_cols)
  x <- x[, all_cols]
  if(dim(x)[[2]] < 2){
    stop('Must score at least two columns')
  }
  
  if(!is.null(rev_score_cols)){
    x[, rev_score_cols] <- -1 * (x[, rev_score_cols] - max) + min
  }
  
  #If there are missing values, the expected sum is just the average score times
  #the number of items.
  scored_data <- rowMeans(x, na.rm = TRUE) * ncols
  
  return(scored_data)
}
