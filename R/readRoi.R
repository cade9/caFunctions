#' @title readRoi
#'
#' @description let's you read your ROI extracts from ENVI as a dataframe 
#' @param x ttxt file with extract information.
#' @keywords envi, roi
#' @export
#' @examples

readRoi <- function(x){
  # class names 
  classNames <- str_subset(x,"ROI name") %>% str_replace_all(., "; ROI name: ", "")
  # number of points per class
  np <- str_subset(x,"ROI npts") %>% 
        str_replace_all(., "; ROI npts: ", "") %>% 
        as.numeric()
  
  # character list of names 
  exNames <- rep(classNames, np)
  # data
  idPos <- which(str_detect(x, "ID"))
  df <- read_table(x[idPos:length(x)]) %>%
    # remove the column that is a ;
    select(!";") %>%
    # add a column of class names 
    mutate(className = exNames)
  
  # return
  return(df)
}
