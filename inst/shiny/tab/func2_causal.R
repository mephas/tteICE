## functions for MEPHAS

##' @title List of variable types
##'
##' @param data input data frame
##' @export
# var.class <- function(data){
#   x <- sapply(data, class)
#   x[sapply(data, function(v){
#     x <- unique(v)
#     length(x) - sum(is.na(x)) == 2L
#   })] <- "binary"
#   x <- as.data.frame(x)
#   # x <- as.matrix(x)
#   colnames(x) <- "Variable Type"
#   return(x)
# }

var.class <- function(data){
  # x <- sapply(data, class)
  la <- lapply(data, class)  #为了兼容Adam数据
  # x <- unlist(la) #时间类型会有两个，导致数组长度比变量数增加
  colNames <- names(la)
  x <- array(c(colNames),dim =c(length(la),1))
  
  for(i in 1:length(la)){
    x[i,1] <- la[[i]][1]
  }
  y <- sapply(data, function(v){
    x0 <- unique(v)
    length(x0) - sum(is.na(x0)) == 2L
  })
  x[unlist(y)] <- rep("binary")
  # res <- data.frame(x=x)
  res <- as.data.frame(x)
  rownames(res) <- colNames  
  # x <- as.matrix(x)
  colnames(res) <- "Variables type"
  return(res)
}

##' @title Summary of factor variables
##'
##' @param data input data frame
##' @export
desc.factor<-function(data){
  x <- var.class(data)
  a<-x[,1] %in% c("binary", "factor", "character")

  if(sum(a)==0){
    df <-data.frame(NULL)
  }

  else if(sum(a)==1){
    df <- cbind(
      round(table(data[,a, drop=FALSE])),
      round(prop.table(table(data[,a, drop=FALSE])),3)
    )
    colnames(df)<- c("N", "100%")
  }
  
  else{
  x.list<-lapply(data[,a, drop=FALSE], function(v){
    cbind(
      round(table(v)),
      round(prop.table(table(v)),6)
    )
  })
  x.data<-NULL
  for (i in 1:length(x.list)){
    x.data <- rbind(x.data,x.list[[i]])
  }
  var2<- rownames(x.data)
  var1 <- rep(names(x.list), sapply(x.list, nrow))
  df <- cbind.data.frame(var1,var2, x.data)
  if(length(colnames(df))< 4){
    return(df)
  }
  colnames(df)<- c("Variables", "Values", "Frequency", "Proportion")
  rownames(df)<- NULL
  }
  return(df)
}


##' @title Summary of numeric variables
##'
##' @param data input data frame
##' @export
desc.numeric<- function(data){
x <- var.class(data)
a<-x[,1] %in% c("integer", "numeric")

if(sum(a)==0) {df <- data.frame(NULL)}
else{
  data2<- as.data.frame(data[,a,drop=FALSE])
  df <- round(as.data.frame(psych::describe(data2))[,-c(1,6,7)],6)
  rownames(df) = names(data2)
  colnames(df) <- c("N", "Mean", "SD", "Median", "Min", "Max", "Range", "Skewness", "kurtosis", "SE")
}

return(df)
}