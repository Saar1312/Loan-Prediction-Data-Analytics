#------------------ FUNCTIONS -------------------------

#------------------ LOADING DATA -------------------------

# Disabling scientific notation
options(scipen=999)

# Loading data
loadData <- function(path, empty_values)
{
  files = list.files(path)
  for (i in files){
    a = read.csv2(paste(path,i,sep=""), header = TRUE, na.strings=na_values)
    assign(unlist(strsplit(i,"[.]"))[1], a, envir = .GlobalEnv)
  }
  remove(a)
}

#------------------ NA VALUES -------------------------

# Prints number of NAs for each column for each table in frames
checkNa <- function(frames)
{
  cat("###################### Number of NAs for each column ####################\n")
  lapply(frames,function(x) unlist(lapply(x,function(y) sum(is.na(y)))))
}

#------------------ TYPES -------------------------

# Finding out if each column is factor
findFactors <- function(frames)
{
  cat("###################### Finding factors ####################\n")
  lapply(frames,function(x) unlist(lapply(x,function(y) is.factor(y))))
}

# Getting the class of each column of each table to verify types numeric, integer, etc
checkClasses <- function(frames)
{
  cat("###################### Types of each column ####################\n")
  lapply(frames,function(x) unlist(lapply(x,function(y) class(y))))
}

# Getting the type of each column of each table to verify types numeric, integer, etc
checkTypes <- function(frames)
{
  cat("###################### Types of each column ####################\n")
  lapply(frames,function(x) unlist(lapply(x,function(y) typeof(y))))
}

# Change type of columns "cols" in the data frame frame
toFactor <- function(frame, cols)
{
  frame[cols] = unlist(lapply(frame[cols], as.factor))
  return(frame)
}

# Change type of columns "cols" in the data frame frame
toNumeric <- function(frame, cols)
{
  frame[cols] = unlist(lapply(frame[cols], as.numeric))
  return(frame)
}

# Change type of columns "cols" in the data frame frame
toInteger <- function(frame, cols)
{
  frame[cols] = unlist(lapply(frame[cols], as.integer))
  return(frame)
}

toStr <- function(frame, cols)
{
  frame[cols] = unlist(lapply(frame[cols], as.character))
  return(frame)
}

#------------------ DATES -------------------------

formatYear <- function(date)
{
  if (substr(toString(date),1,2) < "11"){
    return(date + 20000000)
  } else{
    return(date + 19000000)
  }
}

# Takes out the +50 months if date is the birthday of a female client  
formatDate <- function(date)
{
  date = formatYear(date)
  if (substr(toString(date),5,6) > "12"){
    return(date - 5000)
  } else {
    return(date)
  }
}

# Determines the gender of a client using the birthdate (female dates have month +50)
getGender <- function(date)
{
  if (substr(toString(date),3,4) <= "12"){
    return(as.factor(0))		# 0: Male
  } else {
    return(as.factor(1))		# 1: Female
  }
}

# Difference in time (weeks) between given date and reference date
getAntiquity <- function(date, refDate, type = "years")
{
  if (type == "years"){
    return(difftime(strptime(toString(refDate), format = "%y%m%d"),
                    strptime(toString(formatDate(date)), format = "%Y%m%d"),units="weeks")/52.25)
  } else {
    return(difftime(strptime(toString(refDate), format = "%y%m%d"),
                    strptime(toString(formatDate(date)), format = "%Y%m%d"),units="weeks"))
  }
}

#---------------- FEATURES --------------------

featureRatio <- function(frame, feature,target)
{
  ratio = table(unlist(frame[,feature]),frame[,target])		# Count instances of each value in column feature
  ratio = ratio + 1										# Avoid further division by 0
  avg = mean(ratio[,1]+ratio[,2]) 						# Average of the counts
  ratio = (ratio[,1]-ratio[,2])/avg 						# Calculate the actual rate: (num_positive-num_negatives)/avg
  ratio = as.data.frame(ratio)
  ratio[feature] = rownames(ratio) 						# Maps rates with values of column "feature"
  res = merge(frame,ratio, by=feature[1])					# Add column with ratings to frame table
  names(res)[names(res) == "ratio"] = paste(feature[1],".ratio",sep="")
  return(res)
}

# Applies operations  mean, standard deviation, max or min of elements in cols[2] with the same id in col[1]
applyOper <- function(table, cols, op)
{
  ids = unique(table[cols[1]])
  res = vector("list",dim(ids)[1])
  index = 1
  for (id in ids[,1]){
    tmp = unlist(table[table[cols[1]] == id,][cols[2]])
    if (op == 1){
      res[index] = mean(tmp) # op = 1 -> mean
    }
    else if (op == 2){
      res[index] = sd(tmp) # op = 2 -> var
    }
    else if (op == 3){
      res[index] = min(tmp) # op = 3 -> min
    }
    else if (op == 4){
      res[index] = max(tmp) # op = 4 -> max
    }
    index = index+1
  }
  return(unlist(res))
}

#----------- PREDICTIVE FUNC. -----------

#Get sample by percentage 
get_sample <- function(frame, perctg) 
{
  n = nrow(frame)
  sp = (perctg*n)%/%100
  tr = frame[1:sp,]
  ts = frame[sp+1:n,][1:(n-sp),]
  return(list("train"=tr,"test"=ts))
}

confMatrix <- function(res)
{
  confusionMatrix(res$Predicted,res$status,positive="1")[2]
}

table_to_df2 <- function(t)
{
  df = data.frame(x=t[1:(length(t)%/%2)],y=t[(1+length(t)%/%2):length(t)])
  colnames(df) = colnames(t)
  rownames(df) = rownames(t)
  return(df)
}

# Boxplot with ggplot, with cols=c("Var","Status")
dotplot2 <- function(df,cols,f=None)
{
  ggplot(df, aes_string( cols[1],cols[2],fill=f,colour=f)) + geom_point()
}
# Boxplot with ggplot, with cols=c("Var","Status")
boxplot2 <- function(df,cols)
{
  ggplot(df, aes_string(x=cols[2], y=cols[1], color=cols[2])) +
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)
}

# Barplot of data frame
barplot2 <- function(df,cols,f=None)
{
  ggplot(df, aes_string(x=cols[1], y=cols[2], fill=f)) + geom_bar(stat="identity")
}

# Barplot of table
bar_table <- function(df,cols)
{
  t <- table(df[cols])
  df2 = data.frame(t)
  colnames(df2)<-c(cols,"Frequency")
  if (length(cols) == 2){
    ggplot(df2, aes_string(x=cols[1], y = "Frequency", fill=cols[2])) + geom_bar(stat="identity")
  }else{
    ggplot(df2, aes_string(x=cols[1], y = "Frequency", fill=cols[1])) + geom_bar(stat="identity")
  }
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

set_outlier <- function(df,col)
{
  if (is.factor(df[,col])){
    View(col)
    cat(col)
    return(df)
  }else{  
    c = df[,col]  
    lowerq = quantile(c)[2]
    upperq = quantile(c)[4]
    iqr = IQR(c)
    thrU = (iqr * 1.5) + upperq
    thrL = lowerq - (iqr * 1.5)
    if (length(df[df[col] > thrU,][,col]) > 0){  
      df[df[col] > thrU,][,col] = median(c)
    }
    if (length(df[df[col] < thrL,][,col]) > 0){  
      df[df[col] < thrL,][,col] = median(c)
    }
    return(df)
  }
}