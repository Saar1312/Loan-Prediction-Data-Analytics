"Knowledge Extraction and Machine Learning
Samuel Arleo & Daniela Socas
Last modified 12/02/2016
Descriptive functions to start the project and pre-processing"

#------------------ LOADING DATA -------------------------

# Disabling scientific notation
options(scipen=999)

# Loading data
loadData <- function(path, empty_values)
{
	files <- list.files(path)
	for (i in files){
		a <- read.csv2(paste(path,i,sep=""), header = TRUE, na.strings=na_values)
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
checkType <- function(frames)
{
	cat("###################### Types of each column ####################\n")
	lapply(frames,function(x) unlist(lapply(x,function(y) typeof(y))))
}

# Change type of columns "cols" in the data frame frame
toFactor <- function(frame, cols)
{
	frame[cols]<-unlist(lapply(frame[cols], as.factor))
	return(frame)
}

# Change type of columns "cols" in the data frame frame
toNumeric <- function(frame, cols)
{
	frame[cols]<-unlist(lapply(frame[cols], as.numeric))
	return(frame)
}

# Change type of columns "cols" in the data frame frame
toInteger <- function(frame, cols)
{
	frame[cols]<-unlist(lapply(frame[cols], as.integer))
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
			return(0)		# 0: Male
	} else {
			return(1)		# 1: Female
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

# Change columns specified by cols to factor
toFactor <- function(table, cols)
	{
		for (c in cols){
			cat(c)
			table[c] = as.factor(unlist(table[c]))
		}
		return(table)
	}

#------------------ DUDAS -------------------------

"
- Los prestamos se toman como ya finalizados?
- En el enunciado dice que hay varios status A,B,C,D pero en los archivos aparece 1 y -1
- Hay datos de testing para las relaciones card, loan y transaction. Hay que hacer predicciones de algo mas ademas
de si un prestamo terminara de forma exitosa o no?
- sanction interest if negative balance?
- Quitar ceros de account en transfers? es noise? afecta en algo?
- interest credited -- interest that a savings institution automatically deposits to a savings account. 
- Falta la relacion Permanent order (esta en el enunciado pero no hay ningun archivo con ese nombre)
- Usar contingency tables para comparar nominal-nominal y boxplot para numerical-nominal
"