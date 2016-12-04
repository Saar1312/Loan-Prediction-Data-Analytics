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

# Counts the number of NA fields in a column 
countNa <- function(df)
  {
    for(col in colnames(df)){
      cat(col," ",sum(is.na(df[[col]])),"\n")
      
    }
}

# Prints number of NAs for each column
checkNa <- function(frames)
{
	for(f in frames){
		countNa(f)
		cat("  ###############################\n")
	}
}

#------------------ DATES -------------------------

# Takes out the +50 months if date is the birthday of a female client  
formatDate <- function(date)
{
	if (substr(toString(date),3,4) > "12"){
		return(date-5000)
	} else {
		return(date)
	}
}

# Determines the gender of a client using the birthdate (female dates have month +50)
getGender <- function(date)
{
	if (substr(toString(date),3,4) <= "12"){
			return("0")
	} else {
			return("1")
	}
}

# Difference in time (weeks) between given date and reference date
getAntiquity <- function(date, refDate)
{
	return(difftime(strptime(toString(refDate), format = "%y%m%d"),
	strptime(paste("19",toString(formatDate(date)),sep=""), format = "%Y%m%d"),units="weeks")/52.25)
}

mergeTables <- function(df1,df2,colName)
{
	merge(df1,df2,by=colName)
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