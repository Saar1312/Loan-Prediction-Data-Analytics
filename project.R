# Disabling scientific notation
options(scipen=999)

# Loading data
loadData <- function(tables, path, empty_values)
{
	files <- paste(path, tables, ".csv",sep="")
	for (f in files){
		read.csv(f, sep=";", na.strings=empty_values)
	}
}

# Counts the number of NA fields in a column 
countNa <- function(df)
{
    for(col in names(df)){
        cat(col," ",sum(is.na(df[[col]])),"\n")
    }
}

# Prints number of NAs for each column
checkNa <- function(frames)
{
	for(f in frames){
		countNa(f)
		cat("#################################\n")
	}
}

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
			return("Hombre")
	} else {
			return("Mujer")
	}
}

# Difference in time (weeks) between given date and reference date
getAntiquity <- function(date, refDate)
{
	return(difftime(strptime(toString(refDate), format = "%y%m%d"),
	strptime(paste("19",toString(formatDate(date)),sep=""), format = "%Y%m%d"),units="weeks")/52.25)
}

# Table files names
tables <- c("account", "card_test", "card_train", "client", "disp", "district", "loan_test", 
			"loan_train", "trans_test", "trans_train")

# Change here the path of data
path <- "~/Mineria/Data/"

# Values of empty fields in .csv files
na_values <- c(""," ","NA","?")

# Esto es para ver los outliers de las columnas numericas importantes
# Probar al final el modelo entrenado con y sin outliers
#boxplot(district[,4:16])
#boxplot(loan_train$amount)

# Putting al tables in a list for passing them to checkNA
dframes <- list(account,card_test,card_train,client,disp,
             district,loan_test,loan_train,trans_test,trans_train)

# Max date on client$birth_number, card$issued to avoid considering dates like 11-01-01 as 2011-01-01 but 1911-01-01
# because data is older than 
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
			trans_train$date,trans_test$date,loan_test$date,loan_train$date)

loadData(tables, c(path) ,na_values)

# Checking number of NA field for each table
checkNa(dframes)

# Adding new columns of gender and age to client table
client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))