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

countNa <- function(df)
{
    for(col in names(df)){
        cat(col," ",sum(is.na(df[[col]])),"\n")
    }
}

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

# Esto es para ver los outliers de las columnas numericas importantes
# Probar al final el modelo entrenado con y sin outliers
#boxplot(district[,4:16])
#boxplot(loan_train$amount)

dframes <- list(account,card_test,card_train,client,disp,
             district,loan_test,loan_train,trans_test,trans_train)

# Max date on client$birth_number, card$issued to avoid considering dates like 11-01-01 as 2011-01-01 but 1911-01-01
# because data is older than 
refDate <- max(client$birth_number,card_train$issued,card_test$issued,account$date,
			trans_train$date,trans_test$date,loan_test$date,loan_train$date)

loadData(c("account", "card_test", "card_train", "client", "disp", "district", "loan_test", 
			"loan_train", "trans_test", "trans_train"), c("~/Mineria/Data/") ,c(""," ","NA","?"))

# Checking number of NA field for each table
checkNa(dframes)

client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))


# antiguedad de tarjetas
# antiguedad de cuenta
# edad de usuario
# tiempo del prestamo por terminar
# para que es fecha de transaccion?