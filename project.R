# Disabling scientific notation
options(scipen=999)

# Loading data
loadData <- function(tables_names, path, empty_values)
{
	files = paste(path, tables_names, ".csv",sep="")
	tables = list()
	for (f in files){
		tables = c(tables,read.csv(f, sep=";", na.strings=empty_values))
	}
	return(tables)
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
			return("Male")
	} else {
			return("Female")
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

# Table files names
tables_names <- c("account", "card_test", "card_train", "client", "disp", "district", "loan_test", 
			"loan_train", "trans_test", "trans_train")

# Change here the path of data
path <- "~/Mineria/Data/"

# Values of empty fields in .csv files
na_values <- c(""," ","NA","?")

tables <- loadData(tables, c(path) ,na_values)

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

# Checking number of NA field for each table
checkNa(dframes)

# Adding new columns of gender and age to client table
client$gender<-unlist(lapply(client$birth_number,getGender))
client$age<-unlist(lapply(client$birth_number,getAntiquity,refDate))

# Joining disp (table that maps clients IDs to accounts IDs) and client tables by client_id
m1 = mergeTables(disp[,c("client_id","account_id")], client[,c("client_id","gender","age")],"client_id")

# Joining m1 with loan data by account_id
m2 = mergeTables(m1,loan_train[,c("account_id","status")],"account_id")

# Age vs Status
boxplot(age~status,data=m2)

# By gender
boxplot(age~status,data=m2[m2$gender == "Hombre",])
boxplot(age~status,data=m2[m2$gender == "Mujer",])

# Contingency table for gender vs status
c1 <- table(m2$gender,m2$status)

# Barplot gender vs status
barplot(c1,
  main = "Gender-Status frequencies",
)

"
# DUDAS
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