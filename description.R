####################### Preparing data for the descriptive steps #######################
colnames(global_train)<-unlist(lapply(colnames(global_train), firstup))

global_train<-toStr(global_train,c("Gender"))

global_train$Gender[global_train$Gender == "1"] = "Female"
global_train$Gender[global_train$Gender == "0"] = "Male"
########################### Modifying outliers with median #############################
cols = colnames(global_train)[c(10:12,17:33)]
for (c in cols){
  global_train <- set_outlier(global_train,c)
}


################################# Descriptive steps ####################################
#Is there a relation between the loan status and...
# Boxplot: Age vs Status)
png(file="Age_status.outl1.png")
plot(boxplot2(global_train,c("Age","Status")))
dev.off()

# Boxplot: Age vs Status by genders (0: Male 1: Female)
png(file="Gender_status.outl1.png")
plot(boxplot2(global_train[global_train$Gender == "Male",],c("Age","Status")))
dev.off()

png(file="Gender_status_female.outl1.png")
plot(boxplot2(global_train[global_train$Gender == "Female",],c("Age","Status")))
dev.off()


# Contingency table: gender vs status
png(file="Gender_status_male.outl1.png")
plot(bar_table(global_train,c("Gender","Status")))
dev.off()


# Some plots to understand data features
# Frequency of each status
png(file="status_freq.outl1.png")
plot(bar_table(global_train, c("Status")))
dev.off()

# Amount
png(file="Amount_status.outl1.png")
plot(boxplot2(global_train,c("Amount","Status")))
dev.off()

# Payments
png(file="Payments_status.outl1.png")
plot(boxplot2(global_train,c("Payments","Status")))
dev.off()
 # The more months to pay, the more likely to fraud
# Ownership
png(file="Owners_status.outl1.png")
plot(bar_table(global_train,c("Owners","Status")))
dev.off()

# Duration
png(file="Duration_status.outl1.png")
plot(boxplot2(global_train,c("Duration","Status")))
dev.off()

# Current_time
png(file="Current_time_status.outl1.png")
plot(boxplot2(global_train,c("Current_time","Status")))
dev.off()

# Region
png(file="Region_status.outl1.png")
plot(barplot2(global_train, c("Region"),"Status"))
dev.off()

# no..of.inhabitants
png(file="inhabitants_status.outl1.png")
plot(boxplot2(global_train,c("No..of.inhabitants","Status")))
dev.off()

# no..of.municipalities.with.inhabitants...499
png(file="municip499_status.outl1.png")
plot(boxplot2(global_train,c("No..of.municipalities.with.inhabitants...499","Status")))
dev.off()

# no..of.municipalities.with.inhabitants.500.1999
png(file="municip5001999_status.outl1.png")
plot(boxplot2(global_train,c("No..of.municipalities.with.inhabitants.500.1999","Status")))
dev.off()

# no..of.municipalities.with.inhabitants.2000.9999
png(file="municip2000_status.outl1.png")
plot(boxplot2(global_train,c("No..of.municipalities.with.inhabitants.2000.9999","Status")))
dev.off()

# no..of.municipalities.with.inhabitants..10000
png(file="municip10000_status.outl1.png")
plot(boxplot2(global_train,c("No..of.municipalities.with.inhabitants..10000","Status")))
dev.off()

# no..of.cities
png(file="cities_status.outl1.png")
plot(boxplot2(global_train,c("No..of.cities","Status")))
dev.off()

# ratio.of.urban.inhabitants
png(file="urban_inh_status.outl1.png")
plot(boxplot2(global_train,c("Ratio.of.urban.inhabitants","Status")))
dev.off()

# average.salary
png(file="average_salary_status.outl1.png")
plot(boxplot2(global_train,c("Average.salary","Status")))
dev.off()

# global_train$unemploymant.rate..95
png(file="unemp95_status.outl1.png")
plot(boxplot2(global_train,c("Unemploymant.rate..95","Status")))
dev.off()

# no..of.enterpreneurs.per.1000.inhabitants
png(file="enterp1000_status.outl1.png")
plot(boxplot2(global_train,c("No..of.enterpreneurs.per.1000.inhabitants","Status")))
dev.off()

# balance_mean
png(file="Balance_mean_status.outl1.png")
plot(boxplot2(global_train,c("Balance_mean","Status")))
dev.off()

# balance_sd
png(file="Balance_sd_status.outl1.png")
plot(boxplot2(global_train,c("Balance_sd","Status")))
dev.off()

# balance_min
png(file="Balance_min_status.outl1.png")
plot(boxplot2(global_train,c("Balance_min","Status")))
dev.off()

# balance_max
png(file="Balance_max_status.outl1.png")
plot(boxplot2(global_train,c("Balance_max","Status")))
dev.off()
