##################### Required packages to run following code ###############################
install.packages("lsr")
library(lsr)
#############################################################################################


##################### Data Setup ############################################################
F <- fertility_Diagnosis # Initial data importation

# Setting the right variable type for each one in a new dataframe Fbis
Fbis <- F
Fbis$V1 <- as.factor(Fbis$V1)
Fbis$V2 <- as.numeric(Fbis$V2)
Fbis$V3 <- as.factor(Fbis$V3)
Fbis$V4 <- as.factor(Fbis$V4)
Fbis$V5 <- as.factor(Fbis$V5)
Fbis$V6 <- as.factor(Fbis$V6)
Fbis$V7 <- as.factor(Fbis$V7)
Fbis$V8 <- as.factor(Fbis$V8)
Fbis$V9 <- as.numeric(Fbis$V9)
Fbis$V10 <- as.factor(Fbis$V10)
#############################################################################################

################## Shoudl data be normalised ################################################

par(mfrow = c(1, 2))  
boxplot(Fbis[, -10], main = "Boxplot Non Normalisé")
boxplot(scale(F[, -10]),  main = "Boxplot Normalisé")
par(mfrow = c(1, 1))  
#############################################################################################

################## Variables associations ###################################################

nominal_vars <- c("V1", "V3", "V4", "V5", "V6", "V7", "V8")  # Nominal vars column names
nominal_pairs <- combn(nominal_vars, 2, simplify = FALSE)    # All the pairs between those variables

# Setting up result tab
variable_associations <- data.frame(
  Var1 = character(length(nominal_pairs)),
  Var2 = character(length(nominal_pairs)),
  CramersV = numeric(length(nominal_pairs)),
  Type = character(length(nominal_pairs)),
  stringsAsFactors = FALSE
)

# For all pairs
for(i in 1:length(nominal_pairs)){
  
  #getting the varaibles name from the pair
  var1 <- nominal_pairs[[i]][1]
  var2 <- nominal_pairs[[i]][2]
  
  # Computing Cramers value
  cramers_value <- cramersV(Fbis[[var1]], Fbis[[var2]])
  
  # Adding the cramers value to the result tab
  variable_associations[i, ] <- list(
    Var1 = var1,
    Var2 = var2,
    CramersV = cramers_value,
    Type = "Nominal"
  )
}

# Adding V2 and V9 correlation (only numerical pair)
correlation_value <- cor(Fbis$V2, Fbis$V9)

# Adding it to the tab results
variable_associations <- rbind(
  variable_associations,
  data.frame(Var1 = "V2", Var2 = "V9", CramersV = correlation_value, Type = "Numeric")
)

# Printing final tab le tableau final
print(variable_associations)
#############################################################################################


################## All variables impact on the class ########################################

par(mfrow = c(2, 5))  

# Boucle pour afficher les boxplots de V1 à V9 selon V10
for(i in 1:9) {
     boxplot(split(Fbis[[paste0("V", i)]], Fbis$V10),
             main = paste("Impact de V", i, "sur V10"),
             xlab = paste("V", i),
             ylab = "V10",
             col = "lightblue",
             border = "black")
}

par(mfrow = c(1, 1))  
#############################################################################################

################## Some not significatnt analysis ###########################################

plot(Fbis) # No direct pattern from the variable
cor(Fbis$V2, Fbis$V9) # a very large line between the two variables
summary(Fbis) # nothing significant either
#############################################################################################