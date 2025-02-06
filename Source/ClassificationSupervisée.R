##################### Required packages to run following code ###############################
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
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



######################### Default Decision Tree ##############################################

# Each sample of Fbis will contain 80% of N and 80% of 0
Fsub <- c(sample(which(Fbis$V10 == "O"), round(0.8 * sum(Fbis$V10 == "O"), digits = 0)), sample(which(Fbis$V10 == "N"), round(0.8 * sum(Fbis$V10 == "N"), digits = 0)))

# plotting default parameters decision Tree
fit <- rpart(Fbis$V10~ ., data=Fbis, subset=Fsub, control = rpart.control(minsplit = 4))
rpart.plot(fit, type = 3, extra = 101)

# Printing error rate
tab <- table(predict(fit, Fbis[-Fsub,], type="class"), Fbis[-Fsub,"V10"])
error_rate <- (sum(tab) - sum(diag(tab))) / sum(tab)
print(paste("Taux d'erreur :", error_rate))
##############################################################################################

######################## Extracting best minsplit for decision Tree ##########################

nbMinSplit <- 10                         # Range to which minsplit will be tested
nbiter <- 1000                           # Number of iteration for each minsplit
mean_error_rates <- numeric(nbMinSplit)  # Store mean error rates
var_error_rates <- numeric(nbMinSplit)   # Store variance of error rates

for (j in 1:nbMinSplit) {
  error_rates <- numeric(nbiter)  # Reset error rates for each minsplit
  
  for (i in 1:nbiter) {
    # New Sample 80% of N, 80% of O
    Fsub <- c(sample(which(Fbis$V10 == "O"), round(0.8 * sum(Fbis$V10 == "O"), digits = 0)), 
              sample(which(Fbis$V10 == "N"), round(0.8 * sum(Fbis$V10 == "N"), digits = 0)))
    
    # Training
    fit <- rpart(Fbis$V10 ~ ., data = Fbis, subset = Fsub, control = rpart.control(minsplit = j))
    
    # Confusion matrix
    tab <- table(predict(fit, Fbis[-Fsub,], type="class"), Fbis[-Fsub, "V10"])
    
    # Compute the error rate
    error_rates[i] <- (sum(tab) - sum(diag(tab))) / sum(tab)
  }
  
  # Mean error and var rate
  mean_error_rates[j] <- mean(error_rates, na.rm = TRUE)
  var_error_rates[j] <- var(error_rates, na.rm = TRUE)
}

# Formatting data results
results <- data.frame(mean_error_rate = mean_error_rates, variance_error_rate = var_error_rates)
print(results)
##############################################################################################


######################## Best Decision Tree with Minsplit 8 ##################################

nbiter <- 1000          # Number of iteration
minsplit_value <- 8     # Best Minsplit value based on previous experiment
best_fit <- NULL        # Best fit found
lowest_error <- Inf     # Lowest error rate found

for (i in 1:nbiter) {
  # New Sample 80% of N, 80% of O
  Fsub <- c(sample(which(Fbis$V10 == "O"), round(0.8 * sum(Fbis$V10 == "O"), digits = 0)), 
            sample(which(Fbis$V10 == "N"), round(0.8 * sum(Fbis$V10 == "N"), digits = 0)))
  
  # Training
  fit <- rpart(Fbis$V10 ~ ., data = Fbis, subset = Fsub, control = rpart.control(minsplit = minsplit_value))
  
  # Confusion Matrix
  tab <- table(predict(fit, Fbis[-Fsub,], type="class"), Fbis[-Fsub, "V10"])
  
  # Error rate
  error_rate <- (sum(tab) - sum(diag(tab))) / sum(tab)
  
  # Checking for best decision tree
  if (!is.na(error_rate) && error_rate < lowest_error) {
    lowest_error <- error_rate
    best_fit <- fit
  }
}

# Best fit
rpart.plot(best_fit, type = 3, extra = 101)
print(paste("Meilleur taux d'erreur :", lowest_error))

##############################################################################################


