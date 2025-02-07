##################### Required packages to run following code ###############################
library(cluster)

#############################################################################################


##################### Data Setup ############################################################
fertility <- read.table("fertility_Diagnosis.txt")
F <- fertility[,-10]

F$V1 <- as.factor(F$V1)
F$V2 <- as.numeric(F$V2)
F$V3 <- as.factor(F$V3)
F$V4 <- as.factor(F$V4)
F$V5 <- as.factor(F$V5)
F$V6 <- as.factor(F$V6)
F$V7 <- as.factor(F$V7)
F$V8 <- as.factor(F$V8)
F$V9 <- as.numeric(F$V9)

Mode <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]  # Returns the most frequent value
}

#############################################################################################


######################### KMeans ##############################################

# With only 2 numerical values, kmeans is not relevant for defining clusters

###############################################################################


######################### PAM ##############################################
Fpam2<-pam(F,2)
Fpam3<-pam(F,3)
Fpam4<-pam(F,4)
Fpam5<-pam(F,5)
Fpam6<-pam(F,6)
Fpam7<-pam(F,7)
Fpam8<-pam(F,8)
Fpam9<-pam(F,9)

avg<- c(Fpam2$silinfo$avg.width,
        Fpam3$silinfo$avg.width,
        Fpam4$silinfo$avg.width,
        Fpam5$silinfo$avg.width,
        Fpam6$silinfo$avg.width,
        Fpam7$silinfo$avg.width,
        Fpam8$silinfo$avg.width,
        Fpam9$silinfo$avg.width)

plot(avg, type = "o")
par(mfrow=c(2,2))
plot(Fpam2)

table(Fpam2$clustering, fertility$V10)

filtered_data <- fertility[Fpam2$cluster == 1, ]

summary_table <- sapply(filtered_data, Mode)
summary_table

############################################################################


######################### HClust ##############################################
D<-dist(F)
resuhist<-hclust(D,method="ward.D")
plot(resuhist)

cluster_assignment <-cutree(resuhist, k = 2)
table(cluster_assignment, fertility$V10)

summary_table <- sapply(fertility[cluster_assignment == 1, ], Mode)
summary_table

###############################################################################