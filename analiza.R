# Set working directory
setwd("D:/Fakultet/Master studije/Semestar 2/Statistika/Projekat")

# Check if all required packages are installed
.packages(all.available = TRUE)

# Install the 'haven' package if necessary
# install.packages("haven")

# Load the 'haven' package needed to work with .sav files
library(haven) 
library(knitr)

# Load the data from the given file
my_data <- read_sav("data.sav")

View(my_data)
colnames(my_data)
dim(my_data)
summary(my_data)


# Count the total number of missing values in the data frame
num_missing <- sum(is.na(my_data))
print(paste("Number of missing entries:", num_missing))

# Count the number of rows with missing values
num_rows_missing <- sum(rowSums(is.na(my_data)) > 0)
print(paste("Number of rows missing entries:", num_rows_missing))

# Count the number of rows with more than 10 missing values
num_rows_missing_10 <- sum(rowSums(is.na(my_data)) > 10)
print(paste("Number of rows missing more than 10 entries:", num_rows_missing_10))

# Remove rows with more than 10 missing values
my_data <- my_data[rowSums(is.na(my_data)) <= 10,]

dim(my_data)
print(paste("Number of rows missing more than 10 entries:", sum(rowSums(is.na(my_data)) > 10)))


# Replace missing data with median
replace_median <- function(x) { 
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
}
my_data[] <- lapply(my_data, replace_median)
print(paste("Number of missing entries:", sum(is.na(my_data))))

View(my_data)


# --------------- Visualisation ----------------------

hist(my_data$Age, main = "Starost vozača", xlab = "Godina", ylab = "")

svt <- shapiro.test(my_data$Age) 
svt

hist(my_data$Hours_Day, main = "Radnih sati/dan", xlab = "Broj Sati", ylab = "")
hist(my_data$Days_Week, main = "Radnih dana/nedelja", xlab = "Godine", ylab = "")
boxplot(my_data$Hours_Day ~ my_data$Days_Week, data = my_data )
hist(my_data$BMI, main = "Index telesne mase", xlab = "", ylab = "")

svt <- shapiro.test(my_data$BMI) 

hist(my_data$Exercise, main = "Učestalost vežbsanja", xlab = "0 = less, 3 = more", ylab = "")
hist(my_data$Alcohol, main = "Učestalost konzumiranja alkohola", xlab = "0 = less, 3 = more", ylab = "")

svt <- shapiro.test(my_data$BMI) 


tabela.smoking <- table(my_data[, c("Smoking")]) / nrow(my_data)
names(tabela.smoking) <- c("Nije", "Jeste")
pie(tabela.smoking, radius=1,main="Odnos pušača i nepušača")

tabela.accidents <- table(my_data[, c("Acc_3ys_Dic")]) / nrow(my_data)
names(tabela.accidents) <- c("Bez", "Sa")
pie(tabela.accidents, radius=1,main="Odnos vozača sa i bez saobračajnim nezgodama")

hist(my_data$Accidents_3y, main = "Broj saobraćajnih nezgoda", xlab = "Broj nezgoda", ylab = "")



# --------------- Correlation ----------------------

# Calculate correlation matrix
cor_matrix <- cor(my_data)

# Find indices of correlations greater than 0.7
indices <- which(cor_matrix > 0.7, arr.ind = TRUE)

s = 0
for (i in 1:nrow(indices)) {
  row_index <- indices[i, 1]
  col_index <- indices[i, 2]
  if (row_index < col_index) { 
    cat(sprintf("'%s' and '%s' have correlation %.2f\n", names(my_data)[row_index], names(my_data)[col_index], cor_matrix[row_index, col_index]))
    s = s + 1
  }
}
print(paste("Jaka korelacija: ", s))



indices <- which(cor_matrix > 0.5 & cor_matrix < 0.7, arr.ind = TRUE)
s = 0
for (i in 1:nrow(indices)) {
  row_index <- indices[i, 1]
  col_index <- indices[i, 2]
  if (row_index < col_index) { 
    s = s + 1
  }
}
print(paste("Srednja korelacija:", s))



indices <- which(cor_matrix > 0.3 & cor_matrix < 0.5, arr.ind = TRUE)
s = 0
for (i in 1:nrow(indices)) {
  row_index <- indices[i, 1]
  col_index <- indices[i, 2]
  if (row_index < col_index) { 
    s = s + 1
  }
}
print(paste("Slaba korelacija:", s))


detach()
attach(my_data)

par(mfrow=c(3,3), mar=c(4, 4, 2, 2))

plot(my_data$Weight, my_data$BMI, main = "", xlab = "Weight", ylab = "BMI")
plot(my_data$Skill_Discretion, my_data$Control, main = "", xlab = "Skill_Discretion", ylab = "Control")
plot(my_data$Decision_Latitude, my_data$Control, main = "", xlab = "Decision_Latitude", ylab = "Control")

plot(my_data$Supervisor_SS, my_data$Social_Support_SS, main = "", xlab = "Supervisor_SS", ylab = "Social_Support_SS")
plot(my_data$Peer_SS, my_data$Social_Support_SS, main = "", xlab = "Peer_SS", ylab = "Social_Support_SS")
plot(my_data$Demands, my_data$Job_Strain, main = "", xlab = "Demands", ylab = "Job_Strain")

plot(my_data$Efforts, my_data$Imbalance, main = "", xlab = "Efforts", ylab = "Imbalance")
plot(my_data$Rewards, my_data$Imbalance, main = "", xlab = "Rewards", ylab = "Imbalance")
plot(my_data$Accidents_3y, my_data$Acc_3ys_Dic, main = "", xlab = "Accidents_3y", ylab = "Acc_3ys_Dic")

detach()


#------------------- Linear regression ---------------

modelA <- lm(Risk_Behaviors ~ . - Errors - Violations, data = my_data)
summary(modelA)
anova(modelA)

layout(matrix(1:4,2,2))
plot(modelA)
layout(matrix(1))

modelB <- lm(Risk_Behaviors ~ Age + Exercise + Efforts + Acc_3ys_Dic + Psych_Disturbance + Hours_Day, data = my_data)
summary(modelB)
anova(modelB)

layout(matrix(1:4,2,2))
plot(modelB)
layout(matrix(1))


