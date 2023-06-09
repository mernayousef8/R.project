getwd()
setwd("E:\\Dr.Hamada")
dataset <- read.csv("G4_howell.csv")

# Data Cleaning
print(complete.cases(dataset))
# Returns all rows false becaues overweight column is all NA So check each column

print(any(is.na(dataset$age)))
# Age is complete

print(any(is.na(dataset$sex)))
# Sex is complete

print(any(is.na(dataset$weight)))
# Weight is not complete

print(any(is.na(dataset$height)))
# height is complete

# Cleaning weight column
dataset$weight <- gsub("kg", "", dataset$weight)
dataset$weight <- as.numeric(dataset$weight)

# Filling weight column
library(mice)
imputations <- mice(dataset, m = 6, meth = c("", "", "pmm", "", ""), maxit = 5)
dataset <- complete(imputations, 6)
# We can use mean
mean_heigh <- mean(dataset$height)
dataset[is.na(dataset$weight) & dataset$sex == "F", "weight"] <- mean_heigh


# Computing Overweight column , first we compute new column bmi = weight (kg) / (height / 100) ^ 2
dataset$BMI <- dataset$weight / (dataset$height / 100)^2
# If BMI >= 18 overweighted , else no
dataset$Overweight <- as.factor(ifelse(dataset$BMI >= 18, "Yes", "No"))

# Recode column
dataset$fat[dataset$Overweight == "Yes"] <- 1
dataset$fat[dataset$Overweight == "No"] <- 0
# Recode of code
dataset$is_fat[dataset$fat == 1] <- "Yes"
dataset$is_fat[dataset$fat == 0] <- "No"

# Subset the dataset
sub1 <- dataset[dataset$sex == "M" & dataset$is_fat == "Yes", ]
sub2 <- dataset[dataset$Overweight == "No", c("sex", "is_fat", "BMI")]
sub3 <- dataset[dataset$fat == 0 & dataset$is_fat == "No", -c(7, 8)]

# Order the dataset
order1 <- dataset[order(dataset$age), ]
order2 <- dataset[order(dataset$age, dataset$height), ]
order3 <- dataset[order(-dataset$weight), ]

print(summary(dataset))
print(head(dataset, 10))
print(tail(dataset, 10))


# Data Visualization
library(ggplot2)
Scatter_diagram <- ggplot(dataset, aes(x = weight, y = BMI)) +
    geom_point() +
    labs(x = "Weight", y = "BMI", title = "Weight , BMI Relationship")

Scatter_diagram2 <- ggplot(dataset, aes(x = height, y = BMI)) +
    geom_point() +
    labs(x = "Height", y = "BMI", title = "Height , BMI Relationship") +
    stat_smooth(se = TRUE)

Histogram_diagram <- ggplot(dataset, aes(weight)) +
    geom_histogram(binwidth = 12, color = "black", fill = "grey", alpha = 0.8) +
    labs(x = "BMI", y = "Frequency", title = "BMI Frequency")

Bar_chart <- ggplot(dataset, aes(x = Overweight, fill = is_fat)) +
    geom_bar() +
    labs(y = "Count") +
    ggtitle("Rate")
