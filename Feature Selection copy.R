# Compute the correlation matrix
cor_matrix <- cor(df_encoded , use = "complete.obs")

# Optionally, set the diagonal to NA to ignore self-correlations
diag(cor_matrix) <- NA

# Find the most correlated pairs
# Reshape the correlation matrix
cor_melt <- reshape2::melt(cor_matrix)

# Sort by absolute correlation values
cor_melt <- cor_melt[order(-abs(cor_melt$value)), ]

# Remove NAs
cor_melt <- na.omit(cor_melt)

# Get the most correlated pairs
head(cor_melt)

cor_melt


set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
# data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(training_set)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Index and columns
for (i in 1:ncol(df_encoded)) {
  cat("Index:", i, "- Column Name:", names(df_encoded)[i], "\n")
}

