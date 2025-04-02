df = read.csv("Crimes.csv")

########### Libraries ##########

library(ggplot2)
library(stringr)
library(e1071)
library(naivebayes)


set.seed(123)

# Randomly select 100,000 rows from df
filtered_df <- filtered_df %>% 
  sample_n(size = 100000, replace = FALSE)


#################   DATA PREPROCESSING   ######################

# Making the Block column into three different columns naming ZipCode, Direction, Avenue

filtered_df$ZipCode <- str_extract(filtered_df$Block, "^.{5}")
filtered_df$Direction <- str_extract(filtered_df$Block, "(?<=^.{5}\\s)[NSEW]")
filtered_df$Avenue <- str_extract(filtered_df$Block, "(?<=^.{5}\\s[NSEW]\\s).+")

df$Location.Description[df$Location.Description == ""] <- NA

# Remove null values from other columns 
columns_to_check <- setdiff(names(filtered_df), "Location.Description")
filtered_df <- filtered_df[complete.cases(filtered_df[, columns_to_check]), ]

# Null Check
for (col_name in names(filtered_df)) {
  na_count <- sum(is.na(filtered_df[[col_name]]))
  cat(col_name, ": ", na_count, "\n")
}

######### Encoding ##############

filtered_df$IUCR <- as.factor(filtered_df$IUCR)
filtered_df$Primary.Type <- as.factor(filtered_df$Primary.Type)
filtered_df$Description <- as.factor(filtered_df$Description)
filtered_df$Location.Description <- as.factor(filtered_df$Location.Description)
filtered_df$Arrest <- as.factor(filtered_df$Arrest)
filtered_df$Domestic <- as.factor(filtered_df$Domestic)
filtered_df$Beat <- as.factor(filtered_df$Beat)
filtered_df$District <- as.factor(filtered_df$District)
filtered_df$Ward <- as.factor(filtered_df$Ward)
filtered_df$Community.Area <- as.factor(filtered_df$Community.Area)
filtered_df$FBI.Code <- as.factor(filtered_df$FBI.Code)
filtered_df$X.Coordinate <- as.factor(filtered_df$X.Coordinate)
filtered_df$Y.Coordinate <- as.factor(filtered_df$Y.Coordinate)
filtered_df$Year <- as.factor(filtered_df$Year)
filtered_df$Updated.On <- as.factor(filtered_df$Updated.On)
filtered_df$Latitude <- as.factor(filtered_df$Latitude)
filtered_df$Longitude <- as.factor(filtered_df$Longitude)
filtered_df$ZipCode <- as.factor(filtered_df$ZipCode)
filtered_df$Direction <- as.factor(filtered_df$Direction)
filtered_df$Avenue <- as.factor(filtered_df$Avenue)

df_dataset <- filtered_df

df_dataset$IUCR_Encoded <- as.numeric(filtered_df$IUCR)
df_dataset$Primary.Type_Encoded <- as.numeric(filtered_df$Primary.Type)
df_dataset$Description_Encoded <- as.numeric(filtered_df$Description)
df_dataset$Arrest_Encoded <- as.numeric(filtered_df$Arrest)
df_dataset$Domestic_Encoded <- as.numeric(filtered_df$Domestic)
df_dataset$Beat_Encoded <- as.numeric(filtered_df$Beat)
df_dataset$District_Encoded <- as.numeric(filtered_df$District)
df_dataset$Community.Area_Encoded <- as.numeric(filtered_df$Community.Area)
df_dataset$FBI.Code_Encoded <- as.numeric(filtered_df$FBI.Code)
df_dataset$X.Coordinate_Encoded <- as.numeric(filtered_df$X.Coordinate)
df_dataset$Y.Coordinate_Encoded <- as.numeric(filtered_df$Y.Coordinate)
df_dataset$Year_Encoded <- as.numeric(filtered_df$Year)
df_dataset$Updated.On_Encoded <- as.numeric(filtered_df$Updated.On)
df_dataset$Latitude_Encoded <- as.numeric(filtered_df$Latitude)
df_dataset$Longitude_Encoded <- as.numeric(filtered_df$Longitude)
df_dataset$ZipCode_Encoded <- as.numeric(filtered_df$ZipCode)
df_dataset$Direction_Encoded <- as.numeric(filtered_df$Direction)
df_dataset$Avenue_Encoded <- as.numeric(filtered_df$Avenue)
df_dataset$Ward_Encoded <- as.numeric(filtered_df$Ward)
df_dataset$Location.Description_Encoded <- as.numeric(filtered_df$Location.Description)

# Index and columns
for (i in 1:ncol(df_dataset)) {
  cat("Index:", i, "- Column Name:", names(df_dataset)[i], "\n")
}

# Encoded Dataset 
df_encoded <- df_dataset[, 26:45]


####### Splitting Dataset #########

# Test set
test_set <- df_encoded %>% filter(is.na(Location.Description_Encoded))

# Validation and Training sets
df_remaining <- df_encoded %>% filter(!is.na(Location.Description_Encoded))
set.seed(123)
training_indices <- sample(1:nrow(df_remaining), size = 0.9 * nrow(df_remaining))
training_set <- df_remaining[training_indices, ]
validation_set <- df_remaining[-training_indices, ]


for (i in 1:ncol(df_encoded)) {
  cat("Index:", i, "- Column Name:", names(df_encoded)[i], "\n")
}


########## Feature Scaling ############
training_set[, -20] = scale(training_set[, -20])
validation_set[, -20] = scale(validation_set[, -20])
test_set[, -20] = scale(test_set[, -20])


for (col_name in names(test_set)) {
  na_count <- sum(is.na(test_set[[col_name]]))
  cat(col_name, ": ", na_count, "\n")
}


###### Correlation Matrix ######

library(corrplot)

# Calculate the correlation matrix
correlation_matrix <- cor(training_set)

# Create the correlation plot
corrplot(correlation_matrix, method = "color", type = "full", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Compute the correlation matrix
cor_matrix <- cor(df_encoded , use = "complete.obs")

diag(cor_matrix) <- NA
cor_melt <- reshape2::melt(cor_matrix)
cor_melt <- cor_melt[order(-abs(cor_melt$value)), ]
cor_melt <- na.omit(cor_melt)
head(cor_melt)
cor_melt

set.seed(7)

library(mlbench)
library(caret)

correlationMatrix <- cor(training_set)
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Index and columns
for (i in 1:ncol(df_encoded)) {
  cat("Index:", i, "- Column Name:", names(df_encoded)[i], "\n")
}


###### Naive Bayes ######
classifer <- naiveBayes(Location.Description_Encoded ~
                          IUCR_Encoded  +
                          Primary.Type_Encoded  +
                          Beat_Encoded +
                          Arrest_Encoded +
                          Domestic_Encoded +
                          District_Encoded+
                          Ward_Encoded+
                          Avenue_Encoded+
                          ZipCode_Encoded+
                          Year_Encoded+
                          Y.Coordinate_Encoded +
                          X.Coordinate_Encoded +
                          FBI.Code_Encoded +
                          Community.Area_Encoded +
                          Latitude_Encoded
                        ,
                        data = training_set)

summary(classifer)

predited_loc = predict(classifer , newdata = validation_set)
predited_loc

# Convert to factors ensuring both have the same levels
actual <- factor(validation_set$Location.Description_Encoded)
predicted <- factor(predited_loc, levels = levels(actual))

# Create the confusion matrix
cm1 <- confusionMatrix(predicted, actual)
cm1
# Accuracy calculation
accuracy <- (sum(diag(cm1$table)) / sum(cm1$table)*100)
print(paste("Accuracy:", accuracy))
