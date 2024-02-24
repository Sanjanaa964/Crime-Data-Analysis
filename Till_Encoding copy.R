df_large = read.csv("Crimes_-_2001_to_Present.csv")

# Subset
df <- df_large %>% sample_n(10000)
df <- df_large

# Filter rows with null values in 'District' column
null_district_rows <- df_large %>% filter(is.na(District))

# Sample additional rows to make a total of 10,000 rows
non_null_district_rows <- df_large %>% filter(!is.na(District)) %>% sample_n(10000 - nrow(null_district_rows), replace = TRUE)

# Combine the rows
df <- bind_rows(null_district_rows, non_null_district_rows)

# Check the count of null values in 'District' column
cat("Number of Null Values in 'District' column in sampled data:", sum(is.na(df$District)), "\n")


#################   DATA PREPROCESSING   ######################

# Making the Block column into three different columns naming ZipCode, Direction, Avenue

# For extracting ZipCode
df$ZipCode <- str_extract(df$Block, "^.{5}")
df$Direction <- str_extract(df$Block, "(?<=^.{5}\\s)[NSEW]")
df$Avenue <- str_extract(df$Block, "(?<=^.{5}\\s[NSEW]\\s).+")

df_large$Location.Description[df_large$Location.Description == ""] <- NA

# Remove null values from other columns 
columns_to_check <- setdiff(names(df), "Ward")
df <- df[complete.cases(df[, columns_to_check]), ]


# Null Check
for (col_name in names(df_large)) {
  na_count <- sum(is.na(df_large[[col_name]]))
  cat(col_name, ": ", na_count, "\n")
}

# Unique Value check 
unique_value_counts <- sapply(df, function(x) length(unique(x)))
column_names <- names(df)
cat("Column Name : Number of Unique Values\n")
for (i in seq_along(unique_value_counts)) {
  cat(column_names[i], ":", unique_value_counts[i], "\n")
}
#################   ENCODING   ######################

#Encoding Columns
df$IUCR_Encoded <- as.numeric(factor(df$IUCR, levels = unique(df$IUCR)))
df$Primary.Type_Encoded <- as.numeric(factor(df$Primary.Type, levels = unique(df$Primary.Type)))
df$Description_Encoded <- as.numeric(factor(df$Description, levels = unique(df$Description)))
df$Beat_Encoded <- as.numeric(factor(df$Beat, levels = unique(df$Beat)))
df$Arrest_Encoded <- ifelse(df$Arrest == "true", 1, 0)
df$Domestic_Encoded <- ifelse(df$Domestic == "true", 1, 0)
df$District_Encoded <- as.numeric(factor(df$District, levels = unique(df$District)))
df$Location.Description_Encoded <- as.numeric(factor(df$Location.Description, levels = unique(df$Location.Description)))
df$Avenue_Encoded <- as.numeric(factor(df$Avenue, levels = unique(df$Avenue)))
df$ZipCode_Encoded <- as.numeric(factor(df$ZipCode, levels = unique(df$ZipCode)))
df$Longitude_Encoded <- as.numeric(factor(df$Longitude, levels = unique(df$Longitude)))
df$Latitude_Encoded <- as.numeric(factor(df$Latitude, levels = unique(df$Latitude)))
df$Year_Encoded <- as.numeric(factor(df$Year, levels = unique(df$Year)))
df$Y.Coordinate_Encoded <- as.numeric(factor(df$Y.Coordinate, levels = unique(df$Y.Coordinate)))
df$X.Coordinate_Encoded <- as.numeric(factor(df$X.Coordinate, levels = unique(df$X.Coordinate)))
df$FBI.Code_Encoded <- as.numeric(factor(df$FBI.Code, levels = unique(df$FBI.Code)))
df$Community.Area_Encoded <- as.numeric(factor(df$Community.Area, levels = unique(df$Community.Area)))

# Dependant variable
df$Ward_Encoded <- as.numeric(factor(df$Ward, levels = unique(df$Ward)))

# Index and columns
for (i in 1:ncol(df)) {
  cat("Index:", i, "- Column Name:", names(df)[i], "\n")
}

# Encoded Dataset 
df_encoded <- df[, 26:43]


# Train, validation and test

#TEST SET
test_set <- df_encoded %>% filter(is.na(Ward_Encoded))

#Validation and Training sets
df_remaining <- df_encoded %>% filter(!is.na(Ward_Encoded))
set.seed(123)
training_indices <- sample(1:nrow(df_remaining), size = 0.9 * nrow(df_remaining))
# Create the training and validation sets
training_set <- df_remaining[training_indices, ]
validation_set <- df_remaining[-training_indices, ]

# Index and columns
for (i in 1:ncol(df_encoded)) {
  cat("Index:", i, "- Column Name:", names(df_encoded)[i], "\n")
}

# Feature Scaling 
training_set[, -18] = scale(training_set[, -18])
validation_set[, -18] = scale(validation_set[, -18])
test_set[, -18] = scale(test_set[, -18])

