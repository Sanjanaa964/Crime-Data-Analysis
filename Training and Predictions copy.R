################################## Naive bayes ################################## 

classifer <- naiveBayes(Ward_Encoded ~ 
                          IUCR_Encoded  +
                          #Primary.Type_Encoded  +
                         # Description_Encoded +
                          Beat_Encoded +
                         # Arrest_Encoded +
                          #Domestic_Encoded +
                       # Location.Description_Encoded+
                         District_Encoded +
                        #   Longitude_Encoded +
                         Latitude_Encoded +
                        #   Avenue_Encoded+
                        #   ZipCode_Encoded+
                        #    Year_Encoded+
                          Y.Coordinate_Encoded+
                         X.Coordinate_Encoded 
                        # FBI.Code_Encoded +
                        # Community.Area_Encoded 
                       ,  
                                 data = training_set)
                          
summary(classifer)

predited_loc = predict(classifer , newdata = validation_set)
predited_loc

# Convert to factors ensuring both have the same levels
actual <- factor(validation_set$Ward_Encoded)
predicted <- factor(predited_loc, levels = levels(actual))

# Create the confusion matrix
cm1 <- confusionMatrix(predicted, actual)

cm1
# Accuracy calculation
accuracy <- (sum(diag(cm1$table)) / sum(cm1$table)*100)
print(paste("Accuracy:", accuracy))



##################################  Random forest ################################## 

classifer_1 <- randomForest(Location.Description_Encoded ~ .,  
                        data = training_set, 
                        mtry = 4, ntree= 2001, importance = TRUE)

summary(classifer_1)

predited_loc = predict(classifer_1 , newdata = validation_set)
predited_loc

# Convert to factors ensuring both have the same levels
actual <- factor(validation_set$Location.Description_Encoded)
predicted <- factor(predited_loc, levels = levels(actual))

# Create the confusion matrix
cm2 <- confusionMatrix(predicted, actual)
cm2

# Accuracy calculation
accuracy <- sum(diag(cm2$table)) / sum(cm2$table)
print(paste("Accuracy:", accuracy))



################################## SVM ##################################

classifer_3 <- svm(formula = District_Encoded ~ 
                    # IUCR_Encoded  +
                     Primary.Type_Encoded  +
                     Description_Encoded +
                     # Beat_Encoded +
                     # Arrest_Encoded +
                     # Domestic_Encoded +
                     # Location.Description_Encoded+
                    #Ward_Encoded+
                     Longitude_Encoded +
                     Latitude_Encoded +
                   #  Avenue_Encoded+
                   #   ZipCode_Encoded+
                   #   Year_Encoded+
                   Y.Coordinate_Encoded
                   # X.Coordinate_Encoded +
                   #  FBI.Code_Encoded +
                   # Community.Area_Encoded 
                   ,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Use these for the maximum accuracy
# Location.Description_Encoded ~ 
#   +                      FBI.Code_Encoded  +
#   +                      Longitude_Encoded +
#   +                      Latitude_Encoded +
#   +                      Y.Coordinate_Encoded +
#   +                      Description_Encoded 


summary(classifer_3)

predited_dis = predict(classifer_3 , newdata = validation_set)
# predited_dis

# Convert to factors ensuring both have the same levels
actual <- factor(validation_set$District_Encoded)
predicted <- factor(predited_dis, levels = levels(actual))

# Create the confusion matrix
cm3 <- confusionMatrix(predicted, actual)
# cm3

# Accuracy calculation
accuracy <- (sum(diag(cm3$table)) / sum(cm3$table)*100)
print(paste("Accuracy:", accuracy))






