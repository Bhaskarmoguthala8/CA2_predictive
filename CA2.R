install.packages("readxl")
library("readxl")
CA2<-read_excel("C:/Users/bhask/CA2/CA2_predictive/Dataset_2024.xlsx")
str(CA2)
View(CA2)
#Renaming the variables
colnames(CA2)[colnames(CA2) == "Age (years)"] <- "Age_in_Years"
colnames(CA2)[colnames(CA2) == "Body fat (%)"] <- "Body_fat_in_percentage"
colnames(CA2)[colnames(CA2) == "Chest circumference (cm)"] <- "Chest_circumference_cm"
colnames(CA2)[colnames(CA2) == "Density (g/cm³)"] <- "Density_in_g_cm"
colnames(CA2)[colnames(CA2) == "Knee circumference (cm)"] <- "Knee_circumference_cm"
colnames(CA2)[colnames(CA2) == "Weight (lbs)"] <- "Weight_in_lbs"
str(CA2)
# Examine initial linearity between variables in the dataset
library(psych)
windows(20,10)

pairs.panels(CA2,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

windows(20,12)
par(mfrow= c(4,2))
attach(CA2)

scatter.smooth(x = Age_in_Years,
               y = Body_fat_in_percentage,
               xlab = "Age_in_Years   (,000)",
               ylab = "Body_fat %", main = "Correlation of Body_fat ~ Age")

scatter.smooth(x =Chest_circumference_cm,
               y = Body_fat_in_percentage,
               xlab = "Chest_circumference_cm  (,000)",
               ylab = "Body_fat %", main = "Correlation of Body_fat ~ Chest_circumference")
scatter.smooth(x = Density_in_g_cm,
               y = Body_fat_in_percentage,
               xlab = "Density_in_g/cm³   (,000)",
               ylab = "Body_fat %", main = "Correlation of Body_fat ~ Density_in_g/cm³")
scatter.smooth(x = Knee_circumference_cm,
               y = Body_fat_in_percentage,
               xlab ="Knee_circumference_cm   (,000)",
               ylab = "Body_fat %", main = "Correlation of Body_fat ~ Knee_circumference_cm")
scatter.smooth(x = Weight_in_lbs,
               y = Body_fat_in_percentage,
               xlab = "Weight_in_lbs   (,000)",
               ylab = "Body_fat %", main = "Correlation of Body_fat ~ Weight_in_lbs")

# examining the correlation between the dependent and independent variables
cor(CA2)
paste("Correlation for Body_fat and Age: ", round(cor(Body_fat_in_percentage, Age_in_Years ),2))
paste("Correlation for Body_fat and chest circumference: ", round(cor(Body_fat_in_percentage, Chest_circumference_cm),2))
paste("Correlation for Body_fat and density in g/cm: ", round(cor(Body_fat_in_percentage, Density_in_g_cm),2))
paste("Correlation for Body_fat and knee circumferrence: ", round(cor(Body_fat_in_percentage, Knee_circumference_cm),2))
paste("Correlation for Body_fat and weight: ", round(cor(Body_fat_in_percentage, Weight_in_lbs),2))

windows(20,12)
par(mfrow= c(3,2))
boxplot(Age_in_Years, main="Age_in_Years")
boxplot(Body_fat_in_percentage, main="Body_fat_in_percentage")
boxplot(Chest_circumference_cm, main="Chest_circumference_cm")
boxplot(Density_in_g_cm, main="Density_in_g_cm")
boxplot(Knee_circumference_cm , main="Knee_circumference_cm ")
boxplot(Weight_in_lbs , main="Weight_in_lbs ")

# Body_fat variable contains outliers.
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Body_fat_in_percentage)$out # outlier values.
paste("Body_fat_in_percentage outliers: ", paste(outlier_values, sep =", "))
# Remove Body_fat_in_percentage outliers
CA2 <- subset(CA2,
                 CA2$Body_fat_in_percentage != 47.5)
# chest_circumference variable contains outliers.
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Chest_circumference_cm)$out # outlier values.
paste("Chest_circumference_cm outliers: ", paste(outlier_values, sep =", "))
# Remove Chest_circumference_cm outliers
CA2 <- subset(CA2,
              CA2$Chest_circumference_cm != 128.3
              &CA2$Chest_circumference_cm != 136.2 )
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Density_in_g_cm )$out # outlier values.
paste("Density_in_g_cm  outliers: ", paste(outlier_values, sep =", "))
# Remove Density_in_g_cm  outliers
CA2 <- subset(CA2,
              CA2$Density_in_g_cm  != 0.995)
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Knee_circumference_cm )$out # outlier values.
paste("Knee_circumference_cm  outliers: ", paste(outlier_values, sep =", "))
# Remove Knee_circumference_cm  outliers
CA2 <- subset(CA2,
              CA2$Knee_circumference_cm  != 49.1
              &CA2$Knee_circumference_cm  !=45
              &CA2$Knee_circumference_cm  != 46)
# Using boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(Weight_in_lbs )$out # outlier values.
paste("Weight_in_lbs   outliers: ", paste(outlier_values, sep =", "))
# Remove Weight_in_lbs  outliers
CA2 <- subset(CA2,
              CA2$Weight_in_lbs  != 363.15
              &CA2$Weight_in_lbs   != 262.75
              )





