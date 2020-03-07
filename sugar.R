# Import dataset
library(car)
sugar = read.csv('sugar.csv')
sugar

# Check to see if data is normally distributed
hist(sugar$Energy, xlab = "Energy Levels", main = "Distribution of Energy Levels", 
     right = FALSE, col = "turquoise", breaks=0:11)
table(sugar$Energy)

# Display mean, standard deviation, and IQR for all energy levels
mean(sugar$Energy)
sd(sugar$Energy)
IQR(sugar$Energy)

# Investigating explanatory variable #1: Diet Type
# Check distribution
barplot(table(sugar$Diet), ylab = "Frequency", main = "Counts of Diet Type", 
        col = "dark blue")
boxplot(Energy ~ Diet, data = sugar, xlab = "Diet Type", 
        ylab = "Energy Level", main = "Energy Levels by Diet Type", 
        col = c("orchid", "pale green", "salmon"))

# Find mean, sd, and IQR of energy levels for people following a Western Diet
western <- sugar[sugar$Diet == "Western", ]
mean(western$Energy, na.rm = TRUE)
sd(western$Energy, na.rm = TRUE)
IQR(western$Energy)

# Find mean, sd, and IQR of energy levels for people following a Paleo/Keto Diet
paleo <- sugar[sugar$Diet == "Paleo/Keto", ]
mean(paleo$Energy, na.rm = TRUE)
sd(paleo$Energy, na.rm = TRUE)
IQR(paleo$Energy)

# Find mean, sd, and IQR of energy levels for people following a Vegan/Vegetarian diet
vegan <- sugar[sugar$Diet == "Vegan/Vegetarian", ]
mean(vegan$Energy, na.rm = TRUE)
sd(vegan$Energy, na.rm = TRUE)
IQR(vegan$Energy)

#Investigating explanatory variable #2: Sugar Intake
#Check distribution
barplot(table(sugar$Sugar), ylab = "Frequency", main = "Counts of Sugar Intake Levels", 
        col = "dark blue")
boxplot(Energy ~ Sugar, data = sugar, xlab = "Sugar Intake", 
        ylab = "Energy Level", main = "Energy Levels by Sugar", 
        col = c("orchid", "pale green", "salmon"))

# Find mean, sd, and IQR of energy levels for people with a high sugar intake
high <- sugar[sugar$Sugar == "High", ]
mean(high$Energy, na.rm = TRUE)
sd(high$Energy, na.rm = TRUE)
IQR(high$Energy)

# Find mean, sd, and IQR of energy levels for people with a medium sugar intake
medium <- sugar[sugar$Sugar == "Medium", ]
mean(medium$Energy, na.rm = TRUE)
sd(medium$Energy, na.rm = TRUE)
IQR(medium$Energy)

# Find mean, sd, and IQR of energy levels for people with a low sugar intake
low <- sugar[sugar$Sugar == "Low", ]
mean(low$Energy, na.rm = TRUE)
sd(low$Energy, na.rm = TRUE)
IQR(low$Energy)

# Check assumptions using a Levene's test
sugar_diet_energy<-lm(Energy~Diet*Sugar, data=sugar)
leveneTest(sugar_diet_energy) # Homoskedasticity
plot(lm(Energy~Diet*Sugar,data=sugar),2) # Normalization

# Run 2-way ANOVA with interaction
fit_interaction<-lm(Energy~Diet*Sugar, data=sugar)
Anova(fit_interaction)

# Get all means
model.tables(aov(fit_interaction), type = "means")
