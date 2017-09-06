###Chapter 3 Between Groups Factorial ANOVA

###Data Exploration with a Barplot
# Create 6 subgroups
ab_groups <- tapply(ab$errors, list(ab$driving, ab$conversation), sum)

# Make the required barplot
barplot(ab_groups, beside = TRUE, 
        col = c("orange", "blue"), 
        main = "Driving Errors", 
        xlab = "Conversation Demands", 
        ylab = "Errors")

# Add the legend
legend("topright", c("Difficult","Easy"), 
       title = "Driving", 
       fill = c("orange", "blue"))

###The Homogeneity of Variance Assumption
## The data frame ab is preloaded in your workspace

# Test the homogeneity of variance assumption
leveneTest(ab$errors ~ ab$driving * ab$conversation)

###The Factorial ANOVA
## The data frame ab is preloaded in your workspace

# Factorial ANOVA
ab_model <- aov(ab$errors ~ ab$driving * ab$conversation)

# Get the summary table
summary(ab_model)

###The Interaction Effect (1)
# Create the two subsets
ab_1 <- subset(ab, ab$driving == "Easy")
ab_2 <- subset(ab, ab$driving == "Difficult")

# Perform the one-way ANOVA analysis for both subsets
aov_ab_1 <- aov(ab_1$errors ~ ab_1$conversation)
aov_ab_2 <- aov(ab_2$errors ~ ab_2$conversation)

# Get the summary tables for aov_ab_1 and aov_ab_2
summary(aov_ab_1)
summary(aov_ab_2)

###The Effect Sizes
## aov_ab_1 and aov_ab_2 are preloaded in your workspace

# Calculate the etaSquared for the easy driving case
etaSquared(aov_ab_1, anova = TRUE)

# Calculate the etaSquared for the difficult driving case
etaSquared(aov_ab_2, anova = TRUE)

###Pairwise Comparisons
## aov_ab_1 and aov_ab_2 are preloaded in your workspace

# Tukey for easy driving
TukeyHSD(aov_ab_1)

# Tukey for difficult driving
TukeyHSD(aov_ab_2)

