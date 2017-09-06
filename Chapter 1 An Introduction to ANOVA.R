###Chapter 1 An Introduction to ANOVA

###Working Memory Experiment
# Summary statistics by group
describeBy(wm, wm$cond)

# Boxplot of iq versus cond
boxplot(wm$iq ~ wm$cond, main = "Boxplot", xlab = "Group (cond)", ylab = "IQ")

###Generate Density Plot of the F-distribution
# Create the vector x
x <- seq(from = 0, to = 2, length = 200)

# Evaluate the densities
y_1 <- df(x, 1, 1)
y_2 <- df(x, 3, 1)
y_3 <- df(x, 6, 1)
y_4 <- df(x, 3, 3)
y_5 <- df(x, 6, 3)
y_6 <- df(x, 3, 6)
y_7 <- df(x, 6, 6)

# Plot the densities
plot(x, y_1, col = 1, "l")
lines(x, y_2, col = 2)
lines(x, y_3, col = 3)
lines(x, y_4, col = 4)
lines(x, y_5, col = 5)
lines(x, y_6, col = 6)
lines(x, y_7, col = 7)

# Add the legend
legend("topright", title = "F-distributions",
       c("df = (1,1)", "df = (3,1)", "df = (6,1)", "df = (3,3)", 
         "df = (6,3)", "df = (3,6)", "df = (6,6)"), 
       col = c(1, 2, 3, 4, 5, 6, 7), lty = 1)

###Between Group Sum of Squares
# Define number of subjects in each group
n <- 20

# Calculate group means
y_j <- tapply(wm$iq, wm$cond, mean)

# Calculate the grand mean
y_t <- mean(wm$iq)

# Calculate the sum of squares
ss_a <- n*sum((y_j - y_t)^2)

###Within Groups Sum of Squares
# Create a separate vector of IQ gains for each training group
y_i1 <- subset(wm$iq, wm$cond == "8 days")
y_i2 <- subset(wm$iq, wm$cond == "12 days")
y_i3 <- subset(wm$iq, wm$cond == "17 days")
y_i4 <- subset(wm$iq, wm$cond == "19 days")

# Subtract group means from the individual values
s_1 <- y_i1 - y_j[1]
s_2 <- y_i2 - y_j[2]
s_3 <- y_i3 - y_j[3]
s_4 <- y_i4 - y_j[4]

# Put everything back together into one vector
s_t <- c(s_1, s_2, s_3, s_4)

# Calculate the sum of squares using s_t
ss_sa <- sum(s_t^2)

###Calculating the F-ratio
# Number of groups
a <- 4

# Number of subjects in each group
n <- 20

# Define degrees of freedom
df_a <- a - 1
df_sa <- a * (n - 1)

# Calculate mean squares using ss_a and ss_sa
ms_a <- ss_a / df_a
ms_sa <- ss_sa / df_sa

# Calculate the F-ratio
f_rat <- ms_a / ms_sa

###A Faster Way: ANOVA in R
## wm is already loaded

# Apply the aov function
anova_wm <- aov(wm$iq ~ wm$condition)

# Look at the summary table of the result
summary(anova_wm)

###Levene's Test
# wm is already loaded

# Levene's test
leveneTest(wm$iq, wm$cond)

# Levene's test with center = mean
leveneTest(wm$iq, wm$cond, center = mean)

