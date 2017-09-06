###Chapter 2 Post-hoc Analysis

###Calculate and Interpret the Results of Tukey
# Conduct ANOVA
anova_wm <- aov(wm$gain ~ wm$cond)

# View summary
summary(anova_wm)

# Conduct Tukey procedure
tukey <- TukeyHSD(anova_wm)

# Plot confidence intervals
plot(tukey)

###Bonferroni Adjusted p-values
# Use p.adjust
bonferroni_ex <- p.adjust(.005, method = "bonferroni", n = 8) 

# Print bonferroni_ex
bonferroni_ex

# Pairwise t-test
pairwise.t.test(wm$gain, wm$cond, p.adjust = "bonferroni")
