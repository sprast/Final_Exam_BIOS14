#Exam 2025
rm(list = ls())

male_CS = read.csv("../ao9018/Desktop/BIOS14/Data/male_CS.csv")
male_CV = read.csv("../ao9018/Desktop/BIOS14/Data/male_CV.csv")
female_CS = read.csv("../ao9018/Desktop/BIOS14/Data/female_CS.csv")
female_CV = read.csv("../ao9018/Desktop/BIOS14/Data/female_CV.csv")

library(dplyr)
all_data = bind_rows(male_CS, male_CV, female_CS, female_CV)
names(all_data)

# Basic data checks
str(all_data)
summary(all_data)

##Explore data using scatter plots, histograms, correlations
library(psych)
pairs.panels(all_data) 
all_data$sex <- as.factor(all_data$sex)

##Histograms for visualization of data 
par(mfrow = c(2, 2))  

# Histogram for Species CS - Female
hist(all_data$tbl[all_data$sp == "CS" & all_data$sex == "Female"], 
     main = "CS - Female", 
     xlab = "Total Body Length (mm)", 
     col = "pink", 
     border = "black", 
     breaks = 20)

# Histogram for Species CS - Male
hist(all_data$tbl[all_data$sp == "CS" & all_data$sex == "Male"], 
     main = "CS - Male", 
     xlab = "Total Body Length (mm)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)

# Histogram for Species CV - Female
hist(all_data$tbl[all_data$sp == "CV" & all_data$sex == "Female"], 
     main = "CV - Female", 
     xlab = "Total Body Length (mm)", 
     col = "pink", 
     border = "black", 
     breaks = 20)

# Histogram for Species CV - Male
hist(all_data$tbl[all_data$sp == "CV" & all_data$sex == "Male"], 
     main = "CV - Male", 
     xlab = "Total Body Length (mm)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)

# Histogram for Species CS - Female
hist(all_data$fwl[all_data$sp == "CS" & all_data$sex == "Female"], 
     main = "CS - Female", 
     xlab = "Forewing Length (mm)", 
     col = "pink", 
     border = "black", 
     breaks = 20)

# Histogram for Species CS - Male
hist(all_data$fwl[all_data$sp == "CS" & all_data$sex == "Male"], 
     main = "CS - Male", 
     xlab = "Forewing Length (mm)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)

# Histogram for Species CV - Female
hist(all_data$fwl[all_data$sp == "CV" & all_data$sex == "Female"], 
     main = "CV - Female", 
     xlab = "Forewing Length (mm)", 
     col = "pink", 
     border = "black", 
     breaks = 20)

# Histogram for Species CV - Male
hist(all_data$fwl[all_data$sp == "CV" & all_data$sex == "Male"], 
     main = "CV - Male", 
     xlab = "Forewing Length (mm)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)

par(mfrow = c(1, 1))

##Investigation for correlations
cor_matrix <- cor(all_data[, c("tbl", "fwl", "hwl", "abl", "thorl", "thorw", "lifespan", "cop")], use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

##Calculations for Mean, Median, SE, and SD
library(dplyr)

# Calculate summary statistics: Mean, Median, SE, and SD
summary_stats <- all_data %>%
  group_by(sex, sp) %>%
  summarise(
    mean_fwl = mean(fwl, na.rm = TRUE),      
    median_fwl = median(fwl, na.rm = TRUE),  
    se_fwl = sd(fwl, na.rm = TRUE) / sqrt(n()), 
    sd_fwl = sd(fwl, na.rm = TRUE),          
    mean_tbl = mean(tbl, na.rm = TRUE),      
    median_tbl = median(tbl, na.rm = TRUE),  
    se_tbl = sd(tbl, na.rm = TRUE) / sqrt(n()), 
    sd_tbl = sd(tbl, na.rm = TRUE)           
  )

# Create table 
library(gt)
table <- summary_stats %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics of Forewing Length and Total Body Length",
    subtitle = "Grouped by Sex and Species"
  ) %>%
  fmt_number(
    columns = vars(mean_fwl, median_fwl, se_fwl, sd_fwl, mean_tbl, median_tbl, se_tbl, sd_tbl),
    decimals = 2
  ) %>%
  cols_label(
    sex = "Sex",
    sp = "Species",
    mean_fwl = "Mean FWL",
    median_fwl = "Median FWL",
    se_fwl = "SE FWL",
    sd_fwl = "SD FWL",
    mean_tbl = "Mean TBL",
    median_tbl = "Median TBL",
    se_tbl = "SE TBL",
    sd_tbl = "SD TBL"
  ) %>%
  tab_source_note(
    source_note = "All measurements are in millimeters (mm). FWL = Forewing Length, TBL = Total Body Length. SE = Standard Error, SD = Standard Deviation."
  ) %>%
  tab_footnote(
    footnote = "Species: CS = Calopteryx splendens, CV = Calopteryx virgo.",
    locations = cells_column_labels(vars(sp))
  )

print(table)

all_data$sex <- as.factor(all_data$sex)
all_data$sp <- as.factor(all_data$sp)

# Combine sex and species into a single interaction variable
all_data$sex_sp <- interaction(all_data$sex, all_data$sp, sep = ".")

## 2-way ANOVA for fwl with interaction of sex, sp
anova_fwl_interaction <- aov(fwl ~ sex * sp, data = all_data)
summary(anova_fwl_interaction)
library(ggplot2)
library(dplyr)
boxplot(fwl ~ sex_sp, data = all_data, 
        col = c("red", "blue", "pink", "turquoise"),
        xlab = "Sex and Species", ylab = "Forewing Length (mm)",
        cex.lab = 1.5, # Increase size of axis labels
        cex.axis = 1.2) # Increase size of axis tick labels
axis(1, at = 1:4, labels = c("Female.CS", "Male.CS", "Female.CV", "Male.CV"), cex.axis = 1.2) #for better readability

# 2-way ANOVA for tbl with interaction of sex, sp
anova_tbl_interaction <- aov(tbl ~ sex * sp, data = all_data)
summary(anova_tbl_interaction)
boxplot(tbl ~ sex_sp, data = all_data, 
        col = c("red", "blue", "pink", "turquoise"),
        xlab = "Sex and Species", ylab = "Total body length (mm)",
        cex.lab = 1.5, # Increase size of axis labels
        cex.axis = 1.2) # Increase size of axis tick labels
axis(1, at = 1:4, labels = c("Female.CS", "Male.CS", "Female.CV", "Male.CV"), cex.axis = 1.2) #for better readability

##Interaction graph fwl
# Summarize data for plotting
dat_summary <- all_data %>%
  group_by(sex, sp) %>%
  summarise(
    mean_fwl = mean(fwl, na.rm = TRUE),                
    se_fwl = sd(fwl, na.rm = TRUE) / sqrt(n())         
  )

# interaction plot 
ggplot(dat_summary, aes(x = sex, y = mean_fwl, color = sp, group = sp)) +
  geom_point(size = 3) +
  geom_line() +           
  geom_errorbar(aes(ymin = mean_fwl - se_fwl, ymax = mean_fwl + se_fwl), width = 0.2) + 
  labs(
       x = "Sex",
       y = "Mean Forewing Length (mm)",
       color = "Species") +  
  theme_classic() +  
  theme(
    axis.title.x = element_text(size = 20),       
    axis.title.y = element_text(size = 20),       
    axis.text.x = element_text(size = 18),       
    axis.text.y = element_text(size = 18),        
    legend.title = element_text(size = 16),      
    legend.text = element_text(size = 16)        
  )


## Interaction graph tbl
# Summarize data for plotting
dat_summary_tbl <- all_data %>%
  group_by(sex, sp) %>%
  summarise(
    mean_tbl = mean(tbl, na.rm = TRUE),                
    se_tbl = sd(tbl, na.rm = TRUE) / sqrt(n())         
  )

#Interaction plot
ggplot(dat_summary_tbl, aes(x = sex, y = mean_tbl, color = sp, group = sp)) +
  geom_point(size = 3) +  
  geom_line() +           
  geom_errorbar(aes(ymin = mean_tbl - se_tbl, ymax = mean_tbl + se_tbl), width = 0.2) + 
  labs(
    x = "Sex",
    y = "Mean Total Body Length (mm)",
    color = "Species") +  
  theme_classic() +  
  theme(
    axis.title.x = element_text(size = 20),       
    axis.title.y = element_text(size = 20),       
    axis.text.x = element_text(size = 18),        
    axis.text.y = element_text(size = 18),        
    legend.title = element_text(size = 16),       
    legend.text = element_text(size = 16)         
  )


##Allometric analysis
##Investigate how fwl scales with tbl across species and sexes:
lm_fwl_1 <- lm(log(fwl) ~ log(tbl), data = all_data)
summary(lm_fwl_1)

lm_fwl_2 <- lm(log(fwl) ~ log(tbl) *sex, data = all_data)
summary(lm_fwl_2)

lm_fwl_3 <- lm(log(fwl) ~ log(tbl) * sp * sex, data = all_data) ##BEST
summary(lm_fwl_3)

AIC(lm_fwl_1, lm_fwl_2, lm_fwl_3)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(lm_fwl_3)
par(mfrow = c(1, 1))

# Allometric plot
library(ggplot2)
ggplot(all_data, aes(x = log_tbl, y = log_fwl, color = interaction(sex, sp))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
       x = "Log(Total Body Length)",
       y = "Log(Forewing Length)") +
  theme_test()

# Color mapping
custom_colors <- c(
  "Female.CS" = "red",        # Female CS = red
  "Female.CV" = "pink",       # Female CV = pink
  "Male.CS" = "blue",         # Male CS = blue
  "Male.CV" = "turquoise"     # Male CV = turquoise
)

ggplot(all_data, aes(x = log_tbl, y = log_fwl, color = interaction(sex, sp))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Log(Total Body Length)",
    y = "Log(Forewing Length)",
    color = ""  
  ) +
  scale_color_manual(values = custom_colors) +  
  theme_test() +
  theme(
    axis.title.x = element_text(size = 16),       
    axis.title.y = element_text(size = 16),       
    axis.text.x = element_text(size = 14),        
    axis.text.y = element_text(size = 14),        
    legend.title = element_text(size = 14),       
    legend.text = element_text(size = 12)         
  )

##Test if fwl influences mating success (cop) differently by sex and species
glm_cop_1 <- glm(cop ~ fwl, data = all_data, family = binomial)
summary(glm_cop_1)

glm_cop_2 <- glm(cop ~ fwl + tbl, data = all_data, family = binomial)
summary(glm_cop_2)

glm_cop_3 <- glm(cop ~ fwl + tbl + sex, data = all_data, family = binomial) ##BEST
summary(glm_cop_3)

glm_cop_4 <- glm(cop ~ fwl + tbl + sex + sp, data = all_data, family = binomial) 
summary(glm_cop_4)

glm_cop_5 <- glm(cop ~ fwl*sex + tbl*sex, data = all_data, family = binomial)
summary(glm_cop_5)

glm_cop_6 <- glm(cop ~ fwl*sp + tbl*sp, data = all_data, family = binomial) 
summary(glm_cop_6)

glm_cop_7 <- glm(cop ~ fwl*sp + tbl + sex, data = all_data, family = binomial)
summary(glm_cop_7)

AIC(glm_cop_1, glm_cop_2, glm_cop_3, glm_cop_4, glm_cop_5, glm_cop_6, glm_cop_7)

##GLMM for random effects
install.packages("lme4")
install.packages(c("Matrix", "lattice"))
library(Matrix)
library(lme4)
# Fit a GLMM with year as a random effect
glmm_cop_1 <- glmer(cop ~ fwl + tbl + sex + (1 | year),  ##BEST
                  data = all_data, 
                  family = binomial)
summary(glmm_cop_1)

glmm_cop_2 <- glmer(cop ~ fwl + tbl + sex + (1 | year) + (1 | id), 
                    data = all_data, 
                    family = binomial)
summary(glmm_cop_2)

AIC(glmm_cop_1, glmm_cop_2)







