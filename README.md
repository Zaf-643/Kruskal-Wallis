# Kruskal-Wallis
# Loading an example dataset from R
data()
my_data <- PlantGrowth

# Showing the group levels
levels(my_data$group)

# Computing summary statistics by groups
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

# Visualizing the data using box plots
library("ggpubr")
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

# Visualizing the data using mean plot
ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

# Checking the outliers

# install.packages("rstatix")
library(rstatix)

my_data %>% 
  group_by(group) %>%
  identify_outliers(weight)

# Checking the normality assumption

# Building the linear model
model  <- lm(weight ~ group, data = my_data)
# Creating a QQ plot of residuals
ggqqplot(residuals(model))
# Computing Shapiro-Wilk test of normality
shapiro_test(residuals(model))
# Checking normality assumption by groups
my_data %>%
  group_by(group) %>%
  shapiro_test(weight)

# Computing Kruskal-Wallis test
kruskal.test(weight ~ group, data = my_data)

# Multiple pairwise-comparison between groups
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")


# Reporting the main findings
A Kruskal-Wallis test was conducted to determine if there were differences in weight values between different treatment groups: the "ctrl" (n = 10), "trt1" (n = 10), and "trt2" (n = 10). Median of the weight values were statistically significantly different between the different levels of treatment, χ2(2) = 14.468, p = 0.018. Subsequently, pairwise comparisons were performed using Wilcoxon rank sum test with continuity correction. A Benjamini-Hochberg correction for multiple comparisons was made with statistical significance accepted at the p < 0.05 level. This post hoc analysis revealed statistically significant differences in weights between the trt1 (Mdn = 4.55) and trt2 (Mdn = 5.44) (p = 0.027).




