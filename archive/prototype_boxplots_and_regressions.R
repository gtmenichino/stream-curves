## Run best subsets
library(leaps)

#######################################################################################
## Import data
## Next step is to set your working directory. 
setwd("C:/Users/RDCRLGCD/Documents/OSAM Ref Curve")

##Next import the OSAM data.  
osam.dat<- read.csv("OSAM_summarydata.csv", header=TRUE)

## After import you can check your data imported correctly by viewing the data.
View(osam.dat)
################################################################################

# Process data

osam.dat$bufferwidth <- osam.dat$L_Bufferwidth + osam.dat_complete$R_Bufferwidth
osam.dat$DA_km2<-(osam.dat$DA_mi2*2.58999)
osam.dat$Slope_unitless<-osam.dat$Slope_per/100
osam.dat$streampower<-(osam.dat$DA_km2)*(osam.dat$Slope_unitless)

## Make sure categorical variables are read as factors
osam.dat$DACAT<-factor(osam.dat$DACAT)
osam.dat$Ecoregion<-factor(osam.dat$Ecoregion)
osam.dat$Bedmaterial<-factor(osam.dat$Bedmaterial)
osam.dat$FlowRegime<-factor(osam.dat$FlowRegime)
osam.dat$valleytype<-factor(osam.dat$valleytype)
osam.dat$BEHI_NBS<-factor(osam.dat$BEHI_NBS)

## Collapse groupings for some of the factors

osam.dat$StreamType2 <- fct_collapse(
  osam.dat$StreamType,
  "B" = c("B1c", "B3", "B3c", "B4", "B4a", "B4c"),
  "C" = c("C1", "C3", "C4", "C4b"),
  "E" = c("E4", "E6"),
  "F" = c("F4")
  
)

osam.dat$BEHIcombined<-fct_collapse(
  osam.dat$BEHI_NBS,
  "Stable" = c("VL/M", "L/M"),
  "Moderately unstable" = c("M/L", "M/M", "M/H"),
  "Unstable" = c("M/VH", "H/H", "H/M", "H/L", "H/VH"),
)

osam.dat$BEHIeroding<-fct_collapse(
  osam.dat$BEHI_NBS,
  "Non-eroding Bank" = c("VL/M", "L/M", "M/L"),
  "Actively eroding" = c("M/M", "M/H", "M/VH", "H/H", "H/M", "H/L", "H/VH")
)

#################################################################################

### Investigate categorical variables with boxplots ###########################

#####################################################################################

library(ggpubr)

# 1. Define the pairwise comparisons you want to make for Ecoregion
my_comparisons <- list( c("ECBP", "HELP"), c("IP", "HELP"), c("ECBP", "IP") )

# 2. Create the plot
ggboxplot(
  osam.dat,
  x = "Ecoregion",
  y = "perRiffle",
  fill = "Ecoregion",          # Color the boxplots by Ecoregion
  palette = "viridis",     # Use a colorblind-friendly palette
  ) +
  # 3. Add pairwise comparisons with p-values (Wilcoxon test)
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # 4. Add the overall p-value for each panel (Kruskal-Wallis test)
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",   # Position the overall p-value at the top
    label.x.npc = "center" # Center it horizontally
  ) +
  # 5. Improve labels and theme
  labs(
    title = "Percent Riffles by Ecoregion",
    subtitle = "",
    x = "Ecoregion",
    y = "Percent Riffles"
  ) +
  theme(legend.position = "none") # Hide legend as it's redundant

##############################################################################
# 1. Define the pairwise comparisons you want to make for DACAT
my_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

# 2. Create the plot
ggboxplot(
  osam.dat,
  x = "DACAT",
  y = "perRiffle",
  fill = "DACAT",          # Color the boxplots by DACAT
  palette = "viridis",     # Use a colorblind-friendly palette
) +
  # 3. Add pairwise comparisons with p-values (Wilcoxon test)
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # 4. Add the overall p-value for each panel (Kruskal-Wallis test)
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",   # Position the overall p-value at the top
    label.x.npc = "center" # Center it horizontally
  ) +
  # 5. Improve labels and theme
  labs(
    title = "Percent Riffles by DACAT",
    subtitle = "",
    x = "DACAT",
    y = "Percent Riffles"
  ) +
  theme(legend.position = "none") # Hide legend as it's redundant

##############################################################################

#DACAT and Ecoregion
# 1. Define the pairwise comparisons you want to make for DACAT
my_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

# 2. Create the plot
ggboxplot(
  osam.dat,
  x = "DACAT",
  y = "perRiffle",
  fill = "DACAT",          # Color the boxplots by DACAT
  palette = "viridis",     # Use a colorblind-friendly palette
  facet.by = "Ecoregion",  # Create a separate panel for each Ecoregion
  short.panel.labs = TRUE  # Keep facet labels clean
) +
  # 3. Add pairwise comparisons with p-values (Wilcoxon test)
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # 4. Add the overall p-value for each panel (Kruskal-Wallis test)
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",   # Position the overall p-value at the top
    label.x.npc = "center" # Center it horizontally
  ) +
  # 5. Improve labels and theme
  labs(
    title = "Percent Riffle by Drainage Area Category (DACAT) within Ecoregions",
    subtitle = "Statistical comparisons shown for each Ecoregion panel",
    x = "Drainage Area Category (DACAT)",
    y = "Percent Riffles"
  ) +
  theme(legend.position = "none") # Hide legend as it's redundant
####################################################################################

## Explore combining Ecoregions. Should ECBP and IP be combined and HElP be on its own?

#########################################################################################

# 1. Load the necessary libraries
library(tidyverse)
library(ggpubr)

# 3. Create the new combined ecoregion group
data_combined <- osam.dat %>%
  mutate(
    Ecoregion_Group = factor( # Use factor for controlled ordering
      case_when(
        Ecoregion %in% c("ECBP", "IP") ~ "ECBP & IP",
        TRUE                             ~ as.character(Ecoregion)
      )
    )
  )

# Verify the new grouping
table(data_combined$Ecoregion_Group)
#
# ECBP & IP      HELP 
# 31         8 

# 4. Define the pairwise comparisons for DACAT
my_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3") )

# 5. Create the final faceted plot
ggboxplot(
  data_combined,
  x = "DACAT",
  y = "perRiffle",
  fill = "DACAT",
  palette = "jco", # A different color palette
  facet.by = "Ecoregion_Group", # <-- Facet by the new combined group
  short.panel.labs = TRUE
) +
  # Add pairwise Wilcoxon test p-values
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # Add the overall Kruskal-Wallis test p-value for each panel
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",
    label.x.npc = "center"
  ) +
  # Improve labels and theme
  labs(
    title = "Percent Riffle by DACAT for Combined Ecoregions",
    subtitle = "ECBP and IP are grouped; HELP is separate",
    x = "Drainage Area Category (DACAT)",
    y = "% Riffle"
  ) +
  theme(legend.position = "none")



# --- The Summary Code ---
summary_table <- data_combined %>%
  group_by(Ecoregion_Group, DACAT) %>% # Group by the same variables as the plot
  summarize(
    count = n(),
    mean = mean(perRiffle, na.rm = TRUE),
    sd = sd(perRiffle, na.rm = TRUE),
    min = min(perRiffle, na.rm = TRUE),
    q25 = quantile(perRiffle, 0.25, na.rm = TRUE),
    median = median(perRiffle, na.rm = TRUE),
    q75 = quantile(perRiffle, 0.75, na.rm = TRUE),
    max = max(perRiffle, na.rm = TRUE),
    .groups = 'drop' # Recommended to avoid downstream grouping issues
  )

# Print the resulting table
print(summary_table)


################################################################################

## Valley type
# 4. Define the pairwise comparisons for Ecoregion
my_comparisons <- list( c("confined alluvial", "unconfined alluvial"), c("confined alluvial", "colluvial"), c("unconfined alluvial", "colluvial") )

# 3. Create the boxplot comparing valleytype
p1<-ggboxplot(
  osam.dat,
  x = "valleytype",      # <-- X-axis is now the combined group
  y = "perRiffle",
  fill = "valleytype",   # Color by the group as well
  palette = c("red4", "seagreen1", "royalblue2", "gold1") # Example: Blue and Yellow
) +
  # Add pairwise Wilcoxon test p-values
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # Add the overall Kruskal-Wallis test p-value for each panel
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",
    label.x.npc = "center"
  )+
  # 5. Improve labels and theme
  labs(
    title = "",
    x = "Valley Type",
    y = "Percent Riffle"
  ) +
  theme(legend.position = "none") # Hide the redundant legend

##################################################################################
## Bed Grain Size

# 1. Define the pairwise comparisons you want to make for DACAT
my_comparisons <- list( c("Gravel", "Bedrock"), c("Gravel", "Cobble"), c("Gravel", "Cobble/Bedrock"), 
                        c("Cobble", "Bedrock"), c("Cobble", "Cobble/Bedrock"), c("Gravel", "Shale Gravel"), 
                        c("Gravel", "Silt/clay"), c("Cobble", "Shale Gravel"), c("Cobble", "Silt/clay"))

# 2. Create the plot
ggboxplot(
  osam.dat,
  x = "Bedmaterial",
  y = "perRiffle",
  fill = "Bedmaterial",          # Color the boxplots by DACAT
  palette = "viridis",     # Use a colorblind-friendly palette
) +
  # 3. Add pairwise comparisons with p-values (Wilcoxon test)
  stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test"
  ) +
  # 4. Add the overall p-value for each panel (Kruskal-Wallis test)
  stat_compare_means(
    method = "kruskal.test",
    label.y.npc = "top",   # Position the overall p-value at the top
    label.x.npc = "center" # Center it horizontally
  ) +
  # 5. Improve labels and theme
  labs(
    title = "Percent Riffles by Bed material",
    subtitle = "",
    x = "Bed material",
    y = "Percent Riffles"
  ) +
  theme(legend.position = "none") # Hide legend as it's redundant

##########################################################################################

##    Best Subsets Regression

###########################################################################################

best_model <- regsubsets(perRiffle ~ DA_km2 + Slope_per + LWD_frequency +
                           L_Bufferwidth + R_Bufferwidth + Per_erodingbank + d50,
                         data = osam.dat,
                         method = "exhaustive")

# Create the model selection table
s <- summary(best_model)

model_table <- data.frame(
  model = 1:length(s$bic),
  predictors = apply(s$which[,-1], 1, function(x)
    paste(names(x)[x], collapse = ", ")),
  R2 = s$rsq,
  Adj_R2 = s$adjr2,
  Cp = s$cp,
  BIC = s$bic
)

model_table$delta_BIC <- model_table$BIC - min(model_table$BIC)

# Save the ordering
ord <- order(model_table$BIC)

# Reorder both objects
model_table <- model_table[ord, ]
predictor_matrix <- s$which[,-1][ord, ]

# Filter top models
top_models <- subset(model_table, delta_BIC < 2)

# Extract predictors from top models
top_predictors <- predictor_matrix[model_table$delta_BIC < 2, ]

importance <- colMeans(top_predictors)

importance_table <- data.frame(
  Predictor = names(importance),
  Importance = importance
)

importance_table <- importance_table[order(-importance_table$Importance), ]

importance_table

## A way to double check that everything is sorted correctly
head(model_table$predictors)
head(apply(predictor_matrix,1,function(x) paste(names(x)[x], collapse=", ")))

## Create the model selection heatmap
library(ggplot2)
library(reshape2)

## Prepare data
heat_data <- as.data.frame(top_predictors)
heat_data$model <- rownames(heat_data)

heat_long <- melt(heat_data,
                  id.vars = "model",
                  variable.name = "Predictor",
                  value.name = "Included")

## Plot
ggplot(heat_long, aes(x = Predictor, y = model, fill = Included)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("white", "black")) +
  labs(x = "Predictor",
       y = "Top models (ΔBIC < 2)",
       fill = "Included") +
  theme_minimal()


############################################################################

## Explore the best model

###################################################################################

final_model <- lm(perRiffle ~ Ecoregion + Slope_per, data = osam.dat)
summary(final_model)

##Testing for model assumptions
par(mfrow = c(2,2))
plot(final_model)

shapiro.test(residuals(final_model))
# p > 0.05 → residuals approximately normal
# p < 0.05 → evidence of non-normality

#Testing homoscedasticity
library(lmtest)
bptest(final_model)

# Breusch–Pagan test
# p > 0.05 → constant variance
# p < 0.05 → heteroscedasticity

#4. Check multicollinearity
library(car)
vif(final_model)

#Rules of thumb:
# VIF < 5 → acceptable
# VIF > 10 → serious multicollinearity

#Identify influential points
cooks.distance(final_model)
plot(cooks.distance(final_model), type="h")

res <- residuals(final_model)
fit <- fitted(final_model)
plot(fit, res)
abline(h=0, lty=2)

hist(osam.dat$perRiffle)

library(see)
library(performance)
library(ggplot2)

final_model <- lm(perRiffle ~ Ecoregion + Slope_per + LWD_frequency + Per_erodingbank, data = osam.dat)

check_model(final_model)

png("model_diagnostics.png", width=1800, height=1400, res=300)
check_model(final_model)

check_normality(final_model)
check_heteroscedasticity(final_model)
check_collinearity(final_model)
check_outliers(final_model)
