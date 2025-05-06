library(readxl)
library(lattice)
library(tidyverse)
library(plyr)
library(readr)
library(nlme)
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(knitr)
library(rstatix)
drace24 <- read_excel("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\2024DRACE_03042025\\AllplatesDRACE2024.xlsx", sheet = "raw_data")
drace24
drace24 <- drace24[!is.na(drace24$Avg_runs), ]
colnames(drace24) <- make.names(colnames(drace24), unique = TRUE)
colnames(drace24)  
str(drace24)   
unique(drace24$Type)
drace24 <- drace24 %>%
  filter(!(Type %in% c("Standard", "Standard 2", "Standard 3", "Standard 4")))
drace24 <- drace24 %>%
  mutate(Type = recode(Type, "SH" = "Soil_Health", "RG" = "Rotational_Grazing"))

#linear regression models ----
lmodelace <- lmer(Avg_runs ~ Type + County + (1 | Farmer), data = drace24)
summary(lmodelace)

lmodelace_typecounty <- lm(Avg_runs ~ Type + County, data = drace24)
summary(lmodelace_typecounty)

# Perform pairwise comparisons----
pairwiseace24 <- drace24 %>%
  pairwise_t_test(
    Avg_runs ~ Type, # Dependent variable vs group
    p.adjust.method = "bonferroni" # Adjust for multiple comparisons
  )

# Display the results
pairwiseace24

# Perform ANOVA
anovaace24 <- aov(Avg_runs ~ Type, data = drace24)
anovaace24
# Tukey HSD post-hoc test
tukey_ace24 <- TukeyHSD(anovaace24)
print(tukey_ace24)

# Extract p-values and generate letters for significant differences
letters <- multcompLetters4(anovaace24, tukey_ace24)$Type

# Extract grouping letters from the 'letters' object
letters_ACE <- data.frame(
  Type = names(letters$Letters),  # Extract names (factor levels)
  label = letters$Letters         # Extract the letters
)

# Merge the letters with the original dataset
drace24 <- drace24 %>%
  left_join(letters_ACE, by = "Type")

drace24$Type <- factor(drace24$Type, levels = c("Conventional","Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP"))

# Create a distinct data frame for labels (one per Type)
label_pertype <- drace24 %>% 
  distinct(Type, label)

# Plot by county----
ggplot(drace24, aes(x = Type, y = Avg_runs)) +
  geom_boxplot(aes(fill = County), color = "black", alpha = 0.6) +
  geom_text(data = label_pertype, aes(x = Type, y = max(drace24$Avg_runs, na.rm = TRUE) + 1, label = label),  
            inherit.aes = FALSE, size = 5, color = "black") +
  labs(
    title = "Autoclaved Citrate Extractable Protein By Management",
    x = NULL,
    y = " g Protein kg-1 soil"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "black"),  
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),  
    axis.title = element_text(size = 16, face = "bold", color = "black"),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
    legend.position = "right"
  ) +
  scale_fill_manual(values = c("Fillmore" = "brown2", "Grant" = "deepskyblue4"))
#plot not by county----
ggplot(drace24, aes(x = Type, y = Avg_runs)) +
  geom_boxplot(aes(fill = Type), color = "black", alpha = 0.6) +  # Consistent fill
  geom_text(data = label_pertype, aes(x = Type, y = max(drace24$Avg_runs, na.rm = TRUE) + 1, label = label),  
            inherit.aes = FALSE, size = 5, color = "black") + 
  labs(
    title = "Autoclaved Citrate Extractable Protein By Management",
    x = NULL,
    y = "g Protein kg-1 soil"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "black"),  
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),  
    axis.title = element_text(size = 16, face = "bold", color = "black"),  
    axis.title.x = element_text(size = 18, color = "black"),  
    axis.title.y = element_text(size = 18, color = "black"),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Paired")
#no legend
ggplot(drace24, aes(x = Type, y = Avg_runs)) +
  geom_boxplot(aes(fill = Type), color = "black", alpha = 0.6) +  # Consistent fill
  geom_text(data = label_pertype, aes(x = Type, y = max(drace24$Avg_runs, na.rm = TRUE) + 1, label = label),  
            inherit.aes = FALSE, size = 5, color = "black") + 
  labs(
    title = "Autoclaved Citrate Extractable Protein By Management",
    x = NULL,
    y = "g Protein kg-1 soil"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold", color = "black"),  
    axis.text.y = element_text(size = 27, face = "bold", color = "black"),  
    axis.title.y = element_text(size = 27,face = "bold",color = "black"),  
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "black"),  
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Paired")

#mehmet code ----
ggbetweenstats(
  data             = drace24,
  x                = Type,
  y                = Avg_runs,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "Set1",
  comparisons      = list(
    c("Conventional", "Soil_Health"),
    c("Conventional", "Rotational_Grazing"),
    c("Conventional", "New_CRP"),
    c("Conventional", "Old_CRP"),
    c("Soil_Health", "Rotational_Grazing"),
    c("Soil_Health", "New_CRP"),
    c("Soil_Health", "Old_CRP"),
    c("Rotational_Grazing", "New_CRP"),
    c("Rotational_Grazing", "Old_CRP"),
    c("New_CRP", "Old_CRP")
  ),
  label            = "p.signif",
  hide.ns          = FALSE,
  title            = "Differences in Avg Runs by Management Type"
)
#no color
grouped_ggbetweenstats(
  data             = drace24,
  x                = Type,
  y                = Avg_runs,
  grouping.var     = Type,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  annotation.args  = list(title = "Differences in Avg Runs by Management Type"),
  comparisons      = list(
    c("Conventional", "Soil_Health"),
    c("Conventional", "Rotational_Grazing"),
    c("Conventional", "New_CRP"),
    c("Conventional", "Old_CRP"),
    c("Soil_Health", "Rotational_Grazing"),
    c("Soil_Health", "New_CRP"),
    c("Soil_Health", "Old_CRP"),
    c("Rotational_Grazing", "New_CRP"),
    c("Rotational_Grazing", "Old_CRP"),
    c("New_CRP", "Old_CRP")
  ),
  label            = "p.signif",
  hide.ns          = FALSE
)


