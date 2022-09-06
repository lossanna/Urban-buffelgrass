library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

street <- read_xlsx("Neighborhood-data_LO.xlsx", sheet = "By street")
unit <- read_xlsx("Neighborhood-data_LO.xlsx", sheet = "Combined Samples")


# Basic data wrangling ----------------------------------------------------

unit <- unit %>% 
  mutate(across(c(Major, Minor, Major_Minor, Wide_Neighborhood, Alley, Lot, Neighborhood_only,
                  Income_level, Racial_Diversity_level), as.factor))


# Visualize ---------------------------------------------------------------

hist(street$BG_Plants, breaks = 50)
hist(unit$Total_BG_Plants, breaks = 50)


# By each street
# By street type
boxplot(BG_Plants + 1 ~ Street_type, data = street, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Street_type")


# By sampling unit
# Presence of alley
boxplot(Total_BG_Plants + 1 ~ Alley, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Alley")

# Presence of any larger roads or alleys
boxplot(Total_BG_Plants + 1 ~ Neighborhood_only, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Neighborhood_only")

# Presence of major road
boxplot(Total_BG_Plants + 1 ~ Major, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Major road")

# Presence of major or minor road
boxplot(Total_BG_Plants + 1 ~ Major_Minor, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Major and/or minor road")


# Income
boxplot(Total_BG_Plants + 1 ~ Income_level, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Income_level")

ggplot(unit, aes(x = Income_level, y = Total_BG_Plants)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_minimal()

# Racial diversity/homogeneity 
boxplot(Total_BG_Plants + 1 ~ Racial_Diversity_level, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Racial diversity")

# Invasion level (levels are mostly correct)
boxplot(Total_BG_Plants + 1 ~ Invasion_level, data = unit, log = "y",
        varwidth = TRUE, ylab = "log(BG count + 1)", xlab = "Invasion level")

# Unit area
plot(Total_BG_Plants + 1 ~ m2, data = unit, log = "y",
    ylab = "log(BG count + 1)", xlab = "m2")

plot(Total_BG_Plants ~ m2, data = unit,
     ylab = "log(BG count + 1)", xlab = "m2")

ggplot(unit, aes(x = m2, y = Total_BG_Plants)) +
  geom_point() +
  theme_bw()


# Remove Block 15, which had 917 plants
unit.rm15 <- unit %>% 
  filter(!Total_BG_Plants > 800)
street.rm15 <- street %>% 
  filter(!BG_Plants > 800)

ggplot(unit.rm15, aes(x = Alley, y = Total_BG_Plants)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_minimal()

ggplot(street.rm15, aes(x = Street_type, y = BG_Plants)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  theme_minimal()


# Attempt at GLM ----------------------------------------------------------

bg.alley <- glm(Total_BG_Plants ~ Alley + Neighborhood_only,
                   data = unit, family = "poisson")
summary(bg.alley)
