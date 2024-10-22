#BB
#10/22/24
#Analyzing the results from the experiment testing whether Alternaria alternata can use both organic and inorganic forms of nitrogen

library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(EnvStats)

theme_set(theme_bw())

ds <- read_xlsx("Org_Inorg_N.xlsx", sheet = 2)

ds <- clean_names(ds)%>%
  mutate(Mass_Gained = post_drying_mass_filter_and_fungi_g - filter_mass_g - 0.025) #0.025 is the estimate for the dried weight of the fungal inoculum plugs added to each flask

ds %>%
  ggplot(aes(x = condition, y = Mass_Gained, fill = condition))+
  geom_boxplot()+
  scale_x_discrete(labels = c('Ammonium Sulfate', "Urea"))+
  stat_n_text()+
  # stat_compare_means(method = "anova")+
  geom_point(aes(shape = condition))+
  theme(legend.position = "none")+
  labs(y = "Fungal Mass Gained", x = "Nitrogen Type")

anova(lm(data = ds, Mass_Gained ~ condition))
