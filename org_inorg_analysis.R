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

avg_inoc_mass <- 0.0029175 #We weighed 4 same diameter inoculum plugs (dry weight), and this was the average

ds <- clean_names(ds)%>%
  mutate(Mass_Gained = post_drying_mass_filter_and_fungi_g - filter_mass_g - avg_inoc_mass) #0.025 is the estimate for the dried weight of the fungal inoculum plugs added to each flask

plot <- ds %>%
  ggplot(aes(x = condition, y = Mass_Gained, fill = condition))+
  geom_boxplot()+
  scale_fill_manual(values = c("#EBCC7F", "#7B98E6"))+
  scale_x_discrete(labels = c('Ammonium sulfate (Inorganic)', "Urea (Organic)"))+
  stat_n_text()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  # stat_compare_means(method = "anova")+
  geom_point(aes(shape = condition))+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),   # Axis titles size
    axis.text = element_text(size = 10),    # Axis text size
    plot.title = element_text(size = 16, face = "bold"), # Title size
    strip.text = element_text(size = 12)    # Facet label size (if faceting)
  )+
  labs(y = "Fungal Mass Gained (g)", x = "Nitrogen Type")

plot

ggsave(plot = plot, "org_inorg_N.png", width = 5, height = 5, units = "in", dpi = 500)


stat <- t.test(data = ds, Mass_Gained ~ condition)

plot(lm(data = ds, Mass_Gained ~ condition)) #Look pretty good.

ds %>%
  ggplot(aes(x = Mass_Gained))+
  geom_histogram(aes(fill = condition), binwidth = 0.015)
#Not horribly different from Gaussian