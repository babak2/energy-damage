library(dplyr)
library(ggplot2)

damages <- read.csv("data/national.csv")
macro_regions <- read.csv("data/macro-regions_v1.csv")

damages <- left_join(damages, macro_regions, by = c("iso" = "region.key"))

damages <- damages %>% 
  filter(!is.na(worldbank) & !is.na(impact) & !is.na(damages))

average_damages <- damages %>%
  group_by(worldbank, impact) %>%
  summarise(mean_damages = mean(damages))

average_damages$worldbank <- factor(average_damages$worldbank, levels = c("low", "lower-middle", "upper-middle", "high", "missing"))

bar_plot <- ggplot(average_damages, aes(x = worldbank, y = mean_damages, fill = impact)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "World Bank Regions", y = "Average Valued Energy Damages", title = "Average Valued Energy Damages by World Bank Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("electricity" = "blue", "other_energy" = "green")) +
  coord_flip()


ggsave("average_damages_by_region.pdf", plot = bar_plot, width = 10, height = 6)




