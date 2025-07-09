

## Load libraries
pacman::p_load(tidyverse,
               janitor,
               jsonlite, 
               gridExtra,
               patchwork, 
               ggpubr, 
               grid)


# Load the data

ta_scene_long = read.csv(file = 'RF_TuningAccuracy.csv', header = T)
str(ta_scene_long)


## Accuracy Plot


b <- ggplot(ta_scene_long, aes(x = Scenario, y = accuracy*100)) +
  geom_col(aes(fill = color), width = 0.60) +
  geom_text(aes(label = round(accuracy*100, 1)), vjust = -1) +
  coord_fixed(ratio = 1/25) +
  labs(y = "Overall Accuracy (%)",
       x = "Classification Scenarios") +
  scale_fill_identity() +
  scale_x_discrete(expand = c(0, 0.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 20), 
                     limits = c(0, 100)) +
    theme_pubr() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


ggsave(filename = "RF_TunedAccuracy.jpg", plot = b, 
       width = 8, height = 6, units = "in", dpi = 400)



## Confusion Matrix

RF_confusion_matrix <- read.csv(file = "RF_Confusion_matrix.csv", header = T)



RF_confusionMatrix_plot <- ggplot(RF_confusion_matrix, 
                                  aes(x = Class2, y = Class1, fill = Pixels)) +
  geom_tile() +
  coord_fixed() + 
  geom_text(aes(label = Pixels), color = "black", size = 3.5) +
  scale_fill_gradient(low = "white", high = "red") + 
  labs(x = "Predicted Class", 
       y = "True Class", 
       fill = "Pixels") +
  theme_pubr(legend = "right",
             border = TRUE) + 
  theme(axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, hjust = 1, vjust = 0.5,
                                   color = 'black', angle = 90),
        axis.title = element_text(size = 12, face = "bold"),
        legend.ticks = element_blank())


ggsave(filename = "RF_ConfustionMatrix.jpg", plot = RF_confusionMatrix_plot,
       width = 10, height = 8, units = 'in')



#### Variable Importance of the Features
## read the data

data= read.csv("variableImportance.csv", header = T)
names(data)


# Normalize the variable importance within the Scenarios
data_long <- data %>%
  filter(Indices != 'pc3') %>% 
  group_by(Scenario) %>%
  mutate(NormalizedImportance = (variableImportance - min(variableImportance)) / 
           (max(variableImportance) - min(variableImportance)),
         relativeImportance = variableImportance/ sum(variableImportance)) %>%
  ungroup()



unique(data_long$Indices)



# Spread data into a wide format to ensure all variables are represented
data_wide <- tidyr::complete(data_long, Indices, 
                             Scenario, fill = list(NormalizedImportance = NA,
                                                   relativeImportance = NA))



# Convert back to long format for ggplot
data_heatmap <- data_wide %>%
  arrange(Indices) %>%
  mutate(Scenario = factor(Scenario, levels = paste0("CS", 1:4)))



ggplot(data_long, aes(x = as.factor(Indices), y = Scenario,
                      fill = NormalizedImportance)) +
  geom_tile(color = "grey") +
  scale_fill_gradient(low = "white", high = "darkred", na.value = "grey95", 
                      name = "Normalized Relative Importance") +
  coord_fixed(ratio = 2) +
  labs(x = "Indices",
       y = "Classification \n Scenarios",
       fill = "Normalized Relative Importance") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title.position = "right", 
        legend.title = element_text(angle = 90, hjust = 0.5))



# Determine the variable order dynamically
indicesOrdered <- data_long %>%
  group_by(Indices) %>%
  summarize(FirstScenario = min(as.numeric(gsub("CS", "", Scenario)))) %>%
  arrange(FirstScenario)



# Reorder variables based on their first appearance
data_long$indices <- factor(data_long$Indices, levels = indicesOrdered$Indices)


ggplot(data_long, aes(x = indices, y = Scenario,
                      fill = NormalizedImportance)) +
  geom_tile(color = 'black') +
  scale_fill_gradient(low = "white", high = "darkred", na.value = "grey95", 
                      name = "Normalized Relative Importance") +
  coord_fixed(ratio = 2) +  # Ensure square tiles
  labs(x = "Indices",
       y = "Classification \n Scenarios",
       fill = "Normalized Importance") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "right",
        legend.title.position = "right", 
        legend.title = element_text(angle = 90, hjust = 0.5),
        legend.ticks = element_blank()
  )


ggsave(filename = "VarImp_V1.jpg", plot = last_plot(),
       width = 11, height = 6, units = 'in',
       dpi = 400)



## Top 10 features

top_features <- data_long %>% 
  group_by(Scenario) %>% 
  dplyr::slice_max(variableImportance, n = 10) %>% 
  ungroup()

top_normalized_features <- data_long %>% 
  group_by(Scenario) %>% 
  dplyr::slice_max(NormalizedImportance, n = 10) %>% 
  ungroup()


write.csv(top_features, file = "Top10Feature.csv")
write.csv(top_normalized_features, file = "Top10NormalizedFeature.csv")











