

## Load libraries
pacman::p_load(tidyverse,
               janitor,
               jsonlite, 
               gridExtra,
               patchwork, 
               ggpubr, 
               grid)



svm_ta_scene = read.csv(file = "SVM_LM_TuningAccuracy.csv", header = T)


# Accuracy of SVM Across the CS

svm1 <- ggplot(svm_ta_scene, aes(x = Scenario, y = accuracy*100, fill = color)) +
   geom_col(aes(fill = color), width = 0.60) +
   geom_text(aes(label = round(accuracy*100, 1)), vjust = -1) +
   coord_fixed(ratio = 1/25) +
   labs(y = "Overall Accuracy (%)",
        x = "Classification Scenarios") + 
  theme_pubr() +
  scale_fill_identity() + 
  scale_x_discrete(expand = c(0, 0.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 20), 
                     limits = c(0, 100)) +
  theme(axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 12, face = "bold"))

ggsave(filename = "SVM_TunedAccuracy.jpg", plot = svm1,
       width = 8, height = 6, units = 'in')




## Confusion Matrix

SVM_LM_confusion_matrix <- read.csv(file = "SVM_LM_confusion_matrix.csv", header = T)

svm2 <- ggplot(SVM_LM_confusion_matrix, 
               aes(x = Class2, y = Class1, fill = Pixels)) +
  geom_tile() +
  coord_fixed() +
  geom_text(aes(label = Pixels), color = "black", size = 3.5) +
  scale_fill_gradient(low = "white", high = "red")+
  theme_pubr(border = T,
             legend = "right") +
  labs(x = "Predicted Class", 
       y = "True Class", 
       fill = "Pixels") +
  theme(axis.text.y = element_text(size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black', angle = 90, 
                                   hjust = 0.9, vjust = 0.2),
        axis.title = element_text(size = 12, face = "bold"),
        legend.ticks = element_blank())


ggsave(filename = "SVM_ConfustionMatrix.jpg", plot = svm2,
       width = 10, height = 8, units = 'in')






















