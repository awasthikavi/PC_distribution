#* This is the R script for plotting the spectral and textural indices 
#* obtained from Google Earth engine. Please set the working directory before 
#* running this analysis. Otherwise you may want to load the dataset by 
#* removing the comment marking below. 



# Load the packages

pacman::p_load(rio, 
               tidyverse, 
               psych,
               geomtextpath,
               patchwork,
               reshape2,
               gridExtra)


# Sorting Levels ----
level_orders = c('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11', 'B12')
wavelengths = c(490, 560, 665, 705, 740, 783, 842, 865, 1610, 2190)


# Labels ----
landcover_labels = c("Water", "Built-up", "Vegetation", "Cropland",
                     "Callery", "Wetland", "Barren/Sand")

# Color for Land cover Class ----
landcover_class = c("Water" = '#2271B2',
                    "Built-up" = '#F8071D',
                    'Vegetation' = '#359B73',
                    "Cropland" = '#f0e442',
                    "Callery" = '#d55e00',
                    "Wetland" = '#3DB7E9',
                    "Barren/Sand" = "#000000")



# Spectral Data ----

sp_data <- read.csv(file = "Spectral_Combined.csv", header = T)
names(sp_data)


# Standard Bands
SB <- sp_data %>% 
  select(Class, starts_with("B") & !ends_with("I")) %>% 
  pivot_longer(!Class, 
               names_to = "Bands",
               values_to = "Reflectance") |>
  mutate(scaled_Reflectance = Reflectance/10000) %>% 
  group_by(Class, Bands) |>
  summarise(AvgReflectance = mean(scaled_Reflectance)) |>
  mutate(Legend = factor(Class, levels = c(0:6),
                         labels = landcover_labels),
         Wavelengths = factor(Bands, levels = level_orders,
                              labels = wavelengths)) 


SB_Plot <- ggplot(data = SB, aes(x = as.numeric(levels(Wavelengths)[Wavelengths]), 
                             y = AvgReflectance, group = Legend)) +
  geom_smooth(method = 'loess', aes(color = Legend), se = F) +
  labs(x = "Wavelengths (nm) ", y = "Average Reflectance") +
  scale_color_manual("", values = landcover_class) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.45)) +
  scale_x_continuous(limits = c(480, 2200), breaks = wavelengths) +
  geom_vline(xintercept = wavelengths, color = 'blue4', linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8, color = 'black', angle = 90),
        legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1))

SB_Plot
  
# ggsave(filename = "WinterReflectanceWOannotationLegend_V5.jpg", plot = last_plot(),
#        width = 8, height = 6, dpi = 400)


## Annotate the Wavelengths

SB_Plot + 
  annotate(geom = "label", x = 490, y= 0.4, label="490", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 560, y= 0.4, label="560", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 665, y= 0.35, label="665", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 705, y= 0.4, label="705", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 740, y= 0.35, label="740", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 783, y= 0.4, label="783", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 842, y= 0.35, label="842", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 875, y= 0.4, label="865", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 1610, y= 0.4, label="1610", angle=90, size = 3, color="blue4") +
  annotate(geom = "label", x = 2190, y= 0.4, label="2190", angle=90, size = 3, color="blue4")


ggsave(filename = "WinterReflectance.jpg", plot = last_plot(),
       width = 10, height = 6, dpi = 400)



## PCA for texture Plot----


texturePCA <- read.csv(file = "Texture_PCA.csv", header = T)

names(texturePCA)

texturePCA1 <- texturePCA %>% 
  select(Class, pc1, pc2) %>% 
  mutate(class_fct = factor(Class, levels = c(0:6),
                         labels = landcover_labels))

tex1Plot <- ggplot(texturePCA1, aes(x = pc1, y = pc2, color = class_fct)) +
  geom_point() +
  coord_fixed(ratio = 1/50) +
  labs(x = "Texture PC1", y = "Texture PC2") +
  scale_color_manual("", values = landcover_class) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 8, color = 'black'),
          legend.position = "none")


texturePCA2 <- texturePCA1 %>% 
  filter(Class == 2|
           Class == 4)


tex2Plot <- ggplot(texturePCA2, aes(x = pc1, y = pc2, color = class_fct)) +
  geom_point() +
  coord_fixed(ratio = 1/30) +
  labs(x = "Texture PC1", y = "Texture PC2") +
  scale_color_manual("", values = landcover_class) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, color = 'black'),
        plot.background = element_blank(),
        legend.position = 'none')


tex3Plot<- tex1Plot + 
  inset_element(p = tex2Plot,
                left =  0,
                right = 1.6,
                bottom = 0.3,
                top = 0.99, clip = F)

tex3Plot

ggsave(tex3Plot, filename = "PCTexture.jpg",
       width = 8, height = 6, dpi = 400)



## Sentinel-1 Plot
s1C <- read.csv(file = 'S1_Values.csv', header=T)

names(s1C)

s1C_clean <- s1C %>% 
  select(Class, VV, VH, VVVH_Ratio, Sentinel_RVI, Sentinel_Span) %>% 
  reshape2::melt(id.var = 'Class') %>% 
  mutate(class_fct = factor(Class, levels = c(0:6),
                          labels = landcover_labels))
  


s1C_plot <- ggplot(s1C_clean, aes(x = '', y = value, fill = class_fct)) +
  geom_boxplot(outliers = F) +
  facet_wrap(~variable, scales = "free") +
  scale_fill_manual("", values = landcover_class) +
  theme_bw() +
  scale_x_discrete(expand = c(0.15,0.15)) +
  labs(x = "", y = "Backscatter Values (dB)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = 'bold'),
        axis.text = element_text(size = 8, color = 'black'),
        legend.position = 'none') +
  guides(fill = guide_legend(ncol = 1,
                             byrow = F))

ggsave(filename = "Sentinel1_boxplot.jpg", 
       plot = s1C_plot,
       width = 8.5, height = 6, dpi = 400)


## PALSAR Plot
PALSAR <- read.csv(file = 'PALSAR_Values.csv', header=T)

names(PALSAR)

PALSAR_clean <- PALSAR %>% 
  select(Class, HH, HV, HHHV_Ratio, ALOS_RVI, ALOS_Span) %>% 
  reshape2::melt(id.var = 'Class') %>% 
  mutate(class_fct = factor(Class, levels = c(0:6),
                            labels = landcover_labels))



PALSAR_plot <- ggplot(PALSAR_clean, aes(x = '', y = value, fill = class_fct)) +
  geom_boxplot(outliers = F) +
  facet_wrap(~variable, scales = "free") +
  scale_fill_manual("", values = landcover_class) +
  theme_bw() +
  scale_x_discrete(expand = c(0.15,0.15)) +
  labs(x = "", y = "Backscatter Values (dB)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = 'bold'),
        axis.text = element_text(size = 8, color = 'black'),
        legend.position = 'none') +
  guides(fill = guide_legend(ncol = 1,
                             byrow = F))

ggsave(filename = "PALSAR_boxplot.jpg", 
       plot = PALSAR_plot,
       width = 8.5, height = 6, dpi = 400)


## Arrange the Plots

plotArranged = gridExtra::grid.arrange(SB_Plot, tex3Plot, 
                                       s1C_plot, PALSAR_plot)


SB_Plot + tex3Plot + s1C_plot + PALSAR_plot +
  plot_layout(guides = 'collect', nrow = 2, ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

  
# ggsave(filename = "RadarPlot_V1.jpg", plot = last_plot(),
#        width = 11, height = 6, dpi = 400, units = 'in')







