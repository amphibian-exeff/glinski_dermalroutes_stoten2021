# 8 samples by 2 media by 1 species (Leopard frog?) by 3 pesticides

dim(gdr_data)
colnames(gdr_data)
unique(gdr_data$Analyte)
# BIF  4-OH CPF  CPO  TFS  TFSa
unique(gdr_data$Media) # Exposure type
# Soil Water 
unique(gdr_data$Matrix) # Measurement type
# Amphibian Soil Water

# we want amphibian matrix 
# comparison of Soil versus Water
# by chemical
which_amphib <- which(gdr_data$Matrix=="Amphibian")
gdr_amphib <- gdr_data[which_amphib,]
dim(gdr_amphib)

# drop 4-OH 4_Hydroxy bifenthrin
# Bifenthrin, Chlorpyrifos, Chlopyrifos oxon, Trifloxystrobin, Trifloxystrobin acid


ggplot(gdr_amphib, aes_string(x='Analyte', y='Concentration', color='Media')) +
  geom_point() +
  xlab("Analyte") + ylab("Concentration") + ggtitle("Amphibian Data") +
  theme_bw()

scale_color_brewer(type='div', palette=2) +
  
ggplot(gdr_amphib, aes(x='Concentration', fill='Media')) +
  geom_density(alpha=0.25) +
  facet_wrap("clarity")
