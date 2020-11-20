
#might need to reimport due to level issues
dim(gdr_data)
colnames(gdr_data)
# "Sample.ID"     "Analyte"       "Media"         "Matrix"        "Concentration"

levels(gdr_data$Analyte)
#[1] "4-OH" "BIF"  "CPF"  "CPO"  "TFS"  "TFSa"

levels(gdr_data$Media)
# [1] "Soil"  "Water"
levels(gdr_data$Matrix)
# [1] "Amphibian" "Soil"      "Water"   

# subset grd_data where Matrix == Amphibian
amphib_data <- gdr_data[which(gdr_data$Matrix=='Amphibian'),]
dim(amphib_data)
levels(amphib_data$Matrix)

conc_summary <- amphib_data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Analyte, Media) %>%   # the grouping variable
  summarise(mean_conc = mean(Concentration),  # calculates the mean of each group
            sd_conc = sd(Concentration), # calculates the standard deviation of each group
            n_conc = n(),  # calculates the sample size per group
            se_conc = sd(Concentration)/sqrt(n())) # calculates the standard error of each group
conc_summary

g <- ggplot(conc_summary, aes(Analyte, mean_conc, fill=Media)) + 
  geom_bar(stat="identity", colour="black", position="dodge") +
  geom_errorbar(aes(ymin=mean_conc, ymax=mean_conc+sd_conc), width=.2, position=position_dodge(.9)) + 
  theme_bw() +
  labs(x = "Treatment", y=expression(paste("Concentration (",mu,"g/g)",sep=""))) +
  guides(fill=guide_legend(title="Chemical"))
g

conc_barplot <- paste(gdr_graphics,"glinski_dermal_routes_fig1.jpg",sep="")
jpeg(conc_barplot, width = 6, height = 7, units = "in",res=600)
g
dev.off()
