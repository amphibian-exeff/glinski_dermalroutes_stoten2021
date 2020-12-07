
#might need to reimport due to level issues
dim(gdr_data)
colnames(gdr_data)
# "Sample.ID"     "Analyte"       "Media"         "Matrix"        "Concentration"

levels(gdr_data$Analyte)
#[1] "4-OH" "BIF"  "CPF"  "CPO"  "TFS"  "TFSa"

#bifenthrin, 4-hydroxy bifenthrin, chlorpyrifos, chlorpyrifos oxon, trifloxystrobin, and trifloxystrobin acid
levels(gdr_data$Analyte) <- c("4-hydroxy bifenthrin", "bifenthrin", "chlorpyrifos", "chlorpyrifos oxon",
                              "trifloxystrobin", "trifloxystrobin acid")

gdr_data$Analyte <- factor(gdr_data$Analyte , levels=c("bifenthrin", "4-hydroxy bifenthrin","chlorpyrifos", "chlorpyrifos oxon",
                                                       "trifloxystrobin", "trifloxystrobin acid"))

levels(gdr_data$Analyte)
#[1] "4-hydroxy bifenthrin" "bifenthrin"           "chlorpyrifos"         "chlorpyrifos oxon"    "trifloxystrobin"      "trifloxystrobin acid"
levels(gdr_data$Media)
# [1] "Soil"  "Water"
levels(gdr_data$Matrix)
# [1] "Amphibian" "Soil"      "Water"   


media_data <- gdr_data[which(gdr_data$Matrix!='Amphibian'),]
dim(media_data)
levels(media_data$Matrix)


#### body media concentrations
# subset grd_data where Matrix != Amphibian
amphib_data <- gdr_data[which(gdr_data$Matrix=='Amphibian'),]
dim(amphib_data)
levels(amphib_data$Matrix)

# box plot on amphibian  body burden observations
g_media_box <- ggplot(aes(y=Concentration, x=Analyte, fill=Media), data=media_data) + 
  geom_boxplot() + 
  theme_bw() +
  labs(x = "", y=expression(paste("Media: Soil (", mu, "g/g), Water (", 
                                                    mu, "g/mL)", sep=""))) +
  guides(fill=guide_legend(title="Media"))
g_media_box

#### amphibian body burden concentrations
amphib_conc_summary <- amphib_data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Analyte, Media) %>%   # the grouping variable
  summarise(mean_conc = mean(Concentration),  # calculates the mean of each group
            sd_conc = sd(Concentration), # calculates the standard deviation of each group
            n_conc = n(),  # calculates the sample size per group
            se_conc = sd(Concentration)/sqrt(n())) # calculates the standard error of each group
amphib_conc_summary

# bar plot on amphibian body burden summary data
g_bar <- ggplot(amphib_conc_summary, aes(Analyte, mean_conc, fill=Media)) + 
  geom_bar(stat="identity", colour="black", position="dodge") +
  geom_errorbar(aes(ymin=mean_conc, ymax=mean_conc+sd_conc), width=.2, position=position_dodge(.9)) + 
  theme_bw() +
  labs(x = "Treatment", y=expression(paste("Concentration (",mu,"g/g)",sep=""))) +
  guides(fill=guide_legend(title="Media"))
g_bar

# box plot on amphibian  body burden observations
g_box <- ggplot(aes(y=Concentration, x=Analyte, fill=Media), data=amphib_data) + 
  geom_boxplot() + 
  theme_bw() +
  labs(x = "Active Ingredients", y=expression(paste("Amphibian Body Burden (",mu,"g/g)",sep="")))+
  guides(fill=guide_legend(title="Route"))
g_box

#combine and stack figures
figure_stacked <- ggarrange(g_media_box, g_box, heights = c(3.5, 3.5),
                            labels = c("A", "B"),
                            ncol = 1, nrow = 2)
figure_stacked

conc_barplot <- paste(gdr_graphics,"/glinski_dermal_routes_comparison_figure.jpg",sep="")
jpeg(conc_barplot, width = 8, height = 7, units = "in",res=600)
  figure_stacked
dev.off()
