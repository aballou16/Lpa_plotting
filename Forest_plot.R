### Script for creating a Forest Plot for SNP effect size and confidence intervals

## Load packages & data
library(tidyverse)
forest_data <- read.csv("SNPS_dummy_data.csv", header = TRUE)

## Data wrangling 
#put SNPs in order by chromosome position
forest_data_2 <- forest_data %>% 
  arrange(CHR, Position) 

#create factor levels for plotting puposes 
forest_data_2$SNP_factor <- factor(forest_data_2$SNP, 
                              levels=c("RSID_1", "RSID_2",
                                       "RSID_3", "RSID_4",
                                       "RSID_5", "RSID_6",
                                       "RSID_7", "RSID_8",
                                       "RSID_9", "RSID_10", 
                                       "RSID_11", "RSID_12",
                                       "RSID_13", "RSID_14",
                                       "RSID_15", "RSID_16",
                                       "RSID_17", "RSID_18"))


## Plotting
# Forest plots by strata
forest_plot <- ggplot(data = forest_data_2, 
                 aes(x=SNP_factor, y = Beta, ymin = lower_lim, ymax = upper_lim, 
                     shape = as.factor(Lead_SNP))) + 
  geom_pointrange() +
  geom_hline(aes(yintercept = 0), linetype = 2) + #null effect line
  xlab("SNP") +
  ylab("Beta (95% Confidence Interval)") +
  scale_y_continuous(limits=c(-1.3, 1.3), expand = c(0,0)) +
  geom_errorbar(aes(ymin = lower_lim, ymax = upper_lim), width = 0.1, cex=1) +
  ggtitle("Forest Plot") +
  facet_wrap(~Strata, strip.position = "top", nrow = 1, scale = "free_y") + #split by strata
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title = element_blank(),
        strip.text.y = element_text(hjust=0, vjust=1, angle=180, face = "bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_discrete(limits = rev(levels(forest_data_2$SNP_factor))) +
  scale_shape_manual(values = c(16, 17), aes(fill = "Shape"), labels = c("Non-Lead SNP", "Lead SNP")) + #shape based on lead SNP vs. non-lead SNP
  coord_flip() #flip axes to make vertical 

# Coded allele frequency by strata
caf_plot <- ggplot(data = forest_data_2, 
                    aes(x=SNP_factor, y = CAF)) +
  geom_point(shape = 21, aes(fill = Strata), size = 2) + #color by strata
  xlab("SNP") +
  ylab("Coded Allele Frequency") +
  scale_y_continuous(limits = c(-0.05, 1.05), expand = c(0,0)) +
  ggtitle("Coded Allele Frequencies") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title = element_blank(),
        strip.text.y = element_text(hjust=0, vjust=1, angle=180, face = "bold"),
        strip.text.x = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_discrete(limits = rev(levels(forest_data_2$SNP_2))) +
  scale_fill_manual(values = c("white", "#afd5a0", "#5d9c53", "#00441d")) + #set color scheme
  coord_flip()




