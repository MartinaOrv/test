setwd("C://Users/Martina/OneDrive - University of Cambridge/MPhil")

library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv("colPT11rep.csv")
data <- read.csv("colonisation_confocal.csv")
data <- read.csv("all colonisation.csv")
data <- read.csv("RNA colonisation.csv")
data <- read.csv("UP225col.csv")

data2 <- read.csv("colPT11repeat.csv")
data2 <- read.csv("col_con_avg.csv")
data2 <- read.csv("all colonisation avg.csv")

my_comparisons <- list( c("HNLP", "HNHP"),
                        c("HNLP", "LNHP"),
                        c("HNLP", "LNLP"),
                        c("LNLP", "LNHP"),
                        c("LNLP", "HNHP"),
                        c("LNHP", "HNHP"))

plot <- data %>%
  arrange(total) %>%
  mutate(treatment = factor(treatment, levels=c("HNLP", "HNHP", "LNHP", "LNLP"))) %>%
ggplot(aes(x = treatment, y = total)) +
  geom_bar(aes(fill = treatment), show.legend = FALSE, position = "dodge", stat = "summary", fun = "mean") +
  geom_point(color = "black", shape = 21, size = 2,aes(fill = treatment) ) +
 scale_fill_manual(values=c("#2B7B00", "#9A49D7", "#E66AD7", "#65B39F")) +
scale_color_manual(values=c("#2B7B00", "#9A49D7", "#E66AD7", "#65B39F")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("% of root length with arbuscules") +
  xlab("nutrient regime") +
  geom_signif(comparisons = my_comparisons, test = t.test,
              y_position = c(0.6,0.55,0.5,0.45,0.4,0.35), 
              vjust = 0.6, map_signif_level=TRUE, color = "black")
 # ylim(0,95)


plot


ggsave("colonisation RNA.png", plot, height = 5, width = 4, dpi = 320)

aov.all <- aov(total ~ treatment, data = data)
summary(aov.all)
TukeyHSD(aov.all)
