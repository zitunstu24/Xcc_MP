library(openxlsx)
library(ggplot2)
library(ggpubr)
library(cowplot)


rm(list=ls())
set.seed(123)

input = read.xlsx("inputs/data.xlsx")
str(input)
input[input == 0.0] = 0
############# checking if there is any NA values

table(is.na(input))

######### trabsforming the variables in to numeric 
input$AE.25 = as.numeric(input$AE.25)
input$AE.50 = as.numeric(input$AE.50)
input$EE.25 = as.numeric(input$EE.25)
input$EE.50 = as.numeric(input$EE.50)

g1 = ggscatter(input, x="AE.50", y="EE.50", add = "reg.line",
              xlab = "50% aqueous extracts", 
              ylab = "50% ethanol extracts")


g2 = ggscatter(input, x="AE.25", y="EE.50", add = "reg.line",
               xlab = "25% aqueous extracts",
               ylab = "")

g3 = ggscatter(input, x="AE.50", y="EE.25", add = "reg.line",
               xlab = " ", 
               ylab = "25% ethanol extracts")


g4 = ggscatter(input, x="AE.25", y="EE.25", add = "reg.line",
               xlab = " ",
               ylab = " ")+
  stat_cor(method = "pearson", label.x = 3, label.y = 30)

pdf("outputs/correlation.pdf",
    width = 6.13,
    height = 5.60)
plot_grid(g3, g4, g1, g2)
dev.off()

types = factor(input$tree.type)
tree = factor(input$med.plants)

pdf("outputs/boxplot_tree_types.pdf")
boxplot(cbind(AE.25, AE.50, EE.25, EE.50)~types, input, ylab = "Extracts", xlab = "Tree types")
dev.off()


model = lm(AE.25~types+tree+types*tree, input)
af <- anova(model)
anova(model)
afss <- af$"Sum Sq"
ve.ae.25 = cbind(af,PctExp=afss/sum(afss)*100)
ve.ae.25$variables = row.names(ve.ae.25)
ve.ae.25.pie = ve.ae.25[, c(6,7)]

pie = ggplot(ve.ae.25.pie, aes(x="", y=PctExp, fill=variables)) + geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(PctExp, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"))+ 
  labs(x = NULL, y = NULL, fill = NULL, title = "25% aqeous extracts")+
  theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))


model = lm(AE.50~types+tree+types*tree, input)
af <- anova(model)
anova(model)
afss <- af$"Sum Sq"
ve.ae.50 = cbind(af,PctExp=afss/sum(afss)*100)
ve.ae.50$variables = row.names(ve.ae.50)
ve.ae.50.pie = ve.ae.50[, c(6,7)]

pie1 = ggplot(ve.ae.50.pie, aes(x="", y=PctExp, fill=variables)) + geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(PctExp, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"))+ 
  labs(x = NULL, y = NULL, fill = NULL, title = "50% aqeous extracts")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

model = lm(EE.25~types+tree+types*tree, input)
af <- anova(model)
anova(model)
afss <- af$"Sum Sq"
ve.ee.25 = cbind(af,PctExp=afss/sum(afss)*100)

ve.ee.25$variables = row.names(ve.ee.25)
ve.ee.25.pie = ve.ee.25[, c(6,7)]
pie2 = ggplot(ve.ee.25.pie, aes(x="", y=PctExp, fill=variables)) + geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(PctExp, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"))+ 
  labs(x = NULL, y = NULL, fill = NULL, title = "25% ethanol extracts")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

model = lm(EE.50~types+tree+types*tree, input)
af <- anova(model)
anova(model)
afss <- af$"Sum Sq"
ve.ee.50 = cbind(af,PctExp=afss/sum(afss)*100)
ve.ee.50$variables = row.names(ve.ee.50)
ve.ee.50.pie = ve.ee.50[, c(6,7)]
pie3 = ggplot(ve.ee.50.pie, aes(x="", y=PctExp, fill=variables)) + geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  geom_text(aes(label = paste0(round(PctExp, 1), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"))+ 
  labs(x = NULL, y = NULL, fill = NULL, title = "50% ethanol extracts")+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

pedf("outputs/variance_explained.pdf")
plot_grid(pie, pie1, pie2, pie3)
dev.off()
