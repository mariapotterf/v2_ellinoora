# Barplots Winners and losers for Deadwood article (Elli's thesis)
# Author: Maria Triviño
# Date 7th Dec 21

personal_theme = theme(plot.title = 
                         element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

# First trying different formats
#Win.RCP45<-c(19, 17, 17, 16, 14, 20, 16)
#Win.RCP85<-c(17, 18, 17, 15, 14, 21, 16) 
#Los.RCP45<-c(2, 4, 4, 5, 7, 1, 5)
#Los.RCP85<-c(4, 3, 4, 6, 7, 0, 5) 
#names(Win.RCP45)=names(Win.RCP85)=names(Los.RCP45)=names(Los.RCP85)=c("BAU","BAU_15","BAUwGTR","BAUwoT","BAUwoT_m20", "CCF","SA")

#RCP45 = rbind(Win.RCP45,Los.RCP45)
#row.names(RCP45) = c("Winners", "Losers")

#Number = c(19, 17, 17, 16, 14, 20, 16,# Win.RCP45
#           2, 4, 4, 5, 7, 1, 5) # Los.RCP45
#Number2 = c(17, 18, 17, 15, 14, 21, 16, # Win.RCP85
#            4, 3, 4, 6, 7, 0, 5)      #Los.RCP85

######################################################
Number = c(19,2, 17, 4, 17, 4, 16,5, 14, 7, 20, 1, 16, 5) # For RCP45 (Winners and losers)
class(Number) #numeric

#sce <- c ("RCP4.5", "RCP8.5")
#Scenario <- rep(sce, each=14)

typ <- c ("Winners", "Losers")
Type <- rep(typ, times = 7)
Type

reg =c("BAU","BAU_15","BAUwGTR","BAUwoT","BAUwoT_m20", "CCF","SA")
Regime = rep(reg, each=2)
Regime

RCP45 = data.frame(cbind(Regime, Type, Number)) # Otherwise is "matrix" "array" 
class(df)
class(RCP45$Number) # "character"
RCP45$Number <- as.numeric(RCP45$Number)
RCP45$Type <- as.factor(RCP45$Type)
RCP45$Type <- factor(RCP45$Type, levels = c("Winners", "Losers"))


fig1 = ggbarplot(RCP45, x = "Regime", y = "Number",
                 fill = "Type", color = "Type",
                 palette = c("gray", "black"),
                 position = position_dodge(0.9),
                 x.text.angle = 45)

Fig_RCP45 = fig1 +
  #xlab ("Scenario") + 
  #ylab("Relative value") +
  ggtitle("Scenario RCP4.5") +
  personal_theme +
  theme(legend.position="none") +
  theme(legend.title=element_blank()) +
  theme(legend.position="right")

# Same but for scenario RCP85
Number = c(17, 4, 18, 3, 17, 4, 15, 6, 14, 7, 21, 0, 16, 5)# For RCP85 (Winners and losers)
RCP85 = data.frame(cbind(Regime, Type, Number)) # Otherwise is "matrix" "array" 
RCP85$Number <- as.numeric(RCP85$Number)
RCP85$Type <- factor(RCP85$Type, levels = c("Winners", "Losers"))

fig2 = ggbarplot(RCP85, x = "Regime", y = "Number",
                 fill = "Type", color = "Type",
                 palette = c("gray", "black"),
                 position = position_dodge(0.9),
                 x.text.angle = 45)
Fig_RCP85 = fig2 +
  ggtitle("Scenario RCP8.5") +
  personal_theme +
  theme(legend.title=element_blank()) +
  theme(legend.position="right")

fig3 <- ggarrange(Fig_RCP45, Fig_RCP85,
                  labels = c("a", "b"),
                  ncol = 2, nrow = 1)
fig3
ggsave(file = "Fig Win_Los2 Dec21.tiff", plot = fig3, width=25, height=10, units="cm", dpi=120, compression="lzw")
