#new try for the effects of management only#
#12.12.2021#

#initial modifying and organinizing the data is the same than in other figures#
#this code doesn't include those#

#choosing the data#
rslt_HSI_fm<- subset(rslt_HSI, regime == "SA" | regime == "BAU" | regime == "BAUwoT" | 
                       regime == "BAU_15" | regime == "BAUwoT_m20" |
                       regime == "BAUwGTR" | regime == "CCF_2")

length(unique(rslt_HSI$id))      # 1802
length(unique(rslt_HSI$regime))  # 7

dim(rslt_HSI)#675180 rows, 25 columns#


#Grouping by microclimatic preferences#

##Including all species but labeling them with microclimatic preference##
#I choose which species belong to each group#


#choosing current climate scenario only#
current <- subset(rslt_HSI_fm, scenario == "Current")

#changing the data to long format#
rslt_gather <- gather(current,
                      key="Indicator_species",
                      value="HSI_value",
                      Ditylus_laevis:Tropideres_dorsalis)

#creating microclimatic groups#
indifferent <- c("Ditylus_laevis", "Ceruchus_chrysomelinus", "Antrodia_primaeva", 
                 "Antrodia_albobrunnea", "Zavaljus_brunneus", "Wagaicis_wagai", "Hololepta_plana")
shady <- c("Cyphaea_latiuscula", "Pytho_kolwensis", "Piloporia_sajanensis", "Amyloporia_sitchensis",
           "Oligoporus_lowei", "Amyloporia_crassa", "Rigidoporus_crocatus", "Gloiodon_strigosus",
           "Cybebe_gracilenta", "Protomerulius_caryae")
sunny <- c("Lacon_fasciatus", "Boros_schneideri", "Acmaeops_septentrionis", "Tropideres_dorsalis")


rslt.plot <- rslt_gather %>% 
  dplyr::mutate(Groups = ifelse(Indicator_species %in% indifferent, "indifferent",
                                ifelse(Indicator_species %in% shady, "shady","sunny")))
table(rslt.plot$Groups)
dim(rslt.plot)#4715760, 7


#Reordering factor levels of Indicator species-variable#
rslt.plot$Indicator_species<- factor(rslt.plot$Indicator_species, levels=unique(rslt.plot$Indicator_species))
levels(rslt.plot$Indicator_species)


####### BY USING RELATIVE VALUES #############


rslt.mean <- 
  rslt.plot%>% 
  group_by(regime, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%


rslt.mean.out <- 
  rslt.mean %>% 
  group_by(Groups) %>% 
  dplyr::mutate(HSI_mean_BAU = HSI_mean[regime == "BAU"])%>% 
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_BAU)/ HSI_mean_BAU * 100)#%>%

#I drop out the BAU from the plot#
rslt.mean.out.notBAU <- dplyr::filter(rslt.mean.out, regime != "BAU")

#Barplot#

rslt.mean.out.notBAU %>%
  ggplot(aes(x=regime,
             y=relativediff, 
             fill=Groups))+ 
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  scale_fill_manual("legend", values = c("indifferent" = "black", "shady" = "#56B4E9", "sunny" = "#E69F00"))+
  labs(x="management type", y = "relative difference in HSI (%)")+
  theme_bw(base_size = 20)+
  theme(axis.title.x=element_blank())+
  theme(legend.title = element_blank()) + 
  theme(legend.position="top")


#Lineplot#

rslt.mean <- 
  rslt.plot%>% 
  group_by(year, regime, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%


rslt.mean.out <- 
  rslt.mean %>% 
  group_by(year, Groups) %>% 
  dplyr::mutate(HSI_mean_BAU = HSI_mean[regime == "BAU"])%>% 
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_BAU)/ HSI_mean_BAU * 100)#%>%

#I drop out the BAU from the plot#
rslt.mean.out.notBAU <- dplyr::filter(rslt.mean.out, regime != "BAU")

color_palette <- c("black", "#56B4E9", "#E69F00", "#009E73",
                   "#CC79A7", "#F0E442", "#0072B2", "#D55E00")






