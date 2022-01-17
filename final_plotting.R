##Data selection of my thesis results##
##modified 17.11.2021##


# Maya changes: 
#               simplified names of regimes
#               removed the extra productive sites 
#               (having unrealistically high volume > 1000 m3/ha under RCP 85?)
#               removed Vaasa from plots
#               updated summary tables


##modifying and organizing the data ## 
#remove all objects from previous session#
rm(list=ls())
#first remember to set the path for the project#
path <- paste0(getwd(),"/")


#set libraries that I need#
library(ggplot2)
library(plyr)
library(tidyverse)
library(dplyr)
library(data.table)

#reading the data in#

rslt_CC45_all_dw       <- fread(paste0(path, "output/rslt_all_CC45_dw.csv"),       fill = TRUE) # Fil = T to avoid warning msg about 'Discarded single-line footer'
rslt_CC45_SA_all_dw    <- fread(paste0(path, "output/rslt_all_CC45_SA_dw.csv"),    fill = TRUE)
rslt_CC85_all_dw       <- fread(paste0(path, "output/rslt_all_CC85_dw.csv"),       fill = TRUE)
rslt_CC85_SA_all_dw    <- fread(paste0(path, "output/rslt_all_CC85_SA_dw.csv"),    fill = TRUE)
rslt_without_all_dw    <- fread(paste0(path, "output/rslt_all_without_dw.csv"),    fill = TRUE)
rslt_without_SA_all_dw <- fread(paste0(path, "output/rslt_all_without_SA_dw.csv"), fill = TRUE)


##Binding of 2 SA scenarios together in order to create 3 datasets for each climate scenario##
rsltCC45_dw      <- rslt_CC45_all_dw %>%    rbind (rslt_CC45_SA_all_dw)
rsltCC85_dw      <- rslt_CC85_all_dw %>%    rbind (rslt_CC85_SA_all_dw)
rslt_without_dw  <- rslt_without_all_dw %>% rbind (rslt_without_SA_all_dw)

#Now I combine three climate scenarios into one dataframe#
rslt <- rbind(rslt_without_dw, rsltCC45_dw, rsltCC85_dw)



# Subset only regimes of interest --------------------------------------------------------------------
my_regimes = c("SA" ,"BAU", "BAUwoT",  "BAU_15", "BAUwoT_m20", "BAUwGTR", "CCF_2")

# Subset only regimes of interest, simplify the regimes names
rslt2 <- rslt %>% 
  filter(regime %in% my_regimes) %>% 
  mutate(regime = case_when(regime == "BAUwoT"     ~ 'noThin',
                                regime == "BAU_15"     ~ 'extended',
                                regime == "BAUwoT_m20" ~ 'shortened',
                                regime == "BAUwGTR"    ~ 'GTR',
                                regime == "CCF_2"      ~ 'CCF',
                                regime == "BAU"        ~ 'BAU',
                                regime == "SA"         ~ 'SA'))


# Make category for managed/non-managed stands to create Table 6.
rslt2 <- rslt2 %>% 
  #filter(regime %in% my_regimes) %>% 
  mutate(mng = case_when(regime == "SA"     ~ 'set aside',
                         regime != "SA"     ~ 'managed'))


#scenario and watershed are separated to different columns#
rslt2 <- rslt2 %>%  separate(name, c("scenario","gpkg"), sep = "_MV_")


##Changing the name of scenarios##
class(rslt2$scenario)
#I change the order of climate scenarios#
str(rslt2$scenario)
rslt2$scenario <- factor(rslt2$scenario, levels=unique(rslt2$scenario))


#Then I rename scenario names#
rslt2$scenario <- revalue(rslt2$scenario, c("without"    = "Reference", 
                                            "without_SA" = "Reference", 
                                            "CC45"       = "RCP4.5", 
                                            "CC45_SA"    = "RCP4.5", 
                                            "CC85"       = "RCP8.5", 
                                            "CC85_SA"    = "RCP8.5"))

# exclude Vaala: 
unique(rslt2$gpkg)  # "Vaala_rsu" 


# Filtered values only for interested regimes
rslt2 <- rslt2 %>% 
  filter(gpkg != "Vaala_rsu")


# Exclude stands with excessive numbers of volume > 1000
# identify the corrupted stands: 600 ids? in each regime, in all scenarios
exclude_ids <- rslt2 %>% 
  filter(V>1000) %>% 
  distinct(id)  %>% 
  pull()  # make a vector out of it to iuse it to exclude the ids


# Filyter values with unrealistic volume
rslt2 <- rslt2 %>% 
  dplyr::filter(!id %in% exclude_ids)



##Choosing the variables from original table ---------------------------

#6 species got only zero HSI values so I don't include them to the table#
#they are species number: 3,4,14,24,25,27



# Choosing the variables from csv-tables  --------------------------------------

#HSI:s, name of the watershed, regime#
rslt_HSI <- rslt2[,c("id", "year", "HSI_RL_S1", 
                    "HSI_RL_S2",  "HSI_RL_S5",  "HSI_RL_S6",  "HSI_RL_S7", 
                    "HSI_RL_S8",  "HSI_RL_S9",  "HSI_RL_S10", "HSI_RL_S11", 
                    "HSI_RL_S12", "HSI_RL_S13", "HSI_RL_S15", "HSI_RL_S16", 
                    "HSI_RL_S17", "HSI_RL_S18", "HSI_RL_S19", "HSI_RL_S20", 
                    "HSI_RL_S21", "HSI_RL_S22", "HSI_RL_S23", "HSI_RL_S26",
                    "scenario",   "regime")]



#for studying the timber volumes and deadwood volumes - table 5 # 
# Timber volume is only for last years??

rslt_volumes <- rslt2[,c("id", "year", "V_total_deadwood", "V", "scenario", "regime", "mng")] # mng contains value if it is managed or not
T1 <- c(2081, 2086, 2091, 2096, 2101, 2106, 2111)
rslt_volumes <- filter(rslt_volumes, year %in% T1)


# Get a summary table for deadwood by regimes and scenarios
summary_tab <- 
  rslt_volumes %>% 
  group_by(scenario, mng) %>% 
  summarize(mean_V_dw  = mean(V_total_deadwood, na.rm = T),
            sd_V_dw    = sd(V_total_deadwood, na.rm = T),
           # median_V_dw = median(V_total_deadwood, na.rm=T),
            min_V_dw   = min(V_total_deadwood, na.rm=T),
            max_V_dw   = max(V_total_deadwood, na.rm=T),
            mean_V     = mean(V, na.rm = T),
            sd_V       = sd(V, na.rm = T),
           # median_V   = median(V, na.rm=T),
            min_V      = min(V, na.rm=T),
            max_V      = max(V, na.rm=T)) %>% 
  mutate_if(is.numeric, round, 1) #%>%   # keep only 2 decimal numbers


# merge columns together and export as final Table 3
summary_tab_clean <- 
  summary_tab %>% 
    mutate(
      Deadwood_mean    = stringr::str_glue("{mean_V_dw}±{sd_V_dw}"),
      Deadwood_range   = stringr::str_glue("{min_V_dw}-{max_V_dw}"),
      Volume_mean      = stringr::str_glue("{mean_V}±{sd_V}"),
      Volume_range     = stringr::str_glue("{min_V}-{max_V}")) %>% 
  dplyr::select(mng, scenario,Deadwood_mean, Deadwood_range, Volume_mean, Volume_range ) 
    




# Check timber range -----------------------------------------------------

range(rslt_volumes$V_total_deadwood) #0.01881114 66.29698181
range(rslt_volumes$V)  # 0.000 1155.354 # Crazy volume! SIMO simulates until 600 m3/ha:
 # can happen under very productivce stands 


# random pick: 27462752 26196818
# @Kyle:
#There must be a decision rule that isn't operating correctly 
# (i.e. the height or BA limits are not being meet -- which doesn't make sense.)
# I'd just expect that they should be harvested before they get that large.
# BAU woT should be similar to SA - unless there is a regeneration occuring at the beginning of the time horizon

# 
# rslt_volumes %>% 
#   filter(id == '29609175') #%>%  # 29609175 is over 1100 m3/ha
#   ggplot(aes(x = year,
#              y = V_total_deadwood        ,# V,
#              color = regime)) +
#   geom_line() +
#   facet_grid(.~scenario)


#Renaming of indicator species according to their real names#
  # ------------------
# I had to include the "dplyr::" before rename --> otherwise it did not work
# ------------------

rslt_HSI <- rslt_HSI %>%
   dplyr::rename(Ditylus_laevis      = HSI_RL_S1,
              Ceruchus_chrysomelinus = HSI_RL_S2,
              Antrodia_primaeva      = HSI_RL_S5,
              Antrodia_albobrunnea   = HSI_RL_S6,
              Zavaljus_brunneus      = HSI_RL_S7,
              Wagaicis_wagai         = HSI_RL_S8,
              Hololepta_plana        = HSI_RL_S9,
              Cyphaea_latiuscula     = HSI_RL_S10,
              Pytho_kolwensis        = HSI_RL_S11,
              Piloporia_sajanensis   = HSI_RL_S12,
              Amyloporia_sitchensis  = HSI_RL_S13,
              Oligoporus_lowei       = HSI_RL_S15,
              Amyloporia_crassa      = HSI_RL_S16,
              Rigidoporus_crocatus   = HSI_RL_S17,
              Gloiodon_strigosus     = HSI_RL_S18,
              Cybebe_gracilenta      = HSI_RL_S19,
              Protomerulius_caryae   = HSI_RL_S20,
              Lacon_fasciatus        = HSI_RL_S21,
              Boros_schneideri       = HSI_RL_S22,
              Acmaeops_septentrionis = HSI_RL_S23,
              Tropideres_dorsalis    = HSI_RL_S26)



#Let's check the column names#
colnames(rslt_HSI)

##Selection of management regimes##

#This is for studying the effects of climate change#
rslt_HSI_cc <- subset(rslt_HSI, regime == "SA")
length(unique(rslt_HSI$id)) #1758







##########Plotting the effects of climate change on HSI-index#####################

#Organizing the data before plotting#
##Including all species but labeling them with microclimatic preference##

indifferent <- c("Ditylus_laevis", 
                 "Ceruchus_chrysomelinus", 
                 "Antrodia_primaeva", 
                 "Antrodia_albobrunnea", 
                 "Zavaljus_brunneus", 
                 "Wagaicis_wagai", 
                 "Hololepta_plana")

shady <- c("Cyphaea_latiuscula", 
           "Pytho_kolwensis", 
           "Piloporia_sajanensis", 
           "Amyloporia_sitchensis",
           "Oligoporus_lowei", 
           "Amyloporia_crassa", 
           "Rigidoporus_crocatus", 
           "Gloiodon_strigosus",
           "Cybebe_gracilenta", 
           "Protomerulius_caryae")

sunny <- c("Lacon_fasciatus", 
           "Boros_schneideri", 
           "Acmaeops_septentrionis", 
           "Tropideres_dorsalis")




# ----------------------------------------------------------------------------
#                                   Plotting  
# ----------------------------------------------------------------------------

# Set themes ----
theme_set(theme_classic())
theme_update(panel.grid.major = element_line(colour = "grey95",
                                             size = 0.1,
                                             linetype = 2),
             strip.background = element_rect(color="grey95", 
                                             fill="grey95",
                                             size=0.1, 
                                             linetype="solid"))



#set color palette# --------------------------------------------------------

color_palette <- c("black", "#56B4E9", "#E69F00", "#009E73",
                   "#CC79A7", "#F0E442", "#0072B2", "#D55E00")





#Changing the data format from wide to long #
rslt_gather <- gather(rslt_HSI_cc,
                      key="Indicator_species",
                      value="HSI_value",
                      Ditylus_laevis:Tropideres_dorsalis)

rslt.plot <- rslt_gather %>% 
  dplyr::mutate(Groups = ifelse(Indicator_species %in% indifferent, "Indifferent",
                                ifelse(Indicator_species %in% shady, "Shady","Sunny")))
table(rslt.plot$Groups)#756560 rows indiff, 1080800 shady, 432320 sunny
dim(rslt.plot)#2269680, 7


#Reordering factor levels of Indicator species-variable#
rslt.plot$Indicator_species<- factor(rslt.plot$Indicator_species, 
                                     levels=unique(rslt.plot$Indicator_species))
levels(rslt.plot$Indicator_species)


##PLOTTING## --------------------------------------------------------------------------

# Effect of climate change on HSI -----------------------------------------------------

##FIGURE 3 & 4##

##with current scenario as a base line ##
## with microclimatic groups##

#calculating relative differences from current climate scenario# 
#clemens corrected this code, now relative diff is calculated correctly#
rslt.mean <- 
  rslt.plot%>% 
  group_by(year, scenario, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%


rslt.mean.out <- 
  rslt.mean%>% 
  group_by(year, Groups) %>% 
  dplyr::mutate(HSI_mean_current = HSI_mean[scenario == "Reference"])%>%
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_current)/ HSI_mean_current * 100)#%>%

range(rslt.mean.out$relativediff)#relative diff is between -4,58 - 3,97#




# --------------------------------------------------------------------------
#I select only cc scenarios for the plot#
rslt.mean.out.onlycc <- dplyr::filter(rslt.mean.out, scenario != "Reference")



#Lineplot - Figure 3#
windows(width = 7, height = 3.2)
rslt.mean.out.onlycc %>%
  #group_by(year, scenario, Groups)  %>%
  #summarise(diff_mean = mean(diff, na.rm=TRUE)) %>%
  ggplot(aes(x = year,
             y = relativediff,
             group= Groups,
             colour= Groups))+ 
  #scale_y_continuous(labels=scales::percent, limits = c(-0.001, 0.01)) +#
  geom_line(size = 1) +  
  geom_hline(yintercept=0,         # add horizontal line at value 0
             linetype="dashed", 
             col = color_palette[8]) +
  geom_label(aes(x = 2080, y = 0.0004), col = color_palette[8], 
             label = "Reference climate mean", size = 3) +
  #theme_bw(base_size = 20)+
  theme(axis.title.x=element_blank()) +
  theme(legend.title = element_blank()) + 
  theme(legend.position="bottom") +
  #theme(axis.text.x = element_text(angle = 45, #    
  #hjust = 1)) +#
  ylab("Relative difference in HSI (%)") +
  scale_color_manual(values = color_palette) +
  facet_wrap(vars(scenario)) +
  theme(axis.title   = element_text(size = 12),  # labels size
        axis.text    = element_text(size = 10),
        legend.text  = element_text(size = 10),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, 
                                    size=1),
        strip.background =element_rect(color="black", size = 1))  # legend size




## with microclimatic groups - FIGURE 3##

#calculating relative differences from current climate scenario# 
#clemens corrected this code, now relative diff is calculated correctly#


rslt.mean <- 
  rslt.plot%>% 
  group_by(year, scenario, Indicator_species, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%

range(rslt.mean$HSI_mean)

rslt.mean.out <- 
  rslt.mean%>% 
  group_by(year, Indicator_species, Groups) %>% 
  dplyr::mutate(HSI_mean_current = HSI_mean[scenario == "Reference"])%>%
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_current)/ HSI_mean_current * 100)#%>%

range(rslt.mean.out$relativediff)#relative diff is between -4,58 - 3,97#

#I select only cc scenarios for the plot#
rslt.mean.out.onlycc <- dplyr::filter(rslt.mean.out, scenario != "Reference")



# Figure SX: Differences in HSI for individual species ----------------------------------------------


#boxplot with relative change in HSI to current climate - Figure 4#
rslt.mean.out.onlycc %>%
  ggplot(aes(x = Indicator_species,
             y = relativediff,
             colour = Groups)) + 
  geom_boxplot() +
  coord_flip() + 
  stat_boxplot(geom = 'errorbar') +
  geom_hline(yintercept=0,
             linetype="dashed", 
             col = color_palette[8]) +
  theme_bw(base_size = 20)+
  theme(axis.title.y =element_blank()) +
  theme(legend.title = element_blank()) + 
  theme(legend.position="top") +
  #theme(axis.text.x = element_text(hjust = 1)) +
  ylab("Relative difference in HSI (%)") +
  scale_color_manual(values = color_palette) +
  facet_wrap(vars(scenario))





########Effects of forest management ######################

#choosing the data#
rslt_HSI_fm<- rslt_HSI  

length(unique(rslt_HSI$id))      # 1759
length(unique(rslt_HSI$regime))  # 7

dim(rslt_HSI)#550920 rows, 25 columns#


#Grouping by microclimatic preferences#

##Including all species but labeling them with microclimatic preference##
#I choose which species belong to each group#


#choosing current climate scenario only#
current <- subset(rslt_HSI, scenario == "Reference")

#changing the data to long format#
rslt_gather <- gather(current,
                      key="Indicator_species",
                      value="HSI_value",
                      Ditylus_laevis:Tropideres_dorsalis)

#

rslt.plot <- rslt_gather %>% 
  dplyr::mutate(Groups = ifelse(Indicator_species %in% indifferent, "indifferent",
                                ifelse(Indicator_species %in% shady, "shady","sunny")))
table(rslt.plot$Groups)
dim(rslt.plot)#3879120       , 7


#Reordering factor levels of Indicator species-variable#
rslt.plot$Indicator_species<- factor(rslt.plot$Indicator_species, 
                                     levels=unique(rslt.plot$Indicator_species))
levels(rslt.plot$Indicator_species)


#creating new column "Groups" including the names of microclimatic groups#
# 
# rslt_indifferent <- filter(rslt_gather, 
#                            Indicator_species %in% indifferent)
# rslt_shady       <- filter(rslt_gather, 
#                            Indicator_species %in% shady)
# rslt_sunny       <- filter(rslt_gather, 
#                            Indicator_species %in% sunny)
# 
# rslt.plot <- rslt_gather %>% 
#   dplyr::mutate(Groups = ifelse(Indicator_species %in% indifferent, "indifferent",
#                                 ifelse(Indicator_species %in% shady, "shady", "sunny")))


#Reordering factor levels of Indicator species-variable# !!!! need to check this part of code: why is it needed??
#rslt.plot$Indicator_species<- factor(rslt2$Indicator_species, levels=unique(rslt2$Indicator_species))
#levels(rslt2$Indicator_species)




####### BY USING RELATIVE VALUES #############
####with 2016 as a base line #########

##Lineplot with mc groups##

rslt.mean <- 
  rslt.plot%>% 
  group_by(year, regime, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%


rslt.mean.out <- 
  rslt.mean %>% 
  group_by(regime, Groups) %>% 
  dplyr::mutate(HSI_mean_2016 = HSI_mean[year == "2016"])%>% 
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_2016)/ HSI_mean_2016 * 100)#%>%


#Lineplot - Figure 7#
windows(width =7 , height = 3.2)
rslt.mean.out %>%
  ggplot(aes(x = year,
             y = relativediff,
             group= regime,
             colour= regime))+ 
  geom_line(size = 1) +
  geom_hline(yintercept=0, 
             linetype="dashed", 
             col = color_palette[8]) +
  geom_label(aes(x = 2080, y = 0.0004), col = color_palette[8], 
             label = "year 2016 mean", size = 3) +
   ylab("Relative difference in HSI (%)") +
  scale_color_manual(values = color_palette) +
  facet_wrap(vars(Groups)) +
  theme(axis.title.x =element_blank(), 
        legend.title = element_blank(),
        legend.position ="bottom",
        axis.title   = element_text(size = 12),  # labels size
        axis.text    = element_text(size = 10),
        legend.text  = element_text(size = 10),
        panel.border = element_rect(colour = "black", 
                                    fill=NA, 
                                    size=1),
        strip.background =element_rect(color="black", size = 1)) 



##Plotting the effects of climate change on HSI-index - FIGURES 10&11##

#Organizing the data before plotting#

rslt_HSI_ccfm <-rslt_HSI

##Including all species but labeling them with microclimatic preference##
#I choose which species belong to each group#

#Changing the data fromat to long format#
rslt_gather <- gather(rslt_HSI_ccfm,
                      key   ="Indicator_species",
                      value ="HSI_value",
                      Ditylus_laevis:Tropideres_dorsalis)

rslt.plot <- rslt_gather %>% 
  dplyr::mutate(Groups = ifelse(Indicator_species %in% indifferent, "indifferent",
                                ifelse(Indicator_species %in% shady, "shady","sunny")))
table(rslt.plot$Groups)#756560 rows indiff, 1080800 shady, 432320 sunny
dim(rslt.plot)#2269680, 7



#Reordering factor levels of Indicator species-variable#
rslt.plot$Indicator_species<- factor(rslt.plot$Indicator_species, 
                                     levels=unique(rslt.plot$Indicator_species))
levels(rslt.plot$Indicator_species)



##Boxplot with indicator species with current climate as baseline - FIGURE 11#

rslt.mean <- 
  rslt.plot%>% 
  group_by(year, scenario, regime, Indicator_species, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%

range(rslt.mean$HSI_mean)

rslt.mean.out <- 
  rslt.mean%>% 
  group_by(year, regime, Indicator_species, Groups) %>% 
  dplyr::mutate(HSI_mean_current = HSI_mean[scenario == "Reference"])%>%
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_current)/ HSI_mean_current * 100)#%>%

range(rslt.mean.out$relativediff)#relative diff is between -4,58 - 3,97#

#I select only cc scenarios for the plot#
rslt.mean.out.onlycc <- dplyr::filter(rslt.mean.out, scenario != "Reference")



#boxplot with relative change in HSI to current climate - Figure 11#
windows(width = 7, height = 4)
rslt.mean.out.onlycc %>%
  #mutate(Indicator_species = gsub("_", " ", Indicator_species)) %>% # replace the '_' character in the species names 
  ggplot(aes(x = Indicator_species,
             y = relativediff,
             colour = Groups)) + 
  geom_boxplot() +
  coord_flip() + 
  stat_boxplot(geom = 'errorbar') +
  geom_hline(yintercept=0,
             linetype="dashed", 
             col = color_palette[8]) +
  xlab("Indicator species") +
  ylab("Relative difference in HSI (%)") +
  scale_color_manual(values = color_palette) +
  facet_grid(.~ scenario) +
    theme(axis.title   = element_text(size = 12),  # labels size
          axis.text    = element_text(size = 10),
          legend.text  = element_text(size = 10),
          legend.position  = 'bottom',
          panel.border = element_rect(colour = "black", 
                                      fill=NA, 
                                      size=1),
          strip.background =element_rect(color="black", size = 1))  # legend size


#Lineplot _ FIGURE 10#


#calculating relative differences from current climate scenario# 
#clemens corrected this code, now relative diff is calculated correctly#
rslt.mean <- 
  rslt.plot%>% 
  group_by(year, scenario, regime, Groups) %>%
  dplyr::summarise(HSI_mean = mean(HSI_value, na.rm=TRUE))# %>%


rslt.mean.out <- 
  rslt.mean%>% 
  group_by(year, regime, Groups) %>% 
  dplyr::mutate(HSI_mean_current = HSI_mean[scenario == "Reference"])%>%
  dplyr::mutate(relativediff = (HSI_mean - HSI_mean_current)/ HSI_mean_current * 100)#%>%

#dplyr::mutate(diff = HSI_mean - HSI_mean_current) %>%#



#I drop out current climate scenario because I don't want it to my plot#
rslt.mean.out.onlycc <- dplyr::filter(rslt.mean.out, scenario != "Reference")


#Changing the grouping variable from character to a factor#
typeof(rslt.mean.out.onlycc$Groups)
rslt.mean.out.onlycc$Groups <- factor(rslt.mean.out.onlycc$Groups)
class(rslt.mean.out.onlycc$Groups) 



#Plotting the relative differences between climate scenarios with microclimatic groups#
#Lineplot - Figure 10#
windows(width = 7, height = 4.5)
rslt.mean.out.onlycc %>%
  ggplot(aes(x = year,
             y = relativediff,
             group= regime,
             colour= regime))+ 
  geom_line(size = 1) +  
  geom_hline(yintercept=0,         # add horizontal line at value 0
             linetype="dashed", 
             col = color_palette[8]) +
  #geom_label(aes(x = 2080, y = 0.0004), col = color_palette[8], 
  #label = "Reference climate mean", size = 5) +
  ylab("Relative difference in HSI (%)") +
  scale_color_manual(values = color_palette) +
  facet_grid(scenario ~ Groups) +
  theme(axis.title   = element_text(size = 12),  # labels size
        axis.text    = element_text(size = 10),
        legend.text  = element_text(size = 10),
        legend.position  = 'bottom',
        panel.border = element_rect(colour = "black", 
                                    fill=NA, 
                                    size=1),
        strip.background =element_rect(color="black", size = 1))  # legend size

