#Figure 2 graphs

#load data

graph<-read.csv("graphs.csv",header=T) 

##calculate the mean and standard error of the number of fungal colonies cultured from bats #belonging to each WNS-susceptibility group in each geographic site

sum_graph_site <- graph %>%
  group_by(SiteC, Group) %>%
  summarise(n = length(all_fungi), mean = mean(all_fungi), sd = sd(all_fungi), se= sd/sqrt(n))

#arrange by group

newdata <- arrange(sum_graph_site, Group)

#make graph

ggplot(newdata, aes(x=SiteC, y=mean)) +
  geom_point(aes(shape=Group, col=Group), size = 4)+
  geom_errorbar(aes(x = SiteC, ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw(base_size = 26, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.text.align = 0,
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(y= "Mean fungal abundance (CFU)", x= "Site of Origin", color="Bat Species", 
       shape="Bat Species") +
  scale_shape_manual(values=c(15,16,17,10,7,18,8), labels=c(expression(italic("Corynorhinus spp.")), expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")), expression(italic("M. grisescens")), expression(italic("M. leibii")), expression(italic("M. sodalis")), "Susceptible"))+ 
  scale_color_manual(values=c("springgreen", "steelblue", "cyan", "black", "goldenrod", "darkorchid", "firebrick"), labels=c(expression(italic("Corynorhinus spp.")), 
                                                                                                                             expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")), expression(italic("M. grisescens")), expression(italic("M. leibii")), 
                                                                                                                             expression(italic("M. sodalis")), "Susceptible"))
##calculate the mean and standard error of the number of yeast colonies cultured from bats #belonging to each WNS-susceptibility group in each geographic site

sum_graph_site <- graph %>%
  group_by(SiteC, Group) %>%
  summarise(n = length(yeastab), mean = mean(yeastab), sd = sd(yeastab), se= sd/sqrt(n))

#arrange by group

newdata <- arrange(sum_graph_site, Group)

#make graph

ggplot(newdata, aes(x=SiteC, y=mean)) +
  geom_point(aes(shape=Group, col=Group), size = 4)+
  geom_errorbar(aes(x = SiteC, ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw(base_size = 26, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.text.align = 0,
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(y= "Mean yeast abundance (CFU)", x= "Site of Origin", color="Bat Species", 
       shape="Bat Species") +
  scale_shape_manual(values=c(15,16,17,10,7,18,8), labels=c(expression(italic("Corynorhinus spp.")), expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")),
                                                            expression(italic("M. grisescens")), expression(italic("M. leibii")), expression(italic("M. sodalis")), "Susceptible"))+ 
  scale_color_manual(values=c("springgreen", "steelblue", "cyan", "black", "goldenrod", "darkorchid", "firebrick"), labels=c(expression(italic("Corynorhinus spp.")), expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")),  expression(italic("M. grisescens")), expression(italic("M. leibii")), expression(italic("M. sodalis")), "Susceptible"))


#load data

graph2 <- read.csv("finalMalaMod_wMYAU.csv", header=T)

# subset to remove metadata (matrix only)

graph2m<-graph2 [,10:70]

#calculate Shannon Diversity Index for all fungi

shan=diversity(graph2m, index="shan")

graph2$shann <- shan

#calculate the mean and standard error of the Shannon Diversity Index for all fungi cultured #from bats belonging to each WNS-susceptibility group in each geographic site

sum_graph_site <- graph2 %>% group_by(SiteC, Group) %>%
  summarise(n = length(shann), mean = mean(shann), sd = sd(shann), se= sd/sqrt(n))

#arrange by group

newdata <- arrange(sum_graph_site, Group)

#make graph

ggplot(newdata, aes(x=SiteC, y=mean)) +
  geom_point(aes(shape=Group, col=Group), size = 4)+
  geom_errorbar(aes(x = SiteC, ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw(base_size = 26, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.text.align = 0,
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(y= "Mean Shannon Index (all fungi)", x= "Site of Origin", color="Bat Species", 
       shape="Bat Species") +
  scale_shape_manual(values=c(15,16,17,10,7,18,8), labels=c(expression(italic("Corynorhinus spp.")), expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")),
                                                            expression(italic("M. grisescens")), expression(italic("M. leibii")), expression(italic("M. sodalis")), "Susceptible"))+ 
  scale_color_manual(values=c("springgreen", "steelblue", "cyan", "black", "goldenrod", "darkorchid", "firebrick"), labels=c(expression(italic("Corynorhinus spp.")), 
                                                                                                                             expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")), expression(italic("M. grisescens")), expression(italic("M. leibii")), 
                                                                                                                             expression(italic("M. sodalis")), "Susceptible"))

##load data

yeast2 <- read.csv("finalyeast2.csv", header=T)

# subset to remove metadata (matrix only)

yeast2m<-yeast2 [,9:30]

#calculate Shannon Diversity Index for yeasts

shan=diversity(yeast2m, index="shan")

yeast2$shann <- shan

#calculate the mean and standard error of the Shannon Diversity Index for yeasts cultured from #bats belonging to each WNS-susceptibility group in each geographic site

sum_graph_site <- yeast2 %>% group_by(SiteC, Group) %>%
  summarise(n = length(shann), mean = mean(shann), sd = sd(shann), se= sd/sqrt(n))

#arrange by group

newdata <- arrange(sum_graph_site, Group)

#make graph

ggplot(newdata, aes(x=SiteC, y=mean)) +
  geom_point(aes(shape=Group, col=Group), size = 4)+
  geom_errorbar(aes(x = SiteC, ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw(base_size = 26, base_family = 'Helvetica') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), legend.text.align = 0,
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  labs(y= "Mean Shannon Index (yeast only)", x= "Site of Origin", color="Bat Species", 
       shape="Bat Species") +
  scale_shape_manual(values=c(15,16,17,10,7,18,8), labels=c(expression(italic("Corynorhinus spp.")), expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")),
                                                            expression(italic("M. grisescens")), expression(italic("M. leibii")), expression(italic("M. sodalis")), "Susceptible"))+ 
  scale_color_manual(values=c("springgreen", "steelblue", "cyan", "black", "goldenrod", "darkorchid", "firebrick"), labels=c(expression(italic("Corynorhinus spp.")), 
                              expression(italic("Eptesicus fuscus")), expression(italic("Myotis austroriparius")), expression(italic("M. grisescens")), expression(italic("M. leibii")), 
                              expression(italic("M. sodalis")), "Susceptible"))

Code for Figure 3

diffabun_filter <- filter(diffabun, Log2change != 0.00)

diffabun_filter$Indicator <- factor(diffabun_filter$Indicator, levels = c("Yes", "No"))

ggplot(diffabun_filter) +
  geom_point(aes(x=Species, y=Log2change, shape = Indicator, fill = Indicator), size=5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  scale_shape_manual(values = c(22,21), name = "Indicator status") +
  scale_fill_manual(values = c("steelblue","firebrick"), name = "Indicator status") +
  facet_wrap(~Comparison) + 
  theme_bw(base_size=26, base_family = 'Helvetica') + 
  theme(axis.text.x = element_text(angle = -25, size = 17, hjust=0.04, vjust=0.35, family= "Times", face="italic"),
        panel.grid.minor = element_blank(), plot.margin=unit(c(1,5,1,1),"cm"))


Code for Figure S1:

#all fungi abundance

#calculate the mean and standard error of the number of fungal colonies cultured from bats #belonging to each WNS-susceptibility group

sum_final_species <- final %>% group_by(Bat_Species, Group) %>%
  summarise(n = length(all_fungi), mean = mean(all_fungi), sd = sd(all_fungi), se= sd/sqrt(n))

# change order to group the WNS-susceptibility groups together

sum_final_species$Bat_Species <- factor(sum_final_species$Bat_Species, levels = c("CORA",
                                     "COTO", "EPFU","MYAU", "MYGR", "MYLE", "MYSO", "MYLU",
                                                                                  "MYSE", "PESU"))
#make graph

ggplot(sum_final_species) +
  geom_errorbar(aes(x = Bat_Species, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_point(aes(x=Bat_Species, y=mean, col = Group), size = 4) + 
  labs(y = "Mean fungal abundance (CFU)", x = "Bat Species") +
  theme_bw(base_size = 26, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_color_manual(values = c("springgreen", "firebrick", "steelblue"), name = "WNS-Susceptibility Group", labels=c("Impervious", "Resistant", "Susceptible"))

##yeast abundance

#calculate the mean and standard error of the number of yeast colonies cultured from bats #belonging to each WNS-susceptibility group

sum_final_species <- final %>%
  group_by(Bat_Species, Group) %>%
  summarise(n = length(yeastab), mean = mean(yeastab), sd = sd(yeastab), se= sd/sqrt(n))
# change order to group the WNS groups together
sum_final_species$Bat_Species <- factor(sum_final_species$Bat_Species, levels = c("CORA",
                                                                                  "COTO", "EPFU","MYAU",
                                                                                  "MYGR", "MYLE",
                                                                                  "MYSO", "MYLU",
                                                                                  "MYSE", "PESU"))
#make graph

ggplot(sum_final_species) +
  geom_errorbar(aes(x = Bat_Species, ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_point(aes(x=Bat_Species, y=mean, col = Group), size = 4) + 
  labs(y = "Mean yeast abundance (CFU)", x = "Bat Species") +
  theme_bw(base_size = 26, 
           base_family = 'Helvetica') + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_color_manual(values = c("springgreen", "firebrick", "steelblue"), name = "WNS-susceptibility group", labels=c("Impervious", "Resistant", "Susceptible"))

