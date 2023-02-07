install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
grid_view <- read.csv("D:/David Suarez/1.PRO-Gramming/Programming-R/Grid view.csv")

grid_view_names <- grid_view %>% 
  rename(Requires_release = Requieres.realease.)

grid_view_requires_release <- grid_view_names %>% 
  select(Requires_release)%>% 
  count(Requires_release)%>% 
  transmute(Requires_release, Count = n)

grid_view_requires_release_region_yn <- grid_view_names %>% 
  select(Region, Requires_release)%>% 
  group_by(Region) %>%
  count(Requires_release)%>% 
  mutate(Count = n)%>% 
  select(-n)

#grid_view_future_use <- grid_view_names %>% 
  #select(Region, Future.use)
 
  

grid_view_yn <- grid_view_names%>% 
  mutate(y_n = as.integer(sub(pattern = "Yes", replacement = 1, Requires_release)),n_n = as.integer(sub(pattern = "No", replacement = 1, Requires_release))) %>% 
  mutate(Requires_release, y_n = as.integer(sub(pattern = "No", replacement = 0, y_n), n_n = as.integer(sub(pattern = "Yes", replacement = 0, n_n)))) %>% 
  replace_na(list(y_n = 0))%>%
  replace_na(list(n_n = 0))

grid_view_count_region_yes <- grid_view_yn %>% 
  select(Region,Requires_release,Future.use,Future.containment, y_n, n_n)%>% 
  group_by(Region, Requires_release)%>% 
  filter(Requires_release == "Yes")%>%
  count(Region, sort = T)%>% 
  rename(Count = n)
  
grid_view_count_future_use_yes <- grid_view_yn %>% 
  select(Requires_release,Future.use,Future.containment, y_n, n_n)%>% 
  group_by(Future.use, Requires_release)%>% 
  arrange(desc(Future.use))%>%
  filter(Requires_release == "Yes")%>%
  count(Future.use, sort = T)

grid_view_region_contained <- grid_view_names %>%
  group_by(Region, Future.containment,Requires_release) %>% 
  summarize(Count_containment = n()) %>% 
  filter(Requires_release == "Yes")
  
grid_view_pie_yes_future_use <- grid_view_names %>%
  select(Requires_release, Future.use) %>% 
  filter(Requires_release == "Yes") #%>% 
# Tipos de respuesta de spread autonomously 
grid_view_xspread_autonomously_ycount_require_releasewrap <- grid_view_names %>% 
  select(Requires_release, In.case.of.release..would.the.organism.or.parts.spread.autonomously.) %>% 
  count(In.case.of.release..would.the.organism.or.parts.spread.autonomously., Requires_release) %>% 
  mutate(Count = n, n = NULL, Spread_autonomously = In.case.of.release..would.the.organism.or.parts.spread.autonomously., In.case.of.release..would.the.organism.or.parts.spread.autonomously. = NULL) %>% 
  group_by(Spread_autonomously) 
grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes <- grid_view_names %>% 
  select(Requires_release, In.case.of.release..would.the.organism.or.parts.spread.autonomously.) %>% 
  count(In.case.of.release..would.the.organism.or.parts.spread.autonomously., Requires_release) %>% 
  mutate(Count = n, n = NULL, Spread_autonomously = In.case.of.release..would.the.organism.or.parts.spread.autonomously., In.case.of.release..would.the.organism.or.parts.spread.autonomously. = NULL) %>% 
  group_by(Spread_autonomously) %>% 
  filter(Requires_release == "Yes") %>% 
  mutate(Percentages = round((Count /166 *100), 2))

grid_view_xspread_autonomously_ycount_require_releasewrap_requireno <- grid_view_names %>% 
  select(Requires_release, In.case.of.release..would.the.organism.or.parts.spread.autonomously.) %>% 
  count(In.case.of.release..would.the.organism.or.parts.spread.autonomously., Requires_release) %>% 
  mutate(Count = n, n = NULL, Spread_autonomously = In.case.of.release..would.the.organism.or.parts.spread.autonomously., In.case.of.release..would.the.organism.or.parts.spread.autonomously. = NULL) %>% 
  group_by(Spread_autonomously) %>% 
  filter(Requires_release == "No") %>% 
  mutate(Percentages = round((Count / 176 *100),2))

grid_view_containment_release_future_use <- grid_view_names %>%
  select(Requires_release, Future.use, Containment.measures) %>% 
  filter(Requires_release == "Yes") #%>%
  #mutate()

  #Levels of future use: Foundational, In a Factory, Small enclosed device, Other, Biosensor, Consumer Product, Human Body or Food, Agriculture, Natural environment, Cell-Free, Field Use,Digital or non-biological products
Foundational_fu_positions <- grep(pattern = "Foundational", grid_view_pie_yes_future_use$Future.use)
In_a_Factory <- grep(pattern = "In a factory", grid_view_pie_yes_future_use$Future.use)
Small_enclosed_device<-grep(pattern = "Small enclosed", grid_view_pie_yes_future_use$Future.use)
Other<-grep(pattern = "Other", grid_view_pie_yes_future_use$Future.use)
Biosensor<-grep(pattern = "Biosensor", grid_view_pie_yes_future_use$Future.use)
Consumer_product<-grep(pattern = "Consumer product",grid_view_pie_yes_future_use$Future.use)
Human_Body_or_Food<-grep(pattern = "Human",grid_view_pie_yes_future_use$Future.use)
Agriculture<-grep(pattern = "Agriculture", grid_view_pie_yes_future_use$Future.use)
Natural_environment <- grep(pattern = "Natural", grid_view_pie_yes_future_use$Future.use)
Cell_Free<- grep(pattern = "Cell", grid_view_pie_yes_future_use$Future.use)
Field_Use<- grep(pattern = "Field", grid_view_pie_yes_future_use$Future.use)
Digital_or_no<- grep(pattern = "Digital", grid_view_pie_yes_future_use$Future.use)
#---#
Future.use_Foundational <- data.frame(grid_view_pie_yes_future_use$Future.use[Foundational_fu_positions])
Future.use_In_a_Factory <- data.frame(grid_view_pie_yes_future_use$Future.use[In_a_Factory])
Future.use_Small_enclosed_device <- data.frame(grid_view_pie_yes_future_use$Future.use[Small_enclosed_device])
Future.use_Other <- data.frame(grid_view_pie_yes_future_use$Future.use[Other])
Future.use_Biosensor <- data.frame(grid_view_pie_yes_future_use$Future.use[Biosensor])
Future.use_Consumer_Product <- data.frame(grid_view_pie_yes_future_use$Future.use[Consumer_product])
Future.use_Human_body_or_food <- data.frame(grid_view_pie_yes_future_use$Future.use[Human_Body_or_Food])
Future.use_Agriculture <- data.frame(grid_view_pie_yes_future_use$Future.use[Agriculture])
Future.use_Natural_environment <- data.frame(grid_view_pie_yes_future_use$Future.use[Natural_environment])
Future.use_Cell_Free <- data.frame(grid_view_pie_yes_future_use$Future.use[Cell_Free])
Future.use_Field_use <-data.frame(grid_view_pie_yes_future_use$Future.use[Field_Use])
Future.use_Digital_or_non_biological <- data.frame(grid_view_pie_yes_future_use$Future.use[Digital_or_no])

colnames(Future.use_Foundational) <- "Type_Fu"
colnames(Future.use_In_a_Factory) <- "Type_Fu"
colnames(Future.use_Small_enclosed_device) <- "Type_Fu"
colnames(Future.use_Other) <- "Type_Fu"
colnames(Future.use_Biosensor)<- "Type_Fu"
colnames(Future.use_Consumer_Product)<- "Type_Fu"
colnames(Future.use_Human_body_or_food)<- "Type_Fu"
colnames(Future.use_Agriculture)<- "Type_Fu"
colnames(Future.use_Natural_environment)<- "Type_Fu"
colnames(Future.use_Cell_Free)<- "Type_Fu"
colnames(Future.use_Field_use)<- "Type_Fu"
colnames(Future.use_Digital_or_non_biological)<- "Type_Fu"
#Fix to bind rows
Future.use_Foundational_only <- data.frame(gsub(pattern = ".*","Foundational", Future.use_Foundational$Type_Fu))
Future.use_In_a_Factory_only <- data.frame(gsub(pattern = ".*","In a Factory", Future.use_In_a_Factory$Type_Fu))
Future.use_Small_enclosed_device_only <- data.frame(gsub(pattern = ".*","Small enclosed device", Future.use_Small_enclosed_device$Type_Fu))
Future.use_Other_only <- data.frame(gsub(pattern = ".*","Other", Future.use_Other$Type_Fu))
Future.use_Biosensor_only <- data.frame(gsub(pattern = ".*","Biosensor", Future.use_Biosensor$Type_Fu))
Future.use_Consumer_Product_only <- data.frame(gsub(pattern = ".*","Consumer Product", Future.use_Consumer_Product$Type_Fu))
Future.use_Human_body_or_food_only <- data.frame(gsub(pattern = ".*","Human body or food", Future.use_Foundational$Type_Fu))
Future.use_Agriculture_only <- data.frame(gsub(pattern = ".*","Agriculture", Future.use_Agriculture$Type_Fu))
Future.use_Natural_environment_only <- data.frame(gsub(pattern = ".*","Natural environment", Future.use_Natural_environment$Type_Fu))
Future.use_Cell_Free_only <- data.frame(gsub(pattern = ".*","Cell-Free", Future.use_Cell_Free$Type_Fu))
Future.use_Field_use_only <-data.frame(gsub(pattern = ".*","Field use", Future.use_Field_use$Type_Fu))
Future.use_Digital_or_non_biological_only <- data.frame(gsub(pattern = ".*","Digital or non biological products", Future.use_Digital_or_non_biological$Type_Fu))
colnames(Future.use_Foundational_only) <- "Type_Fu"
colnames(Future.use_In_a_Factory_only) <- "Type_Fu"
colnames(Future.use_Small_enclosed_device_only) <- "Type_Fu"
colnames(Future.use_Other_only) <- "Type_Fu"
colnames(Future.use_Biosensor_only)<- "Type_Fu"
colnames(Future.use_Consumer_Product_only)<- "Type_Fu"
colnames(Future.use_Human_body_or_food_only)<- "Type_Fu"
colnames(Future.use_Agriculture_only)<- "Type_Fu"
colnames(Future.use_Natural_environment_only)<- "Type_Fu"
colnames(Future.use_Cell_Free_only)<- "Type_Fu"
colnames(Future.use_Field_use_only)<- "Type_Fu"
colnames(Future.use_Digital_or_non_biological_only)<- "Type_Fu"
#Mother Data Frame for Future use pie
Mother_df <- bind_rows(Future.use_Foundational_only, Future.use_In_a_Factory_only, Future.use_Small_enclosed_device_only,Future.use_Other_only, Future.use_Biosensor_only, Future.use_Consumer_Product_only, Future.use_Human_body_or_food_only, Future.use_Agriculture_only, Future.use_Natural_environment_only, Future.use_Cell_Free_only, Future.use_Field_use_only, Future.use_Digital_or_non_biological_only, .id= NULL)
Mother_df_percentages <- Mother_df %>% 
count(Type_Fu) %>% 
mutate(percentages = round((n / 269 * 100),2))
#factor(Future.use)
#3 medidas de biocontencion por uso a futuro y requerir soltar 
grid_view_futureuse_requirerelease_biocontainment <- grid_view_names %>% 
  select(Requires_release, Containment.measures, Future.use)

#Requires release yes & no
ggplot(grid_view_requires_release, aes(x = Requires_release, y = Count, fill = Requires_release)) + geom_col() + expand_limits(y=200) + ggtitle("Proyect Release Count")+theme(axis.title.x = element_blank())
ggsave("Requires Release yes & no.png", width = 10, height = 7)
#Release y no release por numero y region. 
ggplot(grid_view_requires_release_region_yn, aes(y = Count,x = Requires_release,fill = Requires_release)) + geom_col() + ggtitle("Count Release vs No Release by Regions") + facet_wrap(~Region) + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank())
ggsave("Requires Release by Number and Region.png",width = 10, height = 7)
#Requires release by region
ggplot(grid_view_count_region_yes, aes(x = Region ,y = Count,fill = Region)) + geom_col() + ggtitle("Require Release Over Regions") + theme(axis.ticks.x = element_blank(), axis.title.x = element_blank())
ggsave("Require Release by Region.png",width = 10, height = 7)

#Release y grado de contencion por Region
ggplot(grid_view_region_contained, aes(y = Count_containment,x = Future.containment,fill = Future.containment)) + geom_col() + ggtitle("Require Release and Future Containment by Regions") + facet_wrap(~Region) + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.title.x = element_blank())
 ggsave("Release and Containment by Region.png", width = 10, height = 7)

 #Count of spread by release wrap
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap, aes(x = Spread_autonomously, y = Count, fill = Spread_autonomously)) +geom_col() +facet_wrap(~Requires_release, strip.position = "bottom") +theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +ggtitle("Spread Autonomously count by Requires Release") +xlab("Requires Release")
 ggsave("Spread Autonomously count by Requires Release.png", width = 10, height = 7)
 
 #Pie of spread by release double
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap, aes(x = "", y = grid_view_xspread_autonomously_ycount_require_releasewrap$Count, fill = Spread_autonomously)) +geom_col(color = "Black") +geom_label(aes(label = grid_view_xspread_autonomously_ycount_require_releasewrap$Count), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +facet_wrap(~Requires_release, strip.position = "bottom") +ggtitle("Spread Autonomously count by Requires Release") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.title = element_text(size = 9),legend.key.size = unit(0.3,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6)) +ylab("Requires Release") 
 ggsave("Spread Autonomously by Requires Release double pie.png", width = 10, height = 7)
 #Individual pie yes and no by number and percentages
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes, aes(x = "", y = grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes$Count, fill = Spread_autonomously)) +geom_col(color = "Black") +geom_label(aes(label = grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes$Count), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +facet_wrap(~Requires_release, strip.position = "bottom") +ggtitle("Spread Autonomously count by Requires Release") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 11)) +ylab("Requires Release") 
 ggsave("Spread Autonomously by Requires Release yes frequency.png", width = 10, height = 7)
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap_requireno, aes(x = "", y = grid_view_xspread_autonomously_ycount_require_releasewrap_requireno$Count, fill = Spread_autonomously)) +geom_col(color = "Black") +geom_label(aes(label = grid_view_xspread_autonomously_ycount_require_releasewrap_requireno$Count), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +facet_wrap(~Requires_release, strip.position = "bottom") +ggtitle("Spread Autonomously count by Requires Release") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 11)) +ylab("Requires Release") 
 ggsave("Spread Autonomously by Requires Release no frequency.png", width = 10, height = 7)
 
 #percentages
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes, aes(x = "", y = grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes$Percentages, fill = Spread_autonomously)) +geom_col(color = "Black") +geom_label(aes(label = grid_view_xspread_autonomously_ycount_require_releasewrap_requireyes$Percentages), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +facet_wrap(~Requires_release, strip.position = "bottom") +ggtitle("Spread Autonomously count by Requires Release") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 11)) +ylab("Requires Release") 
 ggsave("Spread Autonomously by Requires Release yes percentages.png", width = 10, height = 7)
 
 ggplot(grid_view_xspread_autonomously_ycount_require_releasewrap_requireno, aes(x = "", y = grid_view_xspread_autonomously_ycount_require_releasewrap_requireno$Percentages, fill = Spread_autonomously)) +geom_col(color = "Black") +geom_label(aes(label = grid_view_xspread_autonomously_ycount_require_releasewrap_requireno$Percentages), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +facet_wrap(~Requires_release, strip.position = "bottom") +ggtitle("Spread Autonomously count by Requires Release") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 11)) +ylab("Requires Release") 
 ggsave("Spread Autonomously by Requires Release no percentages.png", width = 10, height = 7)
 
#percentages of future use by yes release
 ggplot(Mother_df_percentages, aes(x = "", y = Mother_df_percentages$percentages, fill = Mother_df_percentages$Type_Fu)) +geom_col(color = "Black") +geom_label(aes(label = Mother_df_percentages$percentages), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +ggtitle("Future Use Types to be Released by percentages") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 12), axis.title.x = element_blank()) +labs(fill = "Future Use Types")
 ggsave("Future Use Types by Release percentages.png",width = 10, height = 7)

 ggplot(Mother_df_percentages, aes(x = "", y = Mother_df_percentages$n, fill = Mother_df_percentages$Type_Fu)) +geom_col(color = "Black") +geom_label(aes(label = Mother_df_percentages$n), color = "white", position = position_stack(vjust = 0.5), show.legend = F) +coord_polar(theta = "y") +ggtitle("Number of Projects to be Released by Future Types") +theme(axis.title.y =element_blank(), axis.ticks.x = element_blank(), legend.position = "left", legend.title = element_text(size = 9),legend.key.size = unit(.5,"cm"), legend.key.width = unit(0.5, "cm"),  legend.text = element_text(size = 6), plot.title = element_text(size = 12), axis.title.x = element_blank()) +labs(fill = "Future Use Types")
 ggsave("Future Use Types by Release in number.png",width = 10, height = 7)
 
 
 ggplot(grid_view_yesno, aes(x = require_release)) + geom_histogram(binwidth = 20)

grid_view_counts <- grid_view_yn %>% 
  select(-Answer.on.question.16, - Project.goal)%>% 
  group_by(Future.containment, Region, requires_release) %>%
  count(Future.containment, sort = T) %>% 
  mutate(n_containment = n)%>%
  select(-n)%>% 
  group_by(Region)
