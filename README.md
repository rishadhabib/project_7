# project_7

plastic_data_2 <- read_csv ("plastic_regions_data.csv") %>% 
  mutate (MonthNum = ifelse(MonthNum < 10, paste0("0", MonthNum), MonthNum),
    YearMonth = paste(Year, MonthNum, sep = "-"))

plastic_data_2 %>% 
  group_by(YearMonth) %>% 
  summarise(total_plastic = sum(TotalClassifiedItems_EC2020),
            mean_plastic = mean(TotalClassifiedItems_EC2020),
            total_items = sum (Totalltems_EventRecord),
            total_volunteers = sum(TotalVolunteers, na.rm= TRUE), 
            total_length_km = sum (TotalLength_m/1000, na.rm = TRUE),
            plastic_density = (total_plastic/total_length_km)) %>%
  ggplot(aes(YearMonth, plastic_density)) +
  geom_point(col = "darkcyan") +
  xlab(c("Time")) +
  ylab ("Plastic Density in items per km")+
  theme_minimal() + 
  theme(axis.text.x = element_text(hjust = 1, angle = 90))

# creating a column for types of plastics 
plastic_data_3 <-  plastic_data_2 %>% 
    group_by(YearMonth) %>% 
mutate (hard_plastics = (SUM_Hard_PlasticBeverageBottle + SUM_Hard_OtherPlasticBottle + SUM_Hard_BucketOrCrate + SUM_Hard_Lighter +   SUM_OtherHardPlastic + SUM_HardSoftLollipopStick_EarBu), 
        soft_plastics = (SUM_PlasticOrFoamPlatesBowlsCup + SUM_Soft_Bag + SUM_Soft_WrapperOrLabel+ SUM_Soft_Straw + SUM_Soft_OtherPlastic + SUM_Soft_CigaretteButts + SUM_Soft_StringRingRibbon), 
        marine_items = (Fishing_Net + SUM_FishingLineLureRope+  Fishing_BuoysAndFloats), 
        other_plastic = (SUM_HardOrSoft_PlasticBottleCap + SUM_HardSoft_PersonalCareProduc + SUM_Foam_OtherPlasticDebris + SUM_OtherPlasticDebris)) %>% 
  select (YearMonth, region, hard_plastics, soft_plastics, marine_items, other_plastic, TotalLength_m) %>% 
  group_by(YearMonth) %>% 
  summarize(hard_plastics = mean(hard_plastics, na.rm = TRUE),
            soft_plastics  = mean (soft_plastics, na.rm = TRUE),
            marine_plastics = mean (marine_items, na.rm = TRUE), 
            other_plastic = mean (other_plastic, na.rm = TRUE)) %>% 
  gather(key = "plastic_type",
         value = "plastic_density", 
         - YearMonth)
         
         
# Time series by plastic type
plastic_data_3 %>% 
  filter (plastic_density < 5000) %>% 
  group_by(YearMonth, plastic_type) %>% 
  ggplot (aes(YearMonth, plastic_density, 
              group = plastic_type, 
              col = plastic_type)) +
  geom_line(size = 1) +
  xlab(c("Time")) +
  ylab ("Plastic Density")+
  theme_minimal() + 
  theme(axis.text.x = element_text(hjust = 1, angle = 90))
