#load invitro dataset 
invitro_param <- read_csv("data/invitro_processed.csv") 
ec50 <- invitro_param %>%
  filter(Metric == "EC50")
emax <- invitro_param %>%
  filter(Metric == "Emax")
#using all invitro data for model input
ec50_input_compelte <- ec50 %>%
  group_by(Inducer) %>%
  summarise(mean_ec50 = mean(Value), median_ec50 = median(Value))
emax_input_compelte <- emax %>%
  group_by(Inducer) %>%
  summarise(mean_emax = mean(Value), median_emax = median(Value))

ec50_input_hepatocyte_only <- ec50 %>%
  filter(`non human liver cell based` == 0) %>%
  group_by(Inducer) %>%
  summarise(mean_ec50 = mean(Value), median_ec50 = median(Value))
emax_input_hepatocyte_only <- emax %>%
  filter(`non human liver cell based` == 0) %>%
  group_by(Inducer) %>%
  summarise(mean_emax = mean(Value), median_emax = median(Value))
