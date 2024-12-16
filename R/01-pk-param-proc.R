library(readxl)
library(tidyverse)
#read xlsx files and normalize drug names
invitro <- read_excel("data/induction.xlsx",1)
# Clean drug names using stringr
invitro_processed <- invitro %>%
  select(Inducer,Metric, GMR, Incubation, PMID, Notes) %>%
  mutate(Inducer = str_to_lower(Inducer)) %>%
  mutate(Inducer = str_trim(Inducer))

#write a function to check data quality: 
#if the Metric is EC50, the GMR value should contain a unit, e.g.,uM. For Emax, GMR should not have any unit.
check_data_quality <- function(data) {
  # Units commonly used in concentration measurements
  units <- c("uM", "nM", "mM", "M", "mg/L", "ug/L", "ng/L", "mg/mL", "ug/mL", "ng/mL", "umol/L")
  if(data$Metric == "EC50") {
    # Check if the GMR contains any unit
    has_unit <- any(sapply(units, function(x) grepl(x, data$GMR)))
    return(has_unit)
  } else if (data$Metric == "Emax") {
    # Emax should not have any units
    if(any(sapply(units, function(x) grepl(x, data$GMR)))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

# Apply the function to the invitro_processed data
invitro_processed <- invitro_processed %>%
  rowwise() %>%
  mutate(Quality = check_data_quality(across())) %>%
  ungroup()
invitro_processed <- invitro_processed %>%
  mutate(GMR = str_trim(GMR)) %>%
  extract(GMR, into = c("Lower", "Upper", "Unit"), 
          regex = "^\\s*([0-9\\.]+)\\s*(?:-\\s*([0-9\\.]+))?\\s*(.*)$", remove = FALSE) %>%
  mutate(
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper),
    Unit = str_trim(Unit)
  )
#save the processed invitro data
write_csv(invitro_processed, "data/invitro_processed.csv")
#read clinical data
css <- read_excel("data/concentration.xlsx",1) %>%
  select(Drug, Value, Unit, MW)
fu <- read_excel("data/concentration.xlsx",2) %>%
  select(Drug, Value)
#merge the two clinical datasets
Imax <- merge(css, fu, by = "Drug", all = TRUE) %>%
  mutate(Value = (Value.x / MW) * (Value.y / 100) * 1000) %>%
  select(Drug, Value) %>%
  mutate(Unit = "uM")
#save the processed clinical data
write_csv(Imax, "data/Imax_processed.csv")
