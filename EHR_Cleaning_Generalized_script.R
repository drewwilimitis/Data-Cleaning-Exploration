library(readr);library(lubridate);library(dplyr);library(stringr);library(ggplot2);library(plotly)

rm(list = ls())
gc()

# (drug text file used before)
drug <- read_csv("C:/Users/dwilimits/Google Drive/Data_Cleaning_Documentation/Procured_Drug_Treatment_050115_033117.txt")

# For testing purposes (using new load_ehr_ndc file)
rm(list = ls())
drug <- read_csv("C:/Users/dwilimits/Google Drive/Data_Cleaning_Documentation/load_ehrndcscripttest.csv")
sum(is.na(drug$dose))
ndc_file <- drug

# These are Novant EHR data files
diag <- read_csv('C:/Users/jclaghorn/Google Drive/Novant/EHR/Procured_Diagnoses_050115_033117.csv')
drug <- read_csv('C:/Users/jclaghorn/Google Drive/Novant/EHR/Procured_Drug_Treatment_050115_033117.txt')
ord <- read_csv('C:/Users/jclaghorn/Google Drive/Novant/EHR/Procured_Visit_Info_050115_033117.txt')
pres <- read_csv('C:/Users/jclaghorn/Google Drive/Novant/EHR/Procured_Physician_050115_033117.txt')
colnames(diag) <- tolower(gsub(' |/','_',colnames(diag)))
colnames(drug) <- tolower(gsub(' |/','_',colnames(drug)))
colnames(ord) <- tolower(gsub(' |/','_',colnames(ord)))
colnames(pres) <- tolower(gsub(' |/','_',colnames(pres)))

#######################################################
# Cleaning Drug_Treatment Dataset (Mostly used to create NDC table)
#######################################################

# Copying drug file into new NDC file
colnames(drug) <- tolower(gsub(' |/','_',colnames(drug)))

# Using smaller dataset with no missing product_code values for testing
ndc_file <- drug[1:1000000, ] %>%
  filter(product_code != "NULL")

# These are our drug groups
ph_groups <- read_csv('C:/Users/dwilimits/Google Drive/Novant_R_Work/ph_groups.csv')
colnames(ph_groups) <- c(
  'group_id',
  'group_name',
  'dxid',
  'product_name',
  'NDC',
  'ph_profile_id',
  'ph_group_id')

ndc_product <- ph_groups %>%
  select(product_name,NDC) %>%
  distinct()

# Product Code Field to NDC11
# If hyphenated 11 digit code, then split string to get NDC11
# If hyphenated 10 digit code, then SQL table ndc_10_hyphen_to_11 has a crosswalk to hyphenated NDC10 to NDC11
# If hyphenated 10 digit code not found in SQL table, then split string and add leading 0
# analysis.NPI_lean has the NPI directory conataining specialty taxonomies

# Reading in SQL table
ndc10_hyphen_to_11 <- read_csv("C:/Users/dwilimits/Google Drive/Data_Cleaning_Documentation/ndc_10_hyphen_to_11.csv")

ndc_file <- ndc_file %>%
  left_join(ndc10_hyphen_to_11[ , c('NDC10', 'NDC')], by = c('product_code' = 'NDC10')) %>%
  mutate(NDC11 = ifelse(str_length(gsub("-", "", product_code)) == 11,
                        gsub("-", "", product_code),
                        ifelse(is.na(NDC), str_pad(gsub("-", "", product_code), 11, side="left", pad = "0"), NDC)))

# Deleting unnecessary columns
ndc_file <- ndc_file[ , !names(ndc_file) %in% c('product_code', 'NDC')]

# _______________DOSE CLEANING_______________ #
# Splitting dose field into numerical dose and unit columns
split_dose <- str_split_fixed(ndc_file$dose, " ", 2)
ndc_file <- transform(ndc_file,
                      dose = split_dose[, 1],
                      unit = split_dose[, 2])

# Function to split the ranged doses to the mean of the upper and lower boundaries (if not a range then returns the value)
range_to_mean <- function(y) {
  sapply(strsplit(y, "-"),
         function(x)mean(as.numeric(x)))
}

# Applying function to ndc file
ndc_file$dose <- range_to_mean(ndc_file$dose)

# When units given as units per mass and/or unit per time, convert to the correct dose form
# Example NDC where this would improve the accuracy of within_range = Example = NDC 00264780000.
# Unique cases of units given as a ratio
ndc_file_ratio_units <- ndc_file %>%
  select(unit) %>%
  filter(grepl("/", unit)) %>%
  distinct()

# Creating dose conversion factors based on the ratios in the unit field (assuming 80kg as average mass)
ndc_file$unit_dose_conv <- NA
ndc_file$unit_dose_conv <- ifelse(grepl('/kg/day', ndc_file$unit), 80,
                            ifelse(grepl('/kg/hr', ndc_file$unit), 80 * 24,
                                   ifelse(grepl('/kg/min', ndc_file$unit), 80 * 60 * 24,
                                          ifelse(grepl('/kg', ndc_file$unit), 80,
                                                 ifelse(grepl('/hr', ndc_file$unit), 24,
                                                        ifelse(grepl('/min', ndc_file$unit), 60 * 24, 1))))))

# Frequency Field
# Extracting numerical values and storing a list of text frequencies and paired numerical doses_per_day to create frequency conversion table
freq_text_to_num <- data.frame(frequency = ndc_file[, 'frequency']) %>%
  mutate(frequency.raw = frequency,
         frequency = toupper(frequency),
         frequency = gsub('HALF',0.5,frequency),
         frequency = gsub('ONE',1,frequency),
         frequency = gsub('TWO',2,frequency),
         frequency = gsub('THREE',3,frequency),
         frequency = gsub('FOUR',4,frequency),
         frequency = gsub('FIVE',5,frequency),
         frequency = gsub('SIX',6,frequency),
         frequency = gsub('SEVEN',7,frequency),
         frequency = gsub('EIGHT',8,frequency),
         frequency = gsub('NINE',9,frequency),
         frequency = gsub('TWICE','2 TIMES',frequency),
         frequency = gsub('PER WEEK','WEEKLY',frequency),
         doses_per_day = NA,
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("[0-9]+ TIMES DAILY",frequency),as.numeric(gsub('[^0-9]','',str_extract(frequency,'[0-9]+ TIMES DAILY'))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY [0-9]+ HOUR|EVERY [0-9]+.[0-9]+ HOUR",frequency),24/as.numeric(gsub('[^0-9|.]','',str_extract(frequency,'EVERY [0-9]+ HOUR|EVERY [0-9]+.[0-9]+ HOUR'))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY [0-9]+ MIN",frequency),1440/as.numeric(gsub('[^0-9]','',str_extract(frequency,'EVERY [0-9]+ MIN'))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY [0-9]+ DAYS",frequency),1/as.numeric(gsub('[^0-9]','',str_extract(frequency,'EVERY [0-9]+ DAYS'))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY [0-9]+ MONTHS",frequency),1/(30*as.numeric(gsub('[^0-9]','',str_extract(frequency,'EVERY [0-9]+ MONTHS')))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("OVER [0-9]+ HOURS",frequency),24/as.numeric(gsub('[^0-9]','',str_extract(frequency,'OVER [0-9]+ HOURS'))),doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("TWICE WEEKLY",frequency),2/7,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("[0-9]+ TIMES WEEKLY",frequency),as.numeric(gsub('[^0-9]','',str_extract(frequency,'[0-9]+ TIMES WEEKLY')))/7,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("WEEKLY",frequency),1/7,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("DAILY|NIGHTLY|MORNING|EVENING",frequency),1,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY HOUR",frequency),24,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY MWF|EVERY TU, TH, SA",frequency),3/7,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("EVERY OTHER DAY",frequency),1/2,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("[0-9]+, [0-9]+",frequency),2,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day) & grepl("AM AND NOON",frequency),2,doses_per_day),
         doses_per_day = ifelse(is.na(doses_per_day),1,doses_per_day)) %>%
  select(frequency.raw, doses_per_day) %>%
  distinct()

# Load FDB tables
dose_range <- read_csv('C:/Users/dwilimits/Google Drive/Dose_Corrections/RMMADMA1_ADULT_DOSE_MSTR.csv', col_names = FALSE)

strength_table <- read_delim('C:/Users/dwilimits/Google Drive/Dose_Corrections/RPEINS0_NDC_STR_LINK', delim = "|", col_names = FALSE)

ndc_table <- read_csv('C:/Users/dwilimits/Google Drive/Dose_Corrections/RNDC14_NDC_MSTR.csv')

colnames(dose_range) <- c(
  'GCN_SEQNO',
  'MMA_MND',
  'MMA_MNDU',
  'MMA_MNU',
  'MMA_MNUF',
  'MMA_MXD',
  'MMA_MXDU',
  'MMA_MXU',
  'MMA_MXUF'
)

colnames(strength_table) <- c(
  'NDC',
  'HIC_SEQN',
  'STR_CONCT_TYPE_ID',
  'STRENGTH_STATUS_CODE',
  'INGREDIENT_STR',
  'INGREDIENT_UOM_MSTR_ID',
  'STRENGTH_TYP_CODE',
  'VOLUME',
  'VOLUME_UOM_MSTR_ID',
  'ALT_STR',
  'ALT_STR_UOM_MSTR_ID',
  'ALT_STRENGTH_TYP_CODE',
  'TIME_VALUE',
  'TIME_UOM_MSTR_ID',
  'RANGE_MAX',
  'RANGE_MIN',
  'DOSAGE_FORM_ATTRIBUTE_ID',
  'INGREDIENT_SORT_ORDER'
)

colnames(ndc_table) <- c(
  'NDC',
  'LBLRID',
  'GCN_SEQNO',
  'PS',
  'DF',
  'AD',
  'LN',
  'BN',
  'PNDC',
  'REPNDC',
  'NDCFI',
  'DADDNC',
  'DUPDC',
  'DESI',
  'DESDTEC',
  'DESI2',
  'DES2DTEC',
  'DEA',
  'CL',
  'GPI',
  'HOSP',
  'INNOV',
  'IPI',
  'MINI',
  'MAINT',
  'OBC',
  'OBSDTEC',
  'PPI',
  'STPK',
  'REPACK',
  'TOP200',
  'UD',
  'CSP',
  'NDL_GDGE',
  'NDL_LNGTH',
  'SYR_CPCTY',
  'SHLF_PCK',
  'SHIPPER',
  'HCFA_FDA',
  'HCFA_UNIT',
  'HCFA_PS',
  'HCFA_APPC',
  'HCFA_MRKC',
  'HCFA_TRMC',
  'HCFA_TYP',
  'HCFA_DESC1',
  'HCFA_DESI1',
  'UU',
  'PD',
  'LN25',
  'LN25I',
  'GPIDC',
  'BBDC',
  'HOME',
  'INPCKI',
  'OUTPCKI',
  'OBC_EXP',
  'PS_EQUIV',
  'PLBLR',
  'TOP50GEN',
  'OBC3',
  'GMI',
  'GNI',
  'GSI',
  'GTI',
  'NDCGI1',
  'HCFA_DC',
  'LN60'
)

ndc_table$GCN_SEQNO <- as.integer(ndc_table$GCN_SEQNO)

# Pulling in information for frequency to numerical conversion, range, and strength (used for drug text file as before)
ndc_df <- ndc_file %>%
  left_join(freq_text_to_num, by = c("frequency" = "frequency.raw")) %>%
  left_join(ndc_table, by = c("NDC11" = "NDC")) %>%
  left_join(dose_range, by = "GCN_SEQNO") %>%
  select(order_id,NDC11,product_description,product_simple_name,unit,dose,frequency,doses_per_day,unit_dose_conv,start_time,end_time,GCN_SEQNO,MMA_MND,MMA_MNDU,MMA_MNU,MMA_MNUF,MMA_MXD,MMA_MXDU,MMA_MXU,MMA_MXUF)

# Pulling in information for frequency to numerical conversion, range, and strength (used for testing new load_ehrscripttest)
ndc_df <- ndc_file %>%
  left_join(ndc_table, by = c("NDC11" = "NDC")) %>%
  left_join(dose_range, by = "GCN_SEQNO") %>%
  select(ord_id,NDC11,item_description,generic_name,unit,dose,frequency,doses_administered,doses_per_day,unit_dose_conv,start_time,end_time,GCN_SEQNO,MMA_MND,MMA_MNDU,MMA_MNU,MMA_MNUF,MMA_MXD,MMA_MXDU,MMA_MXU,MMA_MXUF)

# Creating total_daily_doses column (which will be used to check whether the total daily dosage is within the range)
ndc_df <- transform(ndc_df,
                    total_daily_dose = as.numeric(dose) * doses_per_day)
sum(is.na(ndc_df$total_daily_dose))

# Checking if total_daily_dose is within the MIN/MAX range (Check whether total_daily_dose is within either range given to
# correct for potential unit differences) 
# If MIN/MAX fields are null, then we use an outlier test to define a new range and check values against that range
ndc_df <- transform(ndc_df,
                    within_range = ifelse((total_daily_dose >= MMA_MNU & total_daily_dose <= MMA_MXU)
                                          | (total_daily_dose >= MMA_MND & total_daily_dose <= MMA_MXD), "Y", "N"))

summary(ndc_df$within_range)

# Checking whether unit ratio conversions improve accuracy
ndc_df$total_daily_dose_converted <- NA
ndc_df$total_daily_dose_converted <- ndc_df$total_daily_dose * ndc_df$unit_dose_conv

ndc_df <- transform(ndc_df,
                    within_range = ifelse((total_daily_dose_converted >= MMA_MNU & total_daily_dose_converted <= MMA_MXU)
                                          | (total_daily_dose_converted >= MMA_MND & total_daily_dose_converted <= MMA_MXD), "Y", "N"))

table(ndc_df$within_range)

# Using outlier test when MIN/MAX fields are NA. (When standard (center = median) Mad = 0, use Mad with center = mean)
ndc_df <- ndc_df %>%
  group_by(NDC11) %>%
  mutate(med_group = median(total_daily_dose, na.rm = TRUE),
         mad_group = ifelse(mad(total_daily_dose, na.rm = TRUE) != 0,
                            mad(total_daily_dose, na.rm = TRUE),
                            mad(total_daily_dose, center = mean(total_daily_dose), na.rm = TRUE))) %>%
  ungroup()
  
table(ndc_df$within_range)
sum(is.na(ndc_df$within_range))

ndc_df <- transform(ndc_df,
                        within_range = ifelse(is.na(within_range) & !is.na(total_daily_dose) & !is.na(med_group) & !is.na(mad_group), 
                                          ifelse(((abs(total_daily_dose - med_group)) / mad_group) <= 3.5, "Y", "N"), within_range))
table(ndc_df$within_range)
sum(is.na(ndc_df$within_range))

# Checking strength form and making a strength form conversion for doses outside the range
strength_conv <- strength_table[, c("NDC", "INGREDIENT_STR", "VOLUME")]
colnames(strength_conv) <- tolower(colnames(strength_conv))
strength_conv$ingredient_str <- as.numeric(strength_conv$ingredient_str)
strength_conv$volume <- as.numeric(strength_conv$volume)
strength_conv <- strength_conv[!duplicated(strength_conv$ndc), ]
strength_conv$volume[is.na(strength_conv$volume)] <- 1

# For doses outside of the range, check whether either strength transformed value is inside the range
# If either strength transformed value is inside the range, then mark "Y" for within_range and update the dose with the
# strength transformed value. If neither value is inside the range, then set the dose to NA.
table(ndc_df$within_range)
ndc_tmp_df <- ndc_df %>%
  filter(within_range == "N") %>%
  left_join(strength_conv, by = c("NDC11" = "ndc")) %>%
  mutate(str_trans_dose1 = total_daily_dose * (ingredient_str / volume),
         str_trans_dose2 = total_daily_dose * (volume / ingredient_str),    
         within_range_1 = ifelse((str_trans_dose1 >= MMA_MNU & str_trans_dose1 <= MMA_MXU)
                                 | (str_trans_dose1 >= MMA_MND & str_trans_dose1 <= MMA_MXD), "Y1", "N"),
         within_range_2 = ifelse((str_trans_dose2 >= MMA_MNU & str_trans_dose2 <= MMA_MXU)
                                 | (str_trans_dose2 >= MMA_MND & str_trans_dose2 <= MMA_MXD), "Y2", "N"),
         within_range = ifelse(((within_range_1 == "Y1" & !is.na(within_range_1))
                                | (within_range_2 == "Y2" & !is.na(within_range_2))), "Y", "N"))

ndc_tmp_df$total_daily_dose[ndc_tmp_df$within_range_1 == "Y1"
                          & !is.na(ndc_tmp_df$within_range_1)] <- ndc_tmp_df$str_trans_dose1[ndc_tmp_df$within_range_1 == "Y1"
                                                                                             & !is.na(ndc_tmp_df$within_range_1)]
ndc_tmp_df$total_daily_dose[ndc_tmp_df$within_range_2 == "Y2"
                            & !is.na(ndc_tmp_df$within_range_2)] <- ndc_tmp_df$str_trans_dose2[ndc_tmp_df$within_range_2 == "Y2"
                                                                                               & !is.na(ndc_tmp_df$within_range_2)]

ndc_df[ndc_df$within_range == "N" & !is.na(ndc_df$within_range), 
            c("total_daily_dose", "within_range")] <- ndc_tmp_df[, c("total_daily_dose", "within_range")]

table(ndc_df$within_range)
sum(is.na(ndc_df$total_daily_dose))
sum(is.na(ndc_df$within_range))

# Setting doses still outside the range (after the strength transformation check) to NULL
ndc_df[ndc_df$within_range == "N" & !is.na(ndc_df$within_range), "total_daily_dose"] <- NA

# Mean imputation for doses that are NA
sum(is.na(ndc_df$total_daily_dose))

ndc_mean_imputation2 <- ndc_df %>%
  group_by(NDC11) %>%
  mutate(impute_dose = median(total_daily_dose, na.rm=TRUE)) %>%
  ungroup() %>%
  select(NDC11, impute_dose) %>%
  distinct() 

ndc_df <- ndc_df %>%
  left_join(ndc_mean_imputation2, by = c('NDC11' = 'NDC11')) %>%
  mutate(total_daily_dose = ifelse(is.na(total_daily_dose), impute_dose, total_daily_dose))

sum(is.na(ndc_df$total_daily_dose))

# For dose values that are still NA, impute with mean of Min/Max range
sum(is.na(ndc_df$total_daily_dose))

ndc_df <- ndc_df %>%
  mutate(total_daily_dose = ifelse(is.na(total_daily_dose) & (!is.na(MMA_MNU) & !is.na(MMA_MXU) & !is.na(MMA_MND) & !is.na(MMA_MXD)),
                                   ifelse(MMA_MXD == 0, (MMA_MNU + MMA_MXU) / 2, (MMA_MND + MMA_MXD) / 2), total_daily_dose))

sum(is.na(ndc_df$total_daily_dose))

# Creating Doses Administered field using doses_per_day
ndc_df$start_time <- as.Date(ndc_df$start_time)
ndc_df$end_time <- as.Date(ndc_df$end_time)

ndc_df <- transform(ndc_df, 
                    doses_administered = (as.numeric(end_time - start_time) + 1) * doses_per_day)

# Writing final text file (might have to rename/drop unnecessary columns first)
ndc_output <- ndc_df[, c("ord_id", "total_daily_dose")]
write.csv(ndc_output, file = "ndc_output.csv")

#######################################################
# Cleaning Diagnoses Dataset (Mostly used to create ICD table)
#######################################################
# SQL to split the ICD10 comma-sep to their own rows # Likely possible to do in R as well

# drop table if exists analysis.PharmaEHR_Novant_ICD;
# create table analysis.PharmaEHR_Novant_ICD
# (
#   order_id	integer(100) references ord_id (PharmaEHR_Novant_Order),
#   diagnosis_code varchar(100) references SEARCH_ICD_CD (FDB_ICD_SEARCH),
#   PRIMARY KEY (order_id, diagnosis_code)
# )
# as
# select distinct
# PharmaEHR_Novant_ICD_tmp.order_id,
# SUBSTRING_INDEX(SUBSTRING_INDEX(PharmaEHR_Novant_ICD_tmp.diagnosis_code, ', ', numbers.n), ', ', -1) diagnosis_code
# from
# (select 1 n union all
#   select 2 union all select 3 union all
#   select 4 union all select 5) numbers INNER JOIN PharmaEHR_Novant_ICD_tmp
# on CHAR_LENGTH(PharmaEHR_Novant_ICD_tmp.diagnosis_code)
# -CHAR_LENGTH(REPLACE(PharmaEHR_Novant_ICD_tmp.diagnosis_code, ', ', ''))>=numbers.n-1
# where diagnosis_code is not null
# and diagnosis_code <> ''
# order by
# order_id, n;

# Splitting Diagnosis.Code comma separations into their own rows (In R)
diag$Diagnosis.Code <- as.character(diag$Diagnosis.Code)
s <- strsplit(diag$Diagnosis.Code, split = ",")

icd_df <- data.frame(Order.ID = rep(diag$Order.ID, sapply(s, length)),
                      Diagnosis.Code = unlist(s),
                      DRG.Code = rep(diag$DRG.Code, sapply(s, length)),
                      Description = rep(diag$Description, sapply(s, length)))

# Writing final text file (might have to rename/drop unnecessary columns first)
write.csv(icd_df, file = "load_ehricd.csv")

#######################################################
# Cleaning Physician Dataset (Mostly used to create Prescriber table)
#######################################################

# Copying prescriber text file into new prescriber table
# (NOTE: Might have to ignore some commas in CSV file that cause unnecessary breaks)
prescriber_df <- pres

# Use SQL Table (npi_specialties) to link specialty to npi_taxonomy
npi_speciality <- read_csv("C:/Users/dwilimits/Google Drive/Data_Cleaning_Documentation/npi_specialty.csv")
prescriber_df <- prescriber_df %>%
  left_join(npi_speciality, by = c('specialty' = 'specialty')) %>%
  select(physician, physician_id, npi_taxonomy)

# Writing final text file (might have to rename/drop unnecessary columns first)
write.csv(prescriber_df, file = "load_ehrprescriber.csv")
                           
#######################################################
# Cleaning Visit_Info Dataset (Mostly used to create Order table)
#######################################################

# Copying visit_info text file into new order df
order_df <- ord

# Have to pull in order_date field from the Drug_Treatment text file, converting strings to dates
order_df$order_date <- as.Date(drug$Date_of_Order)

# Converting admit_date and disch_date to dates
order_df$admit_date <- as.Date(ord$admit_date)
order_df$disch_date <- as.Date(ord$disch_date)

# Creating care_setting field ("IP" -> "I", "OP" -> "O")
order_df$care_setting <- substr(ord$IP/OP, 1, 1)

# Writing final text file (might have to rename/drop unnecessary columns first)
write.csv(order_df, file = "load_ehrorder.csv")




