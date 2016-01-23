# Script to generate files in "data" subdirectory from files in "datasrc" subdirectory
# Run this before running R CMD Check
# 
# $Id: generate_data.R 4008 2016-01-23 20:59:21Z patrick $

library(hash)
library(reshape2)

# make sure data dir exists, create if not
if (!file_test("-d","data")) {
  if (file.exists("data")) {
    stop("file \"data\" exists, unable to make directory \"data\"")
  }
  if (!dir.create("data")) {
    stop("failed attempting to create directory \"data\"")
  }
}

# icd9cm_list is a list of all ICD-9-CM diagnostic and procedure codes
# diagnostic codes are prefixed with "D", procedure codes prefixed with "P"
# Retrieved from CMS URL below

cms_icd9_zip_url <- 'https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip'
cms_icd9_zip <- 'datasrc/icd9cm.zip'
cms_icd9_dx_file <- 'datasrc/CMS32_DESC_SHORT_DX.txt'
cms_icd9_px_file <- 'datasrc/CMS32_DESC_SHORT_SG.txt'
cms_icd9_files <- c(cms_icd9_dx_file, cms_icd9_px_file)

icd9cm_list <- (function(){
  if(!all(file.exists(cms_icd9_files))) {
    if(!file.exists(cms_icd9_zip)) {
      download.file(cms_icd9_zip_url, cms_icd9_zip)
    }
    unzip(cms_icd9_zip, files = basename(cms_icd9_files), exdir = 'datasrc')
  }
  
  f <- read.fwf(cms_icd9_dx_file, widths=c(5,255), colClasses="character", strip.white=T)
  dx <- paste0(rep.int('D',length(f$V1)), f$V1)
  
  f <- read.fwf(cms_icd9_px_file, widths=c(4,255), colClasses="character", strip.white=T)
  px <- paste0(rep.int('P',length(f$V1)), f$V1)
  
  c(dx,px)
})()

charlson_list <- list(
  mi = "Myocardial infarction",
  chf = "Congestive heart failure",
  perivasc = "Peripheral vascular disorders",
  cvd = "Cerebrovascular disease",
  dementia = "Dementia",
  chrnlung = "Chronic pulmonary disease",
  rheum = "Rheumatic disease",
  ulcer = "Peptic ulcer disease",
  liver = "Mild liver disease",
  dm = "Diabetes without chronic complication",
  dmcx = "Diabetes with chronic complication",
  para = "Hemiplegia or paraplegia",
  renal = "Renal disease",
  tumor = "Any malignancy, including lymphoma and leukemia, except malignant neoplasm of skin",
  modliver = "Moderate or severe liver disease",
  mets = "Metastatic solid tumor",
  aids = "AIDS/HIV"
)

elixhauser_list <- list(
  chf = "Congestive heart failure",
  arrhythmia = "Cardiac arrhythmias",
  valve = "Valvular disease",
  pulmcirc = "Pulmonary circulation disorders",
  perivasc = "Peripheral vascular disorders",
  htn = "Hypertension, uncomplicated",
  htncx = "Hypertension, complicated",
  para = "Paralysis",
  neuro = "Other neurological disorders",
  chrnlung = "Chronic pulmonary disease",
  dm = "Diabetes, uncomplicated",
  dmcx = "Diabetes, complicated",
  hypothy = "Hypothyroidism",
  renlfail = "Renal failure",
  liver = "Liver disease",
  ulcer = "Peptic ulcer disease excluding bleeding",
  aids = "AIDS/HIV",
  lymph = "Lymphoma",
  mets = "Metastatic Cancer",
  tumor = "Solid tumor without metastasis",
  rheum = "Rheumatoid arthritis/Collagen vascular diseases",
  coag = "Coagulopathy",
  obese = "Obesity",
  wghtloss = "Weight loss",
  lytes = "Fluid and electrolyte disorders",
  bldloss = "Blood loss anemia",
  anemdef = "Deficiency anemia",
  alcohol = "Alcohol abuse",
  drug = "Drug abuse",
  psych = "Psychoses",
  depress = "Depression"
)
  
rcri_list <- list(
  chf = "Congestive heart failure",
  cvd = "Cerebrovascular disease",
  dm = "Diabetes",
  ischemia = "Ischemic heart disease",
  renlfail = "Renal failure"
)

# Weights
# see data.R for details
charlson_weights_orig <- list(
  mi = 1,
  chf = 1,
  perivasc = 1,
  cvd = 1,
  dementia = 1,
  chrnlung = 1,
  rheum = 1,
  ulcer = 1,
  liver = 1,
  dm = 1,
  dmcx = 2,
  para = 2,
  renal = 2,
  tumor = 2,
  modliver = 3,
  mets = 6,
  aids = 6
)

# see data.R for details
charlson_weights <- list(
  mi = 1,
  chf = 2,
  perivasc = 1,
  cvd = 1,
  dementia = 3,
  chrnlung = 2,
  rheum = 0,
  ulcer = 0,
  liver = 2,
  dm = 1,
  dmcx = 2,
  para = 1,
  renal = 3,
  tumor = 2,
  modliver = 4,
  mets = 6,
  aids = 4
)

# VT Inpatient Sample
# see data.R for details

vt_inp_sample <- (function() {
  df <- read.csv("datasrc/vt_top100.csv", stringsAsFactors=FALSE)

  names(df) <- tolower(names(df))
  df$scu_days[is.na(df$scu_days)] <- 0
  df_melt <- melt(df[,c(1,11:33,54:56)], id.vars=c("id","scu_days","drg","mdc"), na.rm=T,
                    variable.name="dx", value.name="icd9cm")
  df_px_melt <- melt(df[,c(1,11:13,34:53)], id.vars=c("id","scu_days","drg","mdc"), na.rm=T, 
                     variable.name="px", value.name="icd9cm")
  cols <- intersect(names(df_melt), names(df_px_melt))
  df_melt <- df_melt[nchar(df_melt$icd9cm) > 0, cols]
  df_melt$icd9cm <- paste('D', df_melt$icd9cm, sep='') 
  df_px_melt <- df_px_melt[nchar(df_px_melt$icd9cm) > 0, cols]
  df_px_melt$icd9cm <- paste('P', df_px_melt$icd9cm, sep='') 
  df_melt <- rbind(df_melt, df_px_melt)
  df_melt$icd9cm <- factor(df_melt$icd9cm)
  df_sorted <- df_melt[order(df_melt$id),]
  row.names(df_sorted) <- NULL
  df_sorted
})()

rsi_zip_url <- 'http://my.clevelandclinic.org/Documents/anesthesiology/RSI-calculation.zip'
rsi_zip <- 'datasrc/rsi.zip'
rsi_inhosp_csv <- 'datasrc/All Proc - Step 4 - INHOSP - May-20-10.csv'
rsi_30dpod_csv <- 'datasrc/All Proc - Step 4 - 30DPOD - May-20-10.csv'
rsi_1yrpod_csv <- 'datasrc/All Proc - Step 4 - 1YRPOD - May-20-10.csv'
rsi_30dlos_csv <- 'datasrc/All Proc - Step 4 - 30DLOS - May-20-10.csv'
rsi_sampledata_csv <- 'datasrc/sample data rev2.csv'
rsi_sampleresults_csv <- 'datasrc/sample results rev2.csv'
rsi_files <- c(rsi_inhosp_csv, rsi_30dpod_csv, rsi_1yrpod_csv, rsi_30dlos_csv, rsi_sampledata_csv, rsi_sampleresults_csv)

# RSI files
(function(){
  if(!all(file.exists(rsi_zip))) {
    if(!file.exists(rsi_zip)) {
      download.file(rsi_zip_url, rsi_zip)
    }
    unzip(rsi_zip, files = basename(rsi_files), exdir = 'datasrc')
  }

  csv_inhosp <- read.csv(rsi_inhosp_csv, header=TRUE, strip.white=TRUE)
  csv_30dpod <- read.csv(rsi_30dpod_csv, header=TRUE, strip.white=TRUE)
  csv_1yrpod <- read.csv(rsi_1yrpod_csv, header=TRUE, strip.white=TRUE)
  csv_30dlos <- read.csv(rsi_30dlos_csv, header=TRUE, strip.white=TRUE)

  rsi_beta_inhosp <- hash(keys=csv_inhosp$dp_code_cox, values=csv_inhosp$beta)
  rsi_beta_30dpod <- hash(keys=csv_30dpod$dp_code_cox, values=csv_30dpod$beta)
  rsi_beta_1yrpod <- hash(keys=csv_1yrpod$dp_code_cox, values=csv_1yrpod$beta)
  rsi_beta_30dlos <- hash(keys=csv_30dlos$dp_code_cox, values=csv_30dlos$beta)

  rsi_sample_data <- read.csv(rsi_sampledata_csv)
  rsi_sample_results <- read.csv(rsi_sampleresults_csv)

  # For some reason the population beta, where they apply, are repeated for every row
  rsi_beta_inhosp$popbeta <- 0
  rsi_beta_30dpod$popbeta <- csv_30dpod[1,'PopBeta']
  rsi_beta_1yrpod$popbeta <- csv_1yrpod[1,'PopBeta']
  rsi_beta_30dlos$popbeta <- csv_30dlos[1,'PopBeta']
  
  # create RDA files
  save(rsi_beta_inhosp, file="data/rsi_beta_inhosp.rda", compress=TRUE)
  save(rsi_beta_30dpod, file="data/rsi_beta_30dpod.rda", compress=TRUE)
  save(rsi_beta_1yrpod, file="data/rsi_beta_1yrpod.rda", compress=TRUE)
  save(rsi_beta_30dlos, file="data/rsi_beta_30dlos.rda", compress=TRUE)
  save(rsi_sample_data, file="data/rsi_sample_data.rda", compress=TRUE)
  save(rsi_sample_results, file="data/rsi_sample_results.rda", compress=TRUE)
})()

# create RDA files
save(icd9cm_list, file="data/icd9cm_list.rda", compress=TRUE)
save(charlson_list, file="data/charlson_list.rda", compress=TRUE)
save(elixhauser_list, file="data/elixhauser_list.rda", compress=TRUE)
save(charlson_weights_orig, file="data/charlson_weights_orig.rda", compress=TRUE)
save(charlson_weights, file="data/charlson_weights.rda", compress=TRUE)
save(vt_inp_sample, file="data/vt_inp_sample.rda", compress=TRUE)
