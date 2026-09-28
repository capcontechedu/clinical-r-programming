####################################################
# Author : Rajasekaran E
# Project : Passion Project
# Description : Validate the adae dataframe created in R using CDISCPILOT study data for practice.
# dev date : 27Sep2026
# modification history :
#####################################################
validate_adae <- function(dfnam){
  
  if (!is.data.frame({{dfnam}})) {
    stop("Enter a valid final dataframe containing data crafted as per ADaM.ADAE specification")
  }
  
  # required library
  libvec <- c("safetyData","stringr","haven","dplyr","tidyr","lubridate","arsenal")
  
  for (i in libvec) {
    if (!requireNamespace(i, quietly = TRUE)) {
      install.packages(i)
    }
    
    library(i, character.only = TRUE)
  }
  ## ADAE dataset creation 
  
  # read sdtm ae
  write_xpt(sdtm_ae,path = "./temp_ae.xpt",version = 5)
  ae <- read_xpt(file = "./temp_ae.xpt")
  file.remove("./temp_ae.xpt")
  
  # read adam ADSL data
  asl <- adam_adsl
  
  ## ---- DATE VARIABLES ----
  ds01 <- ae |> 
    left_join(asl, by = c("STUDYID","USUBJID")) |> 
    mutate(
      ASTDT = start_dt_imput(AESTDTC,TRTSDT),
      ASTDTF = case_when(
        (str_length(AESTDTC)==7 & !is.na(ASTDT)) ~ "D",
        (str_length(AESTDTC)==4 & !is.na(ASTDT)) ~ "M"
      ),
      AENDT = if_else(!is.na(AEENDTC),ymd(AEENDTC),NA_Date_)
    )
  
  ## ---- DURATION VARIABLES ----
  ds02 <- ds01 |> 
    mutate(
      ASTDY = case_when(
        (!is.na(TRTSDT) & !is.na(ASTDT) & ASTDT>=TRTSDT) ~ (ASTDT-TRTSDT)+1,
        (!is.na(TRTSDT) & !is.na(ASTDT) & TRTSDT>ASTDT) ~ (ASTDT-TRTSDT)
      ),
      AENDY = case_when(
        (!is.na(TRTSDT) & !is.na(AENDT) & AENDT>=TRTSDT) ~ (AENDT-TRTSDT)+1,
        (!is.na(TRTSDT) & !is.na(AENDT) & TRTSDT>AENDT) ~ (AENDT-TRTSDT)
      ),
      ADURN = (AENDT-ASTDT)+1,
      ADURU = if_else(!is.na(ADURN),"DAY","")
    )
  
  for (i in c("ASTDY","AENDY","ADURN")) {
    ds02[[i]] <- as.integer(ds02[[i]])
  }
  
  
  
  
  ## ---- ASSIGNING variables ----
  ds02 <- ds02 |> 
    mutate(
      TRTA = TRT01A,
      TRTAN = TRT01AN
    )
  ## ---- FLAG VARIABLES ----
  ds03 <- ds02 |> 
    mutate(
      TRTEMFL = case_when(
        (!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT) ~ "Y",
        TRUE ~ "N"
      ),
      CQ01NAM = case_when(
        str_detect(str_to_upper(AEDECOD),'APPLICATION|DERMATITIS|ERYTHEMA|BLISTER') ~ "DERMATOLOGIC EVENTS",
        (str_remove_all(AEBODSYS," ") =='SKINANDSUBCUTANEOUSTISSUEDISORDERS' & !(AEDECOD %in% c('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA'))) ~ "DERMATOLOGIC EVENTS",
        TRUE ~ ""
      )
    )
  
  flag_firstrec <- function(indata,cond,sort_ord,flg_grp,varnam) {
    
    aa <- {{indata}} |> 
      filter({{cond}}) |> 
      arrange(across(all_of(sort_ord))) |> 
      group_by(across(all_of(flg_grp))) |> 
      mutate(
        !!sym(varnam) := if_else(row_number()==1,"Y","")
      ) |> 
      ungroup() |> 
      filter(!!sym(varnam) == "Y") |> 
      select(all_of(c(sort_ord,varnam)))
    
    #aa <- aa[c(sort_ord,varnam)]
    
    bb <- left_join({{indata}},aa,by = sort_ord) |> 
      mutate(
        !!sym(varnam) := if_else(is.na(!!sym(varnam)),"",!!sym(varnam))
      )
    
    return(bb)
  }
  
  # AOCCFL derivation 
  ds04 <- flag_firstrec(indata=ds03,
                        cond = (TRTEMFL=="Y"),
                        sort_ord = c("USUBJID","ASTDT","AESEQ"),
                        flg_grp = "USUBJID",
                        varnam="AOCCFL"
  )
  # AOCCSFL drivation 
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y"),
                        sort_ord = c("USUBJID","AEBODSYS","ASTDT","AESEQ"),
                        flg_grp = c("USUBJID","AEBODSYS"),
                        varnam="AOCCSFL"
  )
  # AOCCPFL derivation 
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y"),
                        sort_ord = c("USUBJID","AEBODSYS","AEDECOD","ASTDT","AESEQ"),
                        flg_grp = c("USUBJID","AEBODSYS","AEDECOD"),
                        varnam="AOCCPFL"
  )
  
  # AOCC02FL derivation 
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y" & AESER=="Y"),
                        sort_ord = c("USUBJID","ASTDT","AESEQ"),
                        flg_grp = "USUBJID",
                        varnam="AOCC02FL"
  )
  # AOCC03FL drivation 
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y" & AESER=="Y"),
                        sort_ord = c("USUBJID","AEBODSYS","ASTDT","AESEQ"),
                        flg_grp = c("USUBJID","AEBODSYS"),
                        varnam="AOCC03FL"
  )
  # AOCC04FL derivation
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y" & AESER=="Y"),
                        sort_ord = c("USUBJID","AEBODSYS","AEDECOD","ASTDT","AESEQ"),
                        flg_grp = c("USUBJID","AEBODSYS","AEDECOD"),
                        varnam="AOCC04FL"
  )
  
  # AOCC01FL derivation
  ds04 <- flag_firstrec(indata=ds04,
                        cond = (TRTEMFL=="Y" & CQ01NAM !=""),
                        sort_ord = c("USUBJID","ASTDT","AESEQ"),
                        flg_grp = c("USUBJID"),
                        varnam="AOCC01FL"
  )
  
  ## ---- Final dataset ----
  ds04 <- ds04 |> 
    arrange(USUBJID,AEDTC,ASTDT,AESPID,AEBODSYS,AEDECOD,AETERM,AESEQ) |> 
    group_by(USUBJID) |> 
    mutate(
      ASEQ = row_number()
    ) |> 
    ungroup() |> 
    arrange(USUBJID, AEDTC, ASTDT, AESPID,AETERM, ASEQ)
  
  
  fin_c <- ds04 |> 
    select(STUDYID,SITEID,USUBJID,TRTA,TRTAN,AGE,AGEGR1,AGEGR1N,RACE,RACEN,SEX,SAFFL,TRTSDT,TRTEDT,AEDTC,ASTDT,ASTDTF,ASTDY,AENDT,AENDY,ADURN,ADURU,
           AETERM,AELLT,AELLTCD,AEDECOD,AEPTCD,AEHLT,AEHLTCD,AEHLGT,AEHLGTCD,AEBODSYS,AESOC,AESOCCD,AESEV,AESER,AESCAN,AESCONG,AESDISAB,AESDTH,AESHOSP,AESLIFE,AESOD,
           AEREL,AEACN,AEOUT,AESEQ,TRTEMFL,AOCCFL,AOCCSFL,AOCCPFL,AOCC02FL,AOCC03FL,AOCC04FL,CQ01NAM,AOCC01FL,ASEQ)
  
  variable <- names(fin_c)
  
  lbl <- c('Study Identifier',
           'Study Site Identifier',
           'Unique Subject Identifier',
           'Actual Treatment',
           'Actual Treatment (N)',
           'Age',
           'Pooled Age Group 1',
           'Pooled Age Group 1 (N)',
           'Race',
           'Race (N)',
           'Sex',
           'Safety Population Flag',
           'Date of First Exposure to Treatment',
           'Date of Last Exposure to Treatment',
           'Date/Time of Collection',
           'Analysis Start Date',
           'Analysis Start Date Imputation Flag',
           'Analysis Start Relative Day',
           'Analysis End Date',
           'Analysis End Relative Day',
           'AE Duration (N)',
           'AE Duration Units',
           'Reported Term for the Adverse Event',
           'Lowest Level Term',
           'Lowest Level Term Code',
           'Dictionary-Derived Term',
           'Preferred Term Code',
           'High Level Term',
           'High Level Term Code',
           'High Level Group Term',
           'High Level Group Term Code',
           'Body System or Organ Class',
           'Primary System Organ Class',
           'Primary System Organ Class Code',
           'Severity/Intensity',
           'Serious Event',
           'Involves Cancer',
           'Congenital Anomaly or Birth Defect',
           'Persist or Signif Disability/Incapacity',
           'Results in Death',
           'Requires or Prolongs Hospitalization',
           'Is Life Threatening',
           'Occurred with Overdose',
           'Causality',
           'Action Taken with Study Treatment',
           'Outcome of Adverse Event',
           'Sequence Number',
           'Treatment Emergent Analysis Flag',
           '1st Occurrence of Any AE Flag',
           '1st Occurrence of SOC Flag',
           '1st Occurrence of Preferred Term Flag',
           '1st Occurrence 02 Flag for Serious',
           '1st Occurrence 03 Flag for Serious SOC',
           '1st Occurrence 04 Flag for Serious PT',
           'Customized Query 01 Name',
           '1st Occurrence 01 Flag for CQ01',
           'Analysis Sequence')
  
  
  for (a in 1:length(variable)){
    i <- variable[a]
    j <- lbl[a]
    
    if (is.character(fin_c[[i]])) {
      fin_c[[i]] <- ifelse(is.na(fin_c[[i]]),"",fin_c[[i]])
    }
    
    attr(fin_c[[i]], "label") <- j
  }
  
  attr(fin_c,"label") <- "Adverse Events Analysis Dataset"
  
  
  dev <- {{dfnam}}
  qc <- fin_c
  
  output <- summary(comparedf(x = dev, y = qc))
  return(output)
}
