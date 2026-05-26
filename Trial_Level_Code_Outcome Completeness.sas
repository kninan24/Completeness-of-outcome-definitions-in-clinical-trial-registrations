/*************************************************************************************************
Title: Outcome definitions in clinical trial registrations: trial-level findings
Purpose: This code includes analyses at the trial-level and outputs tables that were used within this study. Manuscript tables were reformatted in Word
Date last updated: 05/21/2026
**************************************************************************************************/

/*******************************/
/*Preparation*/
/*******************************/
%LET lib = data;
%LET filepath = C:\your\filepath\here;
/*UPDATE THIS PATH to the folder where you have saved the data files:
   - Completeness_CTG_trial_level_data.csv
   - Completeness_other_trial_level_data.csv
   Place all data files in the same folder before running.*/
%LET data_CTG = Completeness_CTG_trial_level_data.csv;
%LET data_Other_RCTs = Completeness_other_trial_level_data.csv;
LIBNAME &lib "&filepath";

PROC IMPORT OUT= &lib.data_CTG
    DATAFILE= "&filepath\&data_CTG"
    DBMS=CSV REPLACE;
    GETNAMES=YES;
    GUESSINGROWS=max;
    DATAROW=2;
RUN;

PROC IMPORT OUT= &lib.data_Other_RCTs  
    DATAFILE= "&filepath\&data_Other_RCTs"
    DBMS=CSV REPLACE;
    GETNAMES=YES;
    DATAROW=2;
RUN;

/********************************************/
/*Number of trials in each of the ICMJE member journals*/
/********************************************/

/* Create frequency table for CTG data */
proc freq data=&lib.data_CTG;
    tables journal_book / out=CTG_freq nocum;
run;

/* Create frequency table for Other RCTs data */
proc freq data=&lib.data_Other_RCTs;
    tables journal_book / out=Other_freq nocum;
run;

/* Inputting data for unregistered/unrecognized registries */
data unreg_freq;
    length journal_book $100;
    
    /* Bulletin of the World Health Organization */
    journal_book = "Bulletin of the World Health Organization";
    unreg_count = 1;
    output;
    
    /* PLoS Medicine */
    journal_book = "PLoS medicine";
    unreg_count = 1;
    output;
    
    /* The National Medical Journal of India */
    journal_book = "The National Medical Journal of India";
    unreg_count = 1;
    output;
run;

/* Merge all three frequency tables */
data combined_freq;
    merge CTG_freq(rename=(COUNT=CTG_count PERCENT=CTG_pct))
          Other_freq(rename=(COUNT=Other_count PERCENT=Other_pct))
          unreg_freq;
    by journal_book;
    
    /* Replace missing values with 0 */
    if missing(CTG_count) then CTG_count = 0;
    if missing(Other_count) then Other_count = 0;
    if missing(unreg_count) then unreg_count = 0;
    
    /* Calculate total and row percentages */
    total = CTG_count + Other_count + unreg_count;
    CTG_row_pct = (CTG_count / total) * 100;
    Other_row_pct = (Other_count / total) * 100;
    unreg_row_pct = (unreg_count / total) * 100;
run;

/* Output to RTF file */
ods rtf file="&filepath\ICMJE_journals.rtf";

proc tabulate data=combined_freq format=8.1;
    var CTG_count CTG_row_pct Other_count Other_row_pct unreg_count unreg_row_pct total;
    class journal_book;
    table journal_book all,
          total='Total (n)'*sum=''
          CTG_count='RCTs registered on ClinicalTrials.gov'*sum=''
          CTG_row_pct=' '*sum='%'
          Other_count='RCTs registered on other registries only'*sum=''
          Other_row_pct=' '*sum='%'
          unreg_count='RCTs not registered/not on an ICMJE recognized registry'*sum=''
          unreg_row_pct=' '*sum='%';
    title "Number of trials in each of the ICMJE member journals";
run;

ods rtf close;

/********************************************/
/*Characteristics of included registrations in ClinicalTrials.gov*/
/********************************************/

/* Create numeric version of is_act for ordering */
data data_CTG_chars;
    set &lib.data_CTG;
    
    /* Create numeric version: 1=Yes, 2=No for desired order */
    if is_act = "Yes" then is_act_num = 1;
    else if is_act = "No" then is_act_num = 2;
run;

/* Including denominators in column headers */
proc sql noprint;
    /* Overall N */
    select count(*) into :N_all_chars trimmed from data_CTG_chars;
    
    /* N by ACT status */
    select count(*) into :N_act_no_chars trimmed from data_CTG_chars where is_act="No";
    select count(*) into :N_act_yes_chars trimmed from data_CTG_chars where is_act="Yes";
quit;

/* Creating column headings for ACTs */
proc format;
    value actfmt_b
        1 = "Yes (N=&N_act_yes_chars)"
        2 = "No (N=&N_act_no_chars)";
run;

/* Output to RTF file */
ods rtf file="&filepath\CTG_characteristics.rtf";

proc tabulate data=data_CTG_chars format=8.1 missing;
    class journal_book intervention_model allocation funder_sponsor is_act_num;
    format is_act_num actfmt_b.;
    table (journal_book intervention_model allocation funder_sponsor) all,
          all="Overall (N=&N_all_chars)"*(n='n'*f=8.0 colpctn='%'*f=8.1)
          is_act_num='ACT'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    title "Characteristics of included registrations in ClinicalTrials.gov";
run;

ods rtf close;
