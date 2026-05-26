/*************************************************************************************************
Title: Outcome definitions in clinical trial registrations: outcome-level findings
Purpose: This code includes analyses at the outcome-level and outputs tables that were used within this study. Manuscript tables were reformatted in Word
Date last updated: 05/21/2026
**************************************************************************************************/

/*******************************/
/*Preparation*/
/*******************************/
%LET lib = data;
%LET filepath = C:\your\filepath\here;
/* UPDATE THIS PATH to the folder where you have saved the data files:
   - Completeness_outcome_level_data.csv
   Place all data files in the same folder before running. */
%LET data_outcomes = Completeness_outcome_level_data.csv;
LIBNAME &lib "&filepath";
PROC IMPORT OUT= &lib.data_outcomes
    DATAFILE= "&filepath\&data_outcomes"
    DBMS=CSV REPLACE;
    GETNAMES=YES;
    GUESSINGROWS=max;
    DATAROW=2;
RUN;

/********************************************/
/*Outcome-level completeness findings stratified by priority, ACT, and funder*/
/*Subgroup analyses by priority, ACT, funder were moved to a plot (see seperate R code on this)
/* % completeness for cut-off was calculated separately among outcomes where a cut-off
   was applicable (i.e. excluding Not Applicable from the denominator)(see seperate bootstrap R code on this)
/********************************************/

/* Create derived variables */
data data_outcomes_table3;
    set &lib.data_outcomes;
    
    /* Create outcome_complete variable */
    if meas_rate=1 and metric_def=1 and method_def=1 and cut_rate in (1,3) then outcome_complete=1;
    else outcome_complete=0;
    
    /* Create baseline only variable (time_desc_10) */
    if time_desc_1 = 1 and sum(of time_desc_2-time_desc_9) = 0 then time_desc_10 = 1;
    else time_desc_10 = 0;
    
    /* Create baseline and other timepoints variable (time_desc_11) */
    time_desc_11 = time_desc_1 - time_desc_10;
    
    /* Create funder_sponsor variable */
    length funder_sponsor $60;
    if is_nih_funded = "Yes" or funder_type = "NIH" then funder_sponsor = "NIH";
    else funder_sponsor = funder_type;
    
    /* Create numeric version of is_act for ordering: 1=Yes, 2=No */
    if is_act = "Yes" then is_act_num = 1;
    else if is_act = "No" then is_act_num = 2;
run;

/* Get total N for each stratification */
proc sql noprint;
    /* Overall N */
    select count(*) into :N_all trimmed from data_outcomes_table3;
    
    /* N by outcome priority */
    select count(*) into :N_primary trimmed from data_outcomes_table3 where out_num_cat=1;
    select count(*) into :N_secondary trimmed from data_outcomes_table3 where out_num_cat=2;
    select count(*) into :N_other trimmed from data_outcomes_table3 where out_num_cat=3;
    
    /* N by ACT status */
    select count(*) into :N_act_no trimmed from data_outcomes_table3 where is_act="No";
    select count(*) into :N_act_yes trimmed from data_outcomes_table3 where is_act="Yes";
    
    /* N by funder_sponsor - get counts for each category */
    select count(*) into :N_nih trimmed from data_outcomes_table3 where funder_sponsor="NIH";
    select count(*) into :N_industry trimmed from data_outcomes_table3 where funder_sponsor="INDUSTRY";
    select count(*) into :N_other_fund trimmed from data_outcomes_table3 where funder_sponsor="Other";
quit;

%put Overall N = &N_all;
%put Primary N = &N_primary;
%put Secondary N = &N_secondary;
%put Other Priority N = &N_other;
%put ACT No N = &N_act_no;
%put ACT Yes N = &N_act_yes;

/* Define formats for better labels */
proc format;
    value complete_fmt
        1 = 'Complete'
        2 = 'Incomplete';
    
    value outcome_complete_fmt
        0 = 'Incomplete'
        1 = 'Complete';
    
    value cutrate_fmt
        1 = 'Complete'
        2 = 'Incomplete'
        3 = 'Not applicable';
    
    value timeprio_fmt
        1 = 'Specified'
        2 = 'Not reported'
        3 = 'Not applicable';
    
    value timedesc_fmt
        0 = 'Not described'
        1 = 'Described';
    
    value outpriority_fmt
        1 = "Primary (N=&N_primary)"
        2 = "Secondary (N=&N_secondary)"
        3 = "Other (N=&N_other)";
    
    value actfmt_d
        1 = "Yes (N=&N_act_yes)"
        2 = "No (N=&N_act_no)";
run;

/* Output to RTF file */
ods rtf file="&filepath\Outcome_completeness_stratified.rtf";

proc tabulate data=data_outcomes_table3 format=8.1 missing;
    class outcome_complete meas_rate metric_def method_def cut_rate time_prio
          time_desc_10 time_desc_11 time_desc_2 time_desc_3 time_desc_4 
          time_desc_5 time_desc_6 time_desc_8
          out_num_cat is_act_num funder_sponsor;
    
    format outcome_complete outcome_complete_fmt.
           meas_rate metric_def method_def complete_fmt.
           cut_rate cutrate_fmt.
           time_prio timeprio_fmt.
           time_desc_10 time_desc_11 time_desc_2 time_desc_3 
           time_desc_4 time_desc_5 time_desc_6 time_desc_8 timedesc_fmt.
           out_num_cat outpriority_fmt.
           is_act_num actfmt_d.;
    
    table (outcome_complete='All elements registered completely'
           meas_rate='Specific measurement'
           metric_def='Specific metric'
           method_def='Variable type'
           cut_rate='Cut-off'
           time_prio='Time-point priority'
           time_desc_10='Baseline only'
           time_desc_11='Baseline and other timepoints'
           time_desc_2='Single time point'
           time_desc_3='Multiple time points'
           time_desc_4='Interval or frequency'
           time_desc_5='Event'
           time_desc_6='Estimated time'
           time_desc_8='Non-follow-up information'),
          all="All Outcomes (N=&N_all)"*(n='n'*f=8.0 colpctn='%'*f=8.1)
          out_num_cat='Outcome Priority'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          is_act_num='Applicable Clinical Trials'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          funder_sponsor='Funder/Sponsor'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    
    title "Outcome-level completeness findings stratified by priority, ACT status, and funder/sponsor";
run;

ods rtf close;

/********************************************/
/*Outcome-level findings stratified by journal*/
/********************************************/

/* Create derived variables */
data data_outcomes_byJN;
    set &lib.data_outcomes;
    
    /* Create outcome_complete variable */
    if meas_rate=1 and metric_def=1 and method_def=1 and cut_rate in (1,3) then outcome_complete=1;
    else outcome_complete=0;
    
    /* Create baseline only variable (time_desc_10) */
    if time_desc_1 = 1 and sum(of time_desc_2-time_desc_9) = 0 then time_desc_10 = 1;
    else time_desc_10 = 0;
    
    /* Create baseline and other timepoints variable (time_desc_11) */
    time_desc_11 = time_desc_1 - time_desc_10;
run;

/* Get total N for each stratification */
proc sql noprint;
    /* Overall N */
    select count(*) into :N_all_byJN trimmed from data_outcomes_byJN;
    
    /* Get list of unique journals and their counts */
    select distinct journal_book, count(*) 
    into :journal1-:journal20, :N_journal1-:N_journal20
    from data_outcomes_byJN
    group by journal_book
    order by journal_book;
    
    /* Get count of journals */
    select count(distinct journal_book) into :n_journals trimmed
    from data_outcomes_byJN;
quit;

%put Overall N = &N_all_byJN;
%put Number of journals = &n_journals;

/* Create dynamic format for journals with N values */
proc sql noprint;
    select distinct 
        catx('', "'", journal_book, "' = '", journal_book, " (N=", count(*), ")'")
        into :journal_fmt_list separated by ' '
    from data_outcomes_byJN
    group by journal_book
    order by journal_book;
quit;

/* Define formats for better labels */
proc format;
    value complete_fmt
        1 = 'Complete'
        2 = 'Incomplete';
    
    value outcome_complete_fmt
        0 = 'Incomplete'
        1 = 'Complete';
    
    value cutrate_fmt
        1 = 'Complete'
        2 = 'Incomplete'
        3 = 'Not applicable';
    
    value timeprio_fmt
        1 = 'Specified'
        2 = 'Not reported'
        3 = 'Not applicable';
    
    value timedesc_fmt
        0 = 'Not described'
        1 = 'Described';
    
    value $journal_fmt
        &journal_fmt_list;
run;

/* Output to RTF file */
ods rtf file="&filepath\Outcome_journal_stratified.rtf";

proc tabulate data=data_outcomes_byJN format=8.1 missing;
    class outcome_complete meas_rate metric_def method_def cut_rate time_prio
          time_desc_10 time_desc_11 time_desc_2 time_desc_3 time_desc_4 
          time_desc_5 time_desc_6 time_desc_8
          journal_book;
    
    format outcome_complete outcome_complete_fmt.
           meas_rate metric_def method_def complete_fmt.
           cut_rate cutrate_fmt.
           time_prio timeprio_fmt.
           time_desc_10 time_desc_11 time_desc_2 time_desc_3 
           time_desc_4 time_desc_5 time_desc_6 time_desc_8 timedesc_fmt.
           journal_book $journal_fmt.;
    
    table (outcome_complete='All elements registered completely'
           meas_rate='Specific measurement'
           metric_def='Specific metric'
           method_def='Variable type'
           cut_rate='Cut-off'
           time_prio='Time-point priority'
           time_desc_10='Baseline only'
           time_desc_11='Baseline and other timepoints'
           time_desc_2='Single time point'
           time_desc_3='Multiple time points'
           time_desc_4='Interval or frequency'
           time_desc_5='Event'
           time_desc_6='Estimated time'
           time_desc_8='Non-follow-up information'),
          all="All Outcomes (N=&N_all_byJN)"*(n='n'*f=8.0 colpctn='%'*f=8.1)
          journal_book=' '*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    
    title "Outcome-level findings stratified by journal";
run;

ods rtf close;

/********************************************/
/*Trial-level completeness derived from outcome-level data*/
/*Data from this section was included manually as rows in trial-level data tables*/
/********************************************/

/* Collapse to trial level: trial is complete only if ALL outcomes are complete */
proc sql;
    create table work.trials_level_complete as
    select 
        nct_key,
        min(outcome_complete) as trial_complete   /* 1 only if ALL outcomes=1 */
    from data_outcomes_table3
    group by nct_key;
quit;

/* Get trial-level characteristics (one row per trial) */
proc sort data=data_outcomes_table3 out=work.sorted_for_trial_level;
    by nct_key;
run;

data work.trials_level_chars;
    set work.sorted_for_trial_level;
    by nct_key;
    if first.nct_key;
    keep nct_key is_act journal_book funder_sponsor out_num_cat;
run;

/* Join trial completeness with trial characteristics */
proc sql;
    create table work.trials_complete_with_chars as
    select 
        a.nct_key,
        a.trial_complete,
        b.is_act,
        b.journal_book,
        b.funder_sponsor,
        b.out_num_cat
    from work.trials_level_complete as a
    left join work.trials_level_chars as b
        on a.nct_key = b.nct_key;
quit;

/* Define format for trial_complete */
proc format;
    value trial_complete_fmt
        0 = 'Incomplete'
        1 = 'Complete';
run;

/*Trial-level completeness stratified by ACT status */
ods rtf file="&filepath\Trial_completeness_by_ACT.rtf";

proc tabulate data=work.trials_complete_with_chars format=8.1 missing;
    class trial_complete is_act;
    format trial_complete trial_complete_fmt.;
    
    table trial_complete='Trial Completeness',
          all='All Trials'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          is_act='Applicable Clinical Trials'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    
    title "Trial-level completeness stratified by ACT status";
run;

ods rtf close;

/*Trial-level completeness stratified by Journal */
ods rtf file="&filepath\Trial_completeness_by_Journal.rtf";

proc tabulate data=work.trials_complete_with_chars format=8.1 missing;
    class trial_complete journal_book;
    format trial_complete trial_complete_fmt.;
    
    table trial_complete='Trial Completeness',
          all='All Trials'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          journal_book=' '*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    
    title "Trial-level completeness stratified by Journal";
run;

ods rtf close;

/*Trial-level completeness stratified by Funder/Sponsor */
ods rtf file="&filepath\Trial_completeness_by_Sponsor.rtf";

proc tabulate data=work.trials_complete_with_chars format=8.1 missing;
    class trial_complete funder_sponsor;
    format trial_complete trial_complete_fmt.;
    
    table trial_complete='Trial Completeness',
          all='All Trials'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          funder_sponsor='Funder/Sponsor'*(n='n'*f=8.0 colpctn='%'*f=8.1)
          / printmiss misstext='0';
    
    title "Trial-level completeness stratified by Funder/Sponsor";
run;

ods rtf close;

/*How many trials had all primary outcomes that were complete */
proc sql;
    select count(distinct nct_key) as n_trials_all_primary_complete
    from (
        select nct_key, min(outcome_complete) as all_primary_complete
        from data_outcomes_table3
        where out_num_cat = 1
        group by nct_key
    )
    where all_primary_complete = 1;
quit;
