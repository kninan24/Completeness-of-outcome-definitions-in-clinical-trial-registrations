/*************************************************************************************************
Title: Outcome definitions in clinical trial registrations: agreement calculation
Purpose: This code calculates the element-level and outcome-level agreement between raters
Date last updated: 12/12/2025
**************************************************************************************************/

/*******************************/
/* Preparation                 */
/*******************************/
%LET lib      = yourlibname;  /* Replace with your desired libname */
/* UPDATE THIS PATH to the folder where you have saved the data files:
   - cleanagree.csv
   Place all data files in the same folder before running. */
%LET filepath = C:\your\filepath\here;
%LET data     = cleanagree.csv;
LIBNAME &lib "&filepath";

PROC IMPORT OUT= &lib.data
    DATAFILE= "&filepath\&data"
    DBMS=CSV REPLACE;
    GETNAMES=YES;
    DATAROW=2;
RUN;

/********************************************/
/*Calculating agreement at the element level*/
/********************************************/
DATA work.elem_agree;
    SET &lib.data;
    IF missing(rater1_out) and missing(rater2_out) THEN elem_agree = .;
    ELSE IF rater1_out = rater2_out THEN elem_agree = 1;
    ELSE elem_agree = 0;
RUN;
/*For each row (which corresponds to 1 outcome element), compares the output of each rater to
determine agreement at the element level (elem_agree). In cases where both rater's entries are missing, agreement is missing,
otherwise, any mismatch in output (including 1 missing and 1 with a value) is considered to be a disagreement*/


PROC SQL;
    SELECT (SELECT count(DISTINCT catx('|', nct_id, oc_id))
            FROM work.elem_agree) AS total_outcomes,
            oc_elem,
            sum(elem_agree=1) AS agree_count,
            calculated agree_count / (sum(elem_agree=0) + calculated agree_count)
            format=percent8.2 AS agree_pct
    FROM work.elem_agree
    GROUP BY oc_elem;
QUIT;
/*Calculates the total number of outcomes, total number of agreements for each element, and percent of agreement for each element*/

/********************************************/
/*Calculating agreement at the outcome level*/
/********************************************/
PROC SQL;
    CREATE TABLE work.oc_flags AS
    SELECT nct_id,
           oc_id,
           /*Rater 1 values*/
           max(case WHEN oc_elem="cut_rate"   THEN rater1_out end) AS r1_cut_rate,
           max(case WHEN oc_elem="meas_rate"  THEN rater1_out end) AS r1_meas_rate,
           max(case WHEN oc_elem="method_def" THEN rater1_out end) AS r1_method_def,
           max(case WHEN oc_elem="metric_def" THEN rater1_out end) AS r1_metric_def,
           max(case WHEN oc_elem="time_prio"  THEN rater1_out end) AS r1_time_prio,
           /*Rater 2 values*/
           max(case WHEN oc_elem="cut_rate"   THEN rater2_out end) AS r2_cut_rate,
           max(case WHEN oc_elem="meas_rate"  THEN rater2_out end) AS r2_meas_rate,
           max(case WHEN oc_elem="method_def" THEN rater2_out end) AS r2_method_def,
           max(case WHEN oc_elem="metric_def" THEN rater2_out end) AS r2_metric_def,
           max(case WHEN oc_elem="time_prio"  THEN rater2_out end) AS r2_time_prio
    FROM &lib.data
    WHERE oc_elem IN ("cut_rate","meas_rate","method_def","metric_def","time_prio")
    GROUP BY nct_id, oc_id;
QUIT;
/*Transposes oc_elem to create a wide dataset with 10 new columns, 5 for each rater, corresponding to each element of an outcome, excluding tim_desc
as time description was only rated descriptively rather than to evaluate completeness of an outcome.*/

DATA work.oc_flags;
    SET work.oc_flags;
	/*Rater 1 values*/
    r1_cut = input(r1_cut_rate, best32.);
    r1_meas = input(r1_meas_rate, best32.);
    r1_method = input(r1_method_def, best32.);
    r1_metric = input(r1_metric_def, best32.);
    r1_time = input(r1_time_prio, best32.);
	/*Rater 2 values*/
    r2_cut = input(r2_cut_rate, best32.);
    r2_meas = input(r2_meas_rate, best32.);
    r2_method = input(r2_method_def, best32.);
    r2_metric = input(r2_metric_def, best32.);
    r2_time = input(r2_time_prio, best32.);
    /*Completeness flags*/
    r1_oc = (r1_cut in (1,3) and r1_meas=1 and r1_method=1 and r1_metric=1 and r1_time in (1,3));
    r2_oc = (r2_cut in (1,3) and r2_meas=1 and r2_method=1 and r2_metric=1 and r2_time in (1,3));
RUN;
/*First, converts each of the character variables of the columns created in the previous step into numeric variables. 
Then, creates a new variable for each rater that is 1 only if all elements were rated as "completely reported" or "not applicable" by that rater, and is otherwise 0.
This coresponds to an outcome being completely reported, or incompletely reported if at least 1 element was not completely reported*/


/*Outcome-level agreement*/
PROC SQL;
    SELECT count(*) AS total_outcomes,
           sum(CASE WHEN r1_oc = r2_oc THEN 1 ELSE 0 END) AS n_agree,
           calculated n_agree / calculated total_outcomes AS pct_agree format=percent8.2
    FROM oc_flags;
QUIT;
/*Calculates the total number of outcomes, total number of agreements at the outcome level, and percent of agreements at the outcome level*/
