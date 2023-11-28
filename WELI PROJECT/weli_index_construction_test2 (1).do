/* PROJECT-LEVEL WOMEN'S EMPOWERMENT IN LIVESTOCK INDEX (WELI)
    INDEX CALCULATION
	ADAPTED FROM PRO-WEAI (2 Draft pro-WEAI index construction (May 2020).do)
	
	BY NILS TEUFEL, ILRI (MAY 2020)
	
	MAIN ADAPTATIONS:
	
	IN "OPEN FILE AND LOG"
		SET APPROPRIATE 
			WORKING DIRECTORY, 
			PROJECT NAME, 
			LOG FILE NAME
		CHANGE OPEN FILE TO: 
			"** Open file with WELI indicators
                   use "${projectname}_weli_dataprep", clear"

	FOR FILE AND VARIABLE NAMES
		FIND AND REPLACE
			proweai  -> weli
			PRO-WEAI -> WELI
			PROWEAI  -> WELI
			
	IN GENDER PARITY INDEX, [GENDER [PARITY INDEX ONLY WORKS IF DATASETS HAS MALE AND FEMALE RESPONDENTS FROM SAME HOUSEHOLD]
	"Create a local variable with all CORE indicators varlist_emp "
		ADD "feelinputdecagrl" TO LIST OF VARIABLES, 2X
	"Define the weights."
		ADD "feelinputdecagrl" TO LIST OF VARIABLES, 1X
		CHANGE "gen double w_`var'=1/12" -> "gen double w_`var'=1/13"
    
 ----------------------------------------------------------------------------
 */


/* PROJECT-LEVEL WOMEN'S EMPOWERMENT IN AGRICULTURE INDEX (PRO-WEAI)
	CALCULATE PRO-WEAI DO-FILE
	UDPATED MAY 2020
	CONTACT: IFPRI-WEAI@CGIAR.ORG
	
	THIS DO-FILE CALCULATES PRO-WEAI FROM THE 12 PRO-WEAI INDICATORS. 
	NOTE: THIS DO-FILE IS BASED ON A DRAFT VERSION OF THE PRO-WEAI AND IS SUBJECT TO CHANGE. 
	
	UPDATES:
	May 18, 2020 - Changed from float to double: lines 107, 108, 133, 328, 329, 338-341, 344-347 */

	
********************************************************************************


clear all
set more off

********************************************************************************
					*** OPEN FILE AND LOG *** 
********************************************************************************

** Set working directory
cd "C:\Users\Ken\Dropbox\Research Grant Projects\The GIVE Project\Data Management\WELI Baseline Data\Shevac Project WELI do files"

** Store project name
global projectname "weli_test"

** Open log
capture log close
log using "${projectname}_weli_calculate_log", text replace 

** Open file with WELI indicators
use "${projectname}_weli_dataprep", clear

********************************************************************************
					*** CALCULATE WELI *** 
********************************************************************************

**************************************************
*******   FIVE DOMAINS EMPOWERMENT (5DE)   *******
**************************************************

// So far all_indicators were defined so 1 identifies adequate. //
// Now we transform indicators so 1 identifies inadequate. //

foreach var in autonomy_inc selfeff never_violence feelinputdecagr feelinputdecagrl assetownership credit_accdec incomecontrol work_balance mobility groupmember group_inf respect {
	rename `var' `var'_ndepr
	gen `var'=1 if `var'_ndepr==0
	replace `var'=0 if `var'_ndepr==1
	}

*We are now starting with 0-1 variables where 1 means that the person is inadequate in that indicator. 

gen weight=1 // Note: =1 if unweighted; otherwise, assign variable containing individual sampling weights

save "depr_indicators_2.0.dta", replace

*****************************************************************************
********  Create a local macro with all CORE indicators called varlist_emp ******
*****************************************************************************

#delimit;
local varlist_emp autonomy_inc selfeff never_violence feelinputdecagr feelinputdecagrl assetownership credit_accdec 
                  incomecontrol work_balance mobility groupmember group_inf respect;

gen sample1=(autonomy_inc~=. & selfeff~=. & never_violence~=. & feelinputdecagr~=. & feelinputdecagrl~=. & assetownership~=. & 
             credit_accdec~=. & incomecontrol~=. & work_balance~=. & mobility~=. & groupmember~=. & group_inf~=. & respect~=.);
#delimit cr

**********************************************************************************
**** Define the CORE weights. Weights sum to 1 (not to the number of indicators)**
**********************************************************************************
*********** Create a loop for the variables with the same weight *****************
**********************************************************************************

*We now create the indicatorsí weights.*

foreach var in autonomy_inc selfeff never_violence feelinputdecagr feelinputdecagrl assetownership credit_accdec incomecontrol work_balance mobility groupmember group_inf respect {
	gen double w_`var'=1/13
	}

*******************************************************************
*********     Define the weighted inadequacy g0* matrix       ****
*******************************************************************

// WE FOCUSED ON THE MEASURE OF INADEQUACIES (DISEMPOWERMENT). //

foreach var in `varlist_emp'{
	gen double wg0_`var'= `var'*w_`var'
}

******************************************************************************
*********** Compute the frequency of missing values for indicator ************
******************************************************************************

foreach var in `varlist_emp' {

gen `var'_miss=1 if `var'==.
replace `var'_miss=0 if `var'!=.
}

sum *_miss

********************************************************************************
*************   Define the (weighted) inadequacy count vector "ci" ************
********************************************************************************

egen double ci=rsum(wg0_*)
replace ci=round(ci, 0.0001)
label variable ci "Inadequacy Count"

egen n_missing=rowmiss(wg0_*)
label variable n_missing "Number of missing variables by individual"
gen missing=(n_missing>0)
label variable missing "Individual with missing variables"

*** Check sample drop due to missing values
tab missing
*drop if missing

***********************************************************************
***** Create the identification vector (inadequate/adequate) ***********
***** and compute individual average of inadequacy    *****************
***********************************************************************

egen total_w=total(weight) if missing==0

// FIRST, WE COMPUTED THE DISEMPOWERMENT IN AGRICULTURE INDEX (DAI). //
// AFTERWARDS, WE COMPUTE THE EMPOWERMENT IN AGRICULTURE INDEX (HERE CALLED EAI): EAI = 1 - DAI. //

*These are now percentages - this creates DAI by each percentage. 

forvalues x=1(1)100 { // FOR EACH POSSIBLE CUTOFF X BETWEEN 1% AND 100% //
	gen ch_`x'p=(ci>(`x'/100))	// WE CREATE A VARIABLE THAT IDENTIFIES THE DISEMPOWERED INDIVIDUALS (THOSE WHO HAVE AN INADEQUACY SCORE HIGHER THE X%). //
	replace ch_`x'p=. if missing==1
	gen a_`x'p=(ci) if ch_`x'p==1 // WE COMPUTE THE INDIVIDUAL INADEQUACY OF THOSE WHO ARE DISEMPOWERED. //
	replace a_`x'p=. if missing==1
	egen DAI_`x'p= total(ci*ch_`x'p*weight/total_w) // WE COMPUTE THE DISEMPOWERMENT INDEX (FOR EACH POSSIBLE CUTOFF X) //
	gen EAI_`x'p=1-DAI_`x'p // THEN, WE OBTAIN THE EMPOWERMENT INDEX. //
	label var ch_`x'p "Condition of disempowerment  k=`x'%"
	label var a_`x'p "Individual Average inadequacy  k=`x'"
	label var DAI_`x'p "National Disempowerment Index k=`x'%"
	label var EAI_`x'p "Combined Empowerment Index k=`x'%"
	}

// PLEASE NOTE THAT THESE ARE NOT YET THE 5DE. SO FAR WE ARE STILL LOOKING AT WOMEN AND MEN TOGETHER AND WE HAVE NOT YET DEFINED THE CUTOFF WE WANT TO USE. //

summarize ch_* a_* DAI_* EAI_* [aw=weight]

************************************************************************
******* Compute raw headcounts        **********************************
************************************************************************

foreach var in `varlist_emp' {
gen `var'_raw=(`var')
replace `var'_raw=. if missing==1
}

sum *_raw  [iw=weight]

***********************************************************************************
*********** Compute Censored headcount by subgroups (gender or region etc)   ******
***********************************************************************************

// NOW WE DEFINE THE CUTOFF THAT WE WANT TO USE AND WE START LOOKING AT WOMEN AND MEN SEPARATELY //

* Please define in the first line your cutoff, the example shows k=20 is 20% of the variables
* In the second line replace with the name of the categorical variable (the variable name by which censored headcount is to be generated for the variables)
* that represents the different subgroups. 
* The subgroup variable must be coded in consecutive natural numbers starting in 1

pause

local k=25
gen gender=sex

local r="gender"

foreach var in `varlist_emp' {
gen `var'_CH_`k'p=(`var'==1 & ch_`k'==1)
replace `var'_CH_`k'p=. if missing==1
}

summarize *_CH_`k'p [iw=weight]

*****************************************************************************************
*****************************************************************************************
**** Define decomposition rule (country, sex)
**** We keep the information of the weighted population before reducing the sample to only 
**** those cases with information in all the indicators considered

egen total_b = total(weight)
label var total_b "Total Population Before Sample Drop"
egen pop_shr_before = total(weight/total_b), by(`r')
label var pop_shr_before "Weighted Population Share of Each `r' before Sample Reduction"
gen temp=1 // We generate this variable for counting observations
egen sample_r_before = total(temp), by(`r')
label var sample_r_before "Sample Size of each `r' before Sample Reduction"

egen pop_shr_after = total(weight/total_w) if miss==0, by(`r')
label var pop_shr_after "Weighted Population Share of Each `r' after Sample Reduction"
egen sample_r_after = total(temp) if missing==0, by(`r')
label var sample_r_after  "Sample Size of Each `r' after Sample Reduction"
gen sample_lost_ratio= sample_r_after/sample_r_before
label var sample_lost_ratio  "Relative size of the final sample after reduction in each `r'"

************************************************************************************
**** Collapsing ********************************************************************
* So far, our database has individual level data, if we want to aggregate
* at any level, we use the command ìcollapseî. Collapse calculates weighted 
* averages at the  level defined by the user (gender), if the option "by(gender)"
* is not specified, the observations are aggregated at the national level.
* Before collapse, save your results using the following command
*************************************************************************************

save "WELI_individual_indices_2.0.dta", replace  // SAVES, FOR EACH COUNTRY, A DATASET WITH INDIVIDUAL DATA. //
// THIS DATASET INCLUDES INDIVIDUAL INADEQUACY COUNT, VARIABLES THAT IDENTIFY DISEMPOWERED FOR EACH CUTOFF AND VALUE OF DAI AND EAI FOR EACH CUTOFF. //
// PLEASE REMEMBER THAT DAI AND EAI WERE COMPUTED CONSIDERING WOMEN AND MEN TOGETHER. //
 
* You can use also the commands preserve before the command ìcollapseî and restore just after
* preserve

// NOW WE COMPUTE RELEVANT VARIABLES BY GENDER. //

egen pop_shr = total(weight/total_w) if miss==0, by(`r')

* collapse
* The following command will "collapse" our individual results according to the subgroup previously defined. 
//pause
collapse ch_* a_* *_CH_`k'p *_raw w_* EAI_* *_miss missing DAI_* pop_shr* sample_r_* sample_lost_ratio [aw=weight],by(`r')

* You have already calculated the national DAI. With the following lines you will calculate the 
* DAI for every region using the formulation M0=H*A obtained after collapsing the dataset.

// ATTENTION: DAI AND EAI REFER TO NATIONAL FIGURES. M0 AND EA REFER TO GENDER FIGURES. //

forvalues x=1(1)100 {
	gen M0_`x'p=ch_`x'p*a_`x'p
	label var M0_`x'p "Population Subgroup DAI k=`x'%"
	gen EA_`x'p=1-M0_`x'p
	label var EA_`x'p "Population Subgroup EAI k=`x'%"
	ren ch_`x'p H_`x'p
	label var H_`x'p "Population Subgroup Multidimensional Headcount Ratio k=`x'%"
	ren a_`x'p A_`x'p
	label var A_`x'p "Population Subgroup Average Inadequacy k=`x'%"
	label var DAI_`x'p "National DAI k=`x'%"
	}

foreach var in `varlist_emp' {
	gen `var'_cont_`k'_EAI=((`var'_CH_`k'p* w_`var')/ EA_`k'p)
	label var `var'_cont_`k'_EAI "Decomposed Contribution of `var' to the total Empowerment k=`k'"

	gen `var'_cont_`k'_DAI=((`var'_CH_`k'p* w_`var')/ M0_`k'p)
	label var `var'_cont_`k'_DAI "Decomposed Contribution of `var' to the total Disempowerment k=`k'"

	label var  `var'_CH_`k'p  "Decomposed Censored Headcount `var' k=`k'"
	label var  `var'_raw  "Decomposed Raw Headcount `var'"
	label var  `var'_miss  "Decomposed Missing values `var'"
	}

label variable pop_shr "Population Share"
gen cont_group_`k'=M0_`k'p/DAI_`k'p*pop_shr
label variable cont_group_`k' "Decomposed Contribution"

gen cont_subgroup_DAI_`k'=M0_`k'p/DAI_`k'p*pop_shr_after
label variable cont_subgroup_DAI_`k' "Population Subgroup Contribution to DAI"

gen cont_subgroup_EAI_`k'=EA_`k'p/EAI_`k'p*pop_shr_after
label variable cont_subgroup_EAI_`k' "Population Subgroup Contribution to EAI"

capture decode `r', gen(level)
drop `r'

gen sex=_n
label define sex_lab 1 "Male" 2 "Female"
label values sex sex_lab

save "WELI_results_`r'_2.0.dta", replace
// THE DATASETS INCLUDE THE DISEMPOWERMENT FIGURES FOR ALL CUTOFFS BETWEEN 1% AND 100%. WHEN EXTRACTING THE INFO WE FOCUS ON THE RELEVANT CUTOFF. //
// PLEASE SEE BELOW HOW TO EXTRACT RELEVANT INFORMATION FOR CUTOFF 20%. //

clear

*********************************************
*******   GENDER PARITY INDEX (GPI)   *******
*********************************************

use "depr_indicators_2.0.dta", clear

** Focus on male and female households

sort hhid sex
bys hhid: gen i=_n
bys hhid: egen n=max(i)

tab hh_type n, miss
drop if n==1

*****************************************************************************
********  Create a local variable with all CORE indicators varlist_emp ******
*****************************************************************************

#delimit;
local varlist_5do autonomy_inc selfeff never_violence feelinputdecagr feelinputdecagrl assetownership credit_accdec incomecontrol work_balance mobility groupmember group_inf respect;

gen sample5do=(autonomy_inc~=. & selfeff~=. & never_violence~=. & feelinputdecagr~=.  & feelinputdecagrl~=. & assetownership~=. & credit_accdec~=. & incomecontrol~=. & work_balance~=. & mobility~=. & groupmember~=. & group_inf~=. & respect~=.);
#delimit cr

******************************
**** Define the weights.  ****
******************************

foreach var in autonomy_inc selfeff never_violence feelinputdecagr feelinputdecagrl assetownership credit_accdec incomecontrol work_balance mobility groupmember group_inf respect {
	gen double w_`var'=1/13
	}
	
**********************************************************
*********     Define the weigted inadequacy g0*      ****
**********************************************************

foreach var in `varlist_5do'{
	gen double wg0_`var'= `var'*w_`var'
	}

********************************************************************************
*************   Define the (weighted) inadequacy count vector "ci" ************
********************************************************************************

egen double ci=rsum(wg0_*)
replace ci=round(ci, 0.0001)
replace ci = . if sample5do==0

label variable ci "Inadequacy Count without Parity"

********************************************
*** Compute censored inadequacy scores  ***
********************************************

bys hhid: gen double w_ci_id=ci if sex==2 
bys hhid: gen double m_ci_id=ci if sex==1 
bys hhid: egen double W_ci=max(w_ci_id)
bys hhid: egen double M_ci=max(m_ci_id)
drop w_ci_id m_ci_id

bys hhid: gen double W_cen_ci=W_ci
bys hhid:replace W_cen_ci=`k'/100 if W_cen_ci<=(`k'/100) & W_cen_ci!=.
bys hhid: gen double M_cen_ci=M_ci
bys hhid:replace M_cen_ci=`k'/100 if M_cen_ci<=(`k'/100) & M_cen_ci!=.

******************************************************
*** Identify inadequate in terms of gender parity  ***
******************************************************

bys hhid: gen ci_above=(W_cen_ci>M_cen_ci) 
bys hhid: replace ci_above=. if W_cen_ci==.|M_cen_ci==.
label var ci_above "Equals 1 if individual lives in MF hh where the depr score of the woman is higher than the man - EI 1"

************************************
*** Compute Gender Parity Index  ***
************************************

** Full sample
gen female=(sex==2 & ci_above!=.)
egen women_n=total(female)
egen women_wt=total(female*weight)
drop female

* Verification
gen women_i=(sex==2 & M_cen_ci!=. & W_cen_ci!=.)
egen women_wt2=total(women_i*weight)
tab women_wt women_wt2, miss
drop women_i women_wt2

** Headcount ratio of inadequate women
gen inadequate=(ci_above==1 & sex==2)
egen inadequate_n = total(inadequate)
gen H=inadequate_n/women_n // Considering unweighted sample //
egen inadequate_wt = total(inadequate*weight) 
gen H_wt=inadequate_wt/women_wt // Considering weighted sample //

*Verification
gen inadequate_i=(M_cen_ci<W_cen_ci & sex==2 & M_cen_ci!=. & W_cen_ci!=.)
egen inadequate_wt2=total(inadequate_i*weight)
tab inadequate_wt inadequate_wt2, miss
drop inadequate_i inadequate_wt2

** Computation of normalized gap
qui gen ci_gap=(W_cen_ci-M_cen_ci)/(1-M_cen_ci) if ci_above==1 & sex==2 
egen ci_gap_sum = total(ci_gap*weight)
gen ci_average=ci_gap_sum/inadequate_wt

** Computation of GPI
gen H_GPI=inadequate_wt/women_wt
gen P1=H_GPI*ci_average
gen GPI=1-P1

**************************
*** Summarize results  ***
**************************

sum H_GPI ci_average P1 GPI 
count if sex==2
tab women_n women_wt
capture drop _merge

save "WELI_results_GPI_2.0.dta", replace

*******************************************
*** Merge Individual GPI and Gender WELI datasets  ***
*******************************************

use "WELI_individual_indices_2.0.dta", clear
	capture drop _merge
	merge 1:1 hhid sex using "WELI_results_GPI_2.0.dta"
	drop _merge
	merge m:m sex using "WELI_results_gender_2.0.dta"
	drop _merge

*******************************************
*** Calculate 5DE and WELI score  ***
*******************************************

**** GENERATE KEY VARIABLES ****

	bys sex: gen WELI_3DE=EA_25p
	label var WELI_3DE "3DE"

	bys sex: gen disempowered=1-WELI_3DE
	label var disempowered "1-3DE"

	bys sex: gen achieve_emp=1-H_25p
	label var achieve_emp "1-H % indiv achieving empowerment"

	bys sex: gen noachieve_emp=H_25p
	label var noachieve_emp "H % indiv not achieving empowerment"

	bys sex:  gen mean3DE_unemp=1-A_25p
	label var mean3DE_unemp "1-A mean 3DE score for disempowered"

	bys sex:  gen mean_disemp=A_25p
	label var mean_disemp "A mean diesmp score 1-3DE for disempowered"

	bys sex: gen achieve_GP= 1-H_GPI
	label var achieve_GP "1-H_GPI % achieving gender parity"

	bys sex: gen noachieve_GP= H_GPI
	label var noachieve_GP "H_GPI % not achieving gender parity"

	bys sex:  gen GPI_gap=ci_gap
	label var GPI_gap "ci_gap gap btwn womens and mens weighted inadequacy scores"
	
	bys sex: gen I_GPI=ci_average
	label var I_GPI "ci_average Average empowerment gap"

	gen No_obs= sample_r_after 
	gen percent_dataused = sample_r_after/sample_r_before

	*** CALCULATE WELI SCORE ***
	
	gen WELI=(.9*WELI_3DE)+(.1*GPI) if sex==2
	
	*** CREATE DUAL-HEADED HOUSEHOLD VARIABLE *** 
	
	gen temp1=1 if sex==1 & n_missing==0 // We generate this variable for counting observations
	egen dahh = total(temp1)
	label var dahh "No. of dual adult headed households"

********************************************************************************
						*** SAVE WELI DATA *** 
********************************************************************************
	
saveold "${projectname}_weli_calculate", replace 
log close

clear


