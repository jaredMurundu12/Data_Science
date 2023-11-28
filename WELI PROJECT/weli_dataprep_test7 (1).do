/* PROJECT-LEVEL WOMEN'S EMPOWERMENT IN LIVESTOCK INDEX (WELI)
    DATA PREPARATION
	ADAPTED FROM PRO-WEIA (1 Draft pro-WEAI dataprep (May 2020).do)
	
	BY NILS TEUFEL, ILRI (MAY 2020)
	
******************************************
TO BE CHANGED FOR EACH IMPLEMENTATION:
	
	IN "OPEN FILE AND LOG"
		SET APPROPRIATE 
			WORKING DIRECTORY
			PROJECT NAME
			DATA FILE NAME
			
	IN "SAVE INDICATORS"
		CHANGE FILENAME TO: 
			"saveold "${projectname}_weli_dataprep", replace"
		
	
***************************************
MAIN ADAPTATIONS from proWEIA to WELI (NO NEED TO CHANGE FURTHER):
	IN "PREPARE DATA":
		EXCLUDE "drop if hhid==153" (seems to be some sort of left-over from data cleaning)
		
	IN "Indicator: Input in productive decisions"    (G2b)
		COPY COMPLETE SECTION OF THIS INDICATOR
		PASTE BELOW 
		RENAME HEADING TO "* Indicator: Input in production decisions - livestock"
		CHANGE INDEX LETTERS IN ALL LOOPS TO
			a d e g i j l p q r    (referencing the most important livestock-related activities)
		REPLACE DATA VARIABLE NAMES:g2_01_x -> g2_12_x; g2_02_x -> g2_14_x; 
									g2_03_x -> g2_15_x; g2_04_x -> g2_16_x
		ADD "l" (l for livestock) TO PROCESS VARIABLE NAMES (REPLACE ALL IN SELECTION):
			act_    -> actl_
			agr_    -> agrl_
			actagr  -> actagrl
			dec_    -> decl_
			decagr  -> decagrl
			_prod   -> _prodl
		EXCLUDE, WITH /*..*/, PARAGRAPH (not required for livestock)
			 foreach x in i j { .....
		UPDATE IN "egen someinput_prodl" LIST OF VARIABLES TO FIT INDICES
			"someinput_prodl_a someinput_prodl_d someinput_prodl_e someinput_prodl_g someinput_prodl_i someinput_prodl_j someinput_prodl_l someinput_prodl_p someinput_prodl_q someinput_prodl_r"
		CHANGE VARIABLE LABEL TO INCLUDE LIVESTOCK:					
			"lab var feelinputdecagrl "Input in productive decisions - livestock"
	
	IN "Indicator: Ownership of land and assets"   (G3a)
		ADD AS INDICES n o IN BOTH "for each" (two additional livestock categories (dairy cattle, pigs) included)
		
	IN "Indicator: Group membership"     (G5a)
		ADD AS INDEX j IN BOTH "for each"  (additional group "milk/dairy marketing group")
	
	IN "Indicator: Membership in influential group"     (G5b)
		ADD AS INDEX j I(additional group "milk/dairy marketing group")
	
	IN "Indicator: Respect among household members"   (G7)
		(OPTIONAL ACTIVITY ROWS c & d NOT COLLECTED IN WELI)
		DELETE INDICES/CATEGORIES "c d" in first "foreach", 1st row
		EXCLUDE LINE, WITH *, "replace `x'=`x'_c if `x'==."
		EXCLUDE LINE, WITH *, "replace `x'=`x'_d if `x'==."
   
   IN "Indicator: Attitudes about domestic violence" (G9) BUT CHECK WHICH ACTIVITIES WERE ACTUALLY INCLUDED (SHEVAX ONLY UP TO E)
		ADD INDICES f g (2 additional rows on potential conflict situations with livestock)
		ADD TO "gen never_violence=" VARIABLES " & violence_f==0 & violence_g==0"
		ADD TO "replace never_violence=." VARIABLES " | violence_f==. | violence_g==."
		
	IN "SAVE INDICATORS"
		ADD "feelinputdecagrl" to order variables
 ******************************************************
 
 
 */



******************************************************
/*  PROWEAI INTRODUCTORY TEXT*/

/* PROJECT-LEVEL WOMEN'S EMPOWERMENT IN AGRICULTURE INDEX (PRO-WEAI)
	DATA PREP DO-FILE
	UDPATED MAY 2020
	CONTACT: IFPRI-WEAI@CGIAR.ORG
	CREATED BY: ELENA MARTINEZ
	
	THIS DO-FILE CALCULATES THE 12 PRO-WEAI INDICATORS FROM CLEAN PRO-WEAI DATA.
	VARIABLES SHOULD BE NAMED ACCORDING TO THE FILE "VARIABLE NAMES AND CODEBOOK FOR PILOT PRO-WEAI SURVEY MODULES.XLSX"
	ALL VARIABLES FROM REQUIRED MODULES/QUESTIONS MUST BE PRESENT FOR THIS DO-FILE TO RUN.
	OPTIONAL QUESTIONS ARE IN PURPLE TEXT IN THE PRO-WEAI SURVEY MODULES.
	NOTE: THIS DO-FILE IS BASED ON A DRAFT VERSION OF THE PRO-WEAI AND IS SUBJECT TO CHANGE. 
	
	UPDATES:
	March 13, 2020 : Changed the ownership of land and other assets indicator adequacy definition
	April 24, 2020 : Updated input in productive decisions, ownership of land and other assets, access to and decisions on 
					 financial services, and control over use of income indicators
	May 18, 2020   : Updated code for autonomy in decision making indicator (uses only D stories)
					 Included comments on making categories c (mother/father-in law) and d (co-wife) optional
					 for respect among household members indicator
	*/

	
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
//capture log close
//log using "${projectname}_proweai_dataprep_log", text replace 

** Open file with WELI data
use "WELI_data_200717.dta", clear

********************************************************************************
						*** PREPARE DATA *** 
********************************************************************************

* Generate identification variables
	// Note: g1_01 and g1_02 should be numeric variables
	
	capture drop hhid mid sex hh_type
	
	ren g1_01 hhid // household ID number
	ren g1_02 mid // member ID number
	ren g1_03 sex // sex of respondent
	ren g1_04 hh_type // household type
	
* Check data structure

	* Household ID and member ID should uniquely identify the observations 
	* I.e., No two households should have the same hhid; no two members in a household should have the same mid.
	*drop if hhid==153
	isid hhid mid 
	
	* The dataset should contain only dual adult and female adult only households.
	* Female adult only households should not contain an adult male. 
	* There should be no missing observations for household type or sex.
	tab hh_type sex, mis
	
	* The dataset should contain 1-2 observations per household (1 female and 0-1 males).
	egen hhcount=count(sex), by(hhid)
	tab hhcount sex, mis

********************************************************************************
						*** GENERATE PRO-WEAI INDICATORS *** 
********************************************************************************

* Indicator: Input in productive decisions 

	foreach x in a b c d e f g h {
		gen partact_`x'=(g2_01_`x'==1) if g2_01_`x'!=.
		}
	egen partactagr=rowtotal(partact_a partact_b partact_c partact_d partact_e partact_f), missing
	label var partactagr "Number of agricultural activities in which individual participates"	
	foreach x in a b c d e f g h i j {
		gen self_`x'=((g2_02_id1_`x'==mid | (g2_02_id2_`x'==mid) | (g2_02_id3_`x'==mid)))
		}
	foreach x in a b c d e f g h {
		gen inputdec_`x'=(g2_03_`x'>1 & g2_03_`x'<98) & partact_`x'==1 //did not make decision but had some input
		replace inputdec_`x'=1 if self_`x'==1 & partact_`x'==1 //respondent made decision
		replace inputdec_`x'=. if g2_03_`x'==. & partact_`x'==0
		replace inputdec_`x'=. if g2_02_id1_`x'==. & g2_02_id2_`x'==. & g2_02_id3_`x'==. & g2_03_`x'==. & partact_`x'==.
		replace inputdec_`x'=. if g2_02_id1_`x'==. & g2_02_id2_`x'==. & g2_02_id3_`x'==. & g2_03_`x'==. & partact_`x'==1  // Set missing those who participate (partact_==1) no decision information  
		replace inputdec_`x'=0 if g2_02_id1_`x'==98 & g2_03_`x'==. & partact_`x'==1  // Set input in decision==0 for those who participate (partact_==1) and decision not applicable (98)
		}	
	foreach x in a b c d e f g h { // activities a-h
		gen feelmakedec_`x'=(g2_04_`x'>2) //feel like they could participate in decisionmaking to medium extent
		replace feelmakedec_`x'=1 if self_`x'==1  //respondent made decision
		replace feelmakedec_`x'=. if self_`x'!=1 & g2_04_`x'==.
		replace feelmakedec_`x'=. if g2_02_id1_`x'==. & g2_02_id2_`x'==. & g2_02_id3_`x'==. & g2_04_`x'==.
		replace feelmakedec_`x'=0 if g2_02_id1_`x'==98 & g2_04_`x'==. & partact_`x'==1  // Set feels like can make decision==0 for those who participate (partact_==1) and decision not applicable (98)
		}
	foreach x in i j { // activities i and j
		gen feelmakedec_`x'=(g2_04_`x'>2) //feel like they could participate in decisionmaking to medium extent
		replace feelmakedec_`x'=1 if self_`x'==1  //respondent made decision
		replace feelmakedec_`x'=. if self_`x'!=1 & g2_04_`x'==.
		replace feelmakedec_`x'=. if g2_02_id1_`x'==. & g2_02_id2_`x'==. & g2_02_id3_`x'==. & g2_04_`x'==.
		replace feelmakedec_`x'=0 if g2_02_id1_`x'==98 & g2_04_`x'==. // Set feels like can make decision==0 for those who participate (partact_==1) and decision not applicable (98)
		}
	foreach x in a b c d e f {
		gen someinput_prod_`x'=(inputdec_`x'==1 | feelmakedec_`x'==1)
		replace someinput_prod_`x'=. if (inputdec_`x'==. & feelmakedec_`x'==.) | (partact_`x'!=1)
		}

	egen someinput_prod=rowtotal(someinput_prod_a someinput_prod_b someinput_prod_c someinput_prod_d someinput_prod_e someinput_prod_f), missing
	gen someinput_prod_ratio=someinput_prod/partactagr // % of ag activities participated in that resp makes decisions about
	gen feelinputdecagr=(someinput_prod_ratio==1) if !missing(someinput_prod_ratio)
	replace feelinputdecagr=0 if partactagr==0
	lab var feelinputdecagr "Input in productive decisions"

* Indicator: Input in productive decisions - livestock
	foreach x in a d e g i j l p q r {
		gen partactl_`x'=(g2_12_`x'==1) if g2_12_`x'!=.
		}
	egen partactagrl=rowtotal(partactl_a partactl_d partactl_e partactl_g partactl_i partactl_j partactl_l partactl_p partactl_q partactl_r), missing
	label var partactagrl "Number of agricultural activities in which individual participates"	
	foreach x in a d e g i j l p q r {
		gen selfl_`x'=((g2_14_id1_`x'==mid | (g2_14_id2_`x'==mid) | (g2_14_id3_`x'==mid)))
		}
	foreach x in a d e g i j l p q r {
		gen inputdecl_`x'=(g2_15_`x'>1 & g2_15_`x'<98) & partactl_`x'==1 //did not make decision but had some input
		replace inputdecl_`x'=1 if selfl_`x'==1 & partactl_`x'==1 //respondent made decision
		replace inputdecl_`x'=. if g2_15_`x'==. & partactl_`x'==0
		replace inputdecl_`x'=. if g2_14_id1_`x'==. & g2_14_id2_`x'==. & g2_14_id3_`x'==. & g2_15_`x'==. & partactl_`x'==.
		replace inputdecl_`x'=. if g2_14_id1_`x'==. & g2_14_id2_`x'==. & g2_14_id3_`x'==. & g2_15_`x'==. & partactl_`x'==1  // Set missing those who participate (partactl_==1) no decision information  
		replace inputdecl_`x'=0 if g2_14_id1_`x'==98 & g2_15_`x'==. & partactl_`x'==1  // Set input in decision==0 for those who participate (partactl_==1) and decision not applicable (98)
		}	
	foreach x in a d e g i j l p q r { // activities a-h
		gen feelmakedecl_`x'=(g2_16_`x'>2) //feel like they could participate in decisionmaking to medium extent
		replace feelmakedecl_`x'=1 if selfl_`x'==1  //respondent made decision
		replace feelmakedecl_`x'=. if selfl_`x'!=1 & g2_16_`x'==.
		replace feelmakedecl_`x'=. if g2_14_id1_`x'==. & g2_14_id2_`x'==. & g2_14_id3_`x'==. & g2_16_`x'==.
		replace feelmakedecl_`x'=0 if g2_14_id1_`x'==98 & g2_16_`x'==. & partactl_`x'==1  // Set feels like can make decision==0 for those who participate (partactl_==1) and decision not applicable (98)
		}
	/* foreach x in i j { // activities i and j //not required for livestock indicator
		gen feelmakedecl_`x'=(g2_16_`x'>2) //feel like they could participate in decisionmaking to medium extent
		replace feelmakedecl_`x'=1 if selfl_`x'==1  //respondent made decision
		replace feelmakedecl_`x'=. if selfl_`x'!=1 & g2_16_`x'==.
		replace feelmakedecl_`x'=. if g2_14_id1_`x'==. & g2_14_id2_`x'==. & g2_14_id3_`x'==. & g2_16_`x'==.
		replace feelmakedecl_`x'=0 if g2_14_id1_`x'==98 & g2_16_`x'==. // Set feels like can make decision==0 for those who participate (partactl_==1) and decision not applicable (98)
		} */    
	foreach x in a d e g i j l p q r {
		gen someinput_prodl_`x'=(inputdecl_`x'==1 | feelmakedecl_`x'==1)
		replace someinput_prodl_`x'=. if (inputdecl_`x'==. & feelmakedecl_`x'==.) | (partactl_`x'!=1)
		}

	egen someinput_prodl=rowtotal(someinput_prodl_a someinput_prodl_d someinput_prodl_e someinput_prodl_g someinput_prodl_i someinput_prodl_j someinput_prodl_l someinput_prodl_p someinput_prodl_q someinput_prodl_r), missing
	gen someinput_prodl_ratio=someinput_prodl/partactagrl // % of ag activities participated in that resp makes decisions about
	gen feelinputdecagrl=(someinput_prodl_ratio==1) if !missing(someinput_prodl_ratio)
	replace feelinputdecagrl=0 if partactagrl==0
	lab var feelinputdecagrl "Input in productive decisions - livestock"
	
* Indicator: Ownership of land and other assets

	gen ownland=(g3_05<4 & g3_05>=1) if !missing(g3_05)
	replace ownland=0 if g3_01==2 // replace individual ownership==0 if HH does not own item
	foreach x in a b c d e f g h i j k l m n o{
		gen own_`x'=(g3_06_`x'==1) if !missing(g3_06_`x')
		}				
	egen own_sum=rowtotal(own_a-own_m ownland), missing
	foreach x in a b c d e f g h i j k l m n o{
		gen selfjointown_`x'=(g3_07_`x'<4) if !missing(g3_07_`x')
		replace selfjointown_`x'=0 if own_`x'==0 // replace individual ownership==0 if HH does not own item
		}

	* Asset aggregation
	unab selfjointown: selfjointown_*
	egen assetcount=rowtotal(`selfjointown'), missing
	
	* Aggregated asset ownership indicator
	gen assetownership=(assetcount>=3 | ownland==1) // land or any three assets
	replace assetownership=. if (assetcount==. & ownland==.)
	replace assetownership=. if own_sum==.
	replace assetownership=0 if own_sum==0 | (assetcount<=2 & ownland==0)
	lab var assetownership "Ownership of land and other assets"

* Indicator: Access to and decisions on credit

	foreach x in a b c d e f {
		gen creditaccess_`x'=(g3_08_`x'==1 | g3_08_`x'==3) if !missing(g3_08_`x') /*yes/maybe has access to credit*/
		}
	egen creditaccess=rowtotal(creditaccess_*), missing
	capture drop credituse_*
	foreach x in a b c d e f {
		gen credituse_`x'=(g3_09_`x'==1 | g3_09_`x'==2 | g3_09_`x'==3) if !missing(g3_09_`x') /*used credit*/
		replace credituse_`x'=0 if creditaccess_`x'==0 // replace credit use ==0 if HH could not access credit
		}
	egen credituse=rowtotal(credituse_*), missing
	foreach x in a b c d e f {
		gen creditdec_`x'=(g3_10_id1_`x'==mid | g3_10_id2_`x'==mid | g3_10_id3_`x'==mid | g3_11_id1_`x'==mid | g3_11_id2_`x'==mid | g3_11_id3_`x'==mid)
		replace creditdec_`x'=. if (g3_10_id1_`x'==. & g3_10_id2_`x'==. & g3_10_id3_`x'==. & g3_11_id1_`x'==. & g3_11_id2_`x'==. & g3_11_id3_`x'==.) /*solely/jointly made a decision on credit used*/
		}
	foreach x in a c b d e f {
		gen creditusedec_`x'=(credituse_`x'==1 & creditdec_`x'==1) 
		replace creditusedec_`x'=. if (credituse_`x'==. & creditdec_`x'==.)
		replace creditusedec_`x'=0 if creditaccess_`x'==0
		}
	egen creditsum=rowtotal(creditusedec_a-creditusedec_f), missing
	gen credit_accdec=(creditsum>0 | g3_13==1 | (credituse==0 & creditaccess>0)) // adequate if made decisions about credit OR has access to a financial account OR did not use credit but yes/maybe had access
		replace credit_accdec=. if creditsum==. | creditaccess==. | credituse==. /*missing if no info about credit access or use*/
		replace credit_accdec=0 if (creditsum==0 | creditaccess==0 | credituse==0) & g3_13==2 /*inadequate if did no use credit, did not have access to credit, or did not make decisions on credit, and did not have access to a financial account*/
	lab var credit_accdec "Access to and decisions on credit" 

* Indicator: Control over use of income

	foreach x in a b c d e f {
		gen someinput_output_`x'=(g2_06_`x'==2 | g2_06_`x'==3 | g2_06_`x'==98) if !missing(g2_06_`x') & g2_01_`x'==1 /*input about outputs*/
		replace someinput_output_`x'=0 if g2_02_id1_`x'==98 & g2_06_`x'==. & partact_`x'==1 // replace input in outputs==0 if no decision made and missing information in output decision and respondent participated in activity
		}
	foreach x in a b c d e f g h {
		gen someinput_income_`x'=(g2_07_`x'==2 | g2_07_`x'==3 | g2_07_`x'==98) if !missing(g2_07_`x') & g2_01_`x'==1 /*input about outputs*/
		replace someinput_income_`x'=0 if g2_02_id1_`x'==98 & g2_07_`x'==. & partact_`x'==1 // replace input in income==0 if no decision made and missing information in income decision and respondent participated in activity
}
	foreach x in a b c d e f {
		gen someinput_both_`x'=(someinput_output_`x'==1 & someinput_income_`x'==1) 
		replace someinput_both_`x'=. if someinput_output_`x'==. & someinput_income_`x'==.
		}
	egen someinput_output=rowtotal(someinput_output_*), missing
	egen someinput_income_ag=rowtotal(someinput_income_a-someinput_income_f), missing
	egen someinput_income_nonag=rowtotal(someinput_income_g-someinput_income_h), missing
	egen someinput_income_all=rowtotal(someinput_income_a-someinput_income_h), missing
	egen someinput_both=rowtotal(someinput_both_*), missing
		
	egen someinput_sum=rowtotal(someinput_both_a someinput_both_b someinput_both_c someinput_both_d ///
								someinput_both_e someinput_both_f someinput_income_g someinput_income_h), missing
	egen partactagnonag=rowtotal(partact_a partact_b partact_c partact_d partact_e partact_f partact_g partact_h), missing
	replace someinput_sum=0 if partactagnonag==0
	gen someinput_ratio=someinput_sum/partactagnonag
	replace someinput_ratio=0 if partactagnonag==0

	gen incomecontrol=(someinput_ratio==1) if !missing(someinput_ratio)
	replace incomecontrol=0 if partactagnonag==0	
	lab var incomecontrol "Control over use of income"
		
* Indicator: Autonomy in income (code updated May 2020)

	gen autonomy_inc=(g8_01_d4==1 & !(g8_01_d2==1 & g8_01_d3==1))
	replace autonomy_inc=. if g8_01_d2==. | g8_01_d3==. | g8_01_d4==.
	lab var autonomy_inc "Autonomy in income"

* Indicator: Group membership

	foreach x in a b c d e f g h i j {
		gen group_`x'=(g5_01_`x'==1) if !missing(g5_01_`x')
		}
	foreach x in a b c d e f g h i j {	
		gen groupmember_`x'=(g5_03_`x'==1) if !missing(g5_03_`x') // Respondent is active group member (if group exists)
		replace groupmember_`x'=0 if group_`x'==0
		}
	egen groupsum=rowtotal(group_*), missing
	egen groupmembersum=rowtotal(groupmember_*), missing
	gen groupmember=(groupmembersum>0) if !missing(groupmembersum)
	replace groupmember=. if groupsum==.
	replace groupmember=0 if groupsum==0
	lab var groupmember "Group membership"

* Indicator: Membership in influential groups

	foreach x in a b c d e f g h i j {
		gen group_inf_`x'=(g5_05_`x'==3 | g5_05_`x'==4) if !missing(g5_05_`x') // adequate if group has medium or higher influence in community
		replace group_inf_`x'=0 if groupmember_`x'==0 | group_`x'==0 // inadequate if no group or not an active member
		}
	egen group_inf_sum=rowtotal(group_inf_*), missing
	gen group_inf=(group_inf_sum>0) if !missing(group_inf_sum)
	lab var group_inf "Membership in influential groups"

* Indicator: Work balance
	/* IMPORTANT: Many projects format their time use data differently from the pro-WEAI survey modules (also outlined in the variable naming and coding guide).
		If your time use data is formatted differently, calculate the time in minutes spent on each activity (minutes_A-minutes_X)
		and the time spent on childcare as a secondary activity (time_childcare).
		Then, skip the following three sections and resume with "* Work balance indicator."		*/

	/* Activity codes for labels
	gl time_a "sleeping/resting"
	gl time_b "eating/drinking"
	gl time_c "personal care"
	gl time_d "school/homework"
	gl time_e "work, employed"
	gl time_f "own business work"
	gl time_g "staple grain farming"
	gl time_h "gardens/high value crop farming"
	gl time_i "large livestock raising"
	gl time_j "small livestock raising"
	gl time_k "poultry/small animal raising"
	gl time_l "fishpond culture"
	gl time_m "commuting"
	gl time_n "shopping/getting services"
	gl time_o "weaving/sewing/textiles"
	gl time_p "cooking"
	gl time_q "domestic work"
	gl time_r "caring for children"
	gl time_s "caring for adults"
	gl time_t "traveling"
	gl time_u "exercising"
	gl time_v "social activities/hobbies"
	gl time_w "religious activities"
	gl time_x "other activities"

	* Time spent on activities; time spent on work
		// Note: g4_01_* should be string variables listing one activity code for each 15 minute time slot.
		// g4_01_4_a is 4:00-4:15, g4_01_4_b is 4:15-4:30, etc.
	foreach x in A B C D E F G H I J K L M N O P Q R S T U V W X {
		capture drop *`x'
		}
	foreach x in A B C D E F G H I J K L M N O P Q R S T U V W X {
		foreach y in a b c d {
			forvalues z=1/24 {
				gen time_`z'_`y'_`x'=(g4_01_`z'_`y'=="`x'") if !missing(g4_01_`z'_`y')
				label var time_`z'_`y'_`x' "Activity ${time_`x'} during hour `z'`y'"
				label val time_`z'_`y'_`x' yesno
		}
		}
		}
	foreach x in A B C D E F G H I J K L M N O P Q R S T U V W X {
		egen timeslot_`x'=rowtotal(*`x'), missing
		gen minutes_`x'=15*timeslot_`x'
		label var minutes_`x' "Minutes spent on activity ${time_`x'}"
		}
		
	egen time_activity=rowtotal(minutes_*), missing
	label var time_activity "Minutes spent on any activity"			

	* Time spent on childcare as a secondary activity
	egen timeslot_childcare=rowtotal(g4_02_*), missing
	gen time_childcare=15*timeslot_childcare
	label var time_childcare "Minutes spent caring for a child (secondary activity)"	*/

	* Work balance indicator
	egen time_work=rowtotal(minutes_E minutes_F minutes_G minutes_H ///
		minutes_I minutes_J minutes_K minutes_L minutes_M minutes_N ///
		minutes_O minutes_P minutes_Q minutes_R minutes_S), missing
	label var time_work "Minutes spent on work"
	
	gen workload=(1/60)*(time_work+(0.5)*time_childcare)
	gen work_balance=(workload<10.5) if !missing(workload)
	lab var work_balance "Work balance"

	sort work_balance
	
* Indicator: Visiting important locations (G6)

	gen visit_city=g6_01 	if !missing(g6_01)
	gen visit_market=g6_02 	if !missing(g6_02)
	gen visit_family=g6_03 	if !missing(g6_03)
	gen visit_friend=g6_04 	if !missing(g6_04)
	gen visit_health=g6_05 	if !missing(g6_05)
	gen visit_public=g6_06 	if !missing(g6_06)
	foreach x in city market family health public {
		gen visitweekly_`x'=visit_`x'
		recode visitweekly_`x' (2=1) (3/6=0) (7=.)
		gen visitmonthly_`x'=visit_`x'
		recode visitmonthly_`x' (2/4=1) (5/6=0) (7=.)
		}
	capture drop visitweekly
	egen visitweekly=rowtotal(visitweekly_city visitweekly_market visitweekly_family)
	egen visitmonthly=rowtotal(visitmonthly_health visitmonthly_public)
	gen mobility=(visitweekly>1 | visitmonthly>0) 
		replace mobility=. if visitweekly_city==. & visitweekly_market==. & visitweekly_family==. ///
			& visitmonthly_health==. & visitmonthly_public==.
	lab var mobility "Visiting important locations"

* Indictor: Respect among household members (G7)

	foreach x in a b { // categories c and d are optional and can be deleted from this code
		gen respect_relation_`x'=g7_02_`x' if !missing(g7_02_`x')
		gen respect_resp_`x'=g7_03_`x' if !missing(g7_03_`x')
		gen trust_`x'=g7_04_`x' if !missing(g7_04_`x')
		gen disagree_`x'=g7_05_`x' if !missing(g7_05_`x')
		}
	foreach x in respect_relation respect_resp trust disagree {
		gen `x'=`x'_a
		replace `x'=`x'_b if `x'==.
*		replace `x'=`x'_c if `x'==.	// category c is optional and can be deleted if not asked 
*		replace `x'=`x'_d if `x'==.	// category d is optional and can be deleted if not asked 
		}
	gen respect=(respect_relation==1 & respect_resp==1 & trust==1 & disagree==1)
	replace respect=. if respect_relation==. | respect_resp==. | trust==. | disagree==.
	lab var respect "Respect among household members"

* Indicator: Attitudes about domestic violence (G9)

	foreach x in a b c d e {
		gen violence_`x'=(g9_01_`x'==1) if !missing(g9_01_`x')
		}
	gen never_violence=(violence_a==0 & violence_b==0 & violence_c==0 & violence_d==0 & violence_e==0)
	replace never_violence=. if violence_a==. | violence_b==. | violence_c==. | violence_d==. | violence_e==. 
	lab var never_violence "Attitudes about domestic violence"

* Indicator: Self-efficacy (G8)

	egen ngse=rowtotal(g8_04_a g8_04_b g8_04_c g8_04_d g8_04_e g8_04_f g8_04_g g8_04_h), missing
		replace ngse=. if g8_04_a==. | g8_04_b==. | g8_04_c==. | g8_04_d==. | g8_04_e==. | g8_04_f==. | g8_04_g==. | g8_04_h==. 
	gen selfeff=(ngse>=32) if !missing(ngse)
	lab var selfeff "Self-efficacy"

********************************************************************************
						*** SAVE INDICATORS *** 
********************************************************************************

order hhid mid sex hh_type ///
	autonomy_inc selfeff never_violence respect feelinputdecagr feelinputdecagrl assetownership credit_accdec ///
	incomecontrol work_balance mobility groupmember group_inf 
	
saveold "${projectname}_weli_dataprep", replace 
//log close

clear

