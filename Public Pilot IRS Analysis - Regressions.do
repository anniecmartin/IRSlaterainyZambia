*-------------------- Pilot IRS --------------------*
*Public Pilot IRS Analysis - Regressions
	*-------------------------------------------*
	*		Start Date:		October 31 2024
	*		Author: 		A Martin		
	*		Last Edit Date:	October 31 2024
	*		Last Author:	A Martin		
	*-------------------------------------------*


	*---------- Do-file Summary ----------*
	*This do-file runs the models for the manuscript entitled:
	*Impact of late rainy season indoor residual spraying on holoendemic malaria transmission: a cohort study in northern Zambia
	*It also generates all of the tables, with the exception of Table 1. It also generates Figure 4.
	*-------------------------------------*
	
	*---------- Log  ----------*
	cd "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets"
	log using Pilot_IRS_Results_Stata_publicmodels, replace
	*---Add program for model tables---*
	capt prog drop appendmodels
*! version 1.0.0  14aug2007  Ben Jann
*http://repec.org/bocode/e/estout/advanced901.do
program appendmodels, eclass
    /// using first equation of model
    version 8
    syntax namelist
    tempname b V tmp
    foreach name of local namelist {
        qui est restore `name'
        mat `tmp' = e(b)
        local eq1: coleq `tmp'
        gettoken eq1 : eq1
        mat `tmp' = `tmp'[1,"`eq1':"]
        local cons = colnumb(`tmp',"_cons")
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1,1..`cons'-1]
        }
        mat `b' = nullmat(`b') , `tmp'
        mat `tmp' = e(V)
        mat `tmp' = `tmp'["`eq1':","`eq1':"]
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1..`cons'-1,1..`cons'-1]
        }
        capt confirm matrix `V'
        if _rc {
            mat `V' = `tmp'
        }
        else {
            mat `V' = ///
            ( `V' , J(rowsof(`V'),colsof(`tmp'),0) ) \ ///
            ( J(rowsof(`tmp'),colsof(`V'),0) , `tmp' )
        }
    }
    local names: colfullnames `b'
    mat coln `V' = `names'
    mat rown `V' = `names'
    eret post `b' `V'
    eret local cmd "whatever"
end
	
	*--------------Add and reformat the data for stata outputs: epi dataset-------------*
	clear
	insheet using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/primary.csv"
	label var bzone "Zone"
		replace bzone="1" if bzone=="Inland, Not Sprayed"
		replace bzone="2" if bzone=="Inland, Sprayed"
		replace bzone="3" if bzone=="Lakeside, Not Sprayed"
		replace bzone="4" if bzone=="Lakeside, Sprayed"
		label define bzone 1 "   Inland, Not Sprayed" 2"   Inland, Sprayed" 3 "   Lakeside, Not Sprayed" 4 "   Lakeside, Sprayed"
		destring bzone, replace force
		label values bzone bzone
	destring lasexmicrltr qpcrcall rdtresults rdtresultslag1 rdtresultslag2 enter bzone tvpost barm studytime bageyears bsex bhheduclevel tvnetsleepln tvmvdidsleepaway  tvsleepnightmovefactor tvoutnight tvmcolanophf, replace force
	destring bmalcauseknow tvsleepawaketimefactor bhheave bhhwall bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5, replace force
	destring microresults microresultslag1 microresultslag2 microresultslag3 icasemicro icaserdt, replace force
	destring bmalsympknow bmalcauseknow bageyears tvnetoverbed tvoutnight bageyears bhhmemcount hhappliance4 hhappliance5 tvnetsleepln tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6, replace force
	destring tvnettimeown tvsleepbedtimefactor tvhhnetcov tvhhmalariatesttrt4wk tvsleepnightmovefactor tvsleepawaketimefactor bhhwatersrc bhheave bhhrooftype bhhwall, replace force	
	destring startobs endobs, replace force
	destring isparticipating isparticipatinglag1 isparticipatinglag2 isparticipatinglag3 isparticipatinglag4 isparticipatinglag5 isparticipatinglag6 , replace force
	destring brdtresults rdtresults rdtresultslag1 rdtresultslag2 enter bzone tvpost barm studytime bageyears bsex bhheduclevel tvnetsleepln tvmvdidsleepaway  tvsleepnightmovefactor tvoutnight, replace force
	destring bmalcauseknow tvsleepawaketimefactor bhheave bhhwall bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5, replace force
	destring bmicroresults microresults microresultslag1 microresultslag2 microresultslag3 icasemicro icaserdt, replace force
	destring startobs startobs2 endobs bagecat, replace force
	destring birscat birsoctyn, replace force
	label var rdtresults "RDT positive"
	label var microresults "Microscopy positive "
	label var qpcrcall "qPCR positive "
	label var rdtresultslag1 "Positive RDT last visit"
	label var microresultslag1 "Positive microscopy last visit"
	label var tvpost "Post-IRS Period"
		label define post_ 0 "   Pre-IRS" 1 "   Post IRS"
		label values tvpost post_
	label var barm "Arm"
		label define arm_ 0 "   No Spray" 1 "   Spray"
		label values barm arm_
	label var studytime "Study Month"
		label define studytime_ 0 "   March" 1 "   April" 2 "   May" 3 "   June" 4 "   July" 5 "   August" 6 "   September"
		label values studytime studytime_
	label var bmalsympknow "Knows ≥ 1 symptom of malaria"
	label var bmalcauseknow "Knows moz. bites cause malaria"
	label var bsex "Biological Sex"
	label var bageyears "Age (years)"
	label var tvnetoverbed "Net hanging over bed"
	label var tvoutnight "Spends time outside after dusk"
	label var bhhmemcount "Number of people sleeping in house"
	label var hhappliance4 "Household owns phone"
	label var hhappliance5 "Household owns solar panels"
	label var tvnetsleepln "Slept under bednet last night"
	label var tvhhmalariatesttrt4wk "Visited for malaria test and treat in last month"
	label var tvmcolanophf "Number anopheline caught in house last night"
	label var bdistc "Distance from health center (km)"
	label var bdistr "Distance from road (km)"
	label var bdists1 "Distance from stream, category 1 (km)"
	label var bdists2 "Distance from stream, category 2 (km)"
	label var bdists3 "Distance from stream, category 3 (km)"
	label var bdists4 "Distance from stream, category 4 (km)"
	label var bdists5 "Distance from stream, category 5 (km)"
	label var bdists6 "Distance from stream, category 6 (km)"
	label var tvnettimeown "Time owning net"
		label define net_time_own_ 1 "   < 1 month" 2 "   1 month - 1 year" 3 "   2-5 years" 4 "   >5 years" 5 "   Do not know" 
		label values tvnettimeown net_time_own_
	label var tvsleepbedtimefactor "Time going to bed"
		label define sleep_bedtime_ 1 "   19:00 - 20:00" 2 "   20:00 -21:00" 3 "   21:00 -22:00" 4 "   22:00 - 23:00" 5 "   After 23:00" 6 "   Do not know" 7 "   Other" 
		label values tvsleepbedtimefactor sleep_bedtime_
	label var tvsleepnightmovefactor "Number times up at night"
	label var tvsleepawaketimefactor "Time waking up"
		label define sleep_awaketime_ 1 "   04:00 - 05:00" 2 "   05:00 -06:00" 3 "   06:00 -07:00" 4 "   07:00 -08:00" 5 "   After 09:00" 6 "   Do not know" 7 "   Other" 
		label values tvsleepawaketimefactor sleep_awaketime_
	label var bhhwatersrc "Water source"
		label define hh_water_src_ 1 "   Piped water" 2 "   Bore hole (or bush pump)" 4 "   Open well" 5 "   Surface water" 6 "   Stream/pond" 
		label values bhhwatersrc hh_water_src_
	label var bhheave "Open eaves"
	label var bhhrooftype "Roof type"
			label define hh_rooftype_ 1 "   Thatch" 2 "   Metal" 3 "   Asbestos" 4 "   Iron" 5 "   Tent or canvas" 8 "   Other" 
		label values bhhrooftype hh_rooftype_
	label var bhhwall "Wall type"
		label define hh_wall_ 0 "   Concrete/cement" 1 "   Fired brick" 2 "   Mud" 3 "   Wood" 4 "   Grass" 6 "   Tent or canvas" 5 "   Other" 
		label values bhhwall hh_wall_
	label var birscat "IRS Receipt"
		label define birscat_ 0 "   No IRS" 1 "   Oct. Only" 2 "   April Only" 3 "   Both Oct. and April"
		label values birscat birscat_
	label var birsoctyn "Received IRS in October"
		destring qpcrcalllag1, replace force
		save "primary_formatted_stata.dta", replace

		*--------------Add and reformat the data for stata outputs: ento dataset-------------*

	clear
	insheet using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/entomology.csv"
	destring tvpost tvpropu5 tvpropunet, replace force
	destring bhhmemcount belevation hhappliance4 hhappliance5 tvhhmalariatesttrt4wk tvnumsleep tvpropu5 tvpropunet bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6, replace force
	destring bhhwatersrc bhheave bhhrooftype bhhwall, replace force
	replace bzone="1" if bzone=="Inland, Not Sprayed"
	replace bzone="2" if bzone=="Inland, Sprayed"
    replace bzone="3" if bzone=="Lakeside, Not Sprayed"
    replace bzone="4" if bzone=="Lakeside, Sprayed"
		label define bzone 1 "   Inland, Not Sprayed" 2"   Inland, Sprayed" 3 "   Lakeside, Not Sprayed" 4 "   Lakeside, Sprayed"
	destring bzone, replace force
	label values bzone bzone
	label var tvpost "Post-IRS Period"
		label define post_ 0 "   Pre-IRS" 1 "   Post IRS"
		label values tvpost post_
	label var barm "Arm"
		label define arm_ 0 "   No Spray" 1 "   Spray"
		label values barm arm_
	label var studytime "Study Month"
		label define studytime_ 0 "   March" 1 "   April" 2 "   May" 3 "   June" 4 "   July" 5 "   August" 6 "   September"
		label values studytime studytime_
	label var bdistc "Distance from health center (km)"
	label var bdistr "Distance from road (km)"
	label var bdists1 "Distance from stream, category 1 (km)"
	label var bdists2 "Distance from stream, category 2 (km)"
	label var bdists3 "Distance from stream, category 3 (km)"
	label var bdists4 "Distance from stream, category 4 (km)"
	label var bdists5 "Distance from stream, category 5 (km)"
	label var bdists6 "Distance from stream, category 6 (km)"
	label var bhheave "Open eaves"
	label var bhhrooftype "Roof type"
			label define hh_rooftype_ 1 "   Thatch" 2 "   Metal" 3 "   Asbestos" 4 "   Iron" 5 "   Tent or canvas" 8 "   Other" 
		label values bhhrooftype hh_rooftype_
	label var bhhwall "Wall type"
		label define hh_wall_ 0 "   Concrete/cement" 1 "   Fired brick" 2 "   Mud" 3 "   Wood" 4 "   Grass" 6 "   Tent or canvas" 5 "   Other" 
		label values bhhwall hh_wall_
	label var belevation "Elevation (meters)"
	label var tvpropu5 "Proportion of household under 5"
	save "entomology_formatted_stata.dta", replace
	
	*---------------------------------------------------------------------------------------*

	*-----------------------------------------------------Run the models-----------------------------------------------------*
	*------------------------ RDT ------------------------*
		use "primary_formatted_stata.dta", clear
		*Univariate
		meglm rdtresults tvpost##barm||hhnumber:||partid:, family(binomial) eform
		estimates store main 
		
		foreach v of varlist(rdtresultslag1 bmalsympknow bmalcauseknow bsex bageyears tvnetoverbed tvoutnight bageyears bhhmemcount hhappliance4 hhappliance5 bhheave tvnetsleepln tvsleepnightmovefactor tvhhmalariatesttrt4wk tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6) {
		 meglm rdtresults `v'||hhnumber:||partid:, family(binomial)
		estimates store `v' 
	}

		foreach v of varlist(tvnettimeown tvsleepbedtimefactor tvsleepawaketimefactor bhhwatersrc bhhrooftype bhhwall) {
		meglm rdtresults i.`v' ///
			||hhnumber:||partid:, family(binomial)
		estimates store `v'
	}
		eststo rdtunivar: appendmodels main bsex bageyears bmalsympknow bmalcauseknow rdtresultslag1 tvnetoverbed tvnetsleepln tvoutnight tvsleepbedtimefactor tvsleepnightmovefactor tvsleepawaketimefactor tvhhmalariatesttrt4wk bhhmemcount hhappliance4 hhappliance5 bhhwatersrc tvnettimeown bhheave bhhrooftype bhhwall tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6 

		* Multivariate
		meglm rdtresults rdtresultslag1 tvpost##barm ///
			i.bzone i.studytime ///
			bsex bageyears bmalcauseknow bmalsympknow ///
			tvhhmalariatesttrt4wk ///
			bhheave bhhrooftype bdistc bdists2 ///
			||hhnumber:||partid:, family(binomial)	
		estimates store rdtmulti, title(Multivariate Logistic Regression, RDT)
		

		* Table 2a multi and univariate for RDT
		* Resource for esttab: https://www.statalist.org/forums/forum/general-stata-discussion/general/1559272-regression-tables-with-esttab-display-confidence-interval-p-value		
		esttab rdtmulti rdtunivar using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table2a_RDT.rtf", replace ///
		 cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
			lines label nonumbers mtitles("Multivar" "Univar") modelwidth(8) eqlabel(none) ///
			refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
			collabels("Odds Ratio" "95% CI" "p-value")
	
	*------------------------ Microscopy ------------------------*
		*Univariate
	meglm microresults tvpost##barm||hhnumber:||partid:, family(poisson) eform
	estimates store main 
	
	foreach v of varlist(microresultslag1 bmalsympknow bmalcauseknow bsex bageyears tvnetoverbed tvoutnight tvsleepnightmovefactor bhhmemcount bhheave hhappliance4 hhappliance5 tvnetsleepln tvhhmalariatesttrt4wk tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6) {
     meglm microresults `v'||hhnumber:||partid:, family(poisson)
	estimates store `v' 
}

	foreach v of varlist(tvnettimeown tvsleepbedtimefactor tvsleepawaketimefactor bhhwatersrc bhhrooftype bhhwall) {
	meglm microresults i.`v' ///
		||hhnumber:||partid:, family(poisson)
	estimates store `v'
}
	eststo microunivar: appendmodels main bsex bageyears bmalsympknow bmalcauseknow microresultslag1 tvnetoverbed tvnetsleepln tvoutnight tvsleepbedtimefactor tvsleepnightmovefactor tvsleepawaketimefactor tvhhmalariatesttrt4wk bhhmemcount hhappliance4 hhappliance5 bhhwatersrc tvnettimeown bhheave bhhrooftype bhhwall tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6 

	
	* Multivariate
	** Drop any observation that is positive but is not an "incident" case. If they were malaria microscopy positive last visit but rdt negative
	bysort microresults: tab microresultslag1 rdtresultslag1
	preserve
	drop if microresults==1 & microresultslag1==1 & rdtresultslag1==0
	
	meglm microresults microresultslag1 tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears bmalcauseknow bmalsympknow ///
				bhhrooftype bdistc bdists2  ///
				||hhnumber:||partid:, family(poisson)
	estimates store micromulti, title(Multivariate Poisson Regression, Microscopy)
	restore
	
	* Table 2b multi and univariate for Microscopy
	esttab micromulti microunivar using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table2b_Micro.rtf",  ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("Multivar" "Univar") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("IRR" "95% CI" "p-value")

		
	
	*------------------------ qPCR ------------------------*
	*Univariate
	 meglm qpcrcall tvpost##barm||hhnumber:||partid:, family(poisson) eform
	estimates store main 
	
	foreach v of varlist(bmalsympknow bmalcauseknow bsex bageyears tvnetoverbed tvsleepnightmovefactor tvoutnight bageyears bhhmemcount hhappliance4 hhappliance5 bhheave tvnetsleepln tvhhmalariatesttrt4wk tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6) {
     meglm qpcrcall `v'||hhnumber:||partid:, family(poisson)
	estimates store `v' 
}

	foreach v of varlist(tvnettimeown tvsleepbedtimefactor  tvsleepawaketimefactor bhhwatersrc bhhrooftype bhhwall) {
	meglm qpcrcall i.`v' ///
		||hhnumber:||partid:, family(poisson)
	estimates store `v'
}
	eststo qpcrunivar: appendmodels main bsex bageyears bmalsympknow bmalcauseknow tvnetoverbed tvnetsleepln tvoutnight tvsleepbedtimefactor tvsleepnightmovefactor tvsleepawaketimefactor tvhhmalariatesttrt4wk bhhmemcount hhappliance4 hhappliance5 bhhwatersrc tvnettimeown bhheave bhhrooftype bhhwall tvmcolanophf bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6 

	
	* Multivariate
	** Drop any observation that is positive but is not an "incident" case. If they were malaria qpcr positive last visit but rdt negative
	bysort qpcrcall: tab qpcrcalllag1 rdtresultslag1
	preserve
	drop if qpcrcall==1 & qpcrcalllag1==1 & rdtresultslag1==0
	
	meglm qpcrcall tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears ///
				bhheave bhhrooftype bdistc bdists2 ///
				||hhnumber:||partid:, family(poisson) 
	estimates store qpcrmulti, title(Multivariate Poisson Regression, qPCR)
	restore
	
	* Table 2c multi and univariate for qpcr
	esttab qpcrmulti qpcrunivar using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table2c_qpcr.rtf",  ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("Multivar" "Univar") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("IRR" "95% CI" "p-value")
	
	*------------------------ Exploratory: Time to infection (hazard) ------------------------*
	clear
	use "primary_formatted_stata.dta"
	keep if isparticipating==1
	
	* Multiple failure data set up
		stset endobs, id(partid) failure(microresults==1)  time0(startobs2) exit(studyend)

		** STRATIFY analysis: Run three models - all, then two stratefied with those negative at baseline and positive at baseline
		stcox tvpost##barm i.bzone bageyears bmicroresults bsex bmalsympknow bmalcauseknow bhhrooftype bdistc bdists2 if bmicroresults!=., vce(cluster partid)
		estimates store m0, title(Cox Regression, All)

		stcox tvpost##barm i.bzone bageyears bsex bmalsympknow bmalcauseknow bhhrooftype bdistc bdists2 if bmicroresults==1, vce(cluster partid)
		estimates store m1, title(Cox Regression, Positive at Baseline)
		
		stcox tvpost##barm i.bzone bageyears bsex bmalsympknow bmalcauseknow bhhrooftype bdistc bdists2 if bmicroresults==0, vce(cluster partid)
		estimates store m2, title(Cox Regression, Negative at Baseline)

		
	*Print Table 3a
	esttab m0 m1 using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table3a_timeto.rtf",  ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("All" "Baseline Positive" "Baseline Negative") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("HR" "95% CI" "p-value") ///
		addnotes("HR: Hazard Ratio")
	
	* Figure 4: Nelson Aalen
	sts graph, cumhaz by(bmicroresults arm) xtitle("{bf:Follow-up time, days}", size(3)) ///
		ytitle("{bf:Expected infections}", size(3)) ///
		yscale(titlegap(3)) ///
		ylab(,nogrid) ///
		title("{bf:Nelson-Aalen cumulative hazard}", size(4) margin(b=5)) ///
		xline(54, lwidth(2) lcolor(lavender)) ///
		xlabel(20(40)200) ///
		xmlabels(54 "Pilot IRS", labsize(3) labcolor(lavender) angle(45)) ///
		legend(label(1 "Baseline (-), Sprayed") label(2 "Baseline (-), Not Sprayed") label(3 "Baseline (+), Not Sprayed") label(4 "Baseline (+), Sprayed")) ///
		legend(order(4 3 1 2)) ///
		legend(region(lstyle(none))) ///
		legend(size(small)) ///
		scheme(s2color) ///
		graphregion(color(white)) ///
		plotregion(color(white)) ///
		graphregion(margin(large)) ///
		plotregion(margin(medium))
	graph save figure4, replace
	
	* Multiple failure data set up for qpcr - to estimate incidence
		clear
		use "primary_formatted_stata.dta"
		keep if isparticipating==1
		drop if qpcrcall==1 & qpcrcalllag1==1 & rdtresultslag1==0
		stset endobs, id(partid) failure(qpcrcall==1)  time0(startobs2) exit(studyend)
		stdescribe
			
	*------------------------ Subgroups: ITN : slept under net last night ------------------------*
	clear
	use "primary_formatted_stata.dta"
	preserve
	keep if tvnetsleepln==1
	*RDT outcome
	meglm rdtresults rdtresultslag1 tvpost##barm ///
		i.bzone i.studytime ///
		bsex bageyears bmalcauseknow tvsleepawaketimefactor ///
		tvmcolanophf ///
		||hhnumber:||partid:, family(binomial)	
	estimates store rdtmultinet1, title(Multivariate Logistic Regression, RDT Net Usage)
	
	*Microscopy outcome
	meglm microresults microresultslag1 tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears  bmalcauseknow tvsleepnightmovefactor ///
				tvmcolanophf ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store micromultinet1, title(Multivariate Logistic Regression, Microscopy Net Usage)
	
	*qPCR outcome
	meglm qpcrcall tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears ///
				tvmcolanophf ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store qpcrmultinet1, title(Multivariate Logistic Regression, qPCR  Net Usage)
	
	esttab rdtmultinet1 micromultinet1 qpcrmultinet1 using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table5a_netsleep.rtf", ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("RDT" "Microscopy" "qPCR") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("Odds Ratio" "95% CI" "p-value")

	restore
	
	
	*------------------------ Subgroups: ITN : household has adequate net coverage ------------------------*
	preserve
	keep if tvhhnetcov==1
	*RDT outcome
	meglm rdtresults rdtresultslag1 tvpost##barm ///
		i.bzone i.studytime ///
		bsex bageyears bmalcauseknow tvsleepawaketimefactor ///
		tvmcolanophf ///
		||hhnumber:||partid:, family(binomial)	
	estimates store rdtmultinet2, title(Multivariate Logistic Regression, RDT Net Own)
	
	*Microscopy outcome
	meglm microresults microresultslag1 tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears bmalcauseknow tvsleepawaketimefactor ///
				tvmcolanophf ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store micromultinet2, title(Multivariate Logistic Regression, Microscopy Net Own)
	
	*qPCR outcome
	meglm qpcrcall tvpost##barm ///
				i.bzone i.studytime ///
				bsex bageyears ///
				tvmcolanophf ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store qpcrmultinet2, title(Multivariate Logistic Regression, qPCR  Net Own)
	
	esttab rdtmultinet2 micromultinet2 qpcrmultinet2 using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table5b_netcov.rtf", ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("RDT" "Microscopy" "qPCR") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("Odds Ratio" "95% CI" "p-value")

	
	restore
	
	*------------------------ Subgroups: IRS: diff at baseline if sprayed in Oct 2021 ------------------------*
	preserve
	keep if birsoctyn!=. & tvpost==0
	
		* Determine vars associated with birsoctyn to see which are condounders
		foreach v of varlist(bmalsympknow bmalcauseknow bsex bageyears tvoutnight bageyears tvnetsleepln tvhhmalariatesttrt4wk bhheave bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists6) {
		 logit birsoctyn `v'
		estimates store `v' 
	}

		foreach v of varlist( bhhrooftype bhhwall) {
		logit birsoctyn i.`v'
		estimates store `v'
	}
		eststo birsoctyn_assoc: appendmodels bsex bageyears tvoutnight tvnetsleepln tvhhmalariatesttrt4wk bmalsympknow bmalcauseknow bhheave bhhrooftype bhhwall bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists6 

		esttab birsoctyn_assoc
	
	
	*RDT outcome
	meglm rdtresults birsoctyn ///
		i.bzone ///
		bsex bageyears ///
		tvnetsleepln ///
		bhheave bhhwall ///
		||hhnumber:||partid:, family(binomial)	
	estimates store rdtmultiirs1, title(Multivariate Logistic Regression, RDT IRS Base)
	
	*Microscopy outcome
	meglm microresults birsoctyn ///
				i.bzone ///
				bsex bageyears ///
				tvnetsleepln ///
				bhheave bhhwall ///
				||hhnumber:||partid:, family(binomial)
	estimates store micromultiirs1, title(Multivariate Logistic Regression, Microscopy IRS Base)
	
	*qPCR outcome
	meglm qpcrcall birsoctyn ///
				i.bzone ///
				bsex bageyears ///
				tvnetsleepln ///
				bhheave bhhwall ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store qpcrmultiirs1, title(Multivariate Logistic Regression, qPCR IRS Base)
	
	esttab rdtmultiirs1 micromultiirs1 qpcrmultiirs1 using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table6a_irsbase.rtf", ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("RDT" "Microscopy" "qPCR") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("Odds Ratio" "95% CI" "p-value")

	
	restore
	
	*--- Subgroups: IRS: diff between those sprayed just April 2022 versus those sprayed April 2022 and Oct 2021 ---*
	preserve
	keep if birscat!=.
	
	
		* Determine vars associated with birsoctyn to see which are potential condounders
		foreach v of varlist(bmalsympknow bmalcauseknow bsex bageyears tvoutnight bageyears tvnetsleepln tvhhmalariatesttrt4wk bhheave) {
		 logit `v' birsoctyn 
	}
	
		foreach v of varlist(bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists6) {
		 regress `v' birsoctyn 
	}
	
		foreach v of varlist( bhhrooftype bhhwall) {
		tab birsoctyn `v', chi2
		estimates store `v'
	}
		eststo birsoctyn_assoc: appendmodels bsex bageyears tvoutnight tvnetsleepln tvhhmalariatesttrt4wk bmalsympknow bmalcauseknow bhheave bhhrooftype bhhwall bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists6 
		esttab birsoctyn_assoc
	
	*RDT outcome
	meglm rdtresults tvpost#i.birscat ///
		i.bzone i.studytime ///
		bsex bageyears ///
		bhheave bhhwall bhheave ///
		||hhnumber:||partid:, family(binomial)	
	estimates store rdtmultiirs2, title(Multivariate Logistic Regression, RDT IRS Cat)
	
	*Microscopy outcome
	meglm microresults tvpost#i.birscat ///
				i.bzone i.studytime ///
				bsex bageyears ///
				bhheave bhhwall bhheave ///
				||hhnumber:||partid:, family(binomial)
	estimates store micromultiirs2, title(Multivariate Logistic Regression, Microscopy IRS Cat)
	
	*qPCR outcome
	meglm qpcrcall tvpost#i.birscat ///
				i.bzone i.studytime ///
				bsex bageyears ///
				bhheave bhhwall bhheave ///
				||hhnumber:||partid:, family(binomial) eform
	estimates store qpcrmultiirs2, title(Multivariate Logistic Regression, qPCR IRS Cat)
	
	esttab rdtmultiirs2 micromultiirs2 qpcrmultiirs2 using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table6b_irscat.rtf", ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers mtitles("RDT" "Microscopy" "qPCR") modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("Odds Ratio" "95% CI" "p-value")

	
	restore
	
	*------------------------ Entomology Regression ------------------------*
	clear
	use entomology_formatted_stata.dta
	
	*Univariate
	meglm mcolanophelesf tvpost##barm||hhnumber:, family(nbinomial) eform
	estimates store main
	
	foreach v of varlist(bhhmemcount belevation hhappliance4 hhappliance5 bhheave tvhhmalariatesttrt4wk tvnumsleep tvpropu5 tvpropunet bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6) {
     meglm mcolanophelesf `v'||hhnumber:, family(nbinomial)
	estimates store `v' 
}

	foreach v of varlist(bhhwatersrc bhhrooftype bhhwall) {
	meglm mcolanophelesf i.`v' ///
		||hhnumber:, family(nbinomial)
	estimates store `v'
}
	eststo entounivar: appendmodels main bhhmemcount belevation bhhwatersrc bhheave bhhrooftype bhhwall hhappliance4 hhappliance5 tvnumsleep tvhhmalariatesttrt4wk tvpropu5 tvpropunet bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6  
	
	*Associated with exposure
	foreach v of varlist(bhhmemcount belevation hhappliance4 hhappliance5 tvhhmalariatesttrt4wk tvnumsleep tvpropu5 tvpropunet bdistc bdistr bdists1 bdists2 bdists3 bdists4 bdists5 bdists6) {
     ttest `v', by(barm)

	foreach v of varlist(bhhwatersrc bhheave bhhrooftype bhhwall) {
	tab barm `v', chi2
	estimates store `v'
	}
	
	esttab entounivar

	* Multivariate 
	meglm mcolanophelesf tvpost##barm ///
				i.bzone i.studytime ///
				bhhroof bhhwall belevation bdistc bdists2 ///
				tvpropu5 ///
				||hhnumber:, family(nbinomial)
	estimates store entomulti, title(Multivariate Negative Binomial Regression, Indoor Anopheline Counts)

	esttab entomulti using "/Users/annemartin/Documents/PhD/ICEMR/Nchelenge/Pilot IRS/Drafts/July 2024 JID Submission/Final code and datasets/Public code outputs/Table4_ento.rtf", ///
	 replace cells("b(fmt(%9.2f)) ci(fmt(%9.2f) par) p(fmt(%9.2f))") eform ///
		lines label nonumbers  modelwidth(8) eqlabel(none) ///
		refcat(0.tvpost "Period" 0.barm "Intervention Arm" 1.bzone "Zone" 0.studytime "Study Time" 0.bhhwall "Structure wall materials" 1.tvnettimeown "Time owning net" 1.tvsleepbedtimefactor "Bedtime" 1.tvsleepnightmovefactor "Number times up at night" 1.tvsleepawaketimefactor "Awakening time" 1.bhhwatersrc "Water source" 1.bhhrooftype "Roof type", nolabel ) ///
		collabels("Anoph. Count" "95% CI" "p-value")

	
	
	log close

	
