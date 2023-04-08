/******************************************************************/		
/*                    REPLICATION CODE                            */
/*                                                                */
/*            ROBERT JENSEN & NOLAN H. MILLER                     */  
/*        "INFORMATION, DEMAND AND THE GROWTH OF FIRMS:           */
/*         EVIDENCE FROM A NATURAL EXPERIMENT IN INDIA"           */
/*                AMERICAN ECONOMIC REVIEW                        */ 
/*                                                                */
/*                    FOR USE WITH FILES:                         */ 
/*                  BuilderDataSetFinal.dta                       */
/*                 FishermanDataSetFinal.dta                      */  
/*                  VillageDataSetFinal.dta                       */
/*                                                                */
/*               Written in Stata Version 13.1                    */
/*                Final Version: May 30, 2018                     */
/*                                                                */
/******************************************************************/	

	
/******************************************************************/
/*****          CHANGE TO YOUR LOCAL DIRECTORY HERE           *****/ 
/******************************************************************/
	cd c:\AER_Firms_Paper\Replication


/*Environment*/
	#delimit ;
	version 13.1;
	cls;
	set more 1;
	clear;
	cap log close; log using temp, replace;


/*Note: Tables and figures in the order they appear in the paper switch back 
and forth between different data files (e.g., between builders, fishermen or 
villages as the unit of observation/analysis). We group the code in this 
replication file by data set, rather than going in order from the first table 
to the last and switching back and forth between data sets. Thus the order of 
tables as they appear sequentially in the code is:

	Builders Data:	Tables 1, 2, 3, 4 and 6(cols. 1-5), Figures 3, 4 and 5, 
					App. Tables 1, 2, 4 and 5(cols. 1-5)

	Fishermen Data:	Table 5, Figure 2, App. A, App. Tables 3 and 7

	Village Data:	Table 6 (Col. 6&7), App. Table 5 (col. 6 & 7), App. Table 6
*/
	

	
	
	
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/	
/*ooooo              START OF BUILDER DATA RESULTS           ooooo*/	
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/	
	
	
	
/******************************************************************/		
/*                             TABLE 1                            */
/*        SUMMARY STATISTICS FOR KEY VARIABLES AT BASELINE        */
/******************************************************************/	

	use BuilderDataSetFinal;

		egen total_boats_built_year_all=sum(boats_built), by(round);
		gen market_share=boats_built/total_boats_built_year_all;
		gen price_per_year=mean_price/life_expectancy;
			
		sort round district;
		egen total_boats_built_year_region=sum(boats_built), by(round district);
		egen total_workers_year_region=sum(n_workers), by(round district);	
		egen total_capital_year_region=sum(tool_value), by(round district);	
		egen life_built_year_region=sum(boats_built), by(round district);
	
		sum n_workers boats_built market_share life_expectancy_boat auditor_assessment 
			baseline_perceptions mean_price price_per_year if round==2;
		
		/*NOTE: "Baseline" here is Round 2, as noted in the paper*/
			
		/*For mean of quality residual in this table, see below (line 107)*/


/******************************************************************/		
/*                             TABLE 2                            */
/*        REGRESSION RESULTS: EXIT, MARKET SHARE AND EMPLOYMENT   */
/******************************************************************/	
	
		/*First construct new variables*/
		/*Baseline quality*/
			/*Previous boat*/
				gen b_l_e=life_expectancy_boat if round==2;				
				egen baseline_life_expectancy=max(b_l_e), by(town_id);	
				
			/*Auditor asessment*/
				gen b_l_e_a=auditor_assessment if round==2;
				egen baseline_auditor_assessment=max(b_l_e_a), by(town_id);
			
			/*Residuals and associated variables*/
			gen ln_hours=ln(hours_worked); gen ln_materials=ln(material_value); gen ln_tools=ln(tool_value);
			qui regress baseline_life_expectancy ln_hours ln_material ln_tools if round==2;
				predict life_hat;
			gen q_r=baseline_life_expectancy-life_hat if round==2;
			sort town_id;
			egen quality_resid=max(q_r), by(town_id);
					sum quality_resid if round==2;
			gen has_phone_quality_resid=has_phone*quality_resid;	

			/*Perceptions already brought in as baseline values*/
						
		/*Interaction terms*/
			gen has_phone_life_expectancy=has_phone*baseline_life_expectancy;	
			gen has_phone_auditor_assessment=has_phone*baseline_auditor_assessment;
			gen has_phone_perceptions=has_phone*baseline_perceptions;


		/*Regressions*/
		
			local outcomes "exit market_share n_workers boats_built";
		
		/*A. previous boat life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_life_expectanc has_phone baseline_life_expectancy i.district i.round, robust cluster(town_id);
					outreg2 using Table2A, noaster word excel `replace_append' keep(has_phone_life_expectanc has_phone baseline_life_expectancy) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
		
		/*B. auditor life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_auditor_assessment has_phone baseline_auditor_assess i.district i.round, robust cluster(town_id);
					outreg2 using Table2B, noaster word excel `replace_append' keep(has_phone_auditor_assessment has_phone baseline_auditor_assess) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
		
		/*C. perceptions life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_perceptions has_phone baseline_perceptions i.district i.round, robust cluster(town_id);				
					outreg2 using Table2C, noaster word excel `replace_append' keep(has_phone_perceptions has_phone baseline_perceptions) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;

		/*D. quality residuals*/
			local replace_append "replace";		
			foreach y of local outcomes{;
				reg `y' has_phone_quality_resid has_phone quality_resid i.district  i.round, robust cluster(town_id);
					outreg2 using Table2D, noaster word excel `replace_append' keep(has_phone_quality_resid has_phone quality_resid) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 					
			;			

	
/******************************************************************/		
/*                             TABLE 3                            */
/*                   OUTPUT AND INPUTS OVER TIME                  */
/******************************************************************/
			
		/*generate some variables to show region*round aggregates*/
			sort round district;
			egen total_workers=sum(n_workers), by(round district);
			egen total_hours=sum(hours_worked), by(round district);
			egen total_tools=sum(tool_value/1000), by(round district);
			egen total_tools_base=sum(tools_valued_at_base/1000), by(round district);			
			egen total_material=sum((boats_built*material_value)/1000), by(round district);
			egen total_output=sum(boats_built), by(round district);
			gen life_years_built=boats_built*auditor_assessment;
			egen total_boatyears_output=sum(life_years_built), by(round district);
			gen hours_per_boat_2=hours_worked/boats_built;
			gen hours_per_boat=total_hours/total_output;			
			gen kap_per_boat=total_tools/total_output;
			gen kap_per_boat_base=total_tools_base/total_output;			

			gen labor_costs=hours_per_boat*wage_worker;
			gen variable_cost=hours_per_boat*wage_worker+material_value;
			
			gen wwb=wage_worker if round==2;
			sort town_id;
			egen wage_worker_baseline=max(wwb), by(town_id);

			gen labor_costs_valued_base=hours_worked*wage_worker_baseline;
			gen variable_costs_base=labor_costs_valued_base+material_valued_base*boats_built;
			
			sort town_id round;
			egen total_material_costs_base=sum(material_valued_base*boats_built), by(district round);
			egen total_labor_value_base=sum(labor_costs_valued_base), by(district round);
			egen total_varcosts_value_base=sum(variable_costs_base), by(district round);
			egen mean_tasks=mean(tasks_per_worker), by(district round);			

			
			gen total_K_per_total_output=total_tools_base/total_output;
			gen total_H_per_total_output=total_hours/total_output;	
			gen total_LV_per_total_output=total_labor_value_base/total_output;				
			gen total_M_per_total_output=total_material_costs_base/total_output;	
			gen total_VC_per_total_output=total_varcosts_value_base/total_output;				
			gen total_VC_per_total_boatyear=total_varcosts_value_base/total_boatyears_output;		
			gen total_K_per_total_boatyear=total_tools_base/total_boatyears_output;
			gen total_H_per_total_boatyear=total_hours/total_boatyears_output;
			gen total_LV_per_total_boatyear=total_labor_value_base/total_boatyears_output;				
			gen total_M_per_total_boatyear=total_material_costs_base/total_boatyears_output;	
			
			gen boat_years_per_boat=total_boatyears_output/total_output;

			gen round_odd=round if round/2~=round(round/2,1);
		
			forvalues d = 1(1)3 {;
				tabstat total_output total_K_per_total_output total_H_per_total_output total_LV_per_total_output total_M_per_total_output total_VC_per_total_output 
					total_boatyears_output total_K_per_total_boatyear total_H_per_total_boatyear total_LV_per_total_boatyear total_M_per_total_boatyear total_VC_per_total_boatyear 
					boat_years_per_boat	if district==`d', by(round_odd) stat(mean);};
			;
				/*to make table as it is formatted in paper (rounds across, instead of down), select desired data from results window, right click and choose "copy table." 
					Paste into Excel, then within Excel, copy and "paste special," and click the "transpose" box.*/
					
					
		/*tasks per worker*/
			forvalues d = 1(1)3 {;
				forvalues r = 1(1)13 {;
					di "District: " `d' "   Round: " `r';
					ci tasks_per_worker if round==`r' & district==`d' [aw=n_workers];
					di ""; di "";				
				};
			};
			;


/******************************************************************/		
/*                             TABLE 4                            */
/*             INPUTS OVER TIME: WITHIN-FIRM ESTIMATES            */
/******************************************************************/
	
		/*Generate needed firm-level variables*/
			gen f_output=boats_built;
			gen f_K_per_boat=tools_valued_at_base/(1000*boats_built);
			gen f_H_per_boat=hours_worked/boats_built;
			gen f_LV_per_boat=labor_costs_valued_base/boats_built;
			gen f_M_per_boat=material_valued_base;	
			gen f_VC_per_boat=f_LV_per_boat+f_M_per_boat;
				
			gen f_boatyears_output=boats_built*auditor;				
			gen f_K_per_boatyear=tools_valued_at_base/(1000*f_boatyears_output);
			gen f_H_per_boatyear=hours_worked/f_boatyears_output;			
			gen f_LV_per_boatyear=labor_costs_valued_base/f_boatyears_output;
			gen f_M_per_boatyear=material_valued_base/auditor_assessment;
			gen f_VC_per_boatyear=(labor_costs_valued_base+material_valued_base*boats_built)/f_boatyears_output;
	
			xtreg auditor_assessment has_phone i.round , robust cluster(town_id) fe i(town_id);
				outreg2 using Table4, noaster word excel replace keep(has_phone) nor2;		 

			foreach var of varlist f_LV_per_boat f_M_per_boat f_VC_per_boat f_LV_per_boatyear f_M_per_boatyear f_VC_per_boatyear{;					
					xtreg `var' has_phone i.round, robust cluster(town_id) fe i(town_id);
			outreg2 using Table4, noaster word excel append keep(has_phone) nor2;};

			
/******************************************************************/
/*                             TABLE 5                            */
/*             POOLED TREATMENT REGRESSIONS: CONSUMERS            */
/******************************************************************/

	/*Below -- need to use fisherman's data set for this.*/
	
	
/******************************************************************/
/*                            TABLE 6                             */
/*   ADDITIONAL RESULTS FOR TESTING MELITZ AND MELITZ-OTTAVIANO   */
/*	             (COLUMNS 1-5. COLUMNS 6 & 7 BELOW)               */
/******************************************************************/

		gen price_charged_per_year=price_charged/auditor;
		gen markup_per_year=markup/auditor;
		
		local outcomes "profits price_charged price_charged_per_year markup markup_per_year";

			local replace_append "replace";		
			foreach y of local outcomes{;
				reg `y' has_phone i.district i.round if markup~=., robust cluster(town_id);
					outreg2 using Table6A, noaster word excel `replace_append' keep(has_phone) stat(coef se blank)  nor2;
				local replace_append "append";}; 		
			;
			
			local replace_append "replace";		
			foreach y of local outcomes{;
				reg `y' has_phone_life_expectanc has_phone baseline_life_expectancy i.district i.round if markup~=., robust cluster(town_id);
					outreg2 using Table6B, noaster word excel `replace_append' keep(has_phone_life_expectanc has_phone baseline_life_expectancy) stat(coef se blank)  nor2;
				local replace_append "append";}; 	
					;

	
/******************************************************************/
/*                            FIGURE  1                           */
/*        SPREAD OF MOBILE PHONES, JANUARY 1998-JANUARY 2003      */
/******************************************************************/

	/*Figure not based on survey data*/
	
	
	
/******************************************************************/
/*                            FIGURE  2                           */
/*    MOBILE PHONES AND FISHERMEN’S BEHAVIOR AND INFORMATION      */
/******************************************************************/	
	
	/*Below -- need to use fisherman's data set for this.*/


	
/******************************************************************/
/*                             FIGURE  3                          */
/*               MOBILE PHONES AND THE NUMBER OF FIRMS            */
/******************************************************************/			

	/*We created the graphs in excel, but this generates the numbers*/
    
		/*A. Total Number of Firms*/
			forvalues d = 1(1)3 {;
				forvalues r = 1(1)13 {;
					di "District: " `d' "   Round: " `r';
					count if round==`r' & district==`d' & exit==0;
					di ""; di "";				
				   };
			};
			;

		/*B. Number of Firms, By Baseline Lifespan*/
			sort town_id;
			egen median_life_expect=median(b_l_e), by(district);
			gen above_median_baseline_LE=baseline_life_expectancy>median_life_expect;
			
			forvalues d = 1(1)3 {;
				forvalues r = 1(1)13 {;
					di "District: " `d' "   Round: " `r';
					count if round==`r' & district==`d' & above_median_baseline_LE==1;
					count if round==`r' & district==`d' & above_median_baseline_LE==0;						
					di ""; di "";				
				   };
			};
			;		

						
/******************************************************************/
/*                              FIGURE 4                          */
/*                  DISTRIBUTIONS OF BUILDER QUALITY              */
/******************************************************************/		

		/*A. Region I*/
			kdensity baseline_life_expectancy if round==2 & district==1, nograph gen(x1rI fx1rI) bwidth(.75);
			kdensity baseline_life_expectancy if round==13 & district==1, nograph gen(x13rI fx13rI) bwidth(.75);
				twoway line fx1rI x1rI || line fx13rI x13rI,
					xlabel(2(1)8) ylabel(0 .5) xtitle("") ytitle("") legend(off) scheme(s1mono) plotregion(style(none)) saving(Fig4_RI, replace);

		/*B. Region II*/
			kdensity baseline_life_expectancy if round==2 & district==2, nograph gen(x1rII fx1rII) bwidth(.75);
			kdensity baseline_life_expectancy if round==13 & district==2, nograph gen(x13rII fx13rII) bwidth(.75);
				twoway line fx1rII x1rII || line fx13rII x13rII,
					xlabel(2(1)8) ylabel(0 .5) xtitle("") ytitle("") legend(off) scheme(s1mono) plotregion(style(none)) saving(Fig4_RII, replace);

		/*C. Region III*/
			kdensity baseline_life_expectancy if round==2 & district==3, nograph gen(x1rIII fx1rIII) bwidth(.75);
			kdensity baseline_life_expectancy if round==13 & district==3, nograph gen(x13rIII fx13rIII) bwidth(.75);
				twoway line fx1rIII x1rIII || line fx13rIII x13rIII,
					xlabel(2(1)8) ylabel(0 .5) xtitle("") ytitle("") legend(off) scheme(s1mono) plotregion(style(none)) saving(Fig4_RIII, replace);		
					
					
					
/******************************************************************/
/*                            FIGURE 5                            */
/*    ECONOMIES OF SCALE AND RETURNS TO LABOR SPECIALIZATION      */
/******************************************************************/
			
		/*generate some variables*/	
			gen logvc=ln(labor_costs_valued_base/boats_built+material_valued_base);
			gen lnboat=ln(boats_built);

		/*A. Scale and Variable Costs*/
			lowess logvc lnboat, nogr gen(logvc_hat) bwidth(1);
			sort lnboat;
			twoway line logvc_hat lnboat,
				xlabel(0(1)5) ylabel(6.5(.5)7.5)  xtitle("ln(boats built)") ytitle("ln(variable costs)") legend(off) scheme(s1mono) plotregion(style(none)) saving(Figure5A, replace);
			reg logvc_hat lnboat if lnboat>2.5;

		/*B. Scale and Labor Specialization*/				
			lowess tasks_per_worker lnboat, nogr gen(tasks_hat) bwidth(1);
			sort lnboat;
			twoway line tasks_hat lnboat,
				xlabel(0(1)5) ylabel(0(1)7)  xtitle("ln(boats built)") ytitle("tasks per worker") legend(off) scheme(s1mono) plotregion(style(none)) saving(Figure5B, replace);
							
		/*C. Labor Specialization and Variable Costs*/				
			lowess logvc tasks_per_worker, nogr gen(logvc2_hat) bwidth(1);
			sort tasks_per_worker;
			twoway line logvc2_hat tasks_per_worker,
				xlabel(0(2)12) ylabel(6.5(.5)7.5)  xtitle("tasks per worker") ytitle("ln(variable costs)") legend(off) scheme(s1mono) plotregion(style(none)) saving(Figure5C, replace);
								
								

					
/******************************************************************/
/*                          APPENDIX A                            */
/*                EXPLORING OTHER DIMENSIONS OF QUALITY           */
/******************************************************************/

	/*Below -- need to use fisherman's data set for this.*/
			

			
/******************************************************************/
/*                       APPENDIX FIGURE 1                        */
/*                         STUDY REGION                           */
/******************************************************************/

	/*Figure not based on survey data*/

	
		
/******************************************************************/
/*                        APPENDIX TABLE 1                        */
/*       REGRESSION RESULTS (EXCLUDING INLAND REGION III)         */
/******************************************************************/	

			local outcomes "exit market_share n_workers boats_built";
		
		/*A. previous boat life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_life_expectanc has_phone baseline_life_expectancy i.district i.round if district~=3, robust cluster(town_id);
					outreg2 using AppTable1A, noaster word excel `replace_append' keep(has_phone_life_expectanc has_phone baseline_life_expectancy) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
		
		/*B. auditor life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_auditor_assessment has_phone baseline_auditor_assess i.district i.round if district~=3, robust cluster(town_id);
					outreg2 using AppTable1B, noaster word excel `replace_append' keep(has_phone_auditor_assessment has_phone baseline_auditor_assess) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
		;
		
		/*C. perceptions life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				reg `y' has_phone_perceptions has_phone baseline_perceptions i.district i.round if district~=3, robust cluster(town_id);				
					outreg2 using AppTable1C, noaster word excel `replace_append' keep(has_phone_perceptions has_phone baseline_perceptions) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
					;

		/*D. quality residuals*/
			local replace_append "replace";		
			foreach y of local outcomes{;
				reg `y' has_phone_quality_resid has_phone quality_resid i.district  i.round if district~=3, robust cluster(town_id);
					outreg2 using AppTable1D, noaster word excel `replace_append' keep(has_phone_quality_resid has_phone quality_resid) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 					
			;	
		
		
/******************************************************************/
/*                       APPENDIX TABLE 2                         */
/*          REGRESSION RESULTS, WITH BUILDER FIXED EFFECTS        */
/******************************************************************/

		/*A. previous boat life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				xtreg `y' has_phone_life_expectanc has_phone i.round, robust cluster(town_id) fe i(town_id);
					outreg2 using AppTable2A, noaster word excel `replace_append' keep(has_phone_life_expectanc has_phone) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
		
		/*B. auditor life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				xtreg `y' has_phone_auditor_assess has_phone i.round, robust cluster(town_id) fe i(town_id);
					outreg2 using AppTable2B, noaster word excel `replace_append' keep(has_phone_auditor_assessment has_phone) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
		;
		
		/*C. perceptions life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes{;
				xtreg `y' has_phone_perceptions has_phone i.round, robust cluster(town_id) fe i(town_id);			
					outreg2 using AppTable2C, noaster word excel `replace_append' keep(has_phone_perceptions has_phone) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
					;

		/*D. quality residuals*/
			local replace_append "replace";		
			foreach y of local outcomes{;
				xtreg `y' has_phone_quality_resid has_phone i.round, robust cluster(town_id) fe i(town_id);
					outreg2 using AppTable2D, noaster word excel `replace_append' keep(has_phone_quality_resid has_phone) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 					
			;	
	

/******************************************************************/
/*                       APPENDIX TABLE 3                         */
/*        CHANGES IN FISHERMEN’S BEHAVIOR AND INFORMATION         */
/******************************************************************/

	/*Below -- need to use fisherman's data set for this.*/
	
	
/******************************************************************/		
/*                        APPENDIX TABLE 4                        */
/*                  OUTPUT AND INPUTS OVER TIME                   */
/******************************************************************/

			local outcomes_region "total_output total_K_per_total_output total_H_per_total_output total_LV_per_total_output total_M_per_total_output total_VC_per_total_output
					total_boatyears_output total_K_per_total_boatyear total_H_per_total_boatyear total_LV_per_total_boatyear total_M_per_total_boatyear total_VC_per_total_boatyear
					boat_years_per_boat mean_tasks";
		
		/*For district*region level regressions, want to use just one observation per district per round*/
			gen opd=uniform();
			sort district round;
			egen min_opd=min(opd), by(district round);
			gen one_per_district=opd==min_opd;

			local replace_append "replace";
			foreach y of local outcomes_region {;
				reg `y' has_phone i.district if one_per_district==1, robust;
					outreg2 using AppTable4, noaster word excel `replace_append' keep(has_phone) stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
		
	
/******************************************************************/
/*                         APPENDIX TABLE 5                       */
/*      TESTING MELITZ AND MELITZ-OTTAVIANO:  EARLY VS. LATE      */
/*	             (COLUMNS 1-5. COLUMNS 6 & 7 BELOW)               */
/******************************************************************/

		
		local outcomes "profits price_charged price_charged_per_year markup markup_per_year";

		gen late=round>=11;
		gen late_phone=late*has_phone;
		gen early_phone=has_phone==1 & round<11;

			local replace_append "replace";		
			foreach y of local outcomes{;
				reg `y' early_phone late_phone i.district i.round if markup~=., robust cluster(town_id);
					outreg2 using AppTable5, noaster word excel `replace_append' keep(early_phone late_phone) nocons stat(coef se blank) nor2;
				local replace_append "append"; 		
			}; 					
			;	

			
		
/******************************************************************/
/*                       APPENDIX TABLE 6                         */
/*        REGRESSION RESULTS FOR ALTERNATIVE EXPLANATIONS         */
/******************************************************************/	

	/*Below -- need to use village data set for this.*/
	

/******************************************************************/
/*                         APPENDIX TABLE 7                       */
/*                CORRELATES OF BUYING BOATS NON-LOCALLY          */
/******************************************************************/

	/*Below -- need to use fisherman's data set for this.*/
	
	

/******************************************************************/
/*                    DECOMPOSITION, SECTION 4.D                  */
/******************************************************************/
				
			gen cb=f_VC_per_boatyear if round==2;	gen ce=f_VC_per_boatyear if round==13;
			gen mb=market_share if round==2;		gen me=market_share if round==13;

			sort town_id;
			egen c_b=max(cb), by(town_id);			egen c_e=max(ce), by(town_id);
			egen m_b=max(mb), by(town_id);			egen m_e=max(me), by(town_id);
						
			gen m_e_c_e=m_e*c_e;
			gen m_b_c_b=m_b*c_b;
			gen m_e_c_b=m_e*c_b;
		
			keep if round==2;
			
			egen sum_mece=sum(m_e_c_e);
			egen sum_mbcb=sum(m_b_c_b);
			egen sum_mece_minus_mecb=sum(m_e_c_e-m_e_c_b);
			egen sum_mecb_minus_mbcb=sum(m_e_c_b-m_b_c_b);
		
			sum sum_mece sum_mbcb sum_mece_minus_mecb sum_mecb_minus_mbcb;
			
			
			
			
			
	
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/	
/*ooooo            START OF FISHERMAN DATA RESULTS           ooooo*/	
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/	

	
	
/******************************************************************/
/*                             TABLE 5                            */
/*             POOLED TREATMENT REGRESSIONS: CONSUMERS            */
/******************************************************************/

	use BuilderDataSetFinal, clear;
	
		rename town_id where_buy_id;				
		keep round where_buy_id auditor_assessment;
		sort round where_buy_id;
	save temp, replace;

	use FishermanDataSetFinal;
		sort round where_buy_id;
		merge round where_buy_id using temp;
			tab _m;		keep if _m==3; 	drop _m;

		gen price_per_boat_year=price_paid/auditor_assessment;
		
		local outcomes_cons "price_paid auditor_assessment price_per_boat_year";

			local replace_append "replace";
			foreach y of local outcomes_cons {;
			reg `y' has_phone i.round if bought==1, robust cluster(where_buy) ;	
					outreg2 using Table5, noaster word excel `replace_append' keep(has_phone) stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
	
			foreach y of local outcomes_cons {;
			xtreg `y' has_phone i.round if bought==1,  robust cluster(where_buy) fe i(where_buy);
					outreg2 using Table5, noaster word excel append keep(has_phone) stat(coef se blank) nor2;}; 
			;
	
			
/******************************************************************/
/*                            FIGURE  2                           */
/*     MOBILE PHONES AND FISHERMEN’S BEHAVIOR AND INFORMATION     */
/******************************************************************/			

/*note, created this graph in excel, but this generates the numbers*/
	
	use FishermanDataSetFinal, clear;
	
	/*A. % who sell fish in their own village*/
	forvalues d = 1(1)3 {;
		forvalues r = 1(1)13 {;
			di "District: " `d' "   Round: " `r';
			sum sell_local if round==`r' & district==`d';
			di ""; di "";				
           };
	};
;	

	/*B. "Errors in estimating boat lifespan (yrs.)*/
		forvalues d = 1(1)3 {;
			forvalues r = 1(1)13 {;
				di "District: " `d' "   Round: " `r';
				sum abs_error_local_builder if round==`r' & district==`d';
				sum abs_error_non_local_builder if round==`r' & district==`d';
				di ""; di "";				
			};
		};
;


	/*C. % who buy boats in their own village*/
	forvalues d = 1(1)3 {;
		forvalues r = 1(1)13 {;
			di "District: " `d' "   Round: " `r';
			sum buy_local if round==`r' & district==`d';
			di ""; di "";				
           };
	};
;	


/******************************************************************/
/*                          APPENDIX A                            */
/*                EXPLORING OTHER DIMENSIONS OF QUALITY           */
/******************************************************************/
	
		local outcomes_other_q "daily_fish_caught fuel_effic speed";

		local replace_append "replace";		
		foreach y of local outcomes_other_q{;
			xtreg `y' has_local i.round if has_phone==1, robust cluster(town_id) fe i(town_id);
				outreg2 using AppA, noaster word excel `replace_append' keep(has_local) stat(coef se blank) nor2;
			local replace_append "append";}; 					
		;	
	
	

/******************************************************************/
/*                        APPENDIX TABLE 3                        */
/*          CHANGES IN FISHERMEN’S BEHAVIOR AND INFORMATION       */
/******************************************************************/		
	
		local f_outcomes "abs_error_local_builder abs_error_non_local_builder buy_local";
		
		reg sell_local has_phone i.district i.round, robust cluster(town_id);
			outreg2 using AppTable3, noaster word excel replace keep(has_phone) nor2;
		foreach y of local f_outcomes{;
			reg `y' has_phone i.district i.round, robust cluster(town_id);
				outreg2 using AppTable3, noaster word excel append keep(has_phone) nor2;	};				
		;
	

/******************************************************************/
/*                         APPENDIX TABLE 7                       */
/*                CORRELATES OF BUYING BOATS NON-LOCALLY          */
/******************************************************************/

	
	replace total_monthly_income=total_monthly_income/1000;			

	reg has_local total_monthly_income distance i.district i.round if has_phone==1 & bought==1, robust cluster(town_id);
			outreg2 using AppTable7, noaster word excel replace keep(total_monthly_income distance) nocons stat(coef se blank) nor2;
	xtreg has_local total_monthly_income distance i.round if has_phone==1 & bought==1, robust cluster(town_id) fe i(town_id);
			outreg2 using AppTable7, noaster word excel append keep(total_monthly_income distance) nocons stat(coef se blank) nor2;

	
			
			
			
			
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/	
/*ooooo              START OF VILLAGE DATA RESULTS           ooooo*/	
/*oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo*/			
			
			
			
/******************************************************************/
/*                             TABLE 6                            */
/*   ADDITIONAL RESULTS FOR TESTING MELITZ AND MELITZ-OTTAVIANO   */
/*	                      (COLUMNS 6 & 7)                         */
/******************************************************************/

	use VillageDataSetFinal, clear;

			gen has_phone_life_expectancy=has_phone*baseline_life_expectancy;	
			
		local outcomes_vill_mel "daily_wage number_places";

			
			foreach y of local outcomes_vill_mel{;
				reg `y' has_phone i.district i.round, robust cluster(town_id);
					outreg2 using Table6A, noaster word excel append keep(has_phone) stat(coef se blank) nor2;};
				;
				
			foreach y of local outcomes_vill_mel{;
				reg `y' has_phone_life_expectanc has_phone baseline_life_expectancy i.district i.round, robust cluster(town_id);
					outreg2 using Table6B, noaster word excel append keep(has_phone_life_expectanc has_phone baseline_life_expectancy) stat(coef se blank) nor2;};
				;				


/******************************************************************/
/*                         APPENDIX TABLE 5                       */
/*      TESTING MELITZ AND MELITZ-OTTAVIANO:  EARLY VS. LATE      */
/*	                      (COLUMNS 6 & 7)                         */
/******************************************************************/	

		gen late=round>=11;
		gen late_phone=late*has_phone;
		gen early_phone=has_phone==1 & round<11;
	
			foreach y of local outcomes_vill_mel{;
				reg `y' early_phone late_phone i.district i.round, robust cluster(town_id);
					outreg2 using AppTable5, noaster word excel append keep(early_phone late_phone) nocons stat(coef se blank) nor2;}; 					
			;	
			
		
/******************************************************************/
/*                       APPENDIX TABLE 6                         */
/*        REGRESSION RESULTS FOR ALTERNATIVE EXPLANATIONS         */
/******************************************************************/			

		
		local outcomes_alt "has_elect boats price_index";
		
		/*previous boat life expetancy estimates*/
			local replace_append "replace";
			foreach y of local outcomes_alt {;
				reg `y' has_phone_life_expectanc has_phone baseline_life_expectancy i.district i.round, robust cluster(town_id);
					outreg2 using AppTable6, noaster word excel `replace_append' keep(has_phone_life_expectanc has_phone baseline_life_expectancy) nocons stat(coef se blank) nor2;
				local replace_append "append"; }; 
			;
