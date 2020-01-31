
use "$results_data/Appendix/Appendix NoCE/Individual Commodities Dataset.dta",clear
capture log close
qui log using "$results_data/Appendix/Appendix NoCE/Individual Commodities.smcl", replace



**Regressions***

*Table 4a
levelsof commodity, local (commod)
foreach c of local commod {
preserve
di "`c'"
keep if  commodity=="`c'" & lp!=. & lnrgdp_world!=. & sdsp_lag!=. &  spread!=. &  linvent!=. &  real_int_gdpcpi!=. 
sort year
gen trend =_n
label var trend "Linear trend"
reg lp lnrgdp_world sdsp_lag spread linvent real_int_gdpcpi trend , robust
restore
}




*Panel
*Table 5a
egen commodity_num=group(commodity)
preserve
keep if  lp!=. & lnrgdp_world!=. & sdsp_lag!=. &  spread!=. &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
label var trend " Linear Trend" 
xtreg lp lnrgdp_world sdsp_lag spread linvent real_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if  lp!=. & lnrgdp_world!=. & sdsp_lag!=.  &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
label var trend " Linear Trend" 
xtreg lp lnrgdp_world sdsp_lag  linvent real_int_gdpcpi trend, fe i(commodity_num) robust
restore


preserve
keep if  lp!=. & oecdgap!=. & sdsp_lag!=. & spread!=.  &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
label var trend " Linear Trend" 
xtreg lp oecdgap sdsp_lag spread linvent real_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if  lp!=. & oecdgap_update!=. & sdsp_lag!=. & spread!=. &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
label var trend " Linear Trend" 
xtreg lp oecdgap_update sdsp_lag spread linvent real_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if  lp!=. & HP_lnrgdp_world_1!=. & sdsp_lag!=. & spread!=. &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
label var trend " Linear Trend" 
xtreg lp HP_lnrgdp_world_1 sdsp_lag spread linvent real_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if  lp!=. & lnrgdp_world!=. & sdsp_lag!=. &  spread!=. &  linvent!=. &  real_int_gdpcpi!=.
egen trend=group(year)
g trendsq=trend^2
label var trend " Linear Trend" 
label var trendsq " Quadratic Trend"
xtreg lp lnrgdp_world sdsp_lag spread linvent real_int_gdpcpi trend trendsq, fe i(commodity_num) robust
restore



*Table 5b

preserve
keep if   dlp!=. & oecdgap!=. & dsdsp_lag!=. &  dspread!=. &  dlinvent!=. &  dreal_int_gdpcpi!=. 
egen trend=group(year)
label var trend " Linear Trend"
xtreg dlp oecdgap dsdsp_lag dspread dlinvent dreal_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if   dlp!=. & oecdgap_update!=. & dsdsp_lag!=. &  dspread!=. &  dlinvent!=. &  dreal_int_gdpcpi!=. 
egen trend=group(year)
label var trend " Linear Trend"
xtreg dlp oecdgap_update dsdsp_lag dspread dlinvent dreal_int_gdpcpi trend, fe i(commodity_num) robust
restore

preserve
keep if   dlp!=. & HP_lnrgdp_world_1!=. & dsdsp_lag!=. &  dspread!=. &  dlinvent!=. &  dreal_int_gdpcpi!=. 
egen trend=group(year)
label var trend " Linear Trend"
xtreg dlp HP_lnrgdp_world_1 dsdsp_lag dspread dlinvent dreal_int_gdpcpi trend, fe i(commodity_num) robust
outreg2  using "$results_data/Table3b.xls", append label  dec(5)
restore





*Table 5c
levelsof commodity, local (commod)
foreach c of local commod {
preserve
di "`c'"
keep if  commodity=="`c'" & lp!=. & lnrgdp_world!=. & sdsp_lag!=. &  spread!=. &  linvent!=. &  real_int_gdpcpi!=. & lp_lag!=.
sort year
gen trend =_n
label var trend "Linear trend"
reg lp lnrgdp_world sdsp_lag spread linvent real_int_gdpcpi trend lp_lag, robust
restore
}


qui log close

