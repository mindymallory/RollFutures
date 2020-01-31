
use "$results_data/Appendix/Appendix NoCE/Inventory Equation Dataset.dta",clear
capture log close
qui log using "$results_data/Appendix/Appendix NoCE/Inventory Equation.smcl", replace

*Regressions

regress stocks_petroleum_us spec_var_1 ip  trend,robust
regress stocks_petroleum_us spec_var_1  actual_ip_growth    trend,robust
regress stocks_petroleum_us spec_var_1  actual_ip_growth ip trend,robust
regress stocks_petroleum_us spec_var_1 ip stocks_petroleum_us_lag trend,robust

regress stocks_petroleum_us spec_var_1  actual_ip_growth  stocks_petroleum_us_lag  trend,robust
regress stocks_petroleum_us spec_var_1  actual_ip_growth ip stocks_petroleum_us_lag trend,robust

**World Stocks


regress stocks_petroleum_world spec_var_1  igrec stocks_petroleum_world_lag   trend,robust
regress stocks_petroleum_world spec_var_1 igrec   trend,robust
regress stocks_petroleum_world spec_var_1  igrec_quart stocks_petroleum_world_lag   trend,robust
regress stocks_petroleum_world spec_var_1 igrec_quart   trend,robust
regress stocks_petroleum_world spec_var_1  lnworld_real_gdp_quart stocks_petroleum_world_lag   trend,robust
regress stocks_petroleum_world spec_var_1 lnworld_real_gdp_quart   trend,robust

qui log close
