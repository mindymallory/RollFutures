use "$results_data/Appendix/Appendix NoCE/Price Equation Dataset.dta",clear
capture log close
qui log using "$results_data/Appendix/Appendix NoCE/Price Equation.smcl", replace




*US stocks

reg lp lnrgdp_usa conv_imp_volat stocks_petroleum_us real_interest_rate rp trend ,robust
reg lp lnrgdp_usa  conv_imp_volat stocks_petroleum_us real_interest_rate rp  ,robust 
reg lp lnrgdp_usa volatility conv_imp_volat stocks_petroleum_us real_interest_rate rp  ,robust 

reg lp ip conv_imp_volat stocks_petroleum_us real_interest_rate rp trend ,robust
reg lp ip  conv_imp_volat stocks_petroleum_us real_interest_rate rp  ,robust 
reg lp ip volatility conv_imp_volat stocks_petroleum_us real_interest_rate rp  ,robust 

reg lp igrec conv_imp_volat stocks_petroleum_world real_interest_rate rp trend ,robust
reg lp igrec  conv_imp_volat stocks_petroleum_world real_interest_rate rp  ,robust 
reg lp igrec volatility conv_imp_volat stocks_petroleum_world real_interest_rate rp  ,robust 


*IV


ivreg lp lnrgdp_usa conv_imp_volat  real_interest_rate rp trend (stocks_petroleum_us= stocks_petroleum_us_lag) ,robust
ivreg lp lnrgdp_usa  volatility  real_interest_rate rp trend (stocks_petroleum_us=oil_shock stocks_petroleum_us_lag) ,robust
ivreg lp lnrgdp_usa conv_imp_volat volatility  real_interest_rate rp trend (stocks_petroleum_us= stocks_petroleum_us_lag) ,robust

ivreg lp ip conv_imp_volat  real_interest_rate rp trend (stocks_petroleum_us= stocks_petroleum_us_lag) ,robust
ivreg lp ip  volatility  real_interest_rate rp trend (stocks_petroleum_us=oil_shock stocks_petroleum_us_lag) ,robust
ivreg lp ip  conv_imp_volat volatility  real_interest_rate rp trend (stocks_petroleum_us= stocks_petroleum_us_lag) ,robust

ivreg lp igrec conv_imp_volat real_interest_rate rp trend (stocks_petroleum_world= stocks_petroleum_world_lag) ,robust
ivreg lp igrec volatility real_interest_rate rp trend (stocks_petroleum_world=oil_shock stocks_petroleum_world_lag) ,robust

qui log close



