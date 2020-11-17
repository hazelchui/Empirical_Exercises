
//////////////////////////////////////////////////////////////////////////////////////////////////////
// Section 1 Fixed Effects
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Load the dataset
clear
graph drop _all
cd "/Users/hazelchui/Desktop/demo" 
use FE_data.dta

//////////////////////////////////////////////////////////////////////////////////////////////////////
// Figures
//////////////////////////////////////////////////////////////////////////////////////////////////////
// # Prepare data for Figure 1
table year marriage race, contents(mean ln_wage) col replace name("wage")
drop if missing(marriage)
drop if race == 3

graph twoway (line wage1 year  if (marriage==0)&(race==1))  /// white unmarried
	(line wage1 year  if (marriage==1)&(race==1)), /// white married
    ytitle(log real wage) xtitle(Time) ///
	title("White")legend(order(1 "unmarried" 2 "married")) name (f_1a)

graph twoway (line wage1 year  if (marriage==0)&(race==2))  /// black unmarried
	(line wage1 year  if (marriage==1)&(race==2)), /// black married
    ytitle(log real wage) xtitle(Time) ///
	title("Black")legend(order(1 "unmarried" 2 "married")) name (f_1b)
	
grc1leg f_1a f_1b 


// # Prepare data for Figure 2
use FE_data.dta, clear
table year marriage collgrad, contents(mean ln_wage) col replace name("wage")
drop if missing(marriage)

graph twoway (line wage1 year  if (marriage==0)&(collgrad==0))  /// unmarried non-college graduates
	(line wage1 year  if (marriage==1)&(collgrad==0)), /// married non-college graduates 
    ytitle(log real wage) xtitle(Time) ///
	title("Non-college Graduate")legend(order(1 "unmarried" 2 "married")) name (f_2a)

graph twoway (line wage1 year  if (marriage==0)&(collgrad==1))  /// unmarried college graduates 
	(line wage1 year  if (marriage==1)&(collgrad==1)), ///  married college graduates
    ytitle(log real wage) xtitle(Time) ///
	title("College Graduate")legend(order(1 "unmarried" 2 "married")) name (f_2b)
	
grc1leg f_2a f_2b 
	

//////////////////////////////////////////////////////////////////////////////////////////////////////
// Estimation
//////////////////////////////////////////////////////////////////////////////////////////////////////
use FE_data.dta, clear

// Model 1
reg ln_wage marriage age union tenure hour i.race collgrad, r //simple robust SE
reg ln_wage marriage age union tenure hour i.race collgrad, vce(boot) //bootstrap SE


// Declare the dataset to be a panel 
xtset idcode year


// Model 2 -- Individual FE
xtreg ln_wage marriage age union tenure hour, fe i(idcode) vce(clu idcode) //clustered SE
xtreg ln_wage marriage age union tenure hour, fe i(idcode) vce(boot) //clustered boostrapped SE

// Model 3 -- Two-way FE 
xtreg ln_wage marriage age union tenure hour i.year, fe i(idcode) vce(clu idcode)
xtreg ln_wage marriage age union tenure hour i.year, fe i(idcode) vce(boot) 


// Model 4 -- Individual FE + industry
xtreg ln_wage marriage age tenure hour union i.ind_code, fe i(idcode) vce(cluster idcode)
xtreg ln_wage marriage age tenure hour union i.ind_code, fe i(idcode) vce(boot)


// Model 5 -- Two-way FE + industry
xtreg ln_wage marriage age tenure hour union i.ind_code i.year, fe i(idcode) vce(cluster idcode)
xtreg ln_wage marriage age tenure hour union i.ind_code i.year, fe i(idcode) vce(boot)


//////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////////////////
// Section 2 DiD
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Load the dataset
clear
graph drop _all
use DiD_data.dta

// drop useless columns
keep year fcode grant training
replace training = 0 if training >= .

// generate useful variables
//time
gen time = 1
replace time = 2 if year == 1988
replace time = 3 if year == 1989

// firm specific time trend
levelsof fcode
foreach lev in `r(levels)' {
	gen T`lev' = 0
	replace T`lev' = time if fcode == `lev'
}


// Estimation
reg training grant i.time i.fcode, vce(cluster fcode)
reg training grant i.time i.fcode T410032-T419486, vce(cluster fcode)


