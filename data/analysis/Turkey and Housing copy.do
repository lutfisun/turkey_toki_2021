clear
cd "/Users/lutfisun/Desktop/Turkey TOKI Project"

xtset provinceid year

macro define democraphictrends trend_population trend_migration
macro define economictrends trend_gdpk trend_housingstock trend_education

*ELECTION YEAR COEFFICIENTS
reg toki_log akpvoteshare if year==2003, robust
estimates store toki2003
reg toki_log akpvoteshare if year==2007, robust
estimates store toki2007
reg toki_log akpvoteshare if year==2011, robust
estimates store toki2011
reg toki_log akpvoteshare if year==2015, robust
estimates store toki2015
reg toki_log akpvoteshare if year==2016, robust
estimates store toki2016
reg toki_log akpvoteshare if year==2018, robust
estimates store toki2018

#delimit ;
coefplot (toki2003, label(2003)) (toki2007, label(2007)) (toki2011, label(2011)) (toki2015, label(2015)) 
(toki2016, label(2016)), keep(akpvoteshare) vertical yline(0)  xlabel(, nolabels) ytitle("Coefficient Estimate");

*TWO-WAY FIXED EFFECTS REGRESSIONS WITH TIME TRENDS (TABLE 1)

xtreg toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $democraphictrends, fe vce(cluster province)
xtreg toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $economictrends, fe vce(cluster province)
xtreg toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year timetrend*, fe vce(cluster province)
xtscc toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year timetrend*, fe
xtreg toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $democraphictrends, fe vce(cluster province)
xtreg toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $economictrends, fe vce(cluster province)
xtreg toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year timetrend*, fe vce(cluster province)
xtscc toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year timetrend*, fe

*NEGATIVE BINOMIAL MODELS (TABLE 2)

xtnbreg tokihouses akpvoteshare akpvoteshare_preerdogan yeardummy4-yeardummy17, fe 
xtnbreg tokihouses akpvoteshare akpvoteshare_preerdogan yeardummy4-yeardummy17, fe vce(bootstrap, dots(1))
xtnbreg tokihouses chpakpvotediff chpakpvotediff_interact yeardummy4-yeardummy17, fe 
xtnbreg tokihouses chpakpvotediff chpakpvotediff_interact yeardummy4-yeardummy17, fe vce(bootstrap, dots(1))

*DYNAMIC PANEL AND ANDERSON-HSIAO ESTIMATES WITH ONE LAG (TABLE 3)

xtreg toki_log l.toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $democraphictrends, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log])
xtreg toki_log l.toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $economictrends, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log])
xtreg toki_log l.toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year timetrend*, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log])
ivreg2 d.toki_log (ld.toki_log d.akpvoteshare d.akpvoteshare_preerdogan  = l2.toki_log l.akpvoteshare l.akpvoteshare_preerdogan) ld.akpmayor d.yeardummy*, cluster(province)
nlcom _b[d.akpvoteshare]/(1-_b[ld.toki_log])

xtreg toki_log l.toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $democraphictrends, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log])
xtreg toki_log l.toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $economictrends, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log])
xtreg toki_log l.toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year timetrend*, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log])
ivreg2 d.toki_log (ld.toki_log d.chpakpvotediff d.chpakpvotediff_interact = l2.toki_log l.chpakpvotediff l.chpakpvotediff_interact) ld.akpmayor d.yeardummy*, cluster(province)
nlcom _b[d.chpakpvotediff]/(1-_b[ld.toki_log])

*DYNAMIC PANEL, ANDERSON-HSIAO ESTIMATES AND GMM WITH FOUR LAGS (TABLE 4)

xtreg toki_log l(1/4).toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $democraphictrends, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtreg toki_log l(1/4).toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year $economictrends, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtreg toki_log l(1/4).toki_log akpvoteshare akpvoteshare_preerdogan l.akpmayor i.year timetrend*, fe vce(cluster province)
nlcom _b[akpvoteshare]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtabond2 toki_log l(1/4).toki_log akpvoteshare akpvoteshare_preerdogan  yeardummy* l.akpmayor, gmmstyle(toki_log, laglimits(2 5)) gmmstyle(akpvoteshare, laglimits(1 5)) gmmstyle(akpvoteshare_preerdogan, laglimits(1 5)) ivstyle(yeardummy* akpmayor, p) noleveleq robust nodiffsargan
nlcom _b[akpvoteshare]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])

xtreg toki_log l(1/4).toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $democraphictrends, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtreg toki_log l(1/4).toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year $economictrends, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtreg toki_log l(1/4).toki_log chpakpvotediff chpakpvotediff_interact l.akpmayor i.year timetrend*, fe vce(cluster province)
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])
xtabond2 toki_log l(1/4).toki_log chpakpvotediff chpakpvotediff_interact yeardummy* akpmayor, gmmstyle(toki_log, laglimits(2 5)) gmmstyle(chpakpvotediff, laglimits(1 5)) gmmstyle(chpakpvotediff_interact, laglimits(1 5)) ivstyle(yeardummy* l.akpmayor, p) noleveleq robust nodiffsargan
nlcom _b[chpakpvotediff]/(1-_b[l.toki_log]-_b[l2.toki_log]-_b[l3.toki_log]-_b[l4.toki_log])












*************LOCAL LEVEL DATA*****************

macro define controls akp_strongseat akp_greaterchp turnout_rate housespercapita pop_decrease

*TABLE 1 - total expenditure across all projects
//column 1 - no controls
xtreg sum_expndtpercapita l.akp i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp l.akpvotes_preerdogan i.year, fe vce(cluster province_district)
test l.akp + l.akpvotes_preerdogan = 0

//column 2 - controls concerning seats (turnout, majority seat)
xtreg sum_expndtpercapita l.akp l.akp_strongseat l.akp_greaterchp l.turnout_rate i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp l.akpvotes_preerdogan l.akp_strongseat l.akp_greaterchp l.turnout_rate i.year, fe vce(cluster province_district)
test l.akp + l.akpvotes_preerdogan = 0

//column 3 - demographic controls (previous TOKI housing stock and urbanisation)
xtreg sum_expndtpercapita l.akp l.housespercapita l.pop_decrease i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp l.akpvotes_preerdogan l.housespercapita l.pop_decrease i.year, fe vce(cluster province_district)
test l.akp + l.akpvotes_preerdogan = 0

//column 4 - all controls
xtreg sum_expndtpercapita l.akp l.($controls) i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp l.akpvotes_preerdogan l.($controls) i.year, fe vce(cluster province_district)
test l.akp + l.akpvotes_preerdogan = 0



*TABLE 2 - ROBUSTNESS CHECK REGARDING 2015 ELECTION (average 2015 and 2016 vote shares)
//column 1 - no controls
xtreg sum_expndtpercapita l.akp_2015robustness i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp_2015robustness l.akp2015robust_int i.year, fe vce(cluster province_district)
test l.akp_2015robustness + l.akp2015robust_int = 0

//column 2 - controls concerning seats (turnout, majority seat)
xtreg sum_expndtpercapita l.akp_2015robustness l.akp_strongseat l.akp_greaterchp l.turnout_rate i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp_2015robustness l.akp2015robust_int l.akp_strongseat l.akp_greaterchp l.turnout_rate i.year, fe vce(cluster province_district)
test l.akp_2015robustness + l.akp2015robust_int = 0

//column 3 - demographic controls (previous TOKI housing stock and urbanisation)
xtreg sum_expndtpercapita l.akp_2015robustness l.housespercapita l.pop_decrease i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp_2015robustness l.akp2015robust_int l.housespercapita l.pop_decrease i.year, fe vce(cluster province_district)
test l.akp_2015robustness + l.akp2015robust_int = 0

//column 4 - all controls
xtreg sum_expndtpercapita l.akp_2015robustness l.($controls) i.year, fe vce(cluster province_district)
xtreg sum_expndtpercapita l.akp_2015robustness l.akp2015robust_int l.($controls) i.year, fe vce(cluster province_district)
test l.akp_2015robustness + l.akp2015robust_int = 0

*TABLE .. - ROBUSTNESS CHECK (AVERAGING EXPENDITURE BETWEEN ELECTION YEARS)
bys province_district: egen expdntpc_interelect1 = mean(sum_expndtpercapita) if year>=2002 & year<=2006
bys province_district: egen expdntpc_interelect2 = mean(sum_expndtpercapita) if year>=2007 & year<=2010
bys province_district: egen expdntpc_interelect3 = mean(sum_expndtpercapita) if year>=2011 & year<=2014
bys province_district: egen expdntpc_interelect4 = mean(sum_expndtpercapita) if year>=2016 & year<=2017
bys province_district: egen expdntpc_interelect5 = mean(sum_expndtpercapita) if year>=2018 & year<=2020

gen expdntpc_interelect = expdntpc_interelect1 if year>=2002 & year<=2006
replace expdntpc_interelect = expdntpc_interelect2 if year>=2007 & year<=2010
replace expdntpc_interelect = expdntpc_interelect3 if year>=2011 & year<=2014
replace expdntpc_interelect = expdntpc_interelect4 if year>=2016 & year<=2017
replace expdntpc_interelect = expdntpc_interelect5 if year>=2018 & year<=2020
replace expdntpc_interelect = sum_expndtpercapita if year==2015

xtreg expdntpc_interelect l.akp i.year if year==2003 | year==2008 | year==2012 | year==2016 | year==2019, fe vce(cluster province_district)
xtreg expdntpc_interelect l.akp l.akpvotes_preerdogan i.year if year==2003 | year==2008 | year==2012 | year==2016 | year==2019, fe vce(cluster province_district)

xtreg expdntpc_interelect l.akp_2015robustness i.year if year==2003 | year==2008 | year==2012 | year==2016 | year==2019, fe vce(cluster province_district)
xtreg expdntpc_interelect l.akp_2015robustness l.akp2015robust_int i.year if year==2003 | year==2008 | year==2012 | year==2016 | year==2019, fe vce(cluster province_district)

*TABLE - AGGREGATE UP TO PROVINCE LEVEL

bys province: egen sum_expndtpc_prov1 = mean(sum_expndtpercapita) if year>=2002 & year<=2006
bys province: egen sum_expndtpc_prov2 = mean(sum_expndtpercapita) if year>=2007 & year<=2010
bys province: egen sum_expndtpc_prov3 = mean(sum_expndtpercapita) if year>=2011 & year<=2014
bys province: egen sum_expndtpc_prov4 = mean(sum_expndtpercapita) if year>=2016 & year<=2017
bys province: egen sum_expndtpc_prov5 = mean(sum_expndtpercapita) if year>=2018 & year<=2020
bys province: egen sum_expndtpc_prov6 = mean(sum_expndtpercapita) if year==2015

gen prov_sumexpndtpc = .
forvalues i = 1/6 {
	replace prov_sumexpndtpc = sum_expndtpc_prov`i' if sum_expndtpc_prov`i'!=.
}




xtabond2 sum_expndtpercapita l.sum_expndtpercapita l.akp dyear*, gmmstyle(sum_expndtpercapita, laglimits(2 .)) gmmstyle(akp, laglimits(2 .)) ivstyle(dyear*, p) noleveleq robust nodiffsargan































