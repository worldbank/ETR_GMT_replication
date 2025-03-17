/* ----------------------------------------------------------------------------------------- 
This do file creates GMT paper graphs.
 ----------------------------------------------------------------------------------------- */

local user="`c(username)'"

global main 		"C:/Users/`user'/"

global input 		"$main/Cross-Country/ETR/Replication Package/output/metadata"
global figures 		"$main/Cross-Country/ETR/Replication Package/output/figures"
global tables 		"$main/Cross-Country/ETR/Replication Package/output/tables"

graph set window fontface "Arial"

/* ========================================================================================== */
/* Figure 6 - Share of Tax + N. Groups/N. Entities - Orbis vs. CbCR top up (Year 1 Carveouts) */
/* ========================================================================================== */

local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==4
keep TopupLCU OriginalTaxBaseLCUbuilt Ngroupstaxed Nentitytaxed

ren OriginalTaxBaseLCUbuilt total_tax_liability_g	 
ren TopupLCU top_up_15_carv_orb
ren Ngroupstaxed n_g_carv_orb
ren Nentitytaxed n_e_carv_orb

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==4
keep TopupLCU Ngroupstaxed Nentitytaxed 

ren TopupLCU top_up_15_carv_cbc
ren Ngroupstaxed n_g_carv_cbc
ren Nentitytaxed n_e_carv_cbc

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

* GREECE is NA:
replace top_up_15_carv_orb = 0 if  country=="GRC"
replace n_g_carv_orb = 0 if  country=="GRC"
replace n_e_carv_orb = 0 if  country=="GRC"

gen sh1=top_up_15_carv_orb/total_tax_liability_g*100
gen sh2=top_up_15_carv_cbc/total_tax_liability_g*100

ren n_g_carv_cbc ng2
ren n_e_carv_cbc ne2
ren n_g_carv_orb ng1
ren n_e_carv_orb ne1

drop top_up_15_carv_orb top_up_15_carv_cbc total_tax_liability_g

reshape long sh ne ng, i(n) j(t)
order n country t sh

replace n=9 if country=="CRI"
replace n=7 if country=="GRC"
replace n=5 if country=="HND"
replace n=3 if country=="JAM"
replace n=1 if country=="ZAF"
replace n=n+.75 if t==2

gsort -n t

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

expand 2
gen N=_n
replace n=n-.375 if N>10

tostring sh, gen(sh_string)
replace sh_string = "NA" if sh_string == "0"


twoway 	(bar 	sh n if n==9.75 	& t==2, barw(.75) fcolor(midblue%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==9 		& t==1, barw(.75) fcolor(midblue%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==7.75 	& t==2, barw(.75) fcolor(forest_green%60) 	hor	lcolor(black%0)) ///
		(bar 	sh n if n==7 		& t==1, barw(.75) fcolor(forest_green%30)	hor	lcolor(black%0)) ///
		(bar 	sh n if n==5.75 	& t==2, barw(.75) fcolor(lavender%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==5 		& t==1, barw(.75) fcolor(lavender%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3.75 	& t==2, barw(.75) fcolor(dkorange%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3 		& t==1, barw(.75) fcolor(dkorange%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1.75 	& t==2, barw(.75) fcolor(cranberry%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1 		& t==1, barw(.75) fcolor(cranberry%30)		hor	lcolor(black%0)) ///
		(scatter n sh if n==9.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==8.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==7.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==6.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==5.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==4.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==3.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==2.625 	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==1.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter n sh if n==0.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)), ///
		ylabel(9.25 "Costa Rica" 7.25 "Greece" 5.25 "Honduras" 3.25 "Jamaica" 1.25 "South Africa*", angle(0) labsize(medlarge)) ///
		xtitle("") ytitle("") ///
		xlab(0 "0%" 5 "5%" 10 "10%" 15 "15%" 20 "20%" 25 "25%", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) ///
		text(3.75	18 "Dark bars: CbCR method", size(medlarge) box bcolor(dimgray%100) color(black%90) margin(l+1 r+1 t+1 b+1)) ///
		text(3 		18 "Light bars: Orbis method", size(medlarge) box bcolor(dimgray%40) color(black%90) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/F6_GMT_Share_Tax_Orbis_CbC_topup.png", replace width(1600) height(900)
		
		/* Export metadata */
		export excel "$figures/F6_a_metadata.xlsx", replace first(var)
		
/* Create string value for marker label for the number of entities */
gen p1="("
gen p2=")"
egen nel=concat(p1 ne p2), p()
drop p1 p2

gen nep=ng+5
tostring ng, gen(ng_string)
replace ng_string = "NA" if ng == 0
replace nel = "" if ne  == 0


twoway 	(bar 	ng n if n==9.75 	& t==2, barw(.75) fcolor(midblue%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==9 		& t==1, barw(.75) fcolor(midblue%60)			hor	lcolor(black%0)) ///
		(bar 	ng n if n==7.75 	& t==2, barw(.75) fcolor(forest_green%90) 	hor	lcolor(black%0)) ///
		(bar 	ng n if n==7 		& t==1, barw(.75) fcolor(forest_green%60)	hor	lcolor(black%0)) ///
		(bar 	ng n if n==5.75 	& t==2, barw(.75) fcolor(lavender%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==5 		& t==1, barw(.75) fcolor(lavender%60)		hor	lcolor(black%0)) ///
		(bar 	ng n if n==3.75 	& t==2, barw(.75) fcolor(dkorange%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==3 		& t==1, barw(.75) fcolor(dkorange%60)		hor	lcolor(black%0)) ///
		(bar 	ng n if n==1.75 	& t==2, barw(.75) fcolor(cranberry%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==1 		& t==1, barw(.75) fcolor(cranberry%60)		hor	lcolor(black%0)) ///
		(scatter n ng if n==9.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%80)) ///
		(scatter n ng if n==8.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%80)) ///
		(scatter n ng if n==7.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(forest_green%80)) ///
		(scatter n ng if n==6.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng_string) mlabposition(1)  mlabc(forest_green%80)) ///
		(scatter n ng if n==5.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%80)) ///
		(scatter n ng if n==4.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%80)) ///
		(scatter n ng if n==3.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%80)) ///
		(scatter n ng if n==2.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%80)) ///
		(scatter n ng if n==1.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%80)) ///
		(scatter n ng if n==0.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%80)) ///
		(scatter n nep if n==9.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n nep if n==8.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n nep if n==7.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n nep if n==6.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n nep if n==5.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n nep if n==4.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n nep if n==3.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n nep if n==2.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n nep if n==1.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter n nep if n==0.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(cranberry%40)), ///
		ylabel(9.25 "Costa Rica" 7.25 "Greece" 5.25 "Honduras" 3.25 "Jamaica" 1.25 "South Africa*", angle(0) labsize(medlarge)) ///
		xlabel(0 "0" 30 "30" 60 "60" 90 "90", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off) yscale(titlegap(2)) xscale(titlegap(4)) xtitle("") ytitle("") ///
		text(7.5	75 "N. Groups (N. Entities)", size(medlarge) color(black%90)) ///
		text(3.75	70 "Dark bars: CbCR method", size(medlarge) box bcolor(dimgray%100) color(black) margin(l+1 r+1 t+1 b+1)) ///
		text(3		70 "Light bars: Orbis method", size(medlarge) box bcolor(dimgray%40) color(black) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/F6_GMT_Share_N_G_E_Orbis_CbC_topup.png", replace width(1600) height(900)
		
		/* Export metadata */
		export excel "$figures/F6_b_metadata.xlsx", replace first(var)
		
/* ============================================================================================ */
/* Figure B2 - Share of Tax + N. Groups/N. Entities - Orbis vs. CbCR top up (Year 10 Carveouts) */
/* ============================================================================================ */

local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==2
keep TopupLCU OriginalTaxBaseLCUbuilt Ngroupstaxed Nentitytaxed

ren OriginalTaxBaseLCUbuilt total_tax_liability_g	 
ren TopupLCU top_up_15_carv_orb
ren Ngroupstaxed n_g_carv_orb
ren Nentitytaxed n_e_carv_orb

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==2
keep TopupLCU Ngroupstaxed Nentitytaxed 

ren TopupLCU top_up_15_carv_cbc
ren Ngroupstaxed n_g_carv_cbc
ren Nentitytaxed n_e_carv_cbc

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

* GREECE is NA:
replace top_up_15_carv_orb = 0 if  country=="GRC"
replace n_g_carv_orb = 0 if  country=="GRC"
replace n_e_carv_orb = 0 if  country=="GRC"

gen sh1=top_up_15_carv_orb/total_tax_liability_g*100
gen sh2=top_up_15_carv_cbc/total_tax_liability_g*100

ren n_g_carv_cbc ng2
ren n_e_carv_cbc ne2
ren n_g_carv_orb ng1
ren n_e_carv_orb ne1

drop top_up_15_carv_orb top_up_15_carv_cbc total_tax_liability_g

reshape long sh ne ng, i(n) j(t)
order n country t sh

replace n=9 if country=="CRI"
replace n=7 if country=="GRC"
replace n=5 if country=="HND"
replace n=3 if country=="JAM"
replace n=1 if country=="ZAF"
replace n=n+.75 if t==2

gsort -n t

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

expand 2
gen N=_n
replace n=n-.375 if N>10

tostring sh, gen(sh_string)
replace sh_string = "NA" if sh_string == "0"


twoway 	(bar 	sh n if n==9.75 	& t==2, barw(.75) fcolor(midblue%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==9 		& t==1, barw(.75) fcolor(midblue%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==7.75 	& t==2, barw(.75) fcolor(forest_green%60) 	hor	lcolor(black%0)) ///
		(bar 	sh n if n==7 		& t==1, barw(.75) fcolor(forest_green%30)	hor	lcolor(black%0)) ///
		(bar 	sh n if n==5.75 	& t==2, barw(.75) fcolor(lavender%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==5 		& t==1, barw(.75) fcolor(lavender%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3.75 	& t==2, barw(.75) fcolor(dkorange%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3 		& t==1, barw(.75) fcolor(dkorange%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1.75 	& t==2, barw(.75) fcolor(cranberry%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1 		& t==1, barw(.75) fcolor(cranberry%30)		hor	lcolor(black%0)) ///
		(scatter n sh if n==9.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==8.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==7.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==6.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==5.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==4.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==3.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==2.625 	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==1.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter n sh if n==0.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)), ///
		ylabel(9.25 "Costa Rica" 7.25 "Greece" 5.25 "Honduras" 3.25 "Jamaica" 1.25 "South Africa", angle(0) labsize(medlarge)) ///
		xtitle("") ytitle("") ///
		xlab(0 "0%" 5 "5%" 10 "10%" 15 "15%" 20 "20%" 25 "25%", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) ///
		text(3.75	18 "Dark bars: CbCR method", size(medlarge) box bcolor(dimgray%100) color(black%90) margin(l+1 r+1 t+1 b+1)) ///
		text(3 		18 "Light bars: Orbis method", size(medlarge) box bcolor(dimgray%40) color(black%90) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB2_GMT_Share_Tax_Orbis_CbC_topup_Y10.png", replace width(1600) height(900)
	
		/* Export metadata */
		export excel "$figures/FB2_a_metadata.xlsx", replace first(var)
		
/* Create string value for marker label for the number of entities */
gen p1="("
gen p2=")"
egen nel=concat(p1 ne p2), p()
drop p1 p2

gen nep=ng+5
tostring ng, gen(ng_string)
replace ng_string = "NA" if ng == 0
replace nel = "" if ne  == 0


twoway 	(bar 	ng n if n==9.75 	& t==2, barw(.75) fcolor(midblue%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==9 		& t==1, barw(.75) fcolor(midblue%60)			hor	lcolor(black%0)) ///
		(bar 	ng n if n==7.75 	& t==2, barw(.75) fcolor(forest_green%90) 	hor	lcolor(black%0)) ///
		(bar 	ng n if n==7 		& t==1, barw(.75) fcolor(forest_green%60)	hor	lcolor(black%0)) ///
		(bar 	ng n if n==5.75 	& t==2, barw(.75) fcolor(lavender%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==5 		& t==1, barw(.75) fcolor(lavender%60)		hor	lcolor(black%0)) ///
		(bar 	ng n if n==3.75 	& t==2, barw(.75) fcolor(dkorange%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==3 		& t==1, barw(.75) fcolor(dkorange%60)		hor	lcolor(black%0)) ///
		(bar 	ng n if n==1.75 	& t==2, barw(.75) fcolor(cranberry%90) 		hor	lcolor(black%0)) ///
		(bar 	ng n if n==1 		& t==1, barw(.75) fcolor(cranberry%60)		hor	lcolor(black%0)) ///
		(scatter n ng if n==9.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%80)) ///
		(scatter n ng if n==8.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%80)) ///
		(scatter n ng if n==7.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(forest_green%80)) ///
		(scatter n ng if n==6.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng_string) mlabposition(1)  mlabc(forest_green%80)) ///
		(scatter n ng if n==5.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%80)) ///
		(scatter n ng if n==4.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%80)) ///
		(scatter n ng if n==3.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%80)) ///
		(scatter n ng if n==2.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%80)) ///
		(scatter n ng if n==1.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%80)) ///
		(scatter n ng if n==0.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%80)) ///
		(scatter n nep if n==9.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n nep if n==8.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n nep if n==7.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n nep if n==6.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n nep if n==5.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n nep if n==4.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n nep if n==3.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n nep if n==2.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n nep if n==1.375 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter n nep if n==0.625	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(nel) mlabposition(1)  mlabc(cranberry%40)), ///
		ylabel(9.25 "Costa Rica" 7.25 "Greece" 5.25 "Honduras" 3.25 "Jamaica" 1.25 "South Africa*", angle(0) labsize(medlarge)) ///
		xlabel(0 "0" 30 "30" 60 "60" 90 "90", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off) yscale(titlegap(2)) xscale(titlegap(4)) xtitle("") ytitle("") ///
		text(7.5	75 "N. Groups (N. Entities)", size(medlarge) color(black%90)) ///
		text(3.75	70 "Dark bars: CbCR method", size(medlarge) box bcolor(dimgray%100) color(black) margin(l+1 r+1 t+1 b+1)) ///
		text(3		70 "Light bars: Orbis method", size(medlarge) box bcolor(dimgray%40) color(black) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB2_GMT_Share_N_G_E_Orbis_CbC_topup_Y10.png", replace width(1600) height(900)

		/* Export metadata */
		export excel "$figures/FB2_b_metadata.xlsx", replace first(var)
		
/* =============================================================== */
/* Figure 7 - CBCR (topup) - Share of tax under the five scenarios */
/* =============================================================== */

local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

/* ------------------------------------------------- */
/* Load GMT 15% estimates, with 5% and 10% carveouts */
/* ------------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==2 | v1==4
keep v1 TopupLCU OriginalTaxBaseLCUbuilt

ren OriginalTaxBaseLCUbuilt total_tax_liability_g
ren TopupLCU 				top_up_15_carv5_g
gen top_up_15_carv10_g	=	top_up_15_carv5_g[_n+1]
drop v1
drop if top_up_15_carv10_g==.

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ---------------------------------------------- */
/* Load GMT 15% estimates, no carveout, deminimis */
/* ---------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_cbcr_topup.csv", case(preserve) clear

keep if v1==2
keep TopupLCU
ren TopupLCU top_up_15_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ---------------------------------------------- */
/* Load GMT 15% estimates, 5% carveouts + credits */
/* ---------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup_QRTC.csv", case(preserve) clear		// IMPORTANT! CREDITS SCENARIO WITH NEW CODE NEEDS TO BE REVISED!

keep if Carveouts=="5%"
														// USING OLD CODE *OUTPUT* FOR NOW!
keep if GroupETR15=="Yes"
keep TopupLCU
ren TopupLCU top_up_15_carv5_credit_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ------------------------------------------------------------------------- */
/* Load GMT 20% estimates, no carveouts, no deminimis exclusion, group level */
/* ------------------------------------------------------------------------- */
import delimited "$input/GMT_20/`c'20_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==2
keep TopupLCU
ren TopupLCU top_up_20_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* ---------- */
/* Merge data */
/* ---------- */

use `t1', clear

merge 1:1 n using `t2', nogen
merge 1:1 n using `t3', nogen
merge 1:1 n using `t4', nogen
merge 1:1 n using `t5', nogen

order n  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g total_tax_liability_g top_up_20_g

/* ----------------------------------- */
/* Create bar graph - number of groups */
/* ----------------------------------- */

keep n country  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g total_tax_liability_g top_up_20_g


foreach v in  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g top_up_20_g{
		gen sh_`v'=`v'/total_tax_liability_g*100
		drop `v'
	}


drop total_tax_liability_g

local i=0
foreach var of varlist  sh_top_up_15_g sh_top_up_15_carv10_g sh_top_up_15_carv5_g sh_top_up_15_carv5_credit_g sh_top_up_20_g {
local i=`i'+1
	ren `var' `var'`i'
} 

reshape long  sh_top_up_15_g sh_top_up_15_carv5_g sh_top_up_15_carv10_g sh_top_up_15_carv5_credit_g sh_top_up_20_g, i(n) j(v)

gen sh=.
foreach var of varlist  sh_top_up_15_g sh_top_up_15_carv10_g sh_top_up_15_carv5_g sh_top_up_15_carv5_credit_g sh_top_up_20_g {
	replace sh=`var' if `var'!=.
}

keep v country sh

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

/* Fix y-axis values */
replace v=v-6
replace v=-v

* HND
replace sh = 0 if missing(sh) 
replace shx = "0" if shx == "."


/* ------------------------- */
/* Create graphs - Version 2 */
/* ------------------------- */

foreach c in CRI GRC HND JAM ZAF {

// Country specific adjustments
if "`c'"=="CRI" {
	local m = 15
	local g = 5
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 12 "12%" 24 "24%" 36 "36%" "'
	local ylab = `"	5 `" "15% Group +" "de minimis excl." "' 4 `" "Year 1" "Carve-outs" "' 3 `" "Year 10" "Carve-outs" "' 2 `" "Year 10 Carv." "+ Tax Credits" "Conversion" "' 1 `" "20% Group +" "Year 10" "Carve-outs" "' "'
	local color = "midblue"
	local width  = "1200"
	local height = "900"
}

if "`c'"=="JAM" {
	local m = .9
	local g = .3
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" .4 "0.4%" .8 "0.8%" 1.2 "1.2%" "'
	local ylab = `"	5 `" "15% Group +" "de minimis excl." "' 4 `" "Year 1" "Carve-outs" "' 3 `" "Year 10" "Carve-outs" "' 2 `" "Year 10 Carv." "+ Tax Credits" "Conversion" "' 1 `" "20% Group +" "Year 10" "Carve-outs" "' "'
	local color = "dkorange"
	local width  = "1200"
	local height = "900"
}

if "`c'"=="HND" {
	local m = 10
	local g = 2
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 4 "4%" 8 "8%" 12 "12%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "lavender"
	local width  = "1100"
	local height = "900"
}

if "`c'"=="GRC" {
	local m = 6
	local g = 2
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 4 "4%" 8 "8%" 12 "12%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "midblue"
	local color = "forest_green"
	local width  = "1100"
	local height = "900"
}

if "`c'"=="ZAF" {
	local m = 4
	local g = .8
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" .4 "0.4%" .8 "0.8%" 1.2 "1.2%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "cranberry"
	local width  = "1100"
	local height = "900"
}

twoway 	(bar 	 sh v if v==5 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==4 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==3 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==2 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==1 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(scatter v sh if v==5 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==4 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==3 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==2 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==1 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)), ///
		ylabel(`ylab', angle(0) labsize(medlarge)) ///
		xtitle("", size(medium)) graphregion(fcolor(white) lcolor(gs16)) ytitle("") ///
		xlab(`xlab', format(`format') labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) yscale(titlegap(2)) xscale(titlegap(4)) ///
		yline(2.5, lp(dash) lc(black%25)) ///
		legend(off) saving(`c', replace)
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy 
		graph export "$figures/F7_`c'_GMT_Share_Tax_Cbc_topup.png", replace width(`width') height(`height')
		
		/* Export metadata */
		export excel "$figures/F7_`c'_metadata.xlsx", replace first(var)
}

/* ================================================================= */
/* Figure B1 - Orbis (topup) - Share of tax under the five scenarios */
/* ================================================================= */

local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

/* ------------------------------------------------- */
/* Load GMT 15% estimates, with 5% and 10% carveouts */
/* ------------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==2 | v1==4
keep v1 TopupLCU OriginalTaxBaseLCUbuilt

ren OriginalTaxBaseLCUbuilt total_tax_liability_g
ren TopupLCU 				top_up_15_carv5_g
gen top_up_15_carv10_g	=	top_up_15_carv5_g[_n+1]
drop v1
drop if top_up_15_carv10_g==.

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ---------------------------------------------- */
/* Load GMT 15% estimates, no carveout, deminimis */
/* ---------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_orbis.csv", case(preserve) clear

keep if v1==2
keep TopupLCU
ren TopupLCU top_up_15_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ---------------------------------------------- */
/* Load GMT 15% estimates, 5% carveouts + credits */
/* ---------------------------------------------- */
import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis_QRTC.csv", case(preserve) clear		// IMPORTANT! CREDITS SCENARIO WITH NEW CODE NEEDS TO BE REVISED!

keep if Carveouts=="5%"
														// USING OLD CODE *OUTPUT* FOR NOW!
keep if GroupETR15=="Yes"
keep TopupLCU
ren TopupLCU top_up_15_carv5_credit_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

/* ------------------------------------------------------------------------- */
/* Load GMT 20% estimates, no carveouts, no deminimis exclusion, group level */
/* ------------------------------------------------------------------------- */
import delimited "$input/GMT_20/`c'20_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==2
keep TopupLCU
ren TopupLCU top_up_20_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* ---------- */
/* Merge data */
/* ---------- */

use `t1', clear

merge 1:1 n using `t2', nogen
merge 1:1 n using `t3', nogen
merge 1:1 n using `t4', nogen
merge 1:1 n using `t5', nogen

order n  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g total_tax_liability_g top_up_20_g

/* ----------------------------------- */
/* Create bar graph - number of groups */
/* ----------------------------------- */

keep n country  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g total_tax_liability_g top_up_20_g


foreach v in  top_up_15_g top_up_15_carv5_g top_up_15_carv10_g top_up_15_carv5_credit_g top_up_20_g{
		gen sh_`v'=`v'/total_tax_liability_g*100
		drop `v'
	}


drop total_tax_liability_g

local i=0
foreach var of varlist  sh_top_up_15_g sh_top_up_15_carv10_g sh_top_up_15_carv5_g sh_top_up_15_carv5_credit_g sh_top_up_20_g {
local i=`i'+1
	ren `var' `var'`i'
} 

reshape long  sh_top_up_15_g sh_top_up_15_carv5_g sh_top_up_15_carv10_g sh_top_up_15_carv5_credit_g sh_top_up_20_g, i(n) j(v)

gen sh=.
foreach var of varlist  sh_top_up_15_g sh_top_up_15_carv10_g sh_top_up_15_carv5_g sh_top_up_15_carv5_credit_g sh_top_up_20_g {
	replace sh=`var' if `var'!=.
}

keep v country sh

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

/* Fix y-axis values */
replace v=v-6
replace v=-v

* HND
replace sh = 0 if missing(sh) 
replace shx = "0" if shx == "."

/* ------------------------- */
/* Create graphs - Version 2 */
/* ------------------------- */

replace sh=. if country=="GRC"

foreach c in CRI JAM HND ZAF {

// Country specific adjustments
if "`c'"=="CRI" {
	local m = 15
	local g = 5
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 12 "12%" 24 "24%" 36 "36%" "'
	local ylab = `"	5 `" "15% Group +" "de minimis excl." "' 4 `" "Year 1" "Carve-outs" "' 3 `" "Year 10" "Carve-outs" "' 2 `" "Year 10 Carv." "+ Tax Credits" "Conversion" "' 1 `" "20% Group +" "Year 10" "Carve-outs" "' "'
	local color = "midblue"
	local width  = "1200"
	local height = "900"
}

if "`c'"=="JAM" {
	local m = .9
	local g = .3
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" .4 "0.4%" .8 "0.8%" 1.2 "1.2%" "'
	local ylab = `"	5 `" "15% Group +" "de minimis excl." "' 4 `" "Year 1" "Carve-outs" "' 3 `" "Year 10" "Carve-outs" "' 2 `" "Year 10 Carv." "+ Tax Credits" "Conversion" "' 1 `" "20% Group +" "Year 10" "Carve-outs" "' "'
	local color = "dkorange"
	local width  = "1200"
	local height = "900"
}

if "`c'"=="HND" {
	local m = 10
	local g = 2
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 1.5 "1.5%" 3 "3%" 4.5 "4.5%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "lavender"
	local width  = "1100"
	local height = "900"
}

if "`c'"=="GRC" {
	local m = 6
	local g = 2
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" 2 "2%" 4 "4%" 6 "6%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "forest_green"
	local width  = "1100"
	local height = "900"
}

if "`c'"=="ZAF" {
	local m = 4
	local g = .8
	local format = "%12.0fc"
	local formatt = "%12.2fc"
	local xlab = `" 0 "0%" .4 "0.4%" .8 "0.8%" 1.2 "1.2%" "'
	local ylab = `"	5 " " 4 " " 3 " " 2 " " 1 " " "'
	local color = "cranberry"
	local width  = "1100"
	local height = "900"
}

twoway 	(bar 	 sh v if v==5 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==4 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==3 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==2 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==1 & country=="`c'", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(scatter v sh if v==5 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==4 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==3 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==2 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==1 & country=="`c'", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)), ///
		ylabel(`ylab', angle(0) labsize(medlarge)) ///
		xtitle("", size(medium)) graphregion(fcolor(white) lcolor(gs16)) ytitle("") ///
		xlab(`xlab', format(`format') labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) yscale(titlegap(2)) xscale(titlegap(4)) ///
		yline(2.5, lp(dash) lc(black%25)) ///
		legend(off) saving(`c', replace)
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy 
		graph export "$figures/FB1_`c'_GMT_Share_Tax_Orbis_topup.png", replace width(`width') height(`height')
		
		/* Export metadata */
		export excel "$figures/FB1_`c'_metadata.xlsx", replace first(var)
}

twoway 	(bar 	 sh v if v==5 & country=="GRC", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==4 & country=="GRC", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==3 & country=="GRC", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==2 & country=="GRC", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(bar 	 sh v if v==1 & country=="GRC", barw(.75) fcolor(`color'%40) 	hor	lcolor(orange%0)) ///
		(scatter v sh if v==5 & country=="GRC", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==4 & country=="GRC", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==3 & country=="GRC", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==2 & country=="GRC", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)) ///
		(scatter v sh if v==1 & country=="GRC", msymbol(none) mlabf(`formatt')  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(`color'%40)), ///
		ylabel(5 " " 4 " " 3 " " 2 " " 1 " ", angle(0) labsize(medlarge)) ///
		xtitle("", size(medium)) graphregion(fcolor(white) lcolor(gs16)) ytitle("") ///
		xlab(0 "0%" 2 "2%" 4 "4%" 6 "6%", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) yscale(titlegap(2)) xscale(titlegap(4)) ///
		yline(2.5, lp(dash) lc(black%25)) ///
		legend(off) saving(GRC, replace) ///
		text(1		.5 "NA", size(medlarge) color(forest_green%40)) ///
		text(2		.5 "NA", size(medlarge) color(forest_green%40)) ///
		text(3		.5 "NA", size(medlarge) color(forest_green%40)) ///
		text(4		.5 "NA", size(medlarge) color(forest_green%40)) ///
		text(5		.5 "NA", size(medlarge) color(forest_green%40)) 
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy 
		graph export "$figures/FB1_GRC_GMT_Share_Tax_Orbis_topup.png", replace width(1100) height(900)
		
		/* Export metadata */
		export excel "$figures/FB1_GRC_metadata.xlsx", replace first(var)

/* ============================================================ */
**# Figure B3 - All countries, 10% carveouts, groups vs entites */
/* ============================================================ */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==4
keep TopupLCU OriginalTaxBaseLCU

ren OriginalTaxBaseLCU total_tax_liability_g	 
ren TopupLCU top_up_15_carv_g

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis_nogroup.csv", case(preserve) clear

keep if v1==4
keep TopupLCU

ren TopupLCU top_up_15_carv_e

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

gen sh1=top_up_15_carv_g/total_tax_liability_g*100
gen sh2=top_up_15_carv_e/total_tax_liability_g*100

drop top_up_15_carv_g top_up_15_carv_e total_tax_liability_g

* GREECE is NA:
replace sh2 = 0 if  country=="GRC"
replace sh1 = 0 if  country=="GRC"

reshape long sh, i(n) j(t)
order n country t sh

replace n=9 if country=="CRI"
replace n=7 if country=="GRC"
replace n=5 if country=="HND"
replace n=3 if country=="JAM"
replace n=1 if country=="ZAF"
replace n=n+.75 if t==2

gsort -n t

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

expand 2
gen N=_n
replace n=n-.375 if N>10

tostring sh, gen(sh_string)
replace sh_string = "NA" if sh_string == "0"


twoway 	(bar 	sh n if n==9.75 	& t==2, barw(.75) fcolor(midblue%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==9 		& t==1, barw(.75) fcolor(midblue%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==7.75 	& t==2, barw(.75) fcolor(forest_green%60) 	hor	lcolor(black%0)) ///
		(bar 	sh n if n==7 		& t==1, barw(.75) fcolor(forest_green%30)	hor	lcolor(black%0)) ///
		(bar 	sh n if n==5.75 	& t==2, barw(.75) fcolor(lavender%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==5 		& t==1, barw(.75) fcolor(lavender%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3.75 	& t==2, barw(.75) fcolor(dkorange%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==3 		& t==1, barw(.75) fcolor(dkorange%30)		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1.75 	& t==2, barw(.75) fcolor(cranberry%60) 		hor	lcolor(black%0)) ///
		(bar 	sh n if n==1 		& t==1, barw(.75) fcolor(cranberry%30)		hor	lcolor(black%0)) ///
		(scatter n sh if n==9.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==8.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter n sh if n==7.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==6.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter n sh if n==5.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==4.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter n sh if n==3.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==2.625 	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter n sh if n==1.375 	& t==2, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter n sh if n==0.625	& t==1, msymbol(none) mlabf(%12.2fc)  mlabs(medlarge) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)), ///
		ylabel(9.25 "Costa Rica" 7.25 "Greece" 5.25 "Honduras" 3.25 "Jamaica" 1.25 "South Africa*", angle(0) labsize(medlarge)) ///
		xtitle("") ytitle("") ///
		xlab(0 "0%" 5 "5%" 10 "10%" 15 "15%" 20 "20%" 25 "25%", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) ///
		text(3.75	19 "Dark bars: Entity level ", size(medlarge) box bcolor(dimgray%100) color(black) margin(l+1.75 r+1.75 t+1 b+1)) ///
		text(3		19 "Light bars: Group level", size(medlarge) box bcolor(dimgray%40) color(black) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB3_GMT_Share_Tax_Group_Entity.png", replace
		
		/* Export metadata */
		export excel "$figures/FB3_metadata.xlsx", replace first(var)
		
/* =================================================================================================== */
/* Figure B4a - Orbis - Number of groups (15% no exclusion, 15% deminimis) - all countries (VERTICAL)  */
/* =================================================================================================== */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_orbis.csv", case(preserve) clear

keep if v1==2
keep Ngroupstaxed
ren  Ngroupstaxed 	n_g_15_no_carv_g

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step2_topup_orbis.csv", case(preserve) clear

keep if v1==2
keep Ngroupstaxed
ren  Ngroupstaxed  	n_g_15_no_carv_noex_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

ren n_g_15_no_carv_noex_g 	ng1
ren n_g_15_no_carv_g 		ng2


* GREECE is NA:
replace ng1 = 0 if  country=="GRC"
replace ng2 = 0 if  country=="GRC"


reshape long ng, i(n) j(t)
order n country t ng

/* Re do the x-axis var */
replace n=1 if country=="CRI"
replace n=3 if country=="GRC"
replace n=5 if country=="HND"
replace n=7 if country=="JAM"
replace n=9 if country=="ZAF"
replace n=n+.75 if t==2

expand 2
gen N=_n
replace n=n-.1875 if N>10
replace n=n-.1875 if N>18

tostring ng, gen(ng_string)
replace ng_string = "NA" if ng_string == "0"
	
twoway 	(bar 	ng n if n==1 		& t==1, barw(.75) fcolor(midblue%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==1.75 	& t==2, barw(.75) fcolor(midblue%30)			lcolor(black%0)) ///
		(bar 	ng n if n==3 		& t==1, barw(.75) fcolor(forest_green%60) 	lcolor(black%0)) ///
		(bar 	ng n if n==3.75 	& t==2, barw(.75) fcolor(forest_green%30)	lcolor(black%0)) ///
		(bar 	ng n if n==5 		& t==1, barw(.75) fcolor(lavender%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==5.75 	& t==2, barw(.75) fcolor(lavender%30)		lcolor(black%0)) ///
		(bar 	ng n if n==7 		& t==1, barw(.75) fcolor(dkorange%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==7.75 	& t==2, barw(.75) fcolor(dkorange%30)		lcolor(black%0)) ///
		(bar 	ng n if n==9 		& t==1, barw(.75) fcolor(cranberry%60) 		lcolor(black%0) yaxis(2)) ///
		(bar 	ng n if n==9.75		& t==2, barw(.75) fcolor(cranberry%30)		lcolor(black%0) yaxis(2)) ///
		(scatter ng n if n==.8125	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter ng n if n==1.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter ng n if n==2.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter ng n if n==3.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter ng n if n==4.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter ng n if n==5.5625 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter ng n if n==6.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter ng n if n==7.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter ng n if n==8.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%40) yaxis(2)) ///
		(scatter ng n if n==9.375	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%40) yaxis(2)), ///
		xlab(1.25 "Costa Rica" 3.25 "Greece" 5.25 "Honduras" 7.25 "Jamaica" 9.25 `" "South Africa*" "(right)" "', angle(0) labsize(medium)) ///
		ytitle(" ", axis(2)) ytitle(" ") xtitle(" ") ///
		ylab(0(100)400, format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin) axis(2)) ///
		ylab(0(20)80, format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) xline(8.25, lp(dash) lc(black%25))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB4_a_GMT_N_Groups_Orbis_V.png", replace
		
		/* Export metadata */
		export excel "$figures/FB4_a_metadata.xlsx", replace first(var)

/* ======================================================================================================= */
/* Figure B4c - CbCR topup - Number of groups (15% no exclusion, 15% deminimis) - all countries (VERTICAL) */
/* ======================================================================================================= */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_cbcr_topup.csv", case(preserve) clear

keep if v1==2
keep Ngroupstaxed
ren  Ngroupstaxed 	n_g_15_no_carv_g

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step2_topup_cbcr_topup.csv", case(preserve) clear

keep if v1==2
keep Ngroupstaxed
ren  Ngroupstaxed  	n_g_15_no_carv_noex_g

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

ren n_g_15_no_carv_noex_g 	ng1
ren n_g_15_no_carv_g 		ng2

reshape long ng, i(n) j(t)
order n country t ng

/* Re do the x-axis var */
replace n=1 if country=="CRI"
replace n=3 if country=="GRC"
replace n=5 if country=="HND"
replace n=7 if country=="JAM"
replace n=9 if country=="ZAF"
replace n=n+.75 if t==2

expand 2
gen N=_n
replace n=n-.1875 if N>10
replace n=n-.1875 if N>18

	
twoway 	(bar 	ng n if n==1 		& t==1, barw(.75) fcolor(midblue%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==1.75 	& t==2, barw(.75) fcolor(midblue%30)		lcolor(black%0)) ///
		(bar 	ng n if n==3 		& t==1, barw(.75) fcolor(forest_green%60) 	lcolor(black%0)) ///
		(bar 	ng n if n==3.75 	& t==2, barw(.75) fcolor(forest_green%30)	lcolor(black%0)) ///
		(bar 	ng n if n==5 		& t==1, barw(.75) fcolor(lavender%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==5.75 	& t==2, barw(.75) fcolor(lavender%30)		lcolor(black%0)) ///
		(bar 	ng n if n==7 		& t==1, barw(.75) fcolor(dkorange%60) 		lcolor(black%0)) ///
		(bar 	ng n if n==7.75 	& t==2, barw(.75) fcolor(dkorange%30)		lcolor(black%0)) ///
		(bar 	ng n if n==9 		& t==1, barw(.75) fcolor(cranberry%60) 		lcolor(black%0) yaxis(2)) ///
		(bar 	ng n if n==9.75		& t==2, barw(.75) fcolor(cranberry%30)		lcolor(black%0) yaxis(2)) ///
		(scatter ng n if n==.8125	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter ng n if n==1.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter ng n if n==2.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter ng n if n==3.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter ng n if n==4.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter ng n if n==5.5625 	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter ng n if n==6.8125 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter ng n if n==7.5625	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter ng n if n==8.625 	& t==1, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%40) yaxis(2)) ///
		(scatter ng n if n==9.375	& t==2, msymbol(none) mlabf(%12.0fc)  mlabs(medlarge) mlabel(ng) mlabposition(1)  mlabc(cranberry%40) yaxis(2)), ///
		xlab(1.25 "Costa Rica" 3.25 "Greece" 5.25 "Honduras" 7.25 "Jamaica" 9.25 `" "South Africa" "(right)" "', angle(0) labsize(medlarge)) ///
		ytitle(" ", axis(2)) ytitle(" ") xtitle(" ") ///
		ylab(0(100)400, format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin) axis(2)) ///
		ylab(0(50)200, format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) /////
		graphregion(fcolor(white) lcolor(gs16)) legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) xline(8.25, lp(dash) lc(black%25))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB4_c_GMT_N_Groups_CbCR_V_topup.png", replace

		/* Export metadata */
		export excel "$figures/FB4_c_metadata.xlsx", replace first(var)
		
/* ================================================================================================== */
/* Figure B4B - Orbis - Share of Profits (15% no exclusion, 15% deminimis) - all countries (VERTICAL) */	
/* ================================================================================================== */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_orbis.csv", case(preserve) clear

keep if GroupETR15== "Yes"
keep OriginalAggregateProfitLCU ProfitsLCU
ren ProfitsLCU prof
ren OriginalAggregateProfitLCU total_prof

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step2_topup_orbis.csv", case(preserve) clear

keep if GroupETR15== "Yes"
keep OriginalAggregateProfitLCU ProfitsLCU
ren ProfitsLCU prof_noex
ren OriginalAggregateProfitLCU total_prof_noex

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

gen sh1=prof_noex/total_prof*100
gen sh2=prof/total_prof*100

drop total_prof_noex prof_noex prof total_prof

gsort -sh1
drop n
gen n=_n

reshape long sh, i(n) j(t)
order n country t sh

* GREECE is NA:
replace sh = 0 if  country=="GRC"

replace n=1 if country=="CRI"
replace n=3 if country=="GRC"
replace n=5 if country=="HND"
replace n=7 if country=="JAM"
replace n=9 if country=="ZAF"
replace n=n+.75 if t==2

tostring sh, gen(shx) force
gen shxn = substr(shx,1,3)
destring shxn, replace
drop sh
ren shxn sh

expand 2
gen N=_n
replace n=n-.375 if N>10

tostring sh, gen(sh_string)
replace sh_string = "NA" if sh_string == "0"

replace sh_string="17.1" if sh_string=="17" & country=="CRI"

twoway 	(bar 	sh n if n==1 		& t==1, barw(.75) fcolor(midblue%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==1.75 	& t==2, barw(.75) fcolor(midblue%30)		lcolor(black%0)) ///
		(bar 	sh n if n==3 		& t==1, barw(.75) fcolor(forest_green%60) 	lcolor(black%0)) ///
		(bar 	sh n if n==3.75 	& t==2, barw(.75) fcolor(forest_green%30)	lcolor(black%0)) ///
		(bar 	sh n if n==5 		& t==1, barw(.75) fcolor(lavender%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==5.75 	& t==2, barw(.75) fcolor(lavender%30)		lcolor(black%0)) ///
		(bar 	sh n if n==7 		& t==1, barw(.75) fcolor(dkorange%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==7.75 	& t==2, barw(.75) fcolor(dkorange%30)		lcolor(black%0)) ///
		(bar 	sh n if n==9 		& t==1, barw(.75) fcolor(cranberry%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==9.75		& t==2, barw(.75) fcolor(cranberry%30)		lcolor(black%0)) ///
		(scatter sh n if n==.625	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter sh n if n==1.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter sh n if n==2.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter sh n if n==3.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter sh n if n==4.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter sh n if n==5.375 	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter sh n if n==6.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter sh n if n==7.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter sh n if n==8.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter sh n if n==9.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medlarge) mlabel(sh_string) mlabposition(1)  mlabc(cranberry%40)), ///
		xlab(1.25 "Costa Rica" 3.25 "Greece" 5.25 "Honduras" 7.25 "Jamaica" 9.25 `" "South Africa*" " " "', angle(0) labsize(medlarge)) ///
		ytitle(" ", size(medium)) ///
		ylab(0 "0%" 5 "5%" 10 "10%" 15 "15%" 20 "20%", format(%12.1fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) xtitle("") legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4)) ///
		text(16.75	8 "Dark bars: No {it:de minimis} ", size(medlarge) box bcolor(dimgray%100) color(black) margin(l+2 r+2 t+1 b+1)) ///
		text(15		8 "Light bars: {it:de minimis} excl.", size(medlarge) box bcolor(dimgray%40) color(black) margin(l+1.75 r+1.75 t+1 b+1))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB4_b_GMT_Share_Profits_Orbis_V.png", replace	

		/* Export metadata */
		export excel "$figures/FB4_b_metadata.xlsx", replace first(var)
		
/* ======================================================================================================= */
/* Figure FAd - CbCR (topup) Share of Profits (15% no exclusion, 15% deminimis) - all countries (VERTICAL) */		
/* ======================================================================================================= */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step3_topup_nocarv_cbcr_topup.csv", case(preserve) clear

keep if GroupETR15== "Yes"
keep OriginalAggregateProfitLCU ProfitsLCU
ren ProfitsLCU prof
ren OriginalAggregateProfitLCU total_prof

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step2_topup_cbcr_topup.csv", case(preserve) clear

keep if GroupETR15== "Yes"
keep OriginalAggregateProfitLCU ProfitsLCU
ren ProfitsLCU prof_noex
ren OriginalAggregateProfitLCU total_prof_noex

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3' 
append using `t4'
append using `t5'

gen sh1=prof_noex/total_prof*100
gen sh2=prof/total_prof*100

drop total_prof_noex prof_noex prof total_prof

gsort -sh1
drop n
gen n=_n

reshape long sh, i(n) j(t)
order n country t sh

replace n=1 if country=="CRI"
replace n=3 if country=="GRC"
replace n=5 if country=="HND"
replace n=7 if country=="JAM"
replace n=9 if country=="ZAF"
replace n=n+.75 if t==2 

tostring sh, gen(shx) force
gen shxn = substr(shx,1,4)
destring shxn, replace
drop sh
ren shxn sh

expand 2
gen N=_n
replace n=n-.375 if N>10

twoway 	(bar 	sh n if n==1 		& t==1, barw(.75) fcolor(midblue%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==1.75 	& t==2, barw(.75) fcolor(midblue%30)		lcolor(black%0)) ///
		(bar 	sh n if n==3 		& t==1, barw(.75) fcolor(forest_green%60) 	lcolor(black%0)) ///
		(bar 	sh n if n==3.75 	& t==2, barw(.75) fcolor(forest_green%30)	lcolor(black%0)) ///
		(bar 	sh n if n==5 		& t==1, barw(.75) fcolor(lavender%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==5.75 	& t==2, barw(.75) fcolor(lavender%30)		lcolor(black%0)) ///
		(bar 	sh n if n==7 		& t==1, barw(.75) fcolor(dkorange%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==7.75 	& t==2, barw(.75) fcolor(dkorange%30)		lcolor(black%0)) ///
		(bar 	sh n if n==9 		& t==1, barw(.75) fcolor(cranberry%60) 		lcolor(black%0)) ///
		(bar 	sh n if n==9.75		& t==2, barw(.75) fcolor(cranberry%30)		lcolor(black%0)) ///
		(scatter sh n if n==.625	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter sh n if n==1.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(midblue%40)) ///
		(scatter sh n if n==2.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter sh n if n==3.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(forest_green%40)) ///
		(scatter sh n if n==4.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter sh n if n==5.375 	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(lavender%40)) ///
		(scatter sh n if n==6.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter sh n if n==7.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(dkorange%40)) ///
		(scatter sh n if n==8.625 	& t==1, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)) ///
		(scatter sh n if n==9.375	& t==2, msymbol(none) mlabf(%12.1fc)  mlabs(medium) mlabel(sh) mlabposition(1)  mlabc(cranberry%40)), ///
		xlab(1.25 "Costa Rica" 3.25 "Greece" 5.25 "Honduras" 7.25 "Jamaica" 9.25 `" "South Africa" " " "', angle(0) labsize(medlarge)) ///
		ytitle(" ", size(medium)) ///
		ylab(0 "0%" 5 "5%" 10 "10%" 15 "15%" 20 "20%", format(%12.0fc) labsize(medlarge) grid gstyle(dot) glcolor(gray%70) glw(medthin)) ///
		graphregion(fcolor(white) lcolor(gs16)) xtitle("") legend(off)  ///
		yscale(titlegap(2)) xscale(titlegap(4))
		gr_edit .plotregion1.style.editstyle boxstyle(linestyle(color(none))) editcopy
		graph export "$figures/FB4_d_GMT_Share_Profits_CbCR_V_topup.png", replace
		
		/* Export metadata */
		export excel "$figures/FB4_d_metadata.xlsx", replace first(var)
		
/* ===================================================================================== */
**# GMT Table - Share of profits, share of entities liable of top up tax, and meean ETRs */
/* ===================================================================================== */

/* Rest of the countries */
local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_aggregate_statistics.csv", case(preserve) clear

keep if v1==2
keep n_entities

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step5_topup_carv_orbis.csv", case(preserve) clear

keep if v1==4
keep Nentitytaxed meanETRgroup ProfitsLCU OriginalAggregateProfitLCU
ren  Nentitytaxed  					n_entities_taxed
ren  meanETRgroup  					mean_etr
ren  ProfitsLCU  					profits
ren  OriginalAggregateProfitLCU		agg_profits

gen n=`i'
merge 1:1 n using `t`i'', nogen

/* Save data */
tempfile t`i'
save `t`i'', replace

import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==4
keep Nentitytaxed meanETRgroup ProfitsLCU OriginalAggregateProfitLCU
ren  Nentitytaxed  					n_entities_taxed_cbc
ren  meanETRgroup  					mean_etr_cbc
ren  ProfitsLCU  					profits_cbc
ren  OriginalAggregateProfitLCU		agg_profits_cbc

gen n=`i'
merge 1:1 n using `t`i'', nogen

tempfile t`i'
save `t`i'', replace
}


/* Append data */
use `t1', clear
append using `t2'
append using `t3' 
append using `t4'
append using `t5'

gen mean_etrx = substr(mean_etr,1,4)
destring mean_etrx, replace
drop mean_etr
ren mean_etrx mean_etr

gen mean_etrx = substr(mean_etr_cbc,1,4)
destring mean_etrx, replace
drop mean_etr_cbc
ren mean_etrx mean_etr_cbc

replace n_entities=70915 if country=="GRC"

gen sh_filers=n_entities_taxed/n_entities*100
gen sh_profits=profits/agg_profits*100

gen sh_filers_cbc=n_entities_taxed_cbc/n_entities*100
gen sh_profits_cbc=profits_cbc/agg_profits*100

keep country mean_etr sh_filers sh_profits mean_etr_cbc sh_filers_cbc sh_profits_cbc

replace country="Costa Rica" if country=="CRI"
replace country="Jamaica" if country=="JAM"
replace country="Honduras" if country=="HND"
replace country="Greece" if country=="GRC"
replace country="South Africa" if country=="ZAF"

sort country

gen n=_n

foreach i in 1 2 3 4 5 {
		
	qui summ mean_etr 						if `i'==n
	local etr_`i'	=round(`r(mean)', 0.01)

	qui summ sh_filers 						if `i'==n
	local sh_f_`i'=round(`r(mean)', 0.001)
	local sh_f_`i': di %15.2fc `sh_f_`i''
	
	qui summ sh_profits 					if `i'==n
	local sh_pr_`i'=round(`r(mean)', 0.01)
	local sh_pr_`i': di %15.2fc `sh_pr_`i''
	
}

foreach i in 1 2 3 4 5 {
		
	qui summ mean_etr_cbc 						if `i'==n
	local etr_`i'b	=round(`r(mean)', 0.01)

	qui summ sh_filers_cbc 						if `i'==n
	local sh_f_`i'b=round(`r(mean)', 0.001)
	local sh_f_`i'b: di %15.2fc `sh_f_`i'b'
	
	qui summ sh_profits_cbc 					if `i'==n
	local sh_pr_`i'b=round(`r(mean)', 0.01)
	local sh_pr_`i'b: di %15.2fc `sh_pr_`i'b'
	
}

foreach i in 1 2 3 4 5 {	
	di `etr_`i''
	di `sh_f_`i''
	di `sh_pr_`i''
	di `etr_`i'b'
	di `sh_f_`i'b'
	di `sh_pr_`i'b'
}

/* Make table */
capture file close myfile
file open myfile using "$tables/TB1_GMT_Stats.tex", write replace
file write myfile ///
" \\ \\ [-1.5ex] " _n ///
" \hline\hline \\ [-1.5ex] " _n ///
" {} & (1) & (2) & (3)  \\ " _n ///
" [1ex] \\ [-1.5ex] " _n ///
"                	& Mean         		& Share     & Share     & Mean         		& Share     & Share		\\" _n ///
" Coutry      		& Effective       	& of 		& of     	& Effective       	& of 		& of		\\" _n ///
"                	& Tax Rate   		& Filers   	& Profits   & Tax Rate   		& Filers   	& Profits   \\" _n ///
"\\																	  " _n ///
" [1ex] \hline \\ [-1.5ex]											  " _n ///
" Costa Rica 		& `etr_1'			& `sh_f_1'	& `sh_pr_1'	& `etr_1b'			& `sh_f_1b'	& `sh_pr_1b'	\\" _n ///
" Greece 			& NA				& NA 		& NA		& `etr_2b'			& `sh_f_2b'	& `sh_pr_2b'	\\" _n ///
" Honduras 			& `etr_3'			& `sh_f_3'	& `sh_pr_3'	& `etr_3b'			& `sh_f_3b'	& `sh_pr_3b'	\\" _n ///
" Jamaica 			& `etr_4'			& `sh_f_4'	& `sh_pr_4'	& `etr_4b'			& `sh_f_4b'	& `sh_pr_4b'		\\" _n ///
" South Africa*		& `etr_5'			& `sh_f_5'	& `sh_pr_5'	& `etr_5b'			& `sh_f_5b'	& `sh_pr_5b'	\\" _n ///
" [1ex] \hline\hline \\ [-1.5ex] 														  "
capture file close myfile

/* =================================================== */
/* TA9 data generation on the number of entities taxed */
/* =================================================== */

local i=0
foreach c in CRI JAM HND GRC ZAF {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_step5_topup_carv_cbcr_topup.csv", case(preserve) clear

keep if v1==4
keep TopupLCU OriginalTaxBaseLCUbuilt Ngroupstaxed Nentitytaxed

ren OriginalTaxBaseLCUbuilt total_tax_liability_g	 
ren TopupLCU top_up_15_cbcr
ren Ngroupstaxed n_g_cbcr
ren Nentitytaxed n_e_cbcr

gen country="`c'"

gen n=`i'

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2'
append using `t3'
append using `t4'
append using `t5'

/* Save data */
keep country n_g_cbcr n_e_cbcr

export excel "$input\Entities_Taxed_TA9.xlsx", replace first(var)

/* ============================================================================ */
/* TA9 data generation - list of entities taxed to identify those in the top 1% */
/* ============================================================================ */

local i=0
foreach c in CRI JAM HND GRC {
local i=`i'+1

import delimited "$input/GMT_15/`c'15_List_Carveouts_Final_cbcr_topup10.csv", case(preserve) clear

keep if top_up>0
ren group group_m
keep group_m
tostring group_m, replace format("%12.0f")

gen liable_cbcrc_y1=1

export excel "$input/`c'_Groups_cbcr_TA9.xlsx", replace first(var)

gen country="`c'"

/* Save data */
tempfile t`i'
save `t`i'', replace

}

/* Append data */
use `t1', clear
append using `t2', force
append using `t3', force
append using `t4', force

export excel "$input\List_Groups_cbcr_TA9.xlsx", replace first(var)

/* Erase temporary files */
cap erase "$main/Cross-Country/ETR/Replication Package/codes/CRI.gph"
cap erase "$main/Cross-Country/ETR/Replication Package/codes/GRC.gph"
cap erase "$main/Cross-Country/ETR/Replication Package/codes/HND.gph"
cap erase "$main/Cross-Country/ETR/Replication Package/codes/ZAF.gph"
cap erase "$main/Cross-Country/ETR/Replication Package/codes/JAM.gph"