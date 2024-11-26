* maize yield response data shared by Camila (from Meklit)
* try to show uncertainty via rainfall

cd "C:\DATA\Nigeria\EiA\"
ls 
clear all
/*
import delimited "NGA_Jordan.csv", numericcols(13 26 27 28 29 30) 
save "NGA_Jordan.dta", replace
*/
use  "NGA_Jordan.dta", clear

d
tab country
graph tw (kdensity yield if inlist(on_farm, "FALSE")) (kdensity yield if inlist(on_farm, "TRUE"))
tabstat yield, by(on_farm)


vioplot yield, over(on_farm) horizontal ///
title("Violin Plot of Yield") subtitle("By observation type") ///
ytitle("On farm?") ylab(, angle(0)) scheme(s2mono)

gen onfarm = inlist(on_farm, "TRUE")


reg yield c.n_fertilizer##c.n_fertilizer p_fertilizer k_fertilizer i.onfarm c.totalrf##c.totalrf // i.year
margins, dydx(n_fertilizer) at(n_fertilizer=(0(50)300))
marginsplot
margins, predict() at(n_fertilizer=(0(50)300))
marginsplot
margins, predict() at(totalrf=(200(100)1600))
marginsplot

