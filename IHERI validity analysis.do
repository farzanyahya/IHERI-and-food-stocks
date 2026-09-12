
tsset wdate

* ------------------------------------------------------------
* Event dates
* ------------------------------------------------------------

local e1 = wofd(mdy(1,3,2020))     // E1: Soleimani killing
local e2 = wofd(mdy(4,20,2020))    // E2: WTI oil price goes negative
local e3 = wofd(mdy(2,24,2022))    // E3: Russia invades Ukraine
local e4 = wofd(mdy(4,13,2024))    // E4: Iran's direct strike on Israel
local e5 = wofd(mdy(9,27,2024))    // E5: Nasrallah assassination
local e6 = wofd(mdy(6,13,2025))    // E6: 12-day Israel-Iran war begins
local e7 = wofd(mdy(2,28,2026))    // E7: 2026 Iran war begins, Khamenei killed

local zoom_start = wofd(mdy(1,1,2020))

* ------------------------------------------------------------
* Event-annotated timeline with E1-E7 labels and in-figure note key
* ------------------------------------------------------------

twoway (tsline iheri_aggregate, lcolor(navy) lwidth(medthick)) if wdate >= yw(2020,1), ///
    xline(`e1' `e2' `e3' `e4' `e5' `e6' `e7', lcolor(red) lpattern(dash)) ///
    text(23 `e1' "E1", size(small) place(e)) ///
    text(15 `e2' "E2", size(small) place(e)) ///
    text(19 `e3' "E3", size(small) place(e)) ///
    text(23 `e4' "E4", size(small) place(e)) ///
    text(19 `e5' "E5", size(small) place(e)) ///
    text(8  `e6' "E6", size(small) place(e)) ///
    text(23 `e7' "E7", size(small) place(w)) ///
    title("Iran-Hormuz Energy Risk Index (IHERI), Weekly, 2020-2026") ///
    ytitle("Index level") xtitle("") ///
    legend(off) ///
    note("E1 = Soleimani killing (Jan 3, 2020)" ///
         "E2 = WTI crude oil futures turn negative amid COVID-19 demand collapse (Apr 20, 2020)*" ///
         "E3 = Russia invades Ukraine (Feb 24, 2022)*" ///
         "E4 = Iran's direct missile/drone strike on Israel (Apr 13, 2024)" ///
         "E5 = Israeli assassination of Hezbollah leader Hassan Nasrallah (Sept 27, 2024)" ///
         "E6 = 12-day Israel-Iran war begins (Jun 13, 2025)" ///
         "E7 = 2026 Iran war begins, Khamenei killed (Feb 28, 2026)" ///
         "*Global oil-market shock, not Iran-specific, shown for context", ///
         size(vsmall) span) ///
    scheme(s1color)

graph export "IHERI_event_timeline_2020plus.png", replace width(2000) height(1400)

twoway (tsline iheri_c, lcolor(navy)) ///
       (tsline iheri_m, lcolor(maroon)) ///
       (tsline iheri_t, lcolor(orange)) if wdate >= yw(2020,1), ///
    title("IHERI Components Over Time") ///
    ytitle("Index level") xtitle("") ///
    legend(order(1 "Chokepoint" 2 "Market" 3 "Transport") position(6) rows(1)) ///
    scheme(s1color)

graph export "IHERI_components_timeline.png", replace width(2000)

twoway (tsline iheri_aggregate, lcolor(navy) yaxis(1)) ///
       (tsline GPR, lcolor(maroon) yaxis(2)) if wdate >= yw(2020,1), ///
    title("IHERI vs. Geopolitical Risk Index (GPR)") ///
    ytitle("IHERI", axis(1)) ytitle("GPR", axis(2)) xtitle("") ///
    legend(order(1 "IHERI (aggregate)" 2 "GPR")) ///
    scheme(s1color)

graph export "IHERI_vs_GPR.png", replace width(2000)

corr iheri_c iheri_m iheri_t iheri_aggregate OVX GPR GPR_ACT GPR_THREAT ///
     EMV_MACRO INFEXP_10Y INFEXP_2Y

* ------------------------------------------------------------
* 9. Correlation table - restricted to 2020+
*    with significance stars, formatted for export
* ------------------------------------------------------------

* Requires: ssc install estout, replace   (one-time, if not installed)

estpost corr iheri_c iheri_m iheri_t iheri_aggregate OVX GPR GPR_ACT ///
    GPR_THREAT EMV_MACRO INFEXP_10Y INFEXP_2Y if wdate >= yw(2020,1), matrix

esttab using "IHERI_correlation_table.rtf", ///
    unstack not noobs compress replace ///
    title("Correlation of IHERI with Established Uncertainty and Volatility Indicators (2020+)")

