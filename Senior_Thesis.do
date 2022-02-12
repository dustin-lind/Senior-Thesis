cd "C:\Users\dusti\OneDrive\Documents\Senior Thesis Repo\Senior-Thesis"

clear
use Compustat_AF_1962_2020.dta
drop GVKEY indfmt consol popsrc datafmt curcd costat fyr
drop rdip
rename LPERMNO PERMNO 
gen year=year(datadate)
gen month=month(datadate)
drop datadate 
sort PERMNO year month
duplicates tag PERMNO fyear, gen(tag)
sum tag
drop tag
save Compustat_for_merging, replace

clear
use CRSP_MS_1962_2020.dta
drop PRC SHROUT 
gen year=year(date)
gen month=month(date)
sort PERMNO year month
keep if EXCHCD==1 | EXCHCD==2 | EXCHCD==3 
	/* NYSE, AMEX, NASDAQ */
drop EXCHCD
drop if SHRCD>12	/* confirm the codes remaining are 10, 11, 12 */
drop date SHRCD
joinby PERMNO year month using Compustat_for_merging
duplicates tag PERMNO year month, gen(tag)
sum tag	/* no duplicates */
drop tag
duplicates tag PERMNO fyear, gen(tag)
sum tag
drop if tag>0	/* 5 cases; 4 have no R&D; drop them */
drop tag
xtset PERMNO fyear 
gen period=ym(year,month)	/* 246 is July 1980; 599 is Dec 2009 */
replace xrd=0 if xrd<0	/* in a few cases, R&D is negative */
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D intensity */
sum l.rds sg if (period>=246)*(period<=599) /* July 1980-Dec 2009 */
sum l.rds sg if (period>=318)*(period<=599)	
	/* July 1985 as a starting point to allow for a 5-year lag */
	
/*
Ability Regressions
*/

sort PERMNO fyear
gen b1=.
gen b2=.
gen b3=.
gen b4=.
gen b5=.

forvalues i=1(1)5 {
	forvalues t=1978(1)2020 {		

		gen reg_period=(fyear<=`t')*(fyear>=(`t'-7)) /* period for sg */
		gen x_use=l`i'.rds if reg_period
		gen y_use=sg if reg_period
		replace x_use=. if y_use==.
		replace y_use=. if x_use==.
		
		gen rds_nm_chk=(x_use<.)*reg_period
		gen rds_pos_chk=(x_use<.)*(x_use>0)*reg_period
		by PERMNO: egen rds_nm_ind=total(rds_nm_chk)
		by PERMNO: egen rds_pos_ind=total(rds_pos_chk)
		gen perc_pos=rds_pos_ind/8
			/* It isn't clear whether they mean half of all observations
			or half of nonmissing ones (in which case replace 8 with rds_nm_ind) */
		
		by PERMNO: egen mean_x=mean(x_use)
		by PERMNO: egen mean_y=mean(y_use)
		gen dev_x=x_use-mean_x
		gen dev_y=y_use-mean_y
		gen yx_term=dev_x*dev_y
		gen xx_term=dev_x*dev_x
		by PERMNO: egen sum_yx=total(yx_term)
		by PERMNO: egen sum_xx=total(xx_term)
		replace b`i'=sum_yx/sum_xx if (fyear==`t')*(rds_nm_ind>=6)*(perc_pos>=.5)
	
		/* reg sg i.permno i.permno#c.l.rds if reg_period*(rds_nm_ind>=6)*(perc_pos>=.5) */
			/* use this regression with the appropriate lag to confirm it is computing b correctly */

		/* continue, break */	/* use to end the loop to check computations */	
	
		drop reg_period rds_nm_chk rds_pos_chk rds_nm_ind rds_pos_ind perc_pos x_use y_use mean_x mean_y dev_x dev_y yx_term xx_term sum_yx sum_xx
	}
}
gen num_b=(b1<.)+(b2<.)+(b3<.)+(b4<.)+(b5<.)
mvencode b1 b2 b3 b4 b5, mv(0) override
gen avg_b=(b1+b2+b3+b4+b5)/num_b
sum avg_b if (period>=246)*(period<=599)
sum avg_b if (fyear>=1989)*(fyear<=2009)
sum avg_b if (period>=246)*(period<=599)*(num_b==5)
/* I don't know how they're getting SDs that are so low, and the mean is off too */
save b_construction, replace