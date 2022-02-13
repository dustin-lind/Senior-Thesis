cd c:\Rsrch1\Monthly_Returns\Data\Cohen
set matsize 11000
set maxvar 32767

/*******************************************************************************
The web suggests using the CRSP/Compustat merged data is the best way to
obtain Compustat data that can be merged with CRSP data. 

Use the LU and LC links (the default in WRDS). WRDS mentions LS but it
appears to be for ETFs.

The CRSP data needs to be downloaded separately; the sole advantage of the
CCM data appears to be that sometimes mergers or other transactions occur
that make it difficult to tie securities to firms; the merged data does this
for the Compustat data to allow linking to CRSP using permno.

Download the annual fundamentals and monthly security data from 1960-present.
The Cohen et al. paper is so poorly written I lack confidence that they have
even described their sample period correctly. They say July 1980 to Dec 2009
but their ability regressions require 8 years of data and they run them
using a lag of up to 5 years which suggests years 5-12 are examined. It might be
that this is what they're doing, but then starting in July 1980 with a 12
year lag means 1992-2009 is the effective period. Otherwise, if they really
mean the regression starts in 1980, then they use lags going back to the
1960s to generate the ability measures.

It doesn't seem to matter what I do, I cannot get anywhere close to the
SDs they report for R&D instensity and sales growth. I can get close to
the mean R&D intensity they report but not the mean sales growth.
*******************************************************************************/

clear
use Compustat_M_1960_2015
drop gvkey liid indfmt consol popsrc datafmt cusip curcd costat fyr
drop rdip /* might use later but not now */
rename lpermno permno  /* lpermno is "linking" permno */
gen year=year(datadate)
gen month=month(datadate)
drop datadate exchg	/* defer to CRSP for exchange */
sort permno year month
duplicates tag permno fyear, gen(tag)
sum tag	/* calendar year has many duplicates (the fiscal year changes)*/
	/* fyear has far fewer */
drop tag
save Compustat_for_merging, replace

clear
use CRSP_1960_2015
drop prc shrout ret
gen year=year(date)
gen month=month(date)
sort permno year month
keep if exchcd==1 | exchcd==2 | exchcd==3 
	/* NYSE, AMEX, NASDAQ */
drop exchcd
drop if shrcd>12	/* confirm the codes remaining are 10, 11, 12 */
drop date shrcd
joinby permno year month using Compustat_for_merging
duplicates tag permno year month, gen(tag)
sum tag	/* no duplicates */
drop tag
duplicates tag permno fyear, gen(tag)
sum tag
drop if tag>0	/* 5 cases; 4 have no R&D; drop them */
drop tag
xtset permno fyear 
gen period=ym(year,month)	/* 246 is July 1980; 599 is Dec 2009 */
replace xrd=0 if xrd<0	/* in a few cases, R&D is negative */
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D intensity */
sum l.rds sg if (period>=246)*(period<=599) /* July 1980-Dec 2009 */
sum l.rds sg if (period>=318)*(period<=599)	
	/* July 1985 as a starting point to allow for a 5-year lag */
	
/*******************************************************************************
Turn attention to the ability regressions. The text suggests they check R&D
intensity for missing values but not sales growth. Perhaps sales growth is
generally observed when R&D intensity is, but I'll check both.

In the ability regressions, we don't actually need to run the regression;
we just need the slope coefficient. The formula for the slope coefficient
in a binary regression is:

beta = sum(1,n) (Xi-Xbar)(Yi-Ybar)/(sum(1,n) (Xi-Xbar)^2)

This can be computed as the sample covariance of x and y divided by the
sample variance of x (the numerator and denominator would be divided
by n or n-1, and the appropriate n might differ if observations on y are
missing when observations on x are not, but that is not an issue in this data).
*******************************************************************************/

sort permno fyear
gen b1=.
gen b2=.
gen b3=.
gen b4=.
gen b5=.

forvalues i=1(1)5 {
	forvalues t=1978(1)2015 {		

		gen reg_period=(fyear<=`t')*(fyear>=(`t'-7)) /* period for sg */
		gen x_use=l`i'.rds if reg_period
		gen y_use=sg if reg_period
		replace x_use=. if y_use==.
		replace y_use=. if x_use==.
		
		gen rds_nm_chk=(x_use<.)*reg_period
		gen rds_pos_chk=(x_use<.)*(x_use>0)*reg_period
		by permno: egen rds_nm_ind=total(rds_nm_chk)
		by permno: egen rds_pos_ind=total(rds_pos_chk)
		gen perc_pos=rds_pos_ind/8
			/* It isn't clear whether they mean half of all observations
			or half of nonmissing ones (in which case replace 8 with rds_nm_ind) */
		
		by permno: egen mean_x=mean(x_use)
		by permno: egen mean_y=mean(y_use)
		gen dev_x=x_use-mean_x
		gen dev_y=y_use-mean_y
		gen yx_term=dev_x*dev_y
		gen xx_term=dev_x*dev_x
		by permno: egen sum_yx=total(yx_term)
		by permno: egen sum_xx=total(xx_term)
		replace b`i'=sum_yx/sum_xx if (fyear==`t')*(rds_nm_ind>=6)*(perc_pos>=.5)
	
		/* reg sg i.permno i.permno#c.l.rds if reg_period*(rds_nm_ind>=6)*(perc_pos>=.5) */
			/* use this regression with the appropriate lag to confirm it is computing b correctly */

		/* continue, break */	/* use to end the loop to check computations */	
	
		drop reg_period rds_nm_chk rds_pos_chk rds_nm_ind rds_pos_ind perc_pos x_use y_use mean_x mean_y dev_x dev_y yx_term xx_term sum_yx sum_xx
	}
}
gen num_b=(b1<.)+(b2<.)+(b3<.)+(b4<.)+(b5<.)
/* what does the above notation mean? */
mvencode b1 b2 b3 b4 b5, mv(0) override
gen avg_b=(b1+b2+b3+b4+b5)/num_b
sum avg_b if (period>=246)*(period<=599)
sum avg_b if (fyear>=1989)*(fyear<=2009)
sum avg_b if (period>=246)*(period<=599)*(num_b==5)
/* I don't know how they're getting SDs that are so low, and the mean is off too */
save b_construction, replace

clear
use b_construction
gen year_chk=year
replace year_chk=year+1 if month<=6
	/* page 642: use year t-1 if the fyr ends July-Dec and use year t-2 if the fyr ends Jan-June */
sort year_chk
by year_chk: egen avg_b_80=pctile(avg_b), p(80)
	/* they use the top quintile of ability */
by year_chk: egen rds_70=pctile(rds), p(70)
	/* they use a 70% cutoff for R&D intensity */
	
duplicates tag permno year_chk, gen(tag)
drop if tag>0
drop tag

xtset permno year_chk
sort permno year_chk
gen portfolio_firm=(avg_b>=avg_b_80)*(rds>=rds_70)*(avg_b<.)*(rds<.)
keep if portfolio_firm==1
keep conm comnam year_chk
	/* year_chk is the modified year variable - it is the actual year if the month is July-Dec; otherwise the previous year */
sort year_chk conm
gen count_obs=1
collapse (sum) count_obs, by(year_chk)
sum count_obs if (year>=1980)*(year<=2009)
	/* the mean is 10, as in the paper! */
	
/*******************************************************************************
Construct the annual CRSP returns July-June to merge with the portfolio
firms so we can look at which firms accounted for the high performance.
*******************************************************************************/

clear
use CRSP_1960_2015
gen year=year(date)
gen month=month(date)
keep if exchcd==1 | exchcd==2 | exchcd==3 
	/* NYSE, AMEX, NASDAQ */
drop exchcd
drop if shrcd>12	/* confirm the codes remaining are 10, 11, 12 */
drop date shrcd
gen period=ym(year,month)	/* 246 is July 1980; 599 is Dec 2009 */
sort permno period
xtset permno period
#delimit ;
gen one_yr_ret=(1+ret)*(1+f1.ret)*(1+f2.ret)*(1+f3.ret)*(1+f4.ret)*(1+f5.ret)*
	(1+f6.ret)*(1+f7.ret)*(1+f8.ret)*(1+f9.ret)*(1+f10.ret)*(1+f11.ret)-1;
#delimit cr
keep if month==7	/* July */
keep permno year one_yr_ret
drop if one_yr_ret==.
sort permno year
save one_yr_rets, replace

clear
use b_construction
gen year_chk=year
replace year_chk=year+1 if month<=6
	/* page 642: use year t-1 if the fyr ends July-Dec and use year t-2 if the fyr ends Jan-June */
sort year_chk
by year_chk: egen avg_b_80=pctile(avg_b), p(80)
	/* they use the top quintile of ability */
by year_chk: egen rds_70=pctile(rds), p(70)
	/* they use a 70% cutoff for R&D intensity */
	
duplicates tag permno year_chk, gen(tag)
drop if tag>0
drop tag

gen portfolio_firm=(avg_b>=avg_b_80)*(rds>=rds_70)*(avg_b<.)*(rds<.)
keep if portfolio_firm==1

replace year=year+1
	/* to match with the returns data: the lagged avg_b and rds are required */
sort permno year
joinby permno year using one_yr_rets
sort year one_yr_ret
keep permno comnam comn year one_yr_ret	
	/* give this data to Caroline */
	/* keep in mind 1979 is inaccurate because I excluded 1977 from the b calculations */





























permno#cL.rds |
       10006  |    60.0468   97.12585     0.62   0.536    -130.3526    250.4462
       10057  |   120.9432   73.14405     1.65   0.098    -22.44375    264.3302
       10145  |   17.96464   47.88401     0.38   0.708    -75.90416    111.8334
       10154  |   5.844469   6.908045     0.85   0.398    -7.697626    19.38657
       10161  |  -10.90077   23.90153    -0.46   0.648    -57.75583    35.95428
       10162  |     4.4272   1.507682     2.94   0.003     1.471636    7.382765
       10189  |    2.43035   13.78373     0.18   0.860    -24.59041    29.45111
       10241  |   2.782175   40.77716     0.07   0.946     -77.1548    82.71915
       10321  |    32.3499   47.97434     0.67   0.500    -61.69597    126.3958
       10364  |   84.03537   107.7498     0.78   0.435    -127.1904    295.2612
       10372  |  -2.511914   16.03368    -0.16   0.876    -33.94333     28.9195
       10401  |  -8.478078   245.6552    -0.03   0.972     -490.045    473.0889
       10445  |  -13.28249   32.61506    -0.41   0.684      -77.219    50.65401
       10460  |   5.668243   35.72043     0.16   0.874    -64.35583    75.69232
       10479  |   67.26355   88.23445     0.76   0.446    -105.7057    240.2328
       10487  |   17.33978   44.23451     0.39   0.695    -69.37476    104.0543
       10488  |   1.728993   5.485856     0.32   0.753    -9.025133    12.48312
       10604  |    127.133   96.23593     1.32   0.187    -61.52181    315.7879
       10613  |   6.399607   2.353216     2.72   0.007     1.786511     11.0127
       10640  |   11.34976   12.07297     0.94   0.347    -12.31734    35.01686
       10656  |   129.5125   101.4822     1.28   0.202    -69.42685    328.4518
       10664  |  -180.5513   53.34524    -3.38   0.001    -285.1259   -75.97664
       10672  |  -29.12664   32.69025    -0.89   0.373    -93.21054    34.95725
       10751  |   3.721248   7.294342     0.51   0.610    -10.57812    18.02062
       10786  |   44.49711   80.21894     0.55   0.579     -112.759    201.7533
       10808  |   10.14619   8.416435     1.21   0.228    -6.352858    26.64524
       10874  |  -1.869839    40.7308    -0.05   0.963    -81.71594    77.97626
       10890  |   -10.2784   19.90222    -0.52   0.606    -49.29346    28.73667
       10971  |    5.47499   5.036182     1.09   0.277    -4.397624     15.3476
       10989  |   36.51131   76.21196     0.48   0.632    -112.8898    185.9124
       11157  |   4.106805   4.225399     0.97   0.331      -4.1764    12.39001
       11164  |  -5.961802   13.49583    -0.44   0.659    -32.41818    20.49458
       11165  |    12.9599   20.32863     0.64   0.524    -26.89105    52.81086
       11260  |  -35.72682   19.50806    -1.83   0.067    -73.96918    2.515544
       11308  |   41.38875   161.5066     0.26   0.798    -275.2186    357.9961
       11332  |   19.82331   26.80801     0.74   0.460    -32.72942    72.37605
       11447  |   5.744021   39.30375     0.15   0.884    -71.30456     82.7926
       11471  |   35.04024   44.05806     0.80   0.426    -51.32841    121.4089
       11607  |  -19.82143   211.6747    -0.09   0.925    -434.7751    395.1322
       11703  |   3.358645   9.478651     0.35   0.723    -15.22271    21.93999
       11754  |  -8.180532   31.99481    -0.26   0.798    -70.90114    54.54008
       11763  |  -.3627485   1.225951    -0.30   0.767    -2.766025    2.040528
       11850  |    250.647   190.5968     1.32   0.189    -122.9869    624.2809
       11851  |   -3.95572   13.57462    -0.29   0.771    -30.56656    22.65512
       12044  |   150.9732    316.097     0.48   0.633    -468.6833    770.6298
       12052  |   14.21912   70.26597     0.20   0.840    -123.5259    151.9641
       12060  |   12.14147   64.63517     0.19   0.851    -114.5652    138.8482
       12079  |   .3026194   13.71191     0.02   0.982    -26.57734    27.18258
       12087  |   30.59792   72.81159     0.42   0.674    -112.1373    173.3332
       12095  |  -1.536824   16.52332    -0.09   0.926     -33.9281    30.85445
       12140  |   2.019492    26.9603     0.07   0.940    -50.83177    54.87076
       12141  |  -6.514169   8.479649    -0.77   0.442    -23.13714     10.1088
       12431  |   17.23473   21.14244     0.82   0.415    -24.21158    58.68104
       12490  |   2.615886    19.1601     0.14   0.891    -34.94436    40.17613
       12503  |  -38.53015   25.76235    -1.50   0.135    -89.03303    11.97273
       12511  |  -82.87446   60.32473    -1.37   0.170    -201.1313    35.38233
       12546  |   6.476866    12.6763     0.51   0.609    -18.37296    31.32669
       12570  |   1.167912   26.35098     0.04   0.965    -50.48889    52.82471
       12706  |   46.64048   19.85285     2.35   0.019     7.722202    85.55876
       12730  |   31.70379   217.3681     0.15   0.884    -394.4108    457.8184
       12837  |  -36.54694   81.78791    -0.45   0.655    -196.8788    123.7849
       12918  |  -.2379654   40.37309    -0.01   0.995    -79.38282    78.90689
       13047  |   51.04649   58.11915     0.88   0.380    -62.88663    164.9796
       13290  |  -1.658681   28.83065    -0.06   0.954    -58.17647     54.8591
       13303  |   19.63071   42.26088     0.46   0.642    -63.21487    102.4763
       13311  |   39.21236   53.01742     0.74   0.460    -64.71964    143.1444
       13354  |   80.33778   125.6379     0.64   0.523    -165.9548    326.6303
       13398  |   2.018372   7.049333     0.29   0.775     -11.8007    15.83744
       13557  |   -53.2128   68.57201    -0.78   0.438     -187.637    81.21144
       13610  |   12.24004   55.12281     0.22   0.824    -95.81924    120.2993
       13661  |   .6656668   19.01389     0.04   0.972    -36.60796     37.9393
       13928  |   47.37574    58.0686     0.82   0.415    -66.45828    161.2098
       13953  |  -124.0221   86.45548    -1.43   0.151     -293.504    45.45973
       14066  |    88.6128   115.0099     0.77   0.441    -136.8454     314.071
       14090  |  -13.41781   60.76438    -0.22   0.825    -132.5365    105.7008
       14198  |   9.655062   10.40261     0.93   0.353    -10.73757    30.04769
       14227  |   1.919669   2.404944     0.80   0.425    -2.794831    6.634169
       14250  |   59.58868   96.75788     0.62   0.538    -130.0894    249.2667
       14277  |  -.5556655   17.36492    -0.03   0.974    -34.59676    33.48543
       14286  |   1.476343   4.804791     0.31   0.759    -7.942666    10.89535
       14357  |   21.35638   41.49834     0.51   0.607    -59.99435    102.7071
       14525  |  -.9838019   9.214528    -0.11   0.915    -19.04738    17.07978
       14526  |  -21.19142    109.517    -0.19   0.847    -235.8816    193.4987
       14533  |  -83.48336   152.9249    -0.55   0.585    -383.2677     216.301
       14541  |    85.1726   45.72629     1.86   0.063     -4.46634    174.8115
       14569  |   -8.59135   14.61957    -0.59   0.557    -37.25063    20.06793
       14592  |  -11.86928   60.69085    -0.20   0.845    -130.8438    107.1052
       14605  |   12.78511   13.95628     0.92   0.360    -14.57391    40.14412
       14656  |   9.039513   32.47553     0.28   0.781    -54.62346    72.70248
       14702  |   8.805459   3.030877     2.91   0.004      2.86392      14.747
       14736  |   89.01561   54.05418     1.65   0.100     -16.9488      194.98
       14760  |   23.44509   81.46931     0.29   0.774    -136.2622    183.1524
       14795  |   27.63325   121.4348     0.23   0.820      -210.42    265.6865
       14891  |   67.92798   60.59754     1.12   0.262    -50.86362    186.7196
       14912  |   17.07916   16.83827     1.01   0.310    -15.92953    50.08785
       15069  |   103.2007   131.6054     0.78   0.433    -154.7902    361.1916
       15077  |   14.32205    77.7751     0.18   0.854    -138.1433    166.7875
       15202  |   123.0457   140.7704     0.87   0.382    -152.9117     399.003
       15203  |   8.223644   13.24947     0.62   0.535    -17.74978    34.19706
       15229  |   43.52253   65.75224     0.66   0.508    -85.37401    172.4191
       15325  |   1.176934   46.29018     0.03   0.980    -89.56742    91.92129
       15368  |   .1970081   27.79887     0.01   0.994    -54.29815    54.69217
       15472  |  -22.28856   82.52785    -0.27   0.787    -184.0709    139.4938
       15579  |  -1.796767   6.328776    -0.28   0.776     -14.2033    10.60977
       15659  |    4.58616   42.63647     0.11   0.914    -78.99568      88.168
       15667  |   2.008436   13.82812     0.15   0.885    -25.09934    29.11621
       15712  |  -16.79965     65.112    -0.26   0.796    -144.4411    110.8418
       15763  |  -4.692075   13.66225    -0.34   0.731    -31.47468    22.09053
       15959  |   -37.5566   7.276726    -5.16   0.000    -51.82144   -23.29176
       15975  |   7.562931   4.892627     1.55   0.122    -2.028267    17.15413
       16088  |   67.81933   126.5365     0.54   0.592    -180.2348    315.8734

















gen lrevt=l.revt
forvalues i=1(1)12 {
	gen l`i'rds=ln(1+l`i'.xrd/l`i'.revt)
}
sum l1rds sg if (period>=246)*(period<=599)

/*******************************************************************************
Turn attention to the ability regressions. The text suggests they check R&D
intensity for missing values but not sales growth. Perhaps sales growth is
generally observed when R&D intensity is? Yes.
*******************************************************************************/

/* gen sg_m_xrd_n=(revt==.)*(xrd<.)
sum sg_m_xrd_n
keep if sg_m_xrd_n */	
/* only 5 cases in their sample period and only 1 has pos R&D */

forvalues i=1(1)5 {
	gen reg`i'_chk_m=(l`i'rds<.)	/* non-missing */
	gen reg`i'_chk_0=(l`i'rds>0)*(l`i'rds<.)	/* positive and non-missing */
	local j=`i'+1
	local k=`i'+7
	forvalues p=`j'(1)`k' {
		replace reg`i'_chk_m=reg`i'_chk_m+(l`p'rds<.)
		replace reg`i'_chk_0=reg`i'_chk_0+(l`p'rds>0)*(l`p'rds<.)	
	}
}
forvalues i=1(1)5 {
	gen reg`i'_chk_g0=reg`i'_chk_0/reg`i'_chk_m
}
mvencode reg*_chk_g0, mv(0) override










forvalues i=400(1)409 {		/* try some regressions */

	gen reg_period=(period==`i')*period
	gen m_1_chk=reg1_chk_m if (period==`i')
	gen zero_1_chk=reg1_chk_0 if (period==`i')







}





































gen sg_l1rds_nm=(sg<.)*(l1rds<.)
gen sg_l2rds_nm=(sg<.)*(l2rds<.)
gen sg_l3rds_nm=(sg<.)*(l3rds<.)
gen sg_l4rds_nm=(sg<.)*(l4rds<.)
gen sg_l5rds_nm=(sg<.)*(l5rds<.)









































































/*******************************************************************************
Using annual data the SDs are much higher than those in the paper. Perhaps the
authors merged the monthly CRSP and annual Compustat data and then computed
summary stats without considering that they basically just replicated the 
revenues and R&D across months within each fiscal year. This would keep the 
means roughly the same (which is like what we're getting) but reduce the SDs.

The CRSP/Compustat merged data on monthly securities appears to lack the share
codes used to focus on common shares, so just use the CRSP data.

The CRSP/Compustat annual fundamentals data includes lpermno, which the FAQs
suggest can be matched to permno. Try this to see if it eliminates duplicate
observations; the documentation suggests it might help. 
*******************************************************************************/

clear
use CRSP_1980_2009
gen month=month(date)
gen year=year(date)
drop if shrcd>12
drop shrcd
/* keep if primexch=="N" | primexch=="Q" | primexch=="A" */	
	/* this appears insufficient because it still includes exchange codes
	that suggest trading has been halted or suspended or just that the
	issue was on these exchanges when listed (but may not be anymore?);
	use the exchcd */
keep if exchcd==1 | exchcd==2 | exchcd==3 
	/* NYSE, AMEX, NASDAQ */
drop primexch exchcd
sort permno year month
duplicates tag permno year month, gen(tag)
sum tag	/* no duplicates */
drop tag
replace prc=abs(prc)
save CRSP_data, replace /* 2,169,751 obs */

clear
use Compustat_1980_2009
drop indfmt consol popsrc datafmt curcd costat liid
drop gvkey fyear cusip conm exchg
gen year=year(datadate)
gen month=month(datadate)
drop datadate
duplicates drop
duplicates tag lpermno year month, gen(tag)
sum tag	/* no duplicates */
drop tag
rename lpermno permno  /* for merging */
sort permno year month
joinby permno year month using CRSP_data, unmatched(both)
duplicates tag permno year month, gen(tag)
sum tag /* just to be sure: no duplicates */
drop tag
gen period=ym(year,month)	/* converts year and month to months since 1/1/60 */
xtset permno period
gen fill_counter=(_merge==3)
by permno: gen sum_fill=sum(fill_counter)
sort permno sum_fill
by permno sum_fill: egen min_rdip=min(rdip)
by permno sum_fill: egen min_xrd=min(xrd)
by permno sum_fill: egen min_revt=min(revt)
/* This assumes that the R&D, sales, etc. is released with the annual report and
remains in use until the next annual report is released */
drop rdip xrd revt
rename min_rdip rdip
rename min_xrd xrd
rename min_revt revt
drop _merge fill_counter sum_fill cusip date shrout
sort permno period
save combined_data, replace

/*******************************************************************************
Now use the 12-month lag at each point to check the previous year.
*******************************************************************************/

clear
use combined_data
gen sg=ln(revt/l12.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D/sales */
sum rds sg	
/* This does not help reduce the SDs

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |    915,373    .1835829    .5445682  -.0023208   10.15368
          sg |  1,687,556     .113592    .4839458  -10.36268   9.382653
*/
drop if sg==.	/* we cannot run the regression if sales growth is missing */
gen rds_m=(rds==.)	/* R&D/sales is missing */
gen xrd_0=(xrd==0)	/* xrd is zero */
gen rds_missing_chk=l12.rds_m+l24.rds_m+l36.rds_m+l48.rds_m+l60.rds_m+l72.rds_m+l84.rds_m+l96.rds_m
drop if rds_missing_chk>2  	/* require non-missing R&D intensity in at least 6 of the prior 8 years (no more than 2 missing) */
gen xrd_0_chk=l12.xrd_0+l24.xrd_0+l36.xrd_0+l48.xrd_0+l60.xrd_0+l72.xrd_0+l84.xrd_0+l96.xrd_0
drop if xrd_0_chk>4	/* the note to Table 1 says at least half must be non-zero; the text says over half; assume "at least" is correct */
	/* this means no more than 4 zeros */
	/* 6,300 annual observations remain */
sum rds sg
/* This also does not help

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     72,341    .0841268    .2145718          0   6.523366
          sg |     73,748    .0479132    .2982605  -8.031711   4.789451
*/

/*******************************************************************************
Try the Compustat data on its own; this is still from the merged
CRSP_Compustat download.
*******************************************************************************/

clear
use Compustat_1980_2009
drop indfmt consol popsrc datafmt curcd costat liid
drop gvkey cusip datadate rdip conm
keep if exchg==11 | exchg==12 | exchg==14    /* exchg 11, 12 and 14 are NYSE, AMEX and NASDAQ */
drop exchg
sort lpermno fyear
duplicates tag lpermno fyear, gen(tag)
drop if tag>0	/* 4 cases */
drop tag
xtset lpermno fyear
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+l.xrd/l.revt)	/* R&D/sales */
sum rds sg	/* 137,751 obs */
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     54,953     .145745    .4638895  -.0023208   10.15368
          sg |    112,187    .1156085    .3893813  -7.533052   8.779835
*/
sum rds sg if (rds<.)*(sg<.)*(rds>0)
/* The mean RDS is right but the SD is way too high.
The mean sg is also too high.

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     43,616    .1773941    .4867494   .0000157   10.15368
          sg |     43,616    .1171477    .4334412  -7.137279   8.557183
*/
drop if sg==. | rds==.	
gen rds_m=(rds==.)	/* R&D/sales is missing */
gen xrd_0=(xrd==0)	/* xrd is zero */
gen rds_missing_chk=l.rds_m+l2.rds_m+l3.rds_m+l4.rds_m+l5.rds_m+l6.rds_m+l7.rds_m+l8.rds_m
drop if rds_missing_chk>2  	/* require non-missing R&D intensity in at least 6 of the prior 8 years (no more than 2 missing) */
gen xrd_0_chk=l.xrd_0+l2.xrd_0+l3.xrd_0+l4.xrd_0+l5.xrd_0+l6.xrd_0+l7.xrd_0+l8.xrd_0
drop if xrd_0_chk>4	/* the note to Table 1 says at least half must be non-zero; the text says over half; assume "at least" is correct */
	/* this means no more than 4 zeros */
	/* 6,300 annual observations remain */
sum rds sg
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |      6,100     .077398    .1767545          0   3.919711
          sg |      6,215    .0482627    .2716567   -6.31113   4.221467
*/

/*******************************************************************************
Try combining the CRSP share codes I downloaded with the Compustat data
Danny downloaded (I don't know how he get permno in there).
*******************************************************************************/

clear
use CRSP_data
keep permno year
duplicates drop
sort permno year
save permno_year, replace

clear
use annual
gen month=month(datadate)
drop if year<1980
drop if (year==1980)*(month<7)
drop if year>2009	/* to reflect the Cohen et al sample: July 1980-December 2009 */
drop gvkey indfmt consol popsrc datafmt curcd ceq seq xsga costat  
duplicates drop	/* doesn't eliminate anything */
duplicates tag permno fyear, gen(tag)
drop if tag>0	/* 5 cases; R&D is missing in all but 1; drop these */
drop tag
sort permno year 
joinby permno year using permno_year 
	/* drops permnos/year/months where the exchange is not listed in CRSP as Q, N or A */
xtset permno fyear
rename sale revt	/* to conform with the code above */
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+l.xrd/l.revt)	/* R&D/sales */
drop if revt<0 | xrd<0	/* this happens in a few cases */
sum rds sg	
/* 
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     69,497    .1812417    .5410995  -.0023208   10.15368
          sg |    144,976    .1066482    .4894859  -10.36268   9.382653

*/
sum rds sg if (sg!=.)*(rds!=.)
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     69,113    .1753488     .517351  -.0023208   10.15368
          sg |     69,113    .1091101    .5141049  -9.316936   9.382653
*/
drop if sg==. | rds==.
	/* we cannot run the regression if sales growth is missing and the paper seems
	to suggest that non-missing rds is required too */

	
	
	
gen rds_m=(rds==.)	/* R&D/sales is missing */
gen rds_missing_chk=l.rds_m+l2.rds_m+l3.rds_m+l4.rds_m+l5.rds_m+l6.rds_m+l7.rds_m+l8.rds_m
drop if rds_missing_chk>2  	/* require non-missing R&D intensity in at least 6 of the prior 8 years (no more than 2 missing) */
	/* 22,672 obs */
forvalues i=1(1)8 {
	gen xrd`i'_0=(l`i'.xrd>0)*(l`i'.xrd<.)
}
gen xrd_0_chk=xrd1_0+xrd2_0+xrd3_0+xrd4_0+xrd5_0+xrd6_0+xrd7_0+xrd8_0
gen ratio_chk=xrd_0_chk/(8-rds_missing_chk)
drop if ratio_chk<.5  /* at least half of the non-missing R&D observations must be positive */ 
sum rds sg
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     11,124    .1044792    .2854167          0    8.39976
          sg |     11,124    .0468251    .3234139  -8.031711   9.382653
*/

/*******************************************************************************
What happens if we just use the CRSP_Compustat data without further adjustments?
*******************************************************************************/

clear
use Compustat_1980_2009

gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+l.xrd/l.revt)	/* R&D/sales */
sum rds sg	


































forvalues i=1(1)11 {
	if 


}




























clear
use Compustat_1980_2010
drop indfmt consol popsrc datafmt curcd costat
gen year=year(datadate)
gen month=month(datadate)
drop if year>2009
sort cusip year month
duplicates drop
drop if cusip==""
duplicates tag cusip year month, gen(tag)
sum tag
keep if tag>0
keep if exchg==11 | exchg==12 | exchg==14
	/* NYSE, AMEX, NASDAQ */



drop if cusip==""	/* 9,635 obs */
sum exchg /* 9,635 obs */

duplicates tag cusip year month, gen(tag)
sum tag
keep if tag>0
keep if exchg==11 | exchg==12 | exchg==14
	/* NYSE, AMEX, NASDAQ */
/* All of the remaining cases are 2009. Perhaps Compustat does something weird in the last year of a data pull? */


























/***********************************************************************************
Try using CRSP_Compustat merged.

The period is July 1980 to December 2009.
***********************************************************************************/

clear
use CRSP_Compustat
keep if exchg==11 | exchg==12 | exchg==14    /* exchg 11, 12 and 14 are NYSE, AMEX and NASDAQ */
drop exchg
drop indfmt consol popsrc datafmt tic liid curcd costat ipodate
	/* some are in CAD currency (curcd) but keep these for now; drop the ipodate for now */
duplicates drop
egen id=group(gvkey)
sort id fyear
xtset id fyear
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D/sales */
sum rds sg	/* 137,751 obs */
/* These are not close to the Cohen et al. estimates (Table 1 reports means of .18 and .07)

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     60,054      .14846    .4663296  -.0023208   10.15368
          sg |    111,008    .1159858     .391288  -7.533052   8.779835

Try examining the restricted sample they use to run the ability regressions
*/
drop if sg==.	/* we cannot run the regression if sales growth is missing */
gen rds_m=(rds==.)	/* R&D/sales is missing */
gen xrd_0=(xrd==0)	/* xrd is zero */
gen rds_missing_chk=l.rds_m+l2.rds_m+l3.rds_m+l4.rds_m+l5.rds_m+l6.rds_m+l7.rds_m+l8.rds_m
drop if rds_missing_chk>2  	/* require non-missing R&D intensity in at least 6 of the prior 8 years (no more than 2 missing) */
gen xrd_0_chk=l.xrd_0+l2.xrd_0+l3.xrd_0+l4.xrd_0+l5.xrd_0+l6.xrd_0+l7.xrd_0+l8.xrd_0
drop if xrd_0_chk>4	/* the note to Table 1 says at least half must be non-zero; the text says over half; assume "at least" is correct */
	/* this means no more than 4 zeros */
	/* 6,300 annual observations remain */
sum rds sg
/* Still not close

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |      6,185     .076539    .1746378          0   3.919711
          sg |      6,300    .0478199    .2699806   -6.31113   4.221467
*/

/***************************************************************************************
Try using Danny's data but start from scratch and just try to replicate the summary
stats on R&D intensity and sales growth.
***************************************************************************************/

clear
use crsp
gen month=month(date)
keep if primexch=="Q" | primexch=="N" | primexch=="A"
keep permno month year
sort permno year month
save exchange_info, replace

clear
use annual
gen month=month(datadate)
drop if year<1980
drop if (year==1980)*(month<7)
drop if year>2009	/* to reflect the Cohen et al sample: July 1980-December 2009 */
drop gvkey indfmt consol popsrc datafmt curcd ceq seq xsga costat  
duplicates drop	/* doesn't eliminate anything */
duplicates tag permno fyear, gen(tag)
drop if tag>0	/* 5 cases; R&D is missing in all but 1; drop these */
drop tag
sort permno year month
joinby permno year month using exchange_info 
	/* drops permnos/year/months where the exchange is not listed in CRSP as Q, N or A */
xtset permno fyear
rename sale revt	/* to conform with the code above */
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D/sales */
sum rds sg	/* 196,276 obs */
/* The means are closer to those in Table 1 but this might be a fluke; the SDs are still way off

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     86,342    .1735329    .5282767  -.0023208   10.15368
          sg |    157,073    .1063016    .4828622  -10.36268   9.382653
*/
drop if sg==.	/* we cannot run the regression if sales growth is missing */
gen rds_m=(rds==.)	/* R&D/sales is missing */
gen xrd_0=(xrd==0)	/* xrd is zero */
gen rds_missing_chk=l.rds_m+l2.rds_m+l3.rds_m+l4.rds_m+l5.rds_m+l6.rds_m+l7.rds_m+l8.rds_m
drop if rds_missing_chk>2  	/* require non-missing R&D intensity in at least 6 of the prior 8 years (no more than 2 missing) */
gen xrd_0_chk=l.xrd_0+l2.xrd_0+l3.xrd_0+l4.xrd_0+l5.xrd_0+l6.xrd_0+l7.xrd_0+l8.xrd_0
drop if xrd_0_chk>4	/* the note to Table 1 says at least half must be non-zero; the text says over half; assume "at least" is correct */
	/* this means no more than 4 zeros */
	/* 6,881 annual observations remain */
sum rds sg
/* The means are closer to those in Table 1 but this might be a fluke; the SDs are still way off

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     86,342    .1735329    .5282767  -.0023208   10.15368
          sg |    157,073    .1063016    .4828622  -10.36268   9.382653
*/

/***************************************************************************************
Try the same thing with his preregression data.
***************************************************************************************/

clear
use pre-regression
gen month=month(date)
drop if year<1980
drop if (year==1980)*(month<7)
drop if year>2009	/* to reflect the Cohen et al sample: July 1980-December 2009 */
keep if month==fyr	/* keep only the fiscal year-end month */
xtset permno fyear
rename sale revt	/* to conform with the code above */
gen sg=ln(revt/l.revt)	/* sales growth */
gen rds=ln(1+xrd/revt)	/* R&D/sales */
sum rds sg
/*
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
         rds |     86,216    .1734681    .5283514  -.0023208   10.15368
          sg |     74,635    .1088555    .4915546  -8.031711   9.382653
*/











rename fyear year
xtset permno year
gen month=month(datadate)
sort permno year month
joinby permno year month using crsp  







save cleaned, replace

clear
use crsp
merge m:1 permno year using cleaned







"annual.dta", clear

// Clean up
sort permno year
drop if xrd == . | sale == .

sort permno year
drop if permno == permno[_n-1] & year == year[_n-1]
// Now permno year uniquely identifies the data

// Merge
save "merge_cleaned.dta", replace
use "crsp.dta", clear
merge m:1 permno year using "merge_cleaned.dta"

// Drop unmatched
drop if _merge != 3

// Drop small exchanges to match paper
drop if primexch != "Q" & primexch != "N" & primexch != "A"

save "pre-regression.dta", replace











on_missing+l.x







sort permno year




/*
use "annual.dta", clear

// Clean up
sort permno year
drop if xrd == . | sale == .

sort permno year
drop if permno == permno[_n-1] & year == year[_n-1]
// Now permno year uniquely identifies the data

// Merge
save "merge_cleaned.dta", replace
use "crsp.dta", clear
merge m:1 permno year using "merge_cleaned.dta"

// Drop unmatched
drop if _merge != 3

// Drop small exchanges to match paper
drop if primexch != "Q" & primexch != "N" & primexch != "A"

save "pre-regression.dta", replace
*/

use "pre-regression.dta", replace

collapse ret shrout ceq sale seq xrd xsga, by (permno fyear) // Had to collapse since I merged returns data in already, shouldn't make a difference for fundamentals anyways.

xtset permno fyear
gen long obsn=_n

gen sales_growth=log(sale/l.sale)
gen RDS=xrd/sale

// RDS Non-missing check
gen RDS_non_missing=RDS<.
gen chk_RDS_non_missing=l.RDS_non_missing+l2.RDS_non_missing+l3.RDS_non_missing+l4.RDS_non_missing+l5.RDS_non_missing+l6.RDS_non_missing+l7.RDS_non_missing+l8.RDS_non_missing
replace chk_RDS_non_missing=l.RDS_non_missing+l2.RDS_non_missing+l3.RDS_non_missing+l4.RDS_non_missing+l5.RDS_non_missing+l6.RDS_non_missing+l7.RDS_non_missing if chk_RDS_non_missing==.
replace chk_RDS_non_missing=l.RDS_non_missing+l2.RDS_non_missing+l3.RDS_non_missing+l4.RDS_non_missing+l5.RDS_non_missing+l6.RDS_non_missing if chk_RDS_non_missing==.

// Sales Growth Non-missing check
gen sales_growth_non_missing=sales_growth<.
gen chk_sales_growth_non_missing=l.sales_growth_non_missing+l2.sales_growth_non_missing+l3.sales_growth_non_missing+l4.sales_growth_non_missing+l5.sales_growth_non_missing+l6.sales_growth_non_missing+l7.sales_growth_non_missing+l8.sales_growth_non_missing
replace chk_sales_growth_non_missing=l.sales_growth_non_missing+l2.sales_growth_non_missing+l3.sales_growth_non_missing+l4.sales_growth_non_missing+l5.sales_growth_non_missing+l6.sales_growth_non_missing+l7.sales_growth_non_missing if chk_sales_growth_non_missing==.
replace chk_sales_growth_non_missing=l.sales_growth_non_missing+l2.sales_growth_non_missing+l3.sales_growth_non_missing+l4.sales_growth_non_missing+l5.sales_growth_non_missing+l6.sales_growth_non_missing if chk_sales_growth_non_missing==.

// Positive check
gen pos_RDS=(RDS>0)*(RDS<.)       /* remember Stata interprets missing as infinite, so to check positive and finite both conditions are required */
gen chk_RDS_pos=l.pos_RDS+l2.pos_RDS+l3.pos_RDS+l4.pos_RDS+l5.pos_RDS+l6.pos_RDS+l7.pos_RDS+l8.pos_RDS
replace chk_RDS_pos=l.pos_RDS+l2.pos_RDS+l3.pos_RDS+l4.pos_RDS+l5.pos_RDS+l6.pos_RDS+l7.pos_RDS if chk_RDS_pos==.
replace chk_RDS_pos=l.pos_RDS+l2.pos_RDS+l3.pos_RDS+l4.pos_RDS+l5.pos_RDS+l6.pos_RDS if chk_RDS_pos==.

forvalues i=1(1)5 {
	gen log_RDS_`i'=log(1+l`i'.RDS)
	gen log_RDS_non_missing_`i'=log_RDS_`i'<.
	gen chk_log_RDS_non_missing_`i'=l.log_RDS_non_missing_`i'+l2.log_RDS_non_missing_`i'+l3.log_RDS_non_missing_`i'+l4.log_RDS_non_missing_`i'+l5.log_RDS_non_missing_`i'+l6.log_RDS_non_missing_`i'+l7.log_RDS_non_missing_`i'+l8.log_RDS_non_missing_`i'
	replace chk_log_RDS_non_missing_`i'=l.log_RDS_non_missing_`i'+l2.log_RDS_non_missing_`i'+l3.log_RDS_non_missing_`i'+l4.log_RDS_non_missing_`i'+l5.log_RDS_non_missing_`i'+l6.log_RDS_non_missing_`i'+l7.log_RDS_non_missing_`i' if chk_log_RDS_non_missing_`i'==.
	replace chk_log_RDS_non_missing_`i'=l.log_RDS_non_missing_`i'+l2.log_RDS_non_missing_`i'+l3.log_RDS_non_missing_`i'+l4.log_RDS_non_missing_`i'+l5.log_RDS_non_missing_`i'+l6.log_RDS_non_missing_`i' if chk_log_RDS_non_missing_`i'==.
}

gen beta=.
quietly: levelsof permno, local(permnos)
foreach firm of local permnos {
	sum chk_RDS_non_missing if permno==`firm'
	// chk_RDS_non_missing>=6 & chk_RDS_pos>=4
	if (r(N)>0) {
		dis `firm'
		sum fyear if permno==`firm'
		local last=r(max)
		local first=`last'-8
		
		forvalues i=1(1)5 { 
			sum obsn if (fyear==`last'-`i'+1)*(permno==`firm')
			local linen=r(min)
			local regressable=(chk_sales_growth_non_missing[`linen']>=6)*(chk_RDS_non_missing[`linen']>=6)*(chk_log_RDS_non_missing_`i'[`linen']>=6)*(chk_RDS_pos[`linen']>=4)*(chk_RDS_non_missing[`linen']!=.)*(chk_RDS_pos[`linen']!=.)
			if (`regressable'==1) {
				dis `last'
				dis `first'
				dis `i'
				reg sales_growth log_RDS_`i' if (permno==`firm')*(fyear<=`last')*(fyear>=`first')
				replace beta=_b[log_RDS_`i'] if (permno==`firm')*(fyear==`last'-`i'+1)
			}
		}
	}
}

gen log_RDS=log(1+RDS)
by permno: egen ability = mean(beta)
sum sales_growth log_RDS 

collapse ability, by (permno)
sum ability
