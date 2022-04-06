cd "C:\Users\dusti\OneDrive\Documents\Senior Thesis Repo\Senior-Thesis"

clear
use b_construction.dta
drop PERMCO LPERMCO conm at capx ni revt xad xrd ggroup gind gsector gsubind b1 b2 b3 b4 b5


/* Calculate summary stats for my sample */
summarize avg_b sgas gpg if (period>=246) & (period<=725) & avg_b<. & sgas<., d
egen unique = group(PERMNO) if (period>=246) & (period<=725) & gp<. & sgas<.
summarize unique 

/* Calculate summary stats for Cohen sample */
clear
use Cohen_b_construction.dta
summarize avg_b rds gpg if (period>=246) & (period<=725) & avg_b<. & rds<.
egen unique = group(PERMNO) if (period>=246) & (period<=725) & avg_b<. & rds<.
summarize unique

/* Calculate summary stats for Entire Universe */
clear
use CRSP_Compustat_merged.dta

summarize PERMNO if (period>=246) & (period<=725)
egen unique = group(PERMNO) if (period>=246) & (period<=725)
summarize unique

