title 'Infecion time of burn patients';
data burn;
	label t3 ='Days to first infection'
	group ='Treatment Group';
	infile 'H:\Werk\Survival Analysis\survival_analysis\assignment_5\Section1_6.csv' dlm=',';
	/*
	Z1 Treatment: 0-routine bathing 1-Body cleansing
	Z2 Gender (0=male 1=female)
	Z3 Race: 0=nonwhite 1=white
	Z4 Percentage of total surface area burned
	Z5 Burn site indicator: head 1=yes, 0=no
	Z6 Burn site indicator: buttock 1=yes, 0=no
	Z7 Burn site indicator: trunk 1=yes, 0=no
	Z8 Burn site indicator: upper leg 1=yes, 0=no
	Z9 Burn site indicator: lower leg 1=yes, 0=no
	Z10 Burn site indicator: respiratory tract 1=yes, 0=no
	Z11 Type of burn: 1=chemical, 2=scald, 3=electric, 4=flame
	T1 Time to excision or on study time
	D1 Excision indicator: 1=yes 0=no
	T2 Time to prophylactic antibiotic treatment or on study time
	D2 Prophylactic antibiotic treatment: 1=yes 0=no
	T3 Time to straphylocous aureaus infection or on study time
	D3 Straphylocous aureaus infection: 1=yes 0=no
	*/
	input pst group z2 z3 z4 z5 z6 z7 z8 z9	z10 z11 z112 z113 z114 t1 cens1 t2 cens2 t3 cens3;
run;
/*
proc print data=burn;
run;
*/

proc phreg data=burn;
	*model t3*cens3(0)= group / ties=breslow;
	*model t3*cens3(0)= group z2/ ties=breslow;
	*model t3*cens3(0)= group z3/ ties=breslow;
	*model t3*cens3(0)= group z4/ ties=breslow;
	*model t3*cens3(0)= group z112 z113 z114/ ties=breslow;
	*model t3*cens3(0)= z3 z2/ ties=breslow;
	*model t3*cens3(0)= group z112 z113 z114 z3/ ties=breslow;
	*model t3*cens3(0)= group z3 z4/ ties=breslow;
	*model t3*cens3(0)= group z112 z113 z114 z3 z2/ ties=breslow;
	*model t3*cens3(0)= group z112 z113 z114 z3 z4/ ties=breslow;
	*model t3*cens3(0)= group z112 z113 z114 z3 z2 Z4/ ties=breslow;
run;
/*
proc phreg data=burn;
	model t3*cens3(0)= group z4/ covb;
	output out=surv_est survival=survival /method=CH;
run;

proc print data=surv_est;
run;
*/

data variable;
input group z4;
datalines;
0 25
1 25
;

proc phreg data=burn;
	model t3*cens3(0)= group z4/ ties=breslow;
	baseline out=spec_surv_est covariates=variable survival=survival/
	method=CH cltype=loglog;
run;

proc print data=spec_surv_est;
run;

