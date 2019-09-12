/*
title 'Infecion time of burn patients';
data burn;
	label t3 ='Days to first infection'
	group ='Treatment Group';
	infile 'H:\Werk\Survival Analysis\survival_analysis\assignment_5\Section1_6.dat';
	input pst group z2 z3 z4 z5 z6 z7 z8 z9
	z10 z11 t1 cens1 t2 cens2 t3 cens3 @@;
run;

proc lifetest data = burn plots = (s,ls,lls);
time t3*cens3(0);
strata group;
run;

data rats;
input death_times Z1 Z2 censored @@;
cards;
	20 0 0 1
	21 0 0 1
	23 0 0 1
	24 0 0 1
	24 0 0 1
	26 0 0 1
	26 0 0 1
	27 0 0 1
	28 0 0 1
	30 0 0 1
	26 1 0 1
	28 1 0 1
	29 1 0 1
	29 1 0 1
	30 1 0 1
	30 1 0 1
	31 1 0 1
	31 1 0 1
	32 1 0 1
	35 1 0 0
	31 0 1 1
	32 0 1 1
	34 0 1 1
	35 0 1 1
	36 0 1 1
	38 0 1 1
	38 0 1 1
	39 0 1 1
	42 0 1 0
	42 0 1 0
	;
run;

proc print data=rats;
run;

proc phreg data=rats;
model death_times*censored(0)=Z1 Z2 / ties=BRESLOW covb;
run;
*/

data rats2;
input death_times Z1 censored @@;
cards;
	20 0 1
	21 0 1
	23 0 1
	24 0 1
	24 0 1
	26 0 1
	26 0 1
	27 0 1
	28 0 1
	30 0 1
	26 1 1
	28 1 1
	29 1 1
	29 1 1
	30 1 1
	30 1 1
	31 1 1
	31 1 1
	32 1 1
	35 1 0
	31 1 1
	32 1 1
	34 1 1
	35 1 1
	36 1 1
	38 1 1
	38 1 1
	39 1 1
	42 1 0
	42 1 0
	;
run;

proc print data=rats2;
run;

proc phreg data=rats2;
model death_times*censored(0)=Z1 / ties=BRESLOW;
run;
