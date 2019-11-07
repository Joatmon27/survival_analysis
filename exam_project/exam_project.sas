/*Part A*/
/*
Data import
*/
title 'Re-infecion time of std patients';
data std;
	label time ='Days to re-infection' type ='Infection Type';
	infile 'H:\Werk\Survival Analysis\survival_analysis\exam_project\data\std_2019_pw_altered.csv'
		firstobs=2 dlm=',';
	input obs race_B mar_S mar_D age school type part oral12 oral30 rec12 rec30 abdom dis dys condom 
		  itch lesion rash lymph vag disexam node cens time @@;
run;

proc print data=std;
run;

proc phreg data=std;
model time*cens(0)= type / ties=BRESLOW;
run;

/*Question 3*/

proc lifetest data=std;
time time*cens(0);
strata type /test=FLEMING(1,1);
run;

/*Question 4*/

proc lifetest data=std plots=(s);
time time*cens(0);
strata type;
run;

/*Question 5*/

data std_rr;
	label time ='Days to re-infection' type ='Infection Type';
	infile 'H:\Werk\Survival Analysis\survival_analysis\exam_project\data\std_2019_pw_rr.csv'
		firstobs=2 dlm=',';
	input obs race mar age school type part oral12 oral30 rec12 rec30 abdom dis dys condom 
		  itch lesion rash lymph vag disexam node cens time type_chl type_both condom_always @@;
run;

proc print data=std_rr;
run;

proc phreg data=std_rr;
	model time*cens(0)=type_both type_chl /covb  ;
run;

title 'Time to re-infection of STDs';
proc phreg data = std_rr;
model time*cens(0)= type_both type_chl X Y;
X=type_both*(log(time));
Y=type_chl*(log(time));
run;

proc phreg data = std_rr;
model time*cens(0)= type_both type_chl;
run;

/*Part B*/
/*Question 1*/

proc lifetest data=std;
time time*cens(0);
strata type condom /test=FLEMING(1,1);
run;

data std_rr;
	label time ='Days to re-infection' type ='Infection Type';
	infile 'H:\Werk\Survival Analysis\survival_analysis\exam_project\data\std_2019_pw_rr.csv'
		firstobs=2 dlm=',';
	input obs race mar age school type part oral12 oral30 rec12 rec30 abdom dis dys condom 
		  itch lesion rash lymph vag disexam node cens time type_chl type_both condom_always @@;
	condom_both = type_both*condom_always;
	condom_chl = type_chl*condom_always;
run;

proc print data=std_rr;
run;

proc phreg data = std_rr;
model time*cens(0)= type_both type_chl condom_always;
run;

proc phreg data=std_rr;
	model time*cens(0)= condom_always /covb  ;
run;

proc phreg data=std_rr;
	model time*cens(0)=type_both type_chl condom_always /covb  ;
	test1: test type_chl=type_both=0/print;
run;

proc phreg data=std_rr;
	model time*cens(0)=type_both type_chl condom_both condom_chl /covb  ;
	test1: test type_chl=type_both=0/print;
	test2: test condom_both=0/print;
	test3: test condom_chl=0/print;
run;


/*Part C*/
/*Question 1*/

/* Weibull distribution model*/ 
proc lifereg data=std_rr; 
model time*cens(0)=type_chl type_both condom/ dist=weibull alpha=0.05 covb; 
run; 

/* Exponential distribution model*/ 
proc lifereg data=std_rr; 
model time*cens(0)=type_chl type_both condom/ dist=exponential alpha=0.05 covb; 
run; 

/* Log normal distribution*/ 
proc lifereg data=std_rr; 
model time*cens(0)=type_chl type_both condom/ dist=lognormal alpha=0.05 covb; 
run; 

/* Log logistic distribution model*/ 
proc lifereg data=std_rr; 
model time*cens(0)=type_chl type_both condom/ dist=lologistic alpha=0.05 covb; 
run;

/* gamma distribution model*/ 
proc lifereg data=std_rr; 
model time*cens(0)=type_chl type_both condom/ dist=gamma alpha=0.05 covb; 
run;
