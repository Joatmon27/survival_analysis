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
/*
proc phreg data=std;
model time*cens(0)= type / ties=BRESLOW;
run;
*/
/*
Question 3
*//*
proc lifetest data=std;
time time*cens(0);
strata type /test=FLEMING(1,1);
run;*/
/*
Question 4
*/
/*
proc lifetest data=std plots=(s);
time time*cens(0);
strata type;
run;
*/
/*
data std_rr;
	label time ='Days to re-infection' type ='Infection Type';
	infile 'H:\Werk\Survival Analysis\survival_analysis\exam_project\data\std_2019_pw_rr.csv'
		firstobs=2 dlm=',';
	input obs race_B mar_S mar_D age school type part oral12 oral30 rec12 rec30 abdom dis dys condom 
		  itch lesion rash lymph vag disexam node cens time type_both type_chl @@;
run;

proc phreg data=std_rr;
	model time*cens(0)=type_both type_chl /covb  ;
run;
*//*
title 'Time to re-infection of STDs';

proc phreg data = std_rr;
model time*cens(0)= type_both type_chl X Y;
X=type_both*(log(time));
Y=type_chl*(log(time));
run;*/
/*
proc phreg data = std_rr;
model time*cens(0)= type_both type_chl;
run;
*/
/*
Question 5
*/
proc lifetest data=std;
time time*cens(0);
strata type condom /test=FLEMING(1,1);
run;
