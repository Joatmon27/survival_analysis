title 'Re-infecion time of std patients';
data std;
	label time ='Days to re-infection' type ='Infection Type';
	infile 'H:\Werk\Survival Analysis\survival_analysis\exam_project\data\std_2019_pw_altered.csv'
		firstobs=2 dlm=',';
	input obs race_B mar_S mar_D age school type part oral12 oral30 rec12 rec30 abdom dis dys condom 
		  itch lesion rash lymph vag disexam node cens time @@;
run;

proc phreg data=std;
model time*cens(0)= type / ties=BRESLOW;
run;
