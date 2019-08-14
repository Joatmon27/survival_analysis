libname pw "H:\Werk\Survival Analysis\libs";

data pw.cohort_life;
keep freq time cens;
retain time 44.5;
input event cens @@;
label time='age_interval';
time = time + 5;
c=0; freq=cens; output;
c=1; freq=event; output;
cards;
17 29
36 60
62 83
76 441
50 439
9 262
0 7
;

proc lifetest plots=(h p);
intervals=49.5 to 79.5 by 5 method=act;
time time*cens(0), freq freq;
run;
