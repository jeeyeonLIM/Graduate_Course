
/*      주석처리 단축키 : ctrl + /   */

/*   Dataset */
proc import 
datafile="C:\Users\jeeyeon\Desktop\data\ex3-1.csv" 
out=trial 
;
getnames=yes;
run;


/* Q1--------- */
data trial_A;
set trial;
if TRT="A";run;

/* TRT_A boxplot  */
proc sgplot data=trial_A;
	hbox SCORE;
run;

/* TRT boxplot  */
proc sgplot data=trial;
	vbox SCORE / group=TRT;
run;

/*  T-test  */
ods graphics on; 
   proc ttest h0=20 plots(showh0) sides=l data=trial_A;
      var SCORE;
   run;
ods graphics off;


/* Q2--------- */
/*  one-way ANOVA  */
proc glm data=trial;
	class TRT;
	model SCORE = TRT / SS3;
run;

/*  교호효과검정  */
proc glm data=trial;
	class TRT CENTER;
	model SCORE=TRT CENTER TRT*CENTER;
run;

/*  TRT, CENTER효과검정  */
proc glm data=trial;
	class TRT CENTER;
	model SCORE=TRT CENTER;
run;

