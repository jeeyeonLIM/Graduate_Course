
data DOG;
	infile "C:\Users\jeeyeon\Desktop\data\sleeping-dog.csv" delimiter=',' firstobs=2;
	input Dog HO LO HI LI;
run;

proc transpose data=DOG out=temp; 
	by DOG ;
run;

data DOG_GATHER(rename=(_name_ = trt col1 = heartbeat));
	set temp;
	if substr(_name_,1,1) = 'H' then CO2 ='HIGH'; ELSE CO2 = 'LOW';
	IF	 SUBSTR(_NAME_,2,1) = 'O' THEN H = 'OUT';ELSE H='IN';
	run;
PROC SORT DATA=DOG_GATHER; BY TRT; QUIT;


/* Graph */
proc boxplot data=DOG_GATHER;
   plot HEARTBEAT*TRT;
   inset min mean max stddev /
      header = 'Overall Statistics'
      pos    = tm;
   insetgroup min max /
      header = 'Extremes by Day';
run;

PROC SGPANEL DATA=DOG_GATHER;
	panelby H;
	title 'DOG';
   	series X=CO2 Y=HEARTBEAT / group=DOG;
RUN;

PROC SGPANEL DATA=DOG_GATHER;
	panelby CO2;
	title 'DOG';
   	series X=H Y=HEARTBEAT / group=DOG;
RUN;



/* ANOVA */
PROC GLM data=DOG_GATHER;
	CLASS DOG H CO2;
	MODEL HEARTBEAT = CO2 H DOG CO2*H CO2*DOG H*DOG / ss3;
	RANDOM DOG; 
	TEST H = H E = DOG*H;
	TEST H = CO2 E = DOG*CO2;
	QUIT;
RUN;



/*  MANOVA */
ods exclude
	partialCorr
	ErrorSSCP;

proc glm data = DOG;
	class DOG;
	model HO LO HI LI = ;
	repeated H 2, CO2 2 /printe summary;
	quit;
run;


