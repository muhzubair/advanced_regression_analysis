filename medical "/home/u62280666/project2/Prostate(2).dat";

DATA C; 
INFILE medical;
INPUT ID PSA_level Cancer_Volume weight Age B_P_H S_V_I Capsular_penetration Gleason_score;
RUN;

/* Question 1: ScatterPlots*/
PROC SGPLOT data=C;
scatter y=PSA_level x=Cancer_Volume;
TITLE 'PSA Level vs Cancer volume';
RUN;

PROC SGPLOT data=C;
scatter y=PSA_level x=weight;
TITLE 'PSA Level vs weight';
RUN;

PROC SGPLOT data=C;
scatter y=PSA_level x=Age;
TITLE 'PSA Level vs age';
RUN;

PROC SGPLOT data=C;
scatter y=PSA_level x=B_P_H;
TITLE 'PSA Level vs BPH';
RUN;

PROC SGPLOT data=C;
scatter y=PSA_level x=Capsular_penetration;
TITLE 'PSA Level vs Capsular penetration';
RUN;

/* Question 2: */
PROC REG Data=C;
MODEL PSA_level = Cancer_Volume weight Age B_P_H  Capsular_penetration  / lackfit;
OUTPUT OUT=D RSTUDENT=R PREDICTED=P; 
*PLOT RSTUDENT.*footage RSTUDENT.*land RSTUDENT.*rooms RSTUDENT.*baths;
RUN;

PROC PLOT Data=D HPERCENT=50 VPERCENT=50; /* Residual plot (for university edition) */
plot R*(Cancer_Volume weight Age B_P_H  Capsular_penetration);
RUN;

DATA D; SET D;
absR = abs(R); /* save absolute value of residuals */
RUN;

PROC GPLOT DATA = D;
PLOT absR*P; /* absolute residuals vs fitted values to check homogeneity assumption */
TITLE "Plot of absolute residuals";
RUN;

/* Checking for constant variance assumption */
/* Median = 13.30 */ 
PROC UNIVARIATE DATA=C;
VAR PSA_level ;
RUN;

DATA D; SET D; 
Group = (PSA_level>13.33000);
RUN;    

/* Brown Forsythe (or Levene) Test for homogeneity
H0: Homoscedasticity is present (the residuals are distributed with equal variance) vs HA: Heteroscedasticity is present
Since our p-value = 0.0017 > 0.05. We will accept HO and conclude that Homoscedasticity is present. */
PROC GLM Data=D; 
                class Group;
                model R=Group;
                means Group / hovtest=BF; /*BF can be replaced with Levene to perform Levene Test*/
run;

/* Identifying normality assumption */ :
PROC UNIVARIATE DATA=D NORMAL PLOT;     
VAR R;       
RUN;
 
/* Question 3 */
 /* Transforming the variable to follow the normality assumption */
/* Applying the transformation */
DATA C; SET C; 
PSA_level_log = log10(PSA_level);        
RUN;

PROC REG Data = C;
MODEL PSA_level_log = Cancer_Volume weight Age B_P_H S_V_I Capsular_penetration Gleason_score;
OUTPUT OUT=C1 RSTUDENT=R; 
RUN;

PROC UNIVARIATE DATA=C1 NORMAL PLOT; /* Check normality of Studentized residuals */
VAR R;
RUN;

/* Median = 13.30 */ 
PROC UNIVARIATE DATA=C;
VAR PSA_level_log ;
RUN;

DATA C1; SET C1; 
Group = (PSA_level_log>1.12483);
RUN;    

/* Brown Forsythe (or Levene) Test for homogeneity
H0: Homoscedasticity is present (the residuals are distributed with equal variance) vs HA: Heteroscedasticity is present
Since our p-value = 0.2267 > 0.05. We will accept HO and conclude that Homoscedasticity is present. */
PROC GLM Data=C1; 
                class Group;
                model R=Group;
                means Group / hovtest=BF; /*BF can be replaced with Levene to perform Levene Test*/
run;

/* Question 4 */
/* Deciding what variables to keep in model */

/* Type 1 and Type 3 error */
PROC GLM DATA=C ; 
MODEL PSA_level_log = Cancer_Volume weight Age B_P_H Capsular_penetration ; 
RUN;

PROC REG DATA = C; 
MODEL PSA_level_log = Cancer_Volume weight Age B_P_H Capsular_penetration; 

/* F-value = 22.56, P-value < 0.05. Hence, we can conclude that cancer_Volume cannot be dropped from the model */
TEST Cancer_Volume=0; 
TITLE 'Test for Cancer Volume';

PROC REG DATA = C; 
MODEL PSA_level_log = Cancer_Volume weight B_P_H Capsular_penetration; 
/*F-value = 0.57, P-value > 0.05. Hence, we can conclude that weight can be dropped from the model*/
TEST weight=0;
TITLE 'Test for Weight';

PROC REG DATA = C; 
MODEL PSA_level_log = Cancer_Volume Age B_P_H  Capsular_penetration; 

/*F-value = 0.06, P-value > 0.05. Hence, we can conclude that age can be dropped from the model*/
TEST Age=0;
TITLE 'Test for Age';

PROC REG DATA = C; 
MODEL PSA_level_log = Cancer_Volume B_P_H Capsular_penetration; 

/*F-value = 12.40, P-value < 0.05. Hence, we cannot conclude that bph cannot be dropped from the model*/
TEST B_P_H=0;
TITLE 'Test for BPH';

/* Comparing full with reduced model */
PROC REG DATA = C; 
MODEL PSA_level_log = Cancer_Volume weight Age B_P_H Capsular_penetration; 
TEST Capsular_penetration=0;
TEST Age=0;
TEST weight=0;
TITLE 'Comparing full with reduced model';

/* Question:5 */
/*partial correlation coefficient = 0.6927
 Coefficient of partial determination = 0.6927^2 = 0.47 */
proc corr data=C;
var PSA_level_log Cancer_Volume; 
partial B_P_H;
run;

/*partial correlation coefficient = 0.32784
 Coefficient of partial determination = 0.32784^2 = 0.10 */
proc corr data=C;
var PSA_level_log B_P_H; 
partial Cancer_Volume;
run;

/*We can see we get the same coefficient of simple determination = 0.47*/
PROC REG Data = C;
MODEL PSA_level_log = B_P_H;
OUTPUT OUT=C4 r=R1; 
RUN;

/* Verifying the alternative interpretation */
PROC REG Data = C;
MODEL Cancer_Volume = B_P_H;
OUTPUT OUT=C3 r=R2; 
RUN;

DATA D; MERGE C4 C3;  

proc reg data = D;
model R1=R2;
run;


/* Question 6: */
/* Finding mean of B_P_H and Cancer_volume */

/* Mean = 2.53472474 */
PROC UNIVARIATE DATA=C;
VAR B_P_H ;
RUN;

/* Mean = 6.99868247 */
PROC UNIVARIATE DATA=C;
VAR Cancer_Volume ;
RUN;

/* mean of other predictor added to predcitor */
DATA C; SET C; 
b_P_H_mc = B_P_H + 6.99868247;      
RUN;

DATA C; SET C; 
Cancer_Volume_B_P_H = Cancer_Volume + 2.53472474;      
RUN;

/*B_P_H*/
PROC REG Data=C;
MODEL PSA_level_log = b_P_H_mc /clm cli;
output out=D6 predicted=pr stdi=se lclm=lwr_ci uclm=upr_ci lcl=lwr_pi ucl=upr_pi;
run;

/* Calculating simultaneous confidence bands for the entire regression line */
data D6; set D6;
WHU=pr+(sqrt(finv(0.95,3,97-3)*3)*se);
WHL=pr-(sqrt(finv(0.95,3,97-3)*3)*se);
run;

proc print data=D6(obs=6);
Title "Lower and Upper Bounds for CI, PI, and Working Hoteling ";
run;

/* Sorting the column to avoid inconsistency in plot */
proc sort data = D6;
By b_P_H_mc;
Run;

proc sgplot data=D6;

 band x=b_P_H_mc lower=WHL upper=WHU /
       fillattrs=(color=orange)
       legendlabel="95% WHM" name ="band3";
       
  title "Sets of intervals for BPH";
  
  band x=b_P_H_mc lower=lwr_pi upper=upr_pi /
       legendlabel="95% CLI" name="band1";
  
  band x=b_P_H_mc lower=lwr_ci upper=upr_ci /
       fillattrs=GraphConfidence2
       legendlabel="95% CLM" name="band2";
     
  
  scatter x=b_P_H_mc y=PSA_level_log;
RUN;

/* Cancer volume */
PROC REG Data=C;
MODEL PSA_level_log = Cancer_Volume_B_P_H /clm cli;
output out=D6 predicted=pr stdi=se lclm=lwr_ci uclm=upr_ci lcl=lwr_pi ucl=upr_pi;
run;

/* Sorting the column to avoid inconsistency in plot */
proc sort data = D6;
By Cancer_Volume_B_P_H;
Run;

/* Calculating simultaneous confidence bands for the entire regression line */
data D6; set D6;
WHU=pr+(sqrt(finv(0.95,3,97-3)*3)*se);
WHL=pr-(sqrt(finv(0.95,3,97-3)*3)*se);
run;

/* Plotting the Confidence Intervals */
proc sgplot data=D6;
	band x=Cancer_Volume_B_P_H lower=WHL upper=WHU /
       fillattrs=(color=orange)
       legendlabel="95% WHM" name ="band3";
       
  title "Sets of intervals for Cancer Volume";
  
  band x=Cancer_Volume_B_P_H lower=lwr_pi upper=upr_pi /
       legendlabel="95% CLI" name="band1";
  
  band x=Cancer_Volume_B_P_H lower=lwr_ci upper=upr_ci /
       fillattrs=GraphConfidence2
       legendlabel="95% CLM" name="band2";
  scatter x=Cancer_Volume_B_P_H y=PSA_level_log;
RUN;


/*Question 7: */
/* Getting X'X from the code, then calculating everything in the equation*/
proc reg DATA = C;
MODEL PSA_level_log = Cancer_Volume B_P_H/XPX;
run;

/* Question 8: */
/* Getting Inverse from the code, then calculating everything in the equation*/
proc reg Data = C;
MODEL PSA_level_log = Cancer_Volume B_P_H/ I;
run;










