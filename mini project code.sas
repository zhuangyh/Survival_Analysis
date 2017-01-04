/* SAS code for miniproject B6611 in 2011 */ 
OPTIONS PAGESIZE=60 LINESIZE=80;	* Output print options;
PROC FORMAT;
VALUE oca2_status  0 = 'gg' 1 = 'ga' 2 = 'aa'9 = 'missing';
VALUE gender 1 = 'Female ' 2 = 'Male';
Value hispanic 0= 'No' 1='Yes';
Value eyecolor 1= 'blue green or combo' 2='light/dark brown' 3='hazel';
Value haircolor 1= 'blonde' 2='red' 3='brown' 4='black';
RUN;
/* import excel file to SAS */ 
PROC IMPORT OUT= WORK.project DATAFILE= "C:\mole.xls" 
            DBMS=excel2002 REPLACE;
         GETNAMES=YES;
RUN;
/* print to check */ 
Proc Print data=project;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;

/* Produce descriptive statistics for different category */
Proc freq data = project;
table oca2_status gender hispanic haircolor;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;
Proc means data=project ;
Var  molecount2007 molecount2008 number_vacs__birth_thru_2006 number_vacs__birth_thru_2007 ;
Class oca2_status;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;
Proc means data=project ;
Var  molecount2007 molecount2008 number_vacs__birth_thru_2006 number_vacs__birth_thru_2007 ;
Class Gender;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;
Proc means data=project ;
Var  molecount2007 molecount2008 number_vacs__birth_thru_2006 number_vacs__birth_thru_2007 ;
Class hispanic;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;
Proc means data=project;
Var molecount2004 molecount2005 molecount2006 molecount2007 molecount2008;
Class Gender;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;

/* Anova analysis to analyze differences in sub groups */ 
ODS GRAPHICS ON;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/solution; 
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;
ODS GRAPHICS OFF;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS oca2_status;
 MODEL molecount2008 = oca2_status/solution; 
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;
ODS GRAPHICS OFF;

/* post-Hoc comparision:tukey to detect which two groups are differences*/ 
PROC GLM DATA = project ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/noint solution;
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
 MEANS oca2_status/ bon  tukey;
RUN;

PROC GLM DATA = project ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2008 = oca2_status/noint solution;
 MEANS oca2_status/ bon tukey;
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;

/* Anova analysis to analyze differences in sub groups */ 
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS gender;
 MODEL molecount2007 = gender/solution; 
RUN;
ODS GRAPHICS OFF;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS gender;
 MODEL molecount2008 = gender/solution; 
RUN;
ODS GRAPHICS OFF;

PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS hispanic;
 MODEL molecount2007 = hispanic/solution; 
RUN;
ODS GRAPHICS OFF;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS hispanic;
 MODEL molecount2008 = hispanic/solution; 
RUN;
ODS GRAPHICS OFF;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS haircolor;
 MODEL molecount2007 = haircolor/solution; 
RUN;
ODS GRAPHICS OFF;
PROC GLM DATA = project ORDER = internal PLOT=diagnostics;
 CLASS haircolor;
 MODEL molecount2008 = haircolor/solution; 
RUN;
ODS GRAPHICS OFF;


/* Produce descriptive statistics for molecount2007 */
PROC UNIVARIATE DATA=project;
    VAR molecount2007;
RUN;
/* Creat new data which indicate molecount as + if molecount > upper quintle for caculating odjusted OR */
Data projectone;
set project;
IF molecount2007<=53 THEN indic = '-';
  ELSE IF molecount2007>53  THEN indic = '+';
  Run;
Proc print data= projectone;
Run;

/* Produce descriptive statistics for different subgroups */
Proc freq data=projectone ;
table  indic*oca2_status;
Format oca2_status oca2_status. gender gender. hispanic hispanic. ;
Run;

Proc freq data=projectone ;
table  indic*oca2_status*gender;
Format oca2_status oca2_status. gender gender. hispanic hispanic. ;
Run;
Proc freq data=projectone ;
table  indic*oca2_status*gender*hispanic;
Format oca2_status oca2_status. gender gender. hispanic hispanic. ;
Run;

/* Produce descriptive statistics for different subgroups */
PROC FORMAT;
VALUE oca  0 = 'gg' 1 = 'ga' 2='aa' 3='ga/aa' 4='gg/aa' 5='gg/ga';
VALUE gender 1 = 'Female ' 2 = 'Male ';
Value hispanic 0= 'No' 1='Yes';
Value indic 0='-';1='+';
RUN;

/* creat categorical dataset to caculate adjusted OR for gg */
DATA oca0 ;
INPUT oca gender hispanic indic count;
CARDS;
0 1 0 0 87
0 1 0 1 33
0 1 1 0 4
0 1 1 1 2
0 2 0 0 68
0 2 0 1 26
0 2 1 0 8
0 2 1 1 0

3 1 0 0 86
3 1 0 1 17
3 1 1 0 20
3 1 1 1 1
3 2 0 0 64
3 2 0 1 29
3 2 1 0 27
3 2 1 1 0
;
RUN;
Proc print data=oca0;
FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
Run;
PROC FREQ data=oca0;
 TABLES oca*indic /CMH EXPECTED;
 Exact rror;
 WEIGHT count;
RUN;

/* Caculate crude and adjusted OR specified by gender or Hispanic */
PROC FREQ data=oca0;
 TABLES gender*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;

PROC FREQ data=oca0;
 TABLES hispanic*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;

/* creat categorical dataset to caculate adjusted OR for ga */
DATA oca1 ;
INPUT oca gender hispanic indic count;
CARDS;
1 1 0 0 75
1 1 0 1 13
1 1 1 0 14
1 1 1 1 1
1 2 0 0 49
1 2 0 1 26
1 2 1 0 17
1 2 1 1 0

4 1 0 0 98
4 1 0 1 37
4 1 1 0 10
4 1 1 1 2
4 2 0 0 83
4 2 0 1 29
4 2 1 0 18
4 2 1 1 0
;
RUN;
Proc print data=oca1;
FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
Run;
PROC FREQ data=oca1;
 TABLES oca*indic /CMH EXPECTED;
 Exact rror;
 WEIGHT count;
RUN;
/* Caculate crude and adjusted OR specified by gender or Hispanic */
PROC FREQ data=oca1;
 TABLES gender*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;
PROC FREQ data=oca1;
 TABLES hispanic*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;

/* creat categorical dataset to caculate adjusted OR for aa */
DATA oca2 ;
INPUT oca gender hispanic indic count;
CARDS;
2 1 0 0 11
2 1 0 1 4
2 1 1 0 6
2 1 1 1 0
2 2 0 0 15
2 2 0 1 3
2 2 1 0 10
2 2 1 1 0

5 1 0 0 162
5 1 0 1 46
5 1 1 0 18
5 1 1 1 3
5 2 0 0 117
5 2 0 1 52
5 2 1 0 25
5 2 1 1 0
;
RUN;
Proc print data=oca2;
FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
Run;
PROC FREQ data=oca2;
 TABLES oca*indic /CMH EXPECTED;
 Exact rror;
 WEIGHT count;
RUN;
/* Caculate crude and adjusted OR specified by gender or Hispanic */
PROC FREQ data=oca2;
 TABLES gender*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;
PROC FREQ data=oca2;
 TABLES hispanic*oca*indic /CMH EXPECTED RELRISK;
 WEIGHT count;
 FORMAT oca oca. gender gender. hispanic hispanic. indic indic.;
RUN;

/* Anova analysis to compare OCA varient stratified by gender */
/* Anova analysis to compare OCA varient in female */
Data projectadjusted;
Set project;
Where gender =1;
Run;
Proc print data =projectadjusted;
run;

PROC GLM DATA = projectadjusted ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;

/* Anova analysis to compare OCA varient in male */
Data projectadjusted2;
Set project;
Where gender =2;
Run;
Proc print data =projectadjusted2;
run;

Proc freq data=projectadjusted2;
table oca2_status;
run;
Proc means data =projectadjusted2;
Var  molecount2007 molecount2008;
Class oca2_status;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;

PROC GLM DATA = projectadjusted2 ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
  Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;

/* Anova analysis to compare OCA varient in non-hispanic */
Data projectadjusted4;
Set project;
Where hispanic =0;
Run;
Proc means data =projectadjusted4;
Var  molecount2007 molecount2008;
Class oca2_status;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;

PROC GLM DATA = projectadjusted4 ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
  Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;
/* post-Hoc comparision:tukey to detect which two groups are differences*/
PROC GLM DATA = projectadjusted4 ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;

/* Anova analysis to compare OCA varient in hispanic */
Data projectadjusted5;
Set project;
Where hispanic =1;
Run;
Proc means data =projectadjusted4;
Var  molecount2007 molecount2008;
Class oca2_status;
Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
Run;

PROC GLM DATA = projectadjusted5 ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
  Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;
/* post-Hoc comparision:tukey to detect which two groups are differences*/
PROC GLM DATA = projectadjusted5 ORDER=Internal;
 CLASS oca2_status;
 MODEL molecount2007 = oca2_status/ solution;
 MEANS oca2_status/ bon tukey;
 Format oca2_status oca2_status. gender gender. hispanic hispanic. eyecolor eyecolor. haircolor haircolor.;
RUN;




