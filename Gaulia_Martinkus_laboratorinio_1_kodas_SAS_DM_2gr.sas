PROC IMPORT DATAFILE='/home/u45871880/data.csv'
	DBMS=CSV
	OUT=data;
	GETNAMES=YES;
RUN;


PROC SORT data=data;
    BY sex;
RUN;

data colours;
  length value FillColor LineColor $30;
  Id='X'; Value="male"; FillColor='#799fcb'; LineColor='#799fcb'; output;
  Id='X'; Value="female"; FillColor='#f9665e'; LineColor='#f9665e'; output;
run;


/* Tiriamieji grafikai */
%macro box;
%do m=1 %to 4;

proc sgplot data=data dattrmap=colours;
vbox t&m/group=sex category=handedness attrid=X;

%end;
%mend;
%box;
RUN;


/* Vidurkių grafikai */
%macro box;
%do m=1 %to 4;

proc sgplot data=data dattrmap=colours;
   vline handedness / response=t&m group=sex stat=mean attrid=X;
run;

%end;
%mend;
%box;
RUN;


/* Dispersinė analizė */
%macro box;
%do m=1 %to 4;

proc glm data = data plots=diagnostics;
class handedness sex;
model t&m = handedness sex handedness*sex;
lsmeans handedness*sex /adjust=TUKEY linestable plots=None;

%end;
%mend;
%box;
RUN;


/* Transformuoti duomenys */

PROC IMPORT DATAFILE='/home/u45871880/data2.csv'
	DBMS=CSV
	OUT=data2;
	GETNAMES=YES;
RUN;



proc glm data = data2 plots=diagnostics;
class handedness sex;
model t4 = handedness sex handedness*sex;
lsmeans handedness*sex /adjust=TUKEY linestable plots=None;
run;