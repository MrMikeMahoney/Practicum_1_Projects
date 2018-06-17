/* Creating library called FA12 */
libname FA12 '\\Client\C$\Users\Mike\Documents\Practicum 1';
/* Creating file called bigrec */
filename bigrec '\\Client\C$\Users\Mike\Documents\Practicum 1\FA12_Data.txt' lrecl = 65576;
/*“I’m always the first among my friends to have the latest in electronic equipment” >> Reads 5 variables >>
Outputs a data frame with each my_id's response to each of the 5 variables
With a "1" indicating a correct choice*/
data first;
infile bigrec;
input my_id 1-7
first_agree_alot 6825
first_agree_little 6842
first_neither 6876
first_disagree_little 6893
first_disagree_alot 6910;
run;
/* #First Freq - Doing a Frequency for each of the 5 variables */
proc freq data=first;
tables
first_agree_alot 
first_agree_little 
first_neither 
first_disagree_little 
first_disagree_alot;
run;
/* #Change NA - Use an array to turn missing values to zeros */
data mycalcs;
set first;
array missy(1,5)
first_agree_alot 
first_agree_little 
first_neither 
first_disagree_little 
first_disagree_alot;
/* now make missy values 0 >> Loops through i (which is only one in our case because its one question) then 
j (which is each question) >> turning the "." into "0" >> Using an if/then statement */
do i = 1 to 1;
do j = 1 to 5;
if missy(i,j) = . then missy(i,j) = 0;
end;
end;
/*make array for 8 variable sums */
array mysum(1);
/*sum up the vars and make no mark or > 1 mark missing*/
/* now make each variable, meing sure to ignore zeros and larger than 1 */
do k = 1 to 1;
mysum(k) = missy(k,1) + missy(k,2) + missy(k,3) + missy(k,4) + missy(k,5);
end;
/* now if the variable is not zero or greater than 1 create var */
array myvar(1);
do m = 1 to 1;
if mysum(m) = 1 then
myvar(m) = (missy(m,1)*5) + (missy(m,2)*4) + (missy(m,3)*3) + (missy(m,4)*2) + (missy(m,5)*1);
else
myvar(m) = .;
end;
/* Changing the variable name to first */
first = myvar(1);
/*Creates a sum of myvar */
mysum1 = mysum(1);
run;

/* #Binary Freq Dist - Running a second freq distribution on to check if 1 - answered or 0- missing*/
proc freq data=mycalcs;
tables
first_agree_alot 
first_agree_little 
first_neither 
first_disagree_little 
first_disagree_alot;
run;

/* #Multiple Box - Check indicies of multiple check marks >> To see if our surveyors followed instructions*/
proc freq data=mycalcs;
tables
mysum1;
run;

/* #Sniffin' Vars - now sniff variables and compare - should be approx same as cell counts */
proc freq data=mycalcs;
tables
first;
run;
