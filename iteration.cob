       IDENTIFICATION DIVISION.
       PROGRAM-ID.    Iteration.
       AUTHOR.        rdoyle.


       ENVIRONMENT DIVISION.
	   DATA DIVISION.
	   WORKING-STORAGE SECTION.
	       01  Num1           PIC 99  VALUE ZEROS.
	       01  Num2           PIC 99  VALUE ZEROS.
	       01  Result         PIC 9999 VALUE ZEROS.
	       01  Operator       PIC X  VALUE SPACE.
		
	   PROCEDURE DIVISION.
	   MAIN SECTION.
		   DISPLAY "Enter First Number      : ".
	       ACCEPT Num1.
	       DISPLAY Num1.
	       DISPLAY "Enter Second Number     : ".
	       ACCEPT Num2.
	       DISPLAY Num2.
	       DISPLAY "Enter operator (+ or *) : ".
	       ACCEPT Operator.
	       DISPLAY Operator.
	       IF Operator = "+" THEN
	        ADD Num1, Num2 GIVING Result
	       END-IF.
	       IF Operator = "*" THEN
	        MULTIPLY Num1 BY Num2 GIVING Result
	       END-IF.
	       DISPLAY "Result is = ", Result.
	       STOP RUN.
	   END PROGRAM Iteration.
