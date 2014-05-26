IDENTIFICATION DIVISION.
PROGRAM-ID.  Multiplier.
* Example program using ACCEPT, DISPLAY and MULTIPLY to 
* get two single digit numbers from the user and multiply them together
*
DATA DIVISION.

WORKING-STORAGE SECTION.
01  Num1                                PIC 9  VALUE ZEROS.
01  Num2                                PIC 9  VALUE ZEROS.
01  Result                              PIC 99 VALUE ZEROS.

PROCEDURE DIVISION.
MAIN SECTION.

    DISPLAY "Enter first number  (1 digit) : ".
    ACCEPT Num1.
    DISPLAY "Enter second number (1 digit) : ".
    ACCEPT Num2.
    MULTIPLY Num1 BY Num2 GIVING Result.
    DISPLAY "Result is = ", Result.
END program Multiplier.
