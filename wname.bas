01 PROGRAM WNAME
10 INPUT "What is your name: ", U$
20 PRINT "Hello "; U$
30 INPUT "How many stars do you want: ", N
40 S$ = ""
50 FOR I = 1 TO N
60 S$ = S$ + "*"
70 NEXT I
80 PRINT S$
85 CALL PNAME
90 PRINT "Goodbye "; U$
100 END PROGRAM
