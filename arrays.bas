PROGRAM ARRAYS

DIM WORD array_x(1 TO 10, 1 TO 6)
        FOR column = 1 TO 6
            FOR row = 1 TO 10
                array_x(row,column)=(10*row + column)
                PRINT array_x(row,column);
            NEXT row
            PRINT
        NEXT column
        PRINT



END PROGRAM
