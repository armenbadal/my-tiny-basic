10 LET n = 5
20 IF n = 0 THEN GOTO 80
30 GOSUB 60
40 LET n = n - 1
50 GOTO 20
60 PRINT n
70 RETURN 
80 END
