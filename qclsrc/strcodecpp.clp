PGM PARM(&server )

DCL VAR(&server)  TYPE(*CHAR) LEN(10)

DCL VAR(&BIN4)    TYPE(*CHAR) LEN(4)
DCL VAR(&RCV)     TYPE(*CHAR) LEN(893)
DCL VAR(&RCVLEN)  TYPE(*CHAR) LEN(4)
DCL VAR(&RCVLEND) TYPE(*DEC)  LEN(9)
DCL VAR(&ERR)     TYPE(*CHAR) LEN(100)
DCL VAR(&DEC8)    TYPE(*DEC)  LEN(8 0)
DCL VAR(&DEVD)    TYPE(*CHAR) LEN(10)
DCL VAR(&FRMDEV)  TYPE(*CHAR) LEN(8)  VALUE('DEVD0600')
DCL VAR(&IP)      TYPE(*CHAR) LEN(15)

CHGVAR     VAR(&RCVLEND) VALUE(893)
CHGVAR     VAR(%BIN(&RCVLEN)) VALUE(&RCVLEND)
RTVJOBA    JOB(&DEVD)

CHGVAR     VAR(%BINARY(&BIN4)) VALUE(&DEC8)

             CALL       PGM(QDCRDEVD) PARM(&RCV &RCVLEN &FRMDEV +
                          &DEVD &ERR)
             CHGVAR     VAR(&IP)     VALUE(%SST(&RCV 878 15))
             STRCODE    &server RMTLOCNAME(&IP) CMNTYPE(*TCPIP)


 ENDPGM:     ENDPGM
