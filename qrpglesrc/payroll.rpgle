     F************************************************************
     F*  PROGRAM NAME - Payroll
     F*  DESCRIPTION - Time reporting master file maintenance using
     F*                externally described workstation processing.
     F************************************************************
     F*  INDICATORS USED
     F*  50 - No record found on CHAIN operation
     F*  60 - General error condition
     F*  90 - Protect display on delete request
     F*  KC - End of job requested
     F*  KD - Return to application selection
     F*  KE - Return to employee selection
     F*  KF - Return to project selection
     F*  KG - Return to reason code selection
     F*  LR - Last record
     F************************************************************
     F*  SUBROUTINES USED
     F*  EDITSL - Edit application selection display (SELECT)
     F*  ACDESR - Edit action code for all maintenance requests
     F************************************************************
     F*  This program uses all externally described files.  Files
     F*  used are - MSTDSP  - maintenance display file
     F*           - EMPMST  - employee master file
     F*           - PRJMST  - project master file
     F*           - RSNMST  - reason code master file
     F************************************************************
     FMSTDSP    CF   E             WORKSTN
     FEMPMST    UF A E           K DISK
     FPRJMST    UF A E           K DISK
     FRSNMST    UF A E           K DISK
     D*
     D*  Compile time array containing error descriptions.
     D ERR             S             50    DIM(10) CTDATA PERRCD(1)
     D EMESS           S             50
     E*
     C******************************************************
     C*  MAINLINE CALCULATIONS
     C******************************************************
     C*  This mainline routine controls the display file processing and
     C*  editting.  Using the function keys described on each display
     C*  format, you can transfer from one maintenance application to
     C*  another.  The action code you select on the selection formats
     C*  determines if the program will add a new record to the file or
     C*  update an existing record in the file.
     C******************************************************
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C                   EXSR      MAIN
     C* If MAIN is done program ends
     C                   eval      *INLR = *on
      * MAIN SUBROUTINE
     C     MAIN          BEGSR
     C                   dou       *INKC
     C                   EVAL      *IN60  = *OFF
     C                   EVAL      EMESS  = *BLANK
     C                   EVAL      EMPAPL = *BLANK
     C                   EVAL      PRJAPL = *BLANK
     C                   EVAL      RSNAPL = *BLANK
     C*
     C*  Write the SELECT format to display.  If end of job requested,
     C*
     C*
     C                   DOU       not *IN60
     C                   EXFMT     SELECT
     C     *INKC         IFEQ      '1'
     C                   LEAVESR
     C                   ELSE
     C                   EXSR      EDITSL
     C                   END
     C*
     C*  IF the general error indicator *IN60 is on (equal to 1), the
     C*  program continues to loop
     C*
     C                   ENDdo
     C*
     C*  The application selection fields from the SELECT format are
     C*  tested and the program will branch to the section specific to
     C*  that application.
     C*  If EMPAPL (employee maintenance) equals X, the program
     C*  branches to label EMPTAG.
     C*  If PRJAPL (project maintenance) equals X, the program
     C*  branches to label PRJTAG.
     C*  If the prior two tests were not successful, you have chosen
     C*  reason code maintenance.  The program will continue with the
     C*  next executable operation.
     C*
     C                   SELECT
     C                   when      EMPAPL = 'X'
     C                   EXSR      EMPTAG
     C*
     C                   when      PRJAPL = 'X'
     C                   EXSR      PRJTAG
     C*
     C                   when      RSNAPL = 'X'
     C                   EXSR      RSNTAg
     C                   ENDSL
     C* if KC end program
     C                   if        *INKC
     C                   leave
     C                   endif
     C                   enddo
     C                   endsr
     C*
     C**********************************************************
     C*  Reason Code Maintenance.
     C**********************************************************
     C*
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C     RSNTAG        BEGSR
     C                   DOU       *INKC
     C                   MOVE      *BLANKS       EMESS
     C                   MOVE      *BLANKS       RSCDE
     C                   MOVE      *BLANKS       ACODE
     C* start of error loop
     C                   DOU       not *IN60
     C                   MOVE      *BLANKS       RSDSC
     C*
     C*  Display reason code selection format
     C*
     C                   EXFMT     RSNSEL
     C                   IF        *INKD
     C                   LEAVeSR
     C                   END
     C*
     C*  Access reason code master to validate action code request
     C*
     C                   IF        not *INKC
     C     RSCDE         CHAIN     RSNMST                             50
     C                   EXSR      ACDESR
     C                   ELSE
     C                   LEAVESR
     C                   END
     C* end of error loop
     C                   ENDDO
     C*
     C*  Display reason code maintenance format
     C*
     C                   EXFMT     RSNMNT
     C*
     C                   IF        *INKD
     C                   LEAVESR
     C                   END
     C                   IF        *INKG
     C                   ITER
     C                   END
     C*
     C*  Determine update mode and perform record add or update
     C*
     C                   IF        not *inkc
     C
     C                   Select
     C                   WHEN      ACODE = 'A' and *IN50
     C                   MOVE      'A'           ACREC
     C                   WRITE     RCRSN
     C*
     C                   WHEN      ACODE = 'A' and not *IN50 and ACREC = 'D'
     C                   MOVE      'A'           ACREC
     C                   UPDATE    RCRSN
     C*
     C                   When      ACODE = 'D'
     C                   MOVE      'D'           ACREC
     C                   UPDATE    RCRSN
     C*
     C                   WHEN      ACODE = 'C'
     C                   UPDATE    RCRSN
     C                   ENDSL
     C                   ELSE
     C                   LEAVESR
     C                   END
     C*
     C*  Your maintenance request has now been completed and the
     C*  program branches back to the RSNTAG TAG.
     C*
     C                   ENDDO
     C                   ENDsr
     C**********************************************************
     C*  Employee master maintenance routine.
     C**********************************************************
     C*
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C     EMPTAG        BEGSR
     C                   dou       *INKC
     C                   MOVE      '0'           *IN60
     C                   MOVE      *BLANKS       EMESS
     C                   Z-ADD     0             EMPNO
     C                   MOVE      *BLANKS       ACODE
     C* error loop start
     C                   dou       not *IN60
     C                   MOVE      *BLANKS       ENAME
     C                   MOVE      *BLANKS       EMCAT
     C                   MOVE      *BLANKS       EDEPT
     C                   MOVE      *BLANKS       ELOCN
     C                   MOVE      *BLANKS       EUSRI
     C                   Z-ADD     0             ENHRS
     C*
     C*  Display employee selection format
     C*
     C                   EXFMT     EMPSEL
     C* leave subroutine
     C*
     C                   IF        *INKD
     C                   leavesr
     C                   END
     C*
     C*  Access employee master to validate action code request
     C*
     C                   IF        NOT *INKC
     C     EMPNO         CHAIN     EMPMST                             50
     C                   EXSR      ACDESR
     C                   ELSE
     C                   LEAVESR
     C                   END
     C* end of error loop
     C                   ENDdo
     C*
     C*  Display employee maintenance format
     C*
     C                   EXFMT     EMPMNT
     C                   IF        *INKD
     C*end of subroutine
     C                   LEAVESR
     C                   END
     C                   IF        *INKE
     C* End of one loop
     C                   iter
     C                   END
     C*
     C*  Determine update mode and perform record add or update
     C*
     C                   IF        *INKC = *OFF
     C                   select
     C                   when      ACODE ='A'  and *IN50
     C                   MOVE      'A'           ACREC
     C                   WRITE     RCEMP
     C*
     C                   when      ACODE = 'A' and not *IN50 and ACREC = 'D'
     C                   MOVE      'A'           ACREC
     C                   UPDATE    RCEMP
     C*
     C                   when      ACODE = 'D'
     C                   MOVE      'D'           ACREC
     C                   UPDATE    RCEMP
     C*
     C                   when      ACODE = 'C'
     C                   UPDATE    RCEMP
     C                   ENDsl
     C                   ELSE
     C                   LEAVESR
     C                   END
     C*
     C*  Your maintenance request has now been completed and the
     C*  program branches back to the EMPTAG TAG.
     C*
     C                   enddo
     C                   ENDSR
     C**********************************************************
     C*  Project master maintenance routine.
     C**********************************************************
     C*
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C     PRJTAG        begsr
     C                   DOU       *INKC
     C                   MOVE      '0'           *IN60
     C                   MOVE      *BLANKS       EMESS
     C                   MOVE      *BLANKS       PRCDE
     C                   MOVE      *BLANKS       ACODE
     C                   DOU       not *IN60
     C                   MOVE      *BLANKS       PRDSC
     C                   MOVE      *BLANKS       PRRSP
     C                   Z-ADD     0             PRSTR
     C                   Z-ADD     0             PREND
     C                   Z-ADD     0             PRCMP
     C                   Z-ADD     0             PREST
     C*
     C*  Display project selection format
     C*
     C                   EXFMT     PRJSEL
     C                   IF        *INKD
     C                   leavesr
     C                   END
     C*
     C*  Access project master to validate action code request
     C*
     C                   IF        not *INKC
     C     PRCDE         CHAIN     PRJMST                             50
     C                   EXSR      ACDESR
     C                   ELSE
     C                   LEAVESR
     C                   END
     C                   ENDdo
     C*
     C*  Display project maintenance format
     C*
     C                   EXFMT     PRJMNT
     C* leave subroutine
     C                   IF        *INKD
     C                   LEAVESR
     C                   END
     C* end of loop
     C                   IF        *INKF
     C                   ITER
     C                   END
     C*
     C*  Determine update mode and perform record add or update
     C*
     C     *INKC         IFEQ      '0'
     C                   SELECT
     C                   WHEN       ACODE = 'A'  and *IN50
     C                   MOVE      'A'           ACREC
     C                   WRITE     RCPRJ
     C*
     C                   WHEN      ACODE = 'A' and *IN50 and ACREC = 'D'
     C                   MOVE      'A'           ACREC
     C                   UPDATE    RCPRJ
     C* Delete OP
     C                   WHEN      ACODE = 'D'
     C                   MOVE      'D'           ACREC
     C                   UPDATE    RCPRJ
     C* Change OP
     C                   WHEN      ACODE = 'C'
     C                   UPDATE    RCPRJ
     C                   ENDSL
     C                   ELSE
     C                   LEAVESR
     C                   END
     C                   ENDDO
     C*
     C*  Your maintenance request has now been completed and the
     C*  program branches back to the PRJTAG TAG.
     C*
     C*                  GOTO      PRJTAG
     C                   endsr
     C**********************************************************
     C*  End of job requested.  Control is passed to here when you press
     C*  F3 (*INKC).  The last record indicator *INLR is set on and the
     C*  program ends.
     C*
     C*                  ENDSR
     C*
     C********************************************************
     C*  EDITSL subroutine verifies the time reoprting application
     C*  selection display input.
     C********************************************************
     C     EDITSL        BEGSR
     C*
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C                   MOVE      *BLANKS       EMESS
     C                   EVAL      *IN60 = *OFF
     C*
     C*  The following IF AND OR combination checks the application
     C*  selection fields to ensure that only one application has been
     C*  selected.
     C*
     C     EMPAPL        IFEQ      'X'
     C     PRJAPL        ANDEQ     'X'
     C     EMPAPL        OREQ      'X'
     C     RSNAPL        ANDEQ     'X'
     C*    THE BUG IS HERE
     C*    ***************
     C                   MOVE      '1'           *IN60
     C                   MOVE      ERR(2)        EMESS
     C                   ELSE
     C                   MOVE      '0'           *IN60
     C                   END
     C*
     C*  The following IF AND combination ensures that at least one
     C*  application has been selected.
     C*
     C     EMPAPL        IFEQ      ' '
     C     PRJAPL        ANDEQ     ' '
     C     RSNAPL        ANDEQ     ' '
     C                   MOVE      '1'           *IN60
     C                   MOVE      ERR(3)        EMESS
     C                   END
     C*
     C*  The following code checks each application selection field to
     C*  ensure that it is either ' ' (blank) or equal to 'X'.
     C*
     C     EMPAPL        IFNE      ' '
     C     EMPAPL        ANDNE     'X'
     C                   MOVE      '1'           *IN60
     C                   MOVE      ERR(1)        EMESS
     C                   END
     C     PRJAPL        IFNE      ' '
     C     PRJAPL        ANDNE     'X'
     C                   MOVE      '1'           *IN60
     C                   MOVE      ERR(1)        EMESS
     C                   END
     C     RSNAPL        IFNE      ' '
     C     RSNAPL        ANDNE     'X'
     C                   MOVE      '1'           *IN60
     C                   MOVE      ERR(1)        EMESS
     C                   END
     C                   ENDSR
     C*
     C********************************************************
     C*  ACDESR subroutine verifies the time reporting action codes for
     C*  all maintenance selections.
     C********************************************************
     C     ACDESR        BEGSR
     C*
     C*  Housekeeping, clear display fields and reset indicators.
     C*
     C                   EVAL      EMESS = *BLANKS
     C                   EVAL      *IN60 = *OFF
     C                   EVAL      *IN90 = *OFF
     C*
     C*  The following  statements perform
     C*  two functions.  First they determine the type of maintenance
     C*  requested and branche to the appropriate subroutine and secondly
     C*  they determine if the maintenance code entered is invalid.
     C*
     C                   SELECT
     C                   WHEN      ACODE = 'A'
     C                   EXSR      ADDCDE
     C                   WHEN      ACODE = 'C'
     C                   EXSR      CHGCDE
     C                   WHEN      ACODE = 'D'
     C                   EXSR      DELCDE
     C                   OTHER
     C                   EXSR      INVCDE
     C                   ENDSL
     C                   ENDSR
     C*
     C*  The following code verifies the add request.
     C*
     C     ADDCDE        BEGSR
     C                   IF        NOT *IN50 AND ACREC = 'A'
     C                   EVAL      *IN60 = *ON
     C                   MOVE      ERR(5)        EMESS
     C                   ELSE
     C                   IF        NOT *IN50 AND ACREC = 'D'
     C                   MOVE      ERR(6)        EMESS
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*
     C*  The following code verifies the change request.
     C*
     C     CHGCDE        BEGSR
     C                   IF        *IN50
     C                   EVAL      *IN60 = *ON
     C                   MOVE      ERR(7)        EMESS
     C                   ELSE
     C                   IF        NOT *IN50 AND ACREC = 'D'
     C                   EVAL      *IN60 = *ON
     C                   MOVE      ERR(8)        EMESS
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      /FREE
       //*
       //*  The following code verifies the delete request.  The field
       //*  protect indicator *IN90 is first set on (equal to 1) to not
       //*  allow changes to existing data on a delete request.
       //*
       BEGSR DELCDE;
       *IN90 = *ON;
       IF *IN50;
          *IN60 = *ON;
          EMESS =  ERR(9);
       ELSE;
          IF NOT  *IN50 AND ACREC = 'D';
             *IN60 = *ON;
             EMESS = ERR(10);
          ENDIF;
       ENDIF;
       ENDSR;
       //*
       //*  Invalid action code
       //*
       BEGSR INVCDE;
       *IN60 = *ON;
       EMESS = ERR(4);
       ENDSR;
      /END-FREE
     O*
     O*  The compile time array ERR is entered below.  The array is
     O*  precceded by "** " to denote the beginning of the array.
     O*
**  Array ERR - Error descriptions
   MAINTENANCE SELECTION CODE NOT EQUAL TO "X"
MORE THAN ONE APPLICATION SELECTED FOR MAINTENANCE
     NO APPLICATION SELECTED FOR MAINTENANCE
    ACTION CODE NOT EQUAL TO "A", "C" OR "D"
 ADD REQUESTED BUT RECORD ALREADY EXISTS IN FILE
    WARNING - RECORD WAS PREVIOUSLY DELETED
   CHANGE REQUESTED BUT RECORD DOES NOT EXIST
     CHANGE REQUESTED BUT RECORD IS DELETED
   DELETE REQUESTED BUT RECORD DOES NOT EXIST
   DELETE REQUESTED BUT RECORD ALREADY DELETED
