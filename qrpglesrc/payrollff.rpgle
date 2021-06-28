       // ***********************************************************
       //   PROGRAM NAME - Payroll
       //   DESCRIPTION - Time reporting master file maintenance using
       //                 externally described workstation processing.
       // ***********************************************************
       //   INDICATORS USED
       //   50 - No record found on CHAIN operation
       //   60 - General error condition
       //   90 - Protect display on delete request
       //   KC - End of job requested
       //   KD - Return to application selection
       //   KE - Return to employee selection
       //   KF - Return to project selection
       //   KG - Return to reason code selection
       //   LR - Last record
       // ***********************************************************
       //   SUBROUTINES USED
       //   EDITSL - Edit application selection display (SELECT)
       //   ACDESR - Edit action code for all maintenance requests
       // ***********************************************************
       //   This program uses all externally described files.  Files
       //   used are - MSTDSP  - maintenance display file
       //            - EMPMST  - employee master file
       //            - PRJMST  - project master file
       //            - RSNMST  - reason code master file
       // ***********************************************************
       Dcl-F MSTDSP     WORKSTN;
       Dcl-F EMPMST     Usage(*Update:*Delete:*Output) Keyed;
       Dcl-F PRJMST     Usage(*Update:*Delete:*Output) Keyed;
       Dcl-F RSNMST     Usage(*Update:*Delete:*Output) Keyed;
       //
       //   Compile time array containing error descriptions.
       Dcl-S ERR             Char(50)        DIM(10) CTDATA PERRCD(1);
       Dcl-s i int(10);
       Dcl-S EMESS           Char(50);
       //
       // *****************************************************
       //   MAINLINE CALCULATIONS
       // *****************************************************
       //   This mainline routine controls the display file processing and
       //   editting.  Using the function keys described on each display
       //   format, you can transfer from one maintenance application to
       //   another.  The action code you select on the selection formats
       //   determines if the program will add a new record to the file or
       //   update an existing record in the file.
       // *****************************************************
       //   Housekeeping, clear display fields and reset indicators.
       //
       Exsr MAIN;
       //  If MAIN is done program ends
       *INLR = *on;
       //  MAIN SUBROUTINE
       BegSr MAIN;
         DoU *INKC;
           *IN60  = *OFF;
           EMESS  = *BLANK;
           EMPAPL = *BLANK;
           PRJAPL = *BLANK;
           RSNAPL = *BLANK;
       //
       //   Write the SELECT format to display.  If end of job requested,
       //
       //
           DoU not *IN60;
             Exfmt SELECT;
             If *INKC = '1';
               LeaveSr;
             Else;
               Exsr EDITSL;
             EndIf;
       //
       //   IF the general error indicator *IN60 is on (equal to 1), the
       //   program continues to loop
       //
           EndDo;
       //
       //   The application selection fields from the SELECT format are
       //   tested and the program will branch to the section specific to
       //   that application.
       //   If EMPAPL (employee maintenance) equals X, the program
       //   branches to label EMPTAG.
       //   If PRJAPL (project maintenance) equals X, the program
       //   branches to label PRJTAG.
       //   If the prior two tests were not successful, you have chosen
       //   reason code maintenance.  The program will continue with the
       //   next executable operation.
       //
           Select;
             When EMPAPL = 'X';
               Exsr EMPTAG;
       //
             When PRJAPL = 'X';
               Exsr PRJTAG;
       //
             When RSNAPL = 'X';
               Exsr RSNTAX;
           EndSl;
       //  if KC end program
           If *INKC;
             Leave;
           EndIf;
         EndDo;
       EndSr;
       //
       // *********************************************************
       //   Reason Code Maintenance.
       // *********************************************************
       //
       //   Housekeeping, clear display fields and reset indicators.
       //
       BegSr RSNTAG;
         DoU *INKC;
           EMESS = *Blanks;
           RSCDE = *Blanks;
           ACODE = *Blanks;
       //  start of error loop
           DoU not *IN60;
             RSDSC = *Blanks;
       //
       //   Display reason code selection format
       //
             Exfmt RSNSEL;
             If *INKD;
               LeaveSr;
             EndIf;
       //
       //   Access reason code master to validate action code request
       //
             If not *INKC;
               Chain RSCDE RSNMST;
               *IN50 = not %Found;
               Exsr ACDESR;
             Else;
               LeaveSr;
             EndIf;
       //  end of error loop
           EndDo;
       //
       //   Display reason code maintenance format
       //
           Exfmt RSNMNT;
       //
           If *INKD;
             LeaveSr;
           EndIf;
           If *INKG;
             Iter;
           EndIf;
       //
       //   Determine update mode and perform record add or update
       //
           If not *inkc;
       //
             Select;
               When ACODE = 'A' and *IN50;
                 ACREC = 'A';
                 Write RCRSN;
       //
               When ACODE = 'A' and not *IN50 and ACREC = 'D';
                 ACREC = 'A';
                 Update RCRSN;
       //
               When ACODE = 'D';
                 ACREC = 'D';
                 Update RCRSN;
       //
               When ACODE = 'C';
                 Update RCRSN;
             EndSl;
           Else;
             LeaveSr;
           EndIf;
       //
       //   Your maintenance request has now been completed and the
       //   program branches back to the RSNTAG TAG.
       //
         EndDo;
       EndSr;
       // *********************************************************
       //   Employee master maintenance routine.
       // *********************************************************
       //
       //   Housekeeping, clear display fields and reset indicators.
       //
       BegSr EMPTAG;
         DoU *INKC;
           *IN60 = '0';
           EMESS = *Blanks;
           EMPNO = 0;
           ACODE = *Blanks;
       //  error loop start
           DoU not *IN60;
             ENAME = *Blanks;
             EMCAT = *Blanks;
             EDEPT = *Blanks;
             ELOCN = *Blanks;
             EUSRI = *Blanks;
             ENHRS = 0;
       //
       //   Display employee selection format
       //
             Exfmt EMPSEL;
       //  leave subroutine
       //
             If *INKD;
               LeaveSr;
             EndIf;
       //
       //   Access employee master to validate action code request
       //
             If NOT *INKC;
               Chain EMPNO EMPMST;
               *IN50 = not %Found;
               Exsr ACDESR;
             Else;
               LeaveSr;
             EndIf;
       //  end of error loop
           EndDo;
       //
       //   Display employee maintenance format
       //
           Exfmt EMPMNT;
           If *INKD;
       // end of subroutine
             LeaveSr;
           EndIf;
           If *INKE;
       //  End of one loop
             Iter;
           EndIf;
       //
       //   Determine update mode and perform record add or update
       //
           If *INKC = *OFF;
             Select;
               When ACODE ='A'  and *IN50;
                 ACREC = 'A';
                 Write RCEMP;
       //
               When ACODE = 'A' and not *IN50 and ACREC = 'D';
                 ACREC = 'A';
                 Update RCEMP;
       //
               When ACODE = 'D';
                 ACREC = 'D';
                 Update RCEMP;
       //
               When ACODE = 'C';
                 Update RCEMP;
             EndSl;
           Else;
             LeaveSr;
           EndIf;
       //
       //   Your maintenance request has now been completed and the
       //   program branches back to the EMPTAG TAG.
       //
         EndDo;
       EndSr;
       // *********************************************************
       //   Project master maintenance routine.
       // *********************************************************
       //
       //   Housekeeping, clear display fields and reset indicators.
       //
       BegSr PRJTAG;
         DoU *INKC;
           *IN60 = '0';
           EMESS = *Blanks;
           PRCDE = *Blanks;
           ACODE = *Blanks;
           DoU not *IN60;
             PRDSC = *Blanks;
             PRRSP = *Blanks;
             PRSTR = 0;
             PREND = 0;
             PRCMP = 0;
             PREST = 0;
       //
       //   Display project selection format
       //
             Exfmt PRJSEL;
             If *INKD;
               LeaveSr;
             EndIf;
       //
       //   Access project master to validate action code request
       //
             If not *INKC;
               Chain PRCDE PRJMST;
               *IN50 = not %Found;
               Exsr ACDESR;
             Else;
               LeaveSr;
             EndIf;
           EndDo;
       //
       //   Display project maintenance format
       //
           Exfmt PRJMNT;
       //  leave subroutine
           If *INKD;
             LeaveSr;
           EndIf;
       //  end of loop
           If *INKF;
             Iter;
           EndIf;
       //
       //   Determine update mode and perform record add or update
       //
           If *INKC = '0';
             Select;
               When  ACODE = 'A'  and *IN50;
                 ACREC = 'A';
                 Write RCPRJ;
       //
               When ACODE = 'A' and *IN50 and ACREC = 'D';
                 ACREC = 'A';
                 Update RCPRJ;
       //  Delete OP
               When ACODE = 'D';
                 ACREC = 'D';
                 Update RCPRJ;
       //  Change OP
               When ACODE = 'C';
                 Update RCPRJ;
             EndSl;
           Else;
             LeaveSr;
           EndIf;
         EndDo;
       //
       //   Your maintenance request has now been completed and the
       //   program branches back to the PRJTAG TAG.
       //
       //                   GOTO      PRJTAG
       EndSr;
       // *********************************************************
       //   End of job requested.  Control is passed to here when you press
       //   F3 (*INKC).  The last record indicator *INLR is set on and the
       //   program ends.
       //
       //                   ENDSR
       //
       // *******************************************************
       //   EDITSL subroutine verifies the time reoprting application
       //   selection display input.
       // *******************************************************
       BegSr EDITSL;
       //
       //   Housekeeping, clear display fields and reset indicators.
       //
         EMESS = *Blanks;
         *IN60 = *OFF;
       //
       //   The following IF AND OR combination checks the application
       //   selection fields to ensure that only one application has been
       //   selected.
       //
         If EMPAPL = 'X'
           and PRJAPL = 'X'
           or EMPAPL = 'X'
           and RSNAPL = 'X';
       //     THE BUG IS HERE
       //     ***************
           *IN60 = '2';
           EMES = ERR(2);
         Else;
           *IN60 = '0';
         EndIf;
       //
       //   The following IF AND combination ensures that at least one
       //   application has been selected.
       //
         If EMPAPL = ' '
           and PRJAPL = ' '
           and RSNAPL = ' ';
           *IN60 = '1';
           EMESS = ERR(3);
         EndIf;
       //
       //   The following code checks each application selection field to
       //   ensure that it is either ' ' (blank) or equal to 'X'.
       //
         If EMPAPL <> ' '
           and EMPAPL <> 'X';
           *IN60 = '1';
           EMESS = ERR(1);
         EndIf;
         If PRJAPL <> ' '
           and PRJAPL <> 'X';
           *IN60 = '1';
           EMESS = ERR(1);
         EndIf;
         If RSNAPL <> ' '
           and RSNAPL <> 'X';
           *IN60 = '1';
           EMESS = ERR(1);
         EndIf;
       //EndSr;
       //
       // *******************************************************
       //   ACDESR subroutine verifies the time reporting action codes for
       //   all maintenance selections.
       // *******************************************************
       BegSr ACDESR;
       //
       //   Housekeeping, clear display fields and reset indicators.
       //
         EMESS = *BLANKS;
         *IN60 = *OFF;
         *IN90 = *OFF;
       //
       //   The following  statements perform
       //   two functions.  First they determine the type of maintenance
       //   requested and branche to the appropriate subroutine and secondly
       //   they determine if the maintenance code entered is invalid.
       //
         Select;
           When ACODE = 'A';
             Exsr ADDCDE;
           When ACODE = 'C';
             Exsr CHGCDE;
           When ACODE = 'D';
             Exsr DELCDE;
           Other;
             Exsr INVCDE;
         EndSl;
       EndSr;
       //
       //   The following code verifies the add request.
       //
       BegSr ADDCDE;
         If NOT *IN50 AND ACREC = 'A';
           *IN60 = *ON;
           EMESS = ERR(5);
         Else;
           If NOT *IN50 AND ACREC = 'D';
             EMESS = ERR(6);
           EndIf;
         EndIf;
       EndSr;
       //
       //   The following code verifies the change request.
       //
       BegSr CHGCDE;
         If *IN50;
           *IN60 = *ON;
           EMESS = ERR(7);
         Else;
           If NOT *IN50 AND ACREC = 'D';
             *IN60 = *ON;
             EMESS = ERR(8);
           EndIf;
         EndIf;
       EndSr;
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
       //
       //   The compile time array ERR is entered below.  The array is
       //   precceded by "** " to denote the beginning of the array.
       //
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
