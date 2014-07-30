******* PROGRAM_NAME: [ICA_TRB71H_SET_ACCUM_IKUL.COB] **********
*                                                                   *
*                                    [םירצומ תכרעמ תת] :  תכרעמ תת  *
*                                 [םירבוצ תריגס ךילהת] : לודומה םש  *
*       [םילוקיע ירבוצל זוזיק תיצקידניא ןוכדעל הניטור] :הניטורה םש  *
*                                                                   *
*                                                 :ילנויצקנופ רואת  *
*                                                                   *
* (לטובמ)4-מ הנוש םהלש רבוצה סוטטסש םילוקיעה ירבוצ םע ץבוק          *
*             ףסכ שי רבוצבו ,2-מ הנוש םהלש זוזיקה תייצפואו          * 
*        2-ל זוזיקה תייצפוא תא ןכדעל שי ,הזכ רבוצ לכ רובעב          *
*                      םירבוצ תיירוטסה תלבטב המושר ףיסוהלו          *
*                                                                   *
*                                                          :םיחתפמ  *
*                                                                   *
*          ןורחא ןוכדע .ת        ךיראת       עצבמ םש       בלש      *
*                  [    ]    [6-Jul-2012] [TP_ONISSA]      בוציע    *
*                  [    ]    [6-Jul-2012] [TP_ONISSA]      תונכת    *
*                                                                   *
*                                                         :םירטמרפ  *
*                                                                   *
*                                           [    ] :עדימ תינבת      *
*          (שומיש ,םיכרע ,רבסה) רואת   פ/ק           הדש םש         *
*                             [    ]  [  ]           [    ]         *
*                                                                   *
*                                                      :עדימ ירגאמ  *
*                                                                   *
*                תירבעב םש / רואת    פ/ק             הלבט/ץבוק      *
*                          [    ]   [  ]                [    ]      *
*                                                                   *
*                                                   :םישגדהו תורעה  *
*                                                                   *
*                                                       [    ]      *
*                                                                   *
*                                           :םייונישו םינוכדע בקעמ  *
*                                                                   *
*                     יונישה רואת     עצבמ םש         ךיראת    CID  *
* --------------------------------- ------------- ----------- ----- *
*********************************************************************
**********test
*********************************************************************
 IDENTIFICATION DIVISION.
*********************************************************************
 PROGRAM-ID. ICA_TRB71H_SET_ACCUM_IKUL.
 AUTHOR.     TP_ONISSA.

*********************************************************************
 ENVIRONMENT DIVISION.
*********************************************************************
*--------------------------------------------------------------------
 INPUT-OUTPUT                            SECTION.
*--------------------------------------------------------------------
 FILE-CONTROL.

*   755 - םילוקיע ירבוצל זוזיק תייצקידניא ןוכדעל תניטורל טלק ץבוק
    SELECT K_TRB71H_755_IKL    
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   755 - ןוכדעל רבוצ ינותנ טלפ ץבוק
    SELECT P_TRB71H_755     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   875 - ןוכדעל רבוצ תירוטסה ינותנ טלפ ץבוק
    SELECT P_TRB71H_875     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*********************************************************************
 DATA DIVISION.
*********************************************************************
*--------------------------------------------------------------------
 FILE SECTION.
*--------------------------------------------------------------------
 FD K_TRB71H_755_IKL
    VALUE OF ID IS L$_K_TRB71H_755_IKL_NAME.
    COPY "ICD_CDD_WKSP:ICD_PRT_755_DBCR_STOP_PRW" FROM DICTIONARY
       REPLACING    ICD_PRT_755_DBCR_STOP_PRW
              BY    K_TRB71H_755_IKL_REC.                   

 FD P_TRB71H_755
    VALUE OF ID IS L$_P_TRB71H_755_NAME.
    COPY "ICD_CDD_WKSP:ICD_PRT_755_DBCR_STOP_PRW"   FROM DICTIONARY
       REPLACING    ICD_PRT_755_DBCR_STOP_PRW
              BY    P_TRB71H_755_REC.                   

 FD P_TRB71H_875
    VALUE OF ID IS L$_P_TRB71H_875_NAME.
    COPY "ICD_CDD_WKSP:ICD_PRT_875_HST_STP_UPD"   FROM DICTIONARY
       REPLACING    ICD_PRT_875_HST_STP_UPD
              BY    P_TRB71H_875_REC.                   

*--------------------------------------------------------------------
 WORKING-STORAGE                         SECTION.
*--------------------------------------------------------------------
 01 ICA_ICF_EXCEPTION_HANDLER       PIC S9(9) COMP VALUE EXTERNAL ICA_ICF_EXCEPTION_HANDLER.

 01 L$_WS01_FILES_NAME.
    03 L$_K_TRB71H_755_IKL_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(16)	    VALUE "ICA_TRB71H_0040_".
       05 FILLER		    PIC X(04)	    VALUE "ILK_".
       05 L$_WS01_OPER_ID	    PIC 9(16).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71H_755_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(15)	    VALUE "ICA_TRB71H_755_".
       05 FILLER		    PIC X(07)	    VALUE "DEDUCT_".
       05 L$_WS01_OPER_ID	    PIC 9(16).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71H_875_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(15)	    VALUE "ICA_TRB71H_875_".
       05 FILLER		    PIC X(07)	    VALUE "DEDUCT_".
       05 L$_WS01_OPER_ID	    PIC 9(16).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

 01 L$_WS01_VARAIBLES.
    03 L$_WS01_BELONG_TYPE_CODE     PIC 9(02).
 
 01 L$_SW01_SWITCHES.
    03 L$_SW01_EOF_INP_FILE	    PIC 9(01)       VALUE 0.
       88 L$_SW01_EOF_INP_FILE_NO		    VALUE 0.
       88 L$_SW01_EOF_INP_FILE_YES		    VALUE 1.

 01 L$_CT01_COUNTERS.
    03 L$_CT01_REC_READ             PIC 9(09).
    03 L$_CT01_REC_754              PIC 9(09).
    03 L$_CT01_REC_755              PIC 9(09).
    03 L$_CT01_REC_875              PIC 9(09).

 01 L$_CO01_CONSTANTS.
    03 L$_CO01_PRIORITY_2	    PIC 9(04)	    VALUE 2.

* UTL wksp and inc copies
*------------------------
 COPY 'UTL_CDD_WKSP:UTL_EXIT_ROUTINE_WKSP'           FROM DICTIONARY.
 COPY 'UTL_CDD_WKSP:UTL_COBRMS_VALUE_WKSP'           FROM DICTIONARY.

 COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".
 COPY 'UTL_SOURCE:UTL_MESSAGE.INC'.

* ICA wksp and inc copies
*------------------------
 COPY "ICA_CDD_WKSP:ICA_RMS_MSG_WKSP"                FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_CONSTANT_VALUES"	     FROM DICTIONARY.

 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_1083_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_2875_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_481_STOP_APP_VAL"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_755_ACCUM_STATUS_VAL"    FROM DICTIONARY.

* ICD copies
*-----------
*--------------------------------------------------------------------
 LINKAGE                                SECTION.
*--------------------------------------------------------------------
 COPY 'UTL_CDD_WKSP:UTL_USER_ACW'                    FROM DICTIONARY.
 COPY 'UTL_CDD_WKSP:UTL_CONTROL_ACW'                 FROM DICTIONARY.
 COPY 'ICA_CDD_WKSP:ICA_TRB71H_SET_ACCUM_IKUL_WKSP'  FROM DICTIONARY.

*********************************************************************
 PROCEDURE DIVISION USING UTL_USER_ACW
                          UTL_CONTROL_ACW
                          ICA_TRB71H_SET_ACCUM_IKUL_WKSP
                   GIVING SP$_ACW_PROC_AUX_STATUS.
*********************************************************************
 DECLARATIVES. 
*-----------------------------------------------------------------------
 001-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON K_TRB71H_755_IKL.
 001.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_K_TRB71H_755_IKL_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 001-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT

       EXIT PROGRAM
    END-IF
    .
*-----------------------------------------------------------------------
 002-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71H_755.
 002.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71H_755_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 002-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT

       EXIT PROGRAM
    END-IF
    .
*-----------------------------------------------------------------------
 003-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71H_875.
 003.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71H_875_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 003-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT

       EXIT PROGRAM
    END-IF
    .
 END DECLARATIVES.

*--------------------------------------------------------------------
 A-MAIN			    SECTION.
*--------------------------------------------------------------------
 A-00.

    PERFORM B-INIT

    PERFORM UNTIL L$_SW01_EOF_INP_FILE_YES
       PERFORM C-HANDLE

       PERFORM BC-READ
    END-PERFORM

    PERFORM Z-FINISH
    .
 A-EXIT.    EXIT.
*--------------------------------------------------------------------
 B-INIT                     SECTION.
*--------------------------------------------------------------------
 B-00.

    DISPLAY 'Start of program: ICA_TRB71H_SET_ACCUM_IKUL.'

    INITIALIZE L$_WS01_VARAIBLES
               L$_CT01_COUNTERS
               L$_SW01_SWITCHES
               ICA_TRB71H_SET_ACCUM_IKUL_OUT

    MOVE SP$_MSG_NORMAL
      TO SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW

    MOVE OPER_ID	    IN ICA_TRB71H_SET_ACCUM_IKUL_INP
      TO L$_WS01_OPER_ID    IN L$_K_TRB71H_755_IKL_NAME
         L$_WS01_OPER_ID    IN L$_P_TRB71H_755_NAME
         L$_WS01_OPER_ID    IN L$_P_TRB71H_875_NAME

    PERFORM BA-OPEN-OUTPUT-FILES

    PERFORM BC-READ
    .
 B-EXIT.    EXIT.
*--------------------------------------------------------------------
 BA-OPEN-OUTPUT-FILES                     SECTION.
*--------------------------------------------------------------------
 BA-00.

    OPEN OUTPUT P_TRB71H_755
    DISPLAY "Open Output File: " L$_P_TRB71H_755_NAME

    OPEN OUTPUT P_TRB71H_875
    DISPLAY "Open Output File: " L$_P_TRB71H_875_NAME

    OPEN INPUT K_TRB71H_755_IKL
    DISPLAY "Open Input File: " L$_K_TRB71H_755_IKL_NAME
    .
 BA-EXIT.    EXIT.
*--------------------------------------------------------------------
 BC-READ                     SECTION.
*--------------------------------------------------------------------
 BC-00.

    READ K_TRB71H_755_IKL
       AT END
	  SET L$_SW01_EOF_INP_FILE_YES	 TO TRUE

       NOT AT END
          ADD 1	    TO L$_CT01_REC_READ
    END-READ
    .
 BC-EXIT.    EXIT.
*--------------------------------------------------------------------
 C-HANDLE				SECTION.
*--------------------------------------------------------------------
 C-00.

    PERFORM UB-WRITE-755-RECORD

    PERFORM UC-WRITE-875-RECORD
    .
 C-EXIT.    EXIT.
*-----------------------------------------------------------------------------
 UB-WRITE-755-RECORD				SECTION.
*-----------------------------------------------------------------------------
 UB-00.

    INITIALIZE P_TRB71H_755_REC

    MOVE K_TRB71H_755_IKL_REC
      TO P_TRB71H_755_REC

    MOVE 2
      TO DEDUCTION_OPTION		IN P_TRB71H_755_REC

    WRITE P_TRB71H_755_REC

    ADD 1 
     TO L$_CT01_REC_755
    .
 UB-EXIT.    EXIT.
*-----------------------------------------------------------------------------
 UC-WRITE-875-RECORD				SECTION.
*-----------------------------------------------------------------------------
 UC-00.

    INITIALIZE P_TRB71H_875_REC

    MOVE V2875$DEDUCTION_OPTION_CHG
      TO UPDATE_MEANING_CODE		IN P_TRB71H_875_REC

    MOVE 2
      TO ACTION_ID			IN P_TRB71H_875_REC

    MOVE DEDUCTION_OPTION		IN K_TRB71H_755_IKL_REC
      TO OLD_VALUE			IN P_TRB71H_875_REC (2:9)

    MOVE "0" 
      TO OLD_VALUE			IN P_TRB71H_875_REC (1:1)

    MOVE ACCUM_INTR_ID			IN K_TRB71H_755_IKL_REC
      TO ACCUM_INTR_ID			IN P_TRB71H_875_REC

    MOVE SP$_ACW_USER_NAME		IN UTL_USER_ACW  
      TO UPDATE_USER_CODE		IN P_TRB71H_875_REC

    MOVE SP$_DATE_TIME_BINARY		IN ICA_TRB71H_SET_ACCUM_IKUL_INP
      TO UPDATE_TMSP			IN P_TRB71H_875_REC

    WRITE P_TRB71H_875_REC

    ADD 1 
     TO L$_CT01_REC_875
    .
 UC-EXIT.    EXIT.
*--------------------------------------------------------------------
 X-ERRORS				SECTION.
*--------------------------------------------------------------------
 X-00.

    DISPLAY 'Att. Program: ICA_TRB71H_SET_ACCUM_IKUL. ended with errors!!!'

    EXIT PROGRAM
    .
 X-EXIT.     EXIT.
*--------------------------------------------------------------------
 Z-FINISH				SECTION.
*--------------------------------------------------------------------
 Z-00.

    PERFORM ZA-CLOSE-OUTPUT-FILES

    DISPLAY "Total records read from input file 755 IKL:",
            L$_CT01_REC_READ             

    DISPLAY "Total records written in file 755 deduct:",
            L$_CT01_REC_755             

    DISPLAY "Total records written in file 875 deduct:",
            L$_CT01_REC_875             

    DISPLAY 'End of program: ICA_TRB71H_SET_ACCUM_IKUL.'

    EXIT PROGRAM
    .
 Z-EXIT.    EXIT.
*--------------------------------------------------------------------
 ZA-CLOSE-OUTPUT-FILES			SECTION.
*--------------------------------------------------------------------
 ZA-00.

    CLOSE K_TRB71H_755_IKL
    DISPLAY "Close Input File: " L$_K_TRB71H_755_IKL_NAME

    CLOSE  P_TRB71H_755
    DISPLAY "Close Output File: " L$_P_TRB71H_755_NAME

    CLOSE  P_TRB71H_875
    DISPLAY "Close Output File: " L$_P_TRB71H_875_NAME
    .
 ZA-EXIT.    EXIT.
                                                                                                                                                                                                                                                                                                                                               
