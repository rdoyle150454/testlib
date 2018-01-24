**************** PROGRAM_NAME: [ICA_TRB71I_APPL.COB] ****************
* 								    *
* 		                     [םירצומ תכרעמ תת] :  תכרעמ תת  *
*                       [ADC-מ סנכנ ץבוק תטילקל ךילהת] : לודומה םש  *
*                                                                   *
*                                                 :ילנויצקנופ רואת  *
*                                                                   *
*            :םיאבה םירבדה תא תעצבמו ADC-מ סנכנ ץבוק תטלוק תינכותה  *
*                 
                     (ADC - 2401 רוקמ   519 גוס)  *
*                                                   םירבוצ ןוכדע -  *
*                                          םירבוצ תיירוטסה ןוכדע -  *
*                                              הרקב תואלבט ןוכדע -  *
*                                                                   *
*                                                          :םיחתפמ  *
*          ןורחא ןוכדע .ת        ךיראת       עצבמ םש       בלש      *
*                  [    ]     [4-Jul-201
1] [TP_Onissa]     בוציע    *
*                  [    ]     [4-Jul-2011] [TP_Onissa]     תונכת    *
*                                                                   *
*                                                         :םירטמרפ  *
*                                           [    ] :עדימ תינבת      *
*          (שומיש ,םיכרע ,רבסה) רואת   פ/ק           הדש םש         *
*                             [    ]  [  ]           [    ]         *
*                                                             
      *
*                                                      :עדימ ירגאמ  *
*                תירבעב םש / רואת    פ/ק             הלבט/ץבוק      *
*                          [    ]   [  ]                [    ]      *
*                                                                   *
*                                                   :םישגדהו תורעה  *
*                                                       [    ]      *
*                                                                   *
*             
                              :םייונישו םינוכדע בקעמ  *
*                                                                   *
*                     יונישה רואת     עצבמ םש         ךיראת    CID  *
* --------------------------------- ------------- ----------- ----- *
*      לובקת ךופיה לש תומושרב לופיט     ןסינ ןרוא 19-Jun-2012 #0001 *
*********************************************************************

*********************************************************************
 IDENTIFICATION DIVISION.
*********
************************************************************
 PROGRAM-ID. ICA_TRB71I_APPL.
 AUTHOR.     TP_ONISSA.

*********************************************************************
 ENVIRONMENT DIVISION.
*********************************************************************
*--------------------------------------------------------------------
 INPUT-OUTPUT                            SECTION.
*--------------------------------------------------------------------
 FILE-CONTROL.

*   ADC-מ אצוי םירבוצ קשממ 
טלק ץבוק
    SELECT K_TRB71I_ADC
           ASSIGN                 TO           "SYS$DISK"
           ORGANIZATION           IS           SEQUENTIAL
           ACCESS MODE            IS           SEQUENTIAL
           FILE STATUS            IS           SP$_COBRMS_FILE_STATUS.

*   875 - ןוכדעל רבוצ ינוכדע תירוטסה טלפ ץבוק
    SELECT P_TRB71I_875     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_
FILE_STATUS.

*   ATR - ןוכדעל תומושר ינותנ טלפ ץבוק
    SELECT P_TRB71I_ATR_R     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   ATR - ןוכדעל ץבוק ינותנ טלפ ץבוק
    SELECT P_TRB71I_ATR_F     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   תואיגש לש הר
קמב ל"אוד תעדוהל טלפ ץבוק
    SELECT P_TRB71I_MSM_MSG     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   ל"אוד תויוגש תועדוה טלפ ץבוק
    SELECT P_TRB71I_MSM_SEND_MSG     
	   ASSIGN		  TO	       "SYS$DISK"
	   ORGANIZATION		  IS	       SEQUENTIAL
	   ACCESS		  IS	       SEQUENTIAL
           FILE STATUS		  IS	       SP$_COBRMS_FILE_STATUS.

*   ל"אוד םוכיס תועדוה טלפ ץבוק
    SE
LECT P_TRB71I_MSM_SUM_MSG  
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
 
FD K_TRB71I_ADC
    VALUE OF ID IS  L$_K_TRB71I_ADC_NAME.
    COPY "ICA_SOURCE:ICA_TRB71I_ACCUM_INTRF_WKSP.INC"
       REPLACING    ICA_TRB71I_ACCUM_INTRF_WKSP
              BY    K_TRB71I_ADC_REC.

 FD P_TRB71I_875    IS EXTERNAL  
    VALUE OF ID IS L$_P_TRB71I_875_NAME.
    COPY "ICD_CDD_WKSP:ICD_PRT_875_HST_STP_UPD"	    FROM DICTIONARY
       REPLACING    ICD_PRT_875_HST_STP_UPD 
              BY    P_TRB71I_875_REC.                   

 FD P_TRB71I_ATR_R  IS EXTERNAL
    VALUE OF ID IS L$_P_TRB71I_ATR_
R_NAME.
    COPY "ICD_CDD_WKSP:ICD_ATR_6688_ADC_IN_REC"	    FROM DICTIONARY
       REPLACING    ICD_ATR_6688_ADC_IN_REC 
              BY    P_TRB71I_ATR_R_REC.                   

 FD P_TRB71I_ATR_F
    VALUE OF ID IS L$_P_TRB71I_ATR_F_NAME.
    COPY "ICD_CDD_WKSP:ICD_ATR_6699_ADC_IN_FIL"	    FROM DICTIONARY
       REPLACING    ICD_ATR_6699_ADC_IN_FIL 
              BY    P_TRB71I_ATR_F_REC.                   

 FD P_TRB71I_MSM_MSG
    VALUE OF ID IS L$_P_TRB71I_MSM_MSG_NAME.
 01 P_TRB71I_MSM_MSG_REC		PIC 
X(140).

 FD P_TRB71I_MSM_SEND_MSG
    VALUE OF ID IS L$_P_TRB71I_MSM_SEND_MSG_NAME.
 01 P_TRB71I_MSM_SEND_MSG_REC		PIC X(140).

 FD P_TRB71I_MSM_SUM_MSG
    VALUE OF ID IS L$_P_TRB71I_MSM_SUM_MSG_NAME.
 01 P_TRB71I_MSM_SUM_MSG_REC		PIC X(140).

*--------------------------------------------------------------------
 WORKING-STORAGE SECTION.
*--------------------------------------------------------------------
 01 ICA_ICF_EXCEPTION_HANDLER	    PIC S9(9) COMP  VALUE EXTERNAL 
						    ICA_ICF_EXCEPTION_HANDLE
R.

* םיינוציח םיצבק תומש
 01 L$_P_TRB71I_875_NAME	    PIC X(40) EXTERNAL.
 01 L$_P_TRB71I_ATR_R_NAME	    PIC X(42) EXTERNAL.

 01 L$_WS01_FILES_NAME.
    03 L$_K_TRB71I_ADC_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_SI_DAT_DIR:".
       05 FILLER		    PIC X(18)	    VALUE "ICA_SVB609_FILEIN_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71I_875_NAME_X.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    P
IC X(15)	    VALUE "ICA_TRB71I_875_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71I_ATR_R_NAME_X.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(17)	    VALUE "ICA_TRB71I_ATR_R_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71I_ATR_F_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(17)	    VALUE
 "ICA_TRB71I_ATR_F_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71I_MSM_MSG_NAME.
       05 FILLER		    PIC X(09)	    VALUE "ICA_DATA:".
       05 FILLER		    PIC X(11)	    VALUE "ICA_TRB71I_".
       05 L$_WS01_MSG_TYPE	    PIC X(02).
          88 L$_WS01_MSG_TYPE_PROC_ERR		    VALUE "A2".
          88 L$_WS01_MSG_TYPE_SUM_SUB		    VALUE "B9".
          88 L$_WS01_MSG_TYPE_SUM_FILES		    VALUE "C9".
       05 FILLER		    PIC X(04)	    VALUE "
.DAT".

    03 L$_P_TRB71I_MSM_SEND_MSG_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(16)	    VALUE "TRB71I_SEND_MSG_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

    03 L$_P_TRB71I_MSM_SUM_MSG_NAME.
       05 FILLER		    PIC X(15)	    VALUE "ICA_TR_DAT_DIR:".
       05 FILLER		    PIC X(15)	    VALUE "TRB71I_SUM_MSG_".
       05 L$_WS01_TRNF_ID	    PIC 9(06).
       05 FILLER		    PIC X(04)	    VALUE ".DAT".

 01 L$_WS01_VARAIBLES.
    03 L$_WS01_MSM_FILE_NAME        PIC X(30).
    03 L$_WS01_ATR_BATCH_SEQ        PIC 9(06).
    03 L$_PRODUCT_NUM               PIC X(20).
    03 L$_WS01_MIN_CUST_ADC_BALANCE PIC S9(16)V9(02)		    COMP.
    03 L$_WS01_ORG_CUST_ADC_BAL	    PIC S9(16)V9(02)		    COMP.
    03 L$_WS01_TRX_AMOUNT_9         PIC S9(12)V9(02).
    03 L$_WS01_TOTAL_OPEN_BALANCE_9 PIC S9(12)V9(02).
    03 L$_WS01_TOTAL_BAD_DEBT_9	    PIC S9(12)V9(02).
   
    03 L$_CRNT_EXT_ID_NUM_TYP_COD   PIC 9(04).
    03 
L$_CRNT_EXT_ID_NUM_TYP_COD_X  REDEFINES
       L$_CRNT_EXT_ID_NUM_TYP_COD   PIC X(04).

    03 L$_SYSTEM_DATE_START	    PIC S9(11)V9(07)		    COMP.
    03 L$_WS01_DATE_FILE            PIC S9(11)V9(07)		    COMP.

    03 L$_WS01_CURRENT_DATE.
       05 L$_WS01_CURRENT_DATE_DD   PIC 9(02).
       05 FILLER                    PIC X(01).
       05 L$_WS01_CURRENT_DATE_MM   PIC 9(02).
       05 FILLER                    PIC X(01).
       05 L$_WS01_CURRENT_DATE_YYYY PIC 9(04).

 01 L$_CO01_CONSTANTS.
    03 L$_C
O01_DELAY_DEADLOCK	    PIC X(23)	    VALUE "0 00:00:01.00".

 01 L$_CT01_COUNTERS.
    03 L$_CT01_HDR_TRL		    PIC S9(04)			    COMP.
    03 L$_CT01_NUM_TNUOT	    PIC  9(08).
    03 L$_CT01_INP_REC_COUNT	    PIC S9(09)			    COMP.
    03 L$_CT01_ATR_REC_COUNT	    PIC S9(09)			    COMP.
    03 L$_CT01_875_REC_COUNT	    PIC S9(09)			    COMP.
    03 L$_CT01_875_SUM_COUNT	    PIC S9(16)V9(02)		    COMP.

    03 L$_CT01_SUM_ADC_TRN	    PIC S9(16)V9(02)		    COMP.

    03 L$_CT01_INP_TRL_REC	    PIC 9(10)	    VA
LUE 0.
    03 L$_CT01_INP_HDR_REC	    PIC 9(10)	    VALUE 0.
    03 L$_CT01_INP_DATA_REC_OK      PIC 9(10)	    VALUE 0.
    03 L$_CT01_INP_DATA_REC_NOT_OK  PIC 9(10)	    VALUE 0.
    03 L$_CT01_INP_DATA_REC_BAD_DEBT  PIC 9(10)	    VALUE 0.
    03 L$_CT01_INP_REC_SUM	    PIC S9(13)V9(02).
    03 L$_CT01_INP_REC_SUM_BAD	    PIC S9(13)V9(02).
    03 L$_CT01_INP_REC_SUM_BAD_DEBT PIC S9(13)V9(02).

 01 L$_IX01_INDEXES.
    03 L$_IX01_IND	            PIC 9(04).

 01 L$_MH01_KEYS_TO_COMPARE.
    03 L$_MH01_CURR_CU
ST.
       05 L$_MH01_CURR_REC_TYPE		PIC X(03).
          88 L$_MH01_CURR_REC_TYPE_HDR		    VALUE 'AAA'.
          88 L$_MH01_CURR_REC_TYPE_DATA		    VALUE 'BBB'.
          88 L$_MH01_CURR_REC_TYPE_TRL		    VALUE 'ZZZ'.

       05 L$_MH01_CURR_CUST_EXT_ID_TYP	PIC 9(04).

       05 L$_MH01_CURR_CUST_PASSPORT    PIC X(20).
       05 FILLER  REDEFINES  L$_MH01_CURR_CUST_PASSPORT.
	  07 FILLER                     PIC X(11).
          07 L$_MH01_CURR_CUST_EXT_ID   PIC 9(09).

       05 L$_MH01_CURR_CUST_INT_ID	P
IC 9(09).

    03 L$_MH01_PREV_CUST.
       05 L$_MH01_PREV_REC_TYPE		PIC X(03).
          88 L$_MH01_PREV_REC_TYPE_HDR		    VALUE 'AAA'.
          88 L$_MH01_PREV_REC_TYPE_DATA		    VALUE 'BBB'.
          88 L$_MH01_PREV_REC_TYPE_TRL		    VALUE 'ZZZ'.

       05 L$_MH01_PREV_CUST_EXT_ID_TYP	PIC 9(04).

       05 L$_MH01_PREV_CUST_PASSPORT    PIC X(20).
       05 FILLER  REDEFINES  L$_MH01_PREV_CUST_PASSPORT.
	  07 FILLER                     PIC X(11).
          07 L$_MH01_PREV_CUST_EXT_ID   PIC 9(09).

   
    05 L$_MH01_PREV_CUST_INT_ID	PIC 9(09).

 01 L$_SW01_SWITCHES.
    03 L$_SW01_EOF_INP_FILE	    PIC 9(01).
       88 L$_SW01_EOF_INP_FILE_NO		    VALUE 0.              
       88 L$_SW01_EOF_INP_FILE_CUST_YES			    VALUES 6 THRU 9.
       88 L$_SW01_EOF_INP_FILE_YES		    VALUE 9.              

    03 L$_SW01_600_DATA		    PIC 9(01).
       88 L$_SW01_600_DATA_FOUND		    VALUE 0.
       88 L$_SW01_600_DATA_NOT_FOUND		    VALUE 1.
       88 L$_SW01_600_DATA_END			    VALUE 2.

    03 L$_SW01_DATA_REC		    
PIC 9(01).
       88 L$_SW01_DATA_REC_OK			    VALUE 0.
       88 L$_SW01_DATA_REC_NOT_OK		    VALUE 1.

    03 L$_SW01_MSM_TITLE	    PIC 9(01).
       88 L$_SW01_MSM_TITLE_NO			    VALUE 0.
       88 L$_SW01_MSM_TITLE_YES			    VALUE 1.

* UTL wksp and inc copies
*------------------------
 COPY "UTL_CDD_WKSP:UTL_TRAN_LOGICAL_NAME_WKSP"      FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_WKSP"	     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"		     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_U
SER_ACW"		     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_EXIT_ROUTINE_WKSP"	     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_GET_SYMBOL_VALUE_WKSP"	     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_SET_SYMBOL_VALUE_WKSP"	     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_APPLY_DELTA_TIME_V2_WKSP"    FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_WKSP"	     FROM DICTIONARY.
 COPY "UTL_CDD_WKSP:UTL_COBRMS_VALUE_WKSP"	     FROM DICTIONARY.

 COPY "UTL_CDD_WKSP:UTL_SPAWN_PROCESS_WKSP"	     FROM DICTIONARY.
 COPY
 "UTL_CDD_WKSP:UTL_SPAWN_PROCESS_WKSP"	     FROM DICTIONARY
    REPLACING ==UTL_SPAWN_PROCESS_WKSP== 
           BY ==UTL_SPAWN_PROCESS_WKSP_INIT==.

 COPY "UTL_CDD_WKSP:UTL_DECLARE_EXIT_HANDLER_WKSP"   FROM DICTIONARY
    REPLACING ==0==	    BY ==EXTERNAL ICA_TRB71I_EXIT_HANDLER==
              ==POINTER==   BY  ==POINTER VALUE REFERENCE L$_EXIT_STATUS==.

 COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".
 COPY "UTL_SOURCE:UTL_MESSAGE.INC".
 COPY "UTL_SOURCE:UTL_SYMBOLS_DMQ.INC".

* ICA wksp and inc copies
*--------
----------------
 COPY "ICA_CDD_WKSP:ICA_SVM_FILEIN_UPDATE_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVF_CREATE_OPER_ID_WKSP"     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVM_CHECK_DUP_BALANC_WKSP"   FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_CONSTANT_VALUES"             FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_BATCH_CONSTANT_VALUES"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TASK_STANDARD_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVF_HANDLE_PARAM_VALUE_WKSP" FROM DICTIONARY.
 COPY "ICA
_CDD_WKSP:ICA_SVF_HANDLE_OPER_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_ERROR_HANDLE_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_XLATE_MSG_WKSP"		     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_CVT_COMM_STS_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_DMQ_PUT_MSG_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP" FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_DMQ_EXIT_WKSP"		     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_RMS_MSG_WKSP"                FROM DICTION
ARY.
 COPY "ICA_CDD_WKSP:ICA_CVT_BDR_OUT_WKSP"	     FROM DICTIONARY
    REPLACING	==VALUE IS "ACMS"== BY ==VALUE "ACMS"==
		==VALUE IS==        BY ==. 88 DUMMY_REPL VALUE IS==.

 COPY "ICA_CDD_WKSP:ICA_INVERSE_STRING_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_INVERSE_STRING_WKSP"         FROM DICTIONARY
    REPLACING ==ICA_INVERSE_STRING_WKSP== 
           BY ==ICA_INVERSE_STRING_WKSP_INIT==.

 COPY "ICA_CDD_WKSP:ICA_TRB71I_SET_DBCR_STOP_WKSP"   FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TRB71I_SET_D
BCR_STOP_WKSP"   FROM DICTIONARY
    REPLACING ICA_TRB71I_SET_DBCR_STOP_WKSP
	   BY ICA_TRB71I_SET_DBCR_STOP_INIT
              ICA_TRB71I_SET_DBCR_STOP_INP
	   BY ICA_TRB71I_SET_DBCR_STOPI
              ICA_TRB71I_SET_DBCR_STOP_OUT
	   BY ICA_TRB71I_SET_DBCR_STOPO.

 COPY "ICA_CDD_WKSP:ICA_TRB71I_WRITE_ATR_REC_WKSP"   FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TRB71I_WRITE_ATR_REC_WKSP"   FROM DICTIONARY
    REPLACING ICA_TRB71I_WRITE_ATR_REC_WKSP
           BY ICA_TRB71I_WRITE_ATR_REC_INIT
              ICA
_TRB71I_WRITE_ATR_REC_INP
	   BY ICA_TRB71I_WRITE_ATR_REC_I
              ICA_TRB71I_WRITE_ATR_REC_OUT
	   BY ICA_TRB71I_WRITE_ATR_REC_O.

 COPY "ICA_CDD_WKSP:ICA_TRB71I_UPD_TABLES_WKSP"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TRB71I_UPD_TABLES_WKSP"	     FROM DICTIONARY
    REPLACING ICA_TRB71I_UPD_TABLES_WKSP
           BY ICA_TRB71I_UPD_TABLES_WKSP_INIT
              ICA_TRB71I_UPD_TABLES_INP
	   BY ICA_TRB71I_UPD_TABLESI.

 COPY "ICA_SOURCE:ICA_TRB71I_ACCUM_INTRF_WKSP.INC"
    REPLACING ICA_TRB71I
_ACCUM_INTRF_WKSP
           BY L$_WS01_PREV_K_TRB71I_ADC_REC.

 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_0010_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_1007_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_1531_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_2002_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_SVT_072_FILEOUT_DEF_VAL"     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_300_PROD_TYPES_VAL"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_
327_BANKS_VAL"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_346_EXT_ID_N_TP_VAL"     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_352_PRS_CATLOG_VAL"      FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_359_CRT_STS_DEF_VAL"     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TRT_608_IN_OUT_SRC_VAL"	     FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_TRT_725_IN_FILE_STEP_VAL"    FROM DICTIONARY.
 COPY "ICA_CDD_WKSP:ICA_PRT_755_ACCUM_STATUS_VAL"    FROM DICTIONARY.

 COPY "ICA_SOURCE:ICA_VARIABLES.INC".
 COPY "ICA_SOURC
E:ICA_HIGH_LOW_VAL_TYPES.INC".
 COPY "ICA_SOURCE:ICA_DMQ_COB_TYPE_CLASS.INC".
 COPY "ICA_SOURCE:ICA_TRB71I_MSM_MGS_WKSP.INC".
 COPY "ICA_SOURCE:ICA_DMQ_COB_PROCESS.INC".
 COPY "ICA_SOURCE:ICA_MESSAGE.INC".
 COPY "ICA_SOURCE:ICA_MSG_BATCH_STATUS_VAL.INC".

* ICD inc & copies
*-----------------
 COPY "ICD_SOURCE:ICD_CONSTANT_DBA.INC".

 COPY "ICD_CDD_WKSP:ICD_CONNECT_DBW"		     FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_COMMON_DBW"                  FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_SVT_030_ENTITY_DBW"	  
   FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_SVT_035_OPER_PRM_DBW"	     FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_SVT_036_OPER_DBW"	     FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_TRT_600_TRF_MGNT_DBW"	     FROM DICTIONARY.

 COPY "ICD_CDD_WKSP:ICD_CST_500_CST_CATLOG_DBW"	     FROM DICTIONARY.
 COPY "ICD_CDD_WKSP:ICD_CST_500_CST_CATLOG_DBW"	     FROM DICTIONARY
    REPLACING ICD_CST_500_CST_CATLOG_DBW 
           BY ICD_CST_500_CST_CATLOG_DBW_INIT.

 COPY "ICD_CDD_WKSP:ICD_ATR_6699_ADC_IN_FIL_DBW"     FROM DICT
IONARY.
 COPY "ICD_CDD_WKSP:ICD_ATR_6699_ADC_IN_FIL_DBW"     FROM DICTIONARY
    REPLACING ICD_ATR_6699_ADC_IN_FIL_DBW 
           BY INIT_ATR_6699_ADC_IN_FIL_DBW.

 COPY "ICD_CDD_WKSP:UTL_TX_UNIV_WKSP"		     FROM DICTIONARY
    REPLACING ==UTL_TX_UNIV_WKSP== 
           BY ==UTL_TX_UNIV_WKSP EXTERNAL==.

01 L$_DB_CONN_STATUS  PIC 9 EXTERNAL.
01 L$_DMQ_CONN_STATUS PIC 9 EXTERNAL.

01 L$_STATUS			PIC S9(9) COMP.

01 L$_ICA$ENV		PIC X(10).

01 L$_DMQ_ATTACH_STS            PIC 9 VALUE 0.
   88 L$_NOT_CONN_DMQ 
          VALUE 0.
   88 L$_CONN_DMQ               VALUE 1.

01 SYM$_TASK_ID  		PIC X(06).
01 SYM$_USER_NAME		PIC X(12).
01 SYM$_ENTITY_CODE		PIC X(06).
01 SYM$_OPER_ID			PIC 9(16) EXTERNAL.
01 PREV_OPER_ID                 PIC 9(16) EXTERNAL.
01 L$_ACW_USER_NAME		PIC X(12) EXTERNAL.
01 L$_ACW_TASK_ID		PIC X(6)  EXTERNAL.

01 L$_FILE_NAME                 PIC X(58) VALUE "ICA_XX_DAT_DIR:ICA_".
01 L$_FILE_DESC			PIC X(11) VALUE SPACES.
01 L$_FILE_DESC_LEN		PIC S9(4) COMP.

01 L$_EXIT_STATUS		PIC S9(9) COMP.

*
***********************************************************************
 PROCEDURE  DIVISION.
************************************************************************
 DECLARATIVES.
*-----------------------------------------------------------------------
 001-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON K_TRB71I_ADC.
 001.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handli
ng in file: " L$_K_TRB71I_ADC_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLA
RATIVES 001-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .
*-----------------------------------------------------------------------
 004-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71I_875.
 004.
****
    IF NOT SP$_COBRMS_SUCCESS IN
 SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_875_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                  
     TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 004-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .
*-----------------------------------------------------------------------
 015-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_
TRB71I_ATR_R.
 015.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_ATR_R_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_F
AILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 015-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION' 
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .
*-----------------------------------------------------------------------
 016-I-O-PROBLEM	    SECTION.
*--------------------------------------------------------
---------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71I_ATR_F.
 016.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_ATR_F_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_T
EXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 016-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .
*-----------------------------------------------------------------------
 017-I-O-PROBLEM	    SECTION
.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71I_MSM_MSG.
 017.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_MSM_MSG_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       D
ISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 017-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .
*-------------------------------------
----------------------------------
 018-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71I_MSM_SEND_MSG.
 018.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_MSM_SEND_MSG_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_ST
V

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 018-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_
EXIT_ROUTINE_WKSP
    END-IF
    .
*-----------------------------------------------------------------------
 019-I-O-PROBLEM	    SECTION.
*-----------------------------------------------------------------------
    USE AFTER STANDARD ERROR PROCEDURE ON P_TRB71I_MSM_SUM_MSG.
 019.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
       DISPLAY "** Error handling in file: " L$_P_TRB71I_MSM_SUM_MSG_NAME
       DISPLAY "** SP$_COBRMS_FILE_STATUS: " SP$_COBRMS_FILE_STATUS

       MOVE RMS-CURRENT-STS
	TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV	TO P$_RMS_CURRENT_STV

       CALL 'ICA_ICF_XLATE_RMS_STS'  USING  ICA_RMS_MSG_WKSP

       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT	    IN ICA_RMS_MSG_WKSP(2)

       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR                       TO SP$_ACW_PROC_AUX_STATUS 
       MOVE "Sec:DECLARATIVES 019-I-O-PROBLEM"  TO SP$_ACW_FREE_TEXT



*# 12-NOV-2012 14:15:00.57 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_
APPL_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE'  USING BY REFERENCE  UTL_EXIT_ROUTINE_WKSP
    END-IF
    .

 END DECLARATIVES.

*----------------------------------------------------------------------
 A-MAIN		SECTION.
*----------------------------------------------------------------------
 A-00.

    PERFORM B-INIT

    PERFORM UNTIL L$_SW01_EOF_INP_FILE_YES
       PERFORM DA-START-CUST

       PERFORM UNTIL L$_SW01_EOF_INP_FILE_CUST_YES
          PERFORM DB-HANDLE-CUST-DETAILS
                   
          
PERFORM C-READ-INPUT-FILE
          PERFORM DC-BREAK-ON-CUST
       END-PERFORM

       PERFORM DD-END-CUST
    END-PERFORM

    PERFORM Z-FINISH
    .	
 A-EXIT.    EXIT.                    
*----------------------------------------------------------------------
 B-INIT		SECTION.
*----------------------------------------------------------------------
 B-00.
*# 12-NOV-2012 14:15:00.29 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_INIT_APPL.INC".


    DISPLAY "*************************************
***************"
    DISPLAY "****** Start of Program: ICA_TRB71I_APPL.COB *******"
    DISPLAY "****************************************************"


*   Establish condition EXCEPTION_HANDLER.
    CALL 'LIB$ESTABLISH'  USING BY VALUE  ICA_ICF_EXCEPTION_HANDLER

    SET SP$_COBRMS_SUCCESS	    TO TRUE

*   Initialize Exit status.
    SET SP$_EXIT_STATUS_SUCCESS	    TO TRUE
    MOVE SP$_MSG_NORMAL		    TO SP$_ACW_PROC_AUX_STATUS

    INITIALIZE L$_WS01_VARAIBLES
               L$_SW01_SWITCHES
             
  L$_CT01_COUNTERS
               INIT_ATR_6699_ADC_IN_FIL_DBW
	       UTL_SPAWN_PROCESS_WKSP_INIT
               UTL_CVT_DATE_DDMMYYYY_WKSP
	       ICA_TRB71I_SET_DBCR_STOP_INIT

    PERFORM BA-DECLARE-EXIT-HANDLER
    PERFORM BB-GET-LOGICAL-ENVIRONMENT
    PERFORM BC-GET-SYMBOLS
    PERFORM BD-ATTACH-DMQ
    PERFORM BE-CONNECT-DB
    PERFORM BF-UPDATE-OPER
    PERFORM BG-GET-CURRENT-TIME

    PERFORM BI-CHECK-519-FILE-RUNNING

    PERFORM BJ-GET-ENTITY-INFO

    PERFORM BK-GET-FILEIN-600-INFO

*   הטילקה 
תאצמנ ובש בלשה תקידב
    PERFORM BL-CHECK-FILEIN-STAGE
    .
 B-EXIT.     EXIT.
*----------------------------------------------------------------------
 BA-DECLARE-EXIT-HANDLER		SECTION.
*----------------------------------------------------------------------
 BA-00.

    DISPLAY "Declaring exit handler ...".

    CALL 'UTL_DECLARE_EXIT_HANDLER'  USING  UTL_DECLARE_EXIT_HANDLER_WKSP

    IF SP$_SYSPRO_STATUS  OF UTL_DECLARE_EXIT_HANDLER_WKSP  IS FAILURE
       MOVE SP$_SYSPRO_STATUS	    IN UTL_DECLARE_EXIT_H
ANDLER_WKSP 
         TO SP$_ACW_PROC_AUX_STATUS

       MOVE SP$_SYSPRO_STATUS_AUXIL IN UTL_DECLARE_EXIT_HANDLER_WKSP
         TO SP$_ACW_FORM_AUX_STATUS

       MOVE "SEC:BA-DECLARE-EXIT-HANDLER  RTN:UTL_DECLARE_EXIT_HANDLER" 
         TO SP$_ACW_FREE_TEXT

       DISPLAY SP$_ACW_FREE_TEXT

       MOVE SP$_SYSPRO_STATUS_AUXIL IN UTL_DECLARE_EXIT_HANDLER_WKSP
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 BA-EXIT.     EXIT.
*----------------------------------------------------------
------------
 BB-GET-LOGICAL-ENVIRONMENT	    SECTION.
*----------------------------------------------------------------------
 BB-00.

    INITIALIZE UTL_TRAN_LOGICAL_NAME_WKSP

    MOVE "ICA$ENV" 
      TO SP$_LOGICAL_NAME		OF UTL_TRAN_LOGICAL_NAME_WKSP

    MOVE 7 
      TO SP$_LOGICAL_NAME_LEN		OF UTL_TRAN_LOGICAL_NAME_WKSP

    MOVE "LNM$FILE_DEV" 
      TO SP$_LOGICAL_NAME_TABLE		OF UTL_TRAN_LOGICAL_NAME_WKSP

    MOVE 12 
      TO SP$_LOGICAL_NAME_TABLE_LEN	OF UTL_TRAN_LOGICAL_NAME_WKSP

    CALL "UTL
_TRAN_LOGICAL_NAME_V2"  USING  UTL_TRAN_LOGICAL_NAME_WKSP

    IF SP$_SYSPRO_STATUS  OF UTL_TRAN_LOGICAL_NAME_WKSP  IS FAILURE
       DISPLAY "Error in SEC:BB-GET-LOGICAL-ENVIRONMENT "
               "RTN:UTL_TRAN_LOGICAL_NAME_V2"
       DISPLAY "While translating log name:",
               SP$_LOGICAL_NAME   OF UTL_TRAN_LOGICAL_NAME_WKSP  CONVERSION
       DISPLAY "STTS=", 
               SP$_SYSPRO_STATUS  IN UTL_TRAN_LOGICAL_NAME_WKSP  CONVERSION
       DISPLAY "STTSA=", 
               SP$_SYSPRO_STATUS
_AUXIL  IN UTL_TRAN_LOGICAL_NAME_WKSP CONVERSION

       MOVE SP$_SYSPRO_STATUS		 IN UTL_TRAN_LOGICAL_NAME_WKSP 
         TO SP$_ACW_PROC_AUX_STATUS

       MOVE SP$_SYSPRO_STATUS_AUXIL	 IN UTL_TRAN_LOGICAL_NAME_WKSP 
         TO SP$_ACW_FORM_AUX_STATUS

       MOVE "SEC:BB-GET-LOGICAL-ENVIRONMENT RTN:UTL_TRAN_LOGICAL_NAME_V2" 
	 TO SP$_ACW_FREE_TEXT

       MOVE SP$_SYSPRO_STATUS_AUXIL	 IN UTL_TRAN_LOGICAL_NAME_WKSP 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF

    MOVE SP$_LOGICAL_NAME
_VALUE OF UTL_TRAN_LOGICAL_NAME_WKSP  
      TO L$_ICA$ENV

    DISPLAY "  ICA$ENV : ", L$_ICA$ENV  CONVERSION
    .
 BB-EXIT.    EXIT.
*----------------------------------------------------------------------
 BC-GET-SYMBOLS			    SECTION.
*----------------------------------------------------------------------
 BC-00.

*   Get user name.
    MOVE "SYM$_USER_NAME" 
      TO SP$_SYMBOL		 IN UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMBOL_VALUE	 IN UTL_GET_SYMBOL_VALUE_WKSP 
    
  TO SYM$_USER_NAME
         SP$_ACW_USER_NAME
         L$_ACW_USER_NAME
	 SP$_ACW_EXTERN_USER_NAME    IN UTL_CONTROL_ACW

*   Get entity code.
    MOVE "SYM$_ENTITY_CODE" TO SP$_SYMBOL IN UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMBOL_VALUE	 IN UTL_GET_SYMBOL_VALUE_WKSP 
      TO SYM$_ENTITY_CODE
	 SP$_ACW_TASK_ID
	 L$_ACW_TASK_ID

*   Get task id.
    MOVE "SYM$_TASK_ID" 
      TO SP$_SYMBOL		 IN UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMB
OL_VALUE	 IN UTL_GET_SYMBOL_VALUE_WKSP 
      TO SYM$_TASK_ID

*   Get oper id.
    MOVE "SYM$_OPER_ID" 
      TO SP$_SYMBOL		 IN UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMBOL_VALUE	 IN UTL_GET_SYMBOL_VALUE_WKSP 
         (1:SP$_SYMBOL_VALUE_LEN IN UTL_GET_SYMBOL_VALUE_WKSP) 
      TO SYM$_OPER_ID
         L$_WS01_OPER_ID    IN ICA_TRB71I_MSM_MGS_WKSP

    IF L$_ICA$ENV = "PROD" THEN
*      Get previous oper id.
       MOVE "PREV_OPER_ID" TO SP$_SYMBOL IN UTL_GET_SYMBOL_VAL
UE_WKSP

       PERFORM BCA-CALL-SYMBOL-RTN

       MOVE SP$_SYMBOL_VALUE IN UTL_GET_SYMBOL_VALUE_WKSP
            (1:SP$_SYMBOL_VALUE_LEN IN UTL_GET_SYMBOL_VALUE_WKSP) 
         TO PREV_OPER_ID
    END-IF

*   Get extern node name.
    MOVE "SYM$_NODE_NAME" 
      TO SP$_SYMBOL IN UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMBOL_VALUE	 IN UTL_GET_SYMBOL_VALUE_WKSP 
      TO SP$_ACW_EXTERN_NODE_NAME

*   Get terminal name.
    MOVE "SYM$_TERMINAL_NAME" 
      TO SP$_SYMBOL IN 
UTL_GET_SYMBOL_VALUE_WKSP

    PERFORM BCA-CALL-SYMBOL-RTN

    MOVE SP$_SYMBOL_VALUE   IN UTL_GET_SYMBOL_VALUE_WKSP 
      TO SP$_ACW_EXTERN_TERMINAL_NAME
    .
 BC-EXIT.     EXIT.
*----------------------------------------------------------------------
 BCA-CALL-SYMBOL-RTN			    SECTION.
*----------------------------------------------------------------------
 BCA-00.

    CALL "UTL_GET_SYMBOL_VALUE"  USING  UTL_GET_SYMBOL_VALUE_WKSP

    IF SP$_SYSPRO_STATUS  OF UTL_GET_SYMBOL_VALUE_WKSP  IS FAILURE
      
 MOVE SP$_SYSPRO_STATUS IN UTL_GET_SYMBOL_VALUE_WKSP 
         TO SP$_ACW_PROC_AUX_STATUS

       MOVE SP$_SYSPRO_STATUS_AUXIL IN UTL_GET_SYMBOL_VALUE_WKSP
         TO SP$_ACW_FORM_AUX_STATUS

       STRING
          "SEC:BC-GET-SYMBOLS RTN:UTL_GET_SYMBOL_VALUE PAR:" DELIMITED BY SIZE
          SP$_SYMBOL  IN UTL_GET_SYMBOL_VALUE_WKSP           DELIMITED BY SIZE
          INTO  SP$_ACW_FREE_TEXT
       END-STRING

       DISPLAY SP$_ACW_FREE_TEXT

       MOVE SP$_SYSPRO_STATUS_AUXIL IN UTL_GET_SYMBOL_VALUE_
WKSP 
	 TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 BCA-EXIT.     EXIT.
*----------------------------------------------------------------------
 BD-ATTACH-DMQ			    SECTION.
*----------------------------------------------------------------------
 BD-00.

*   attach to dmq queue
    MOVE DMQ$_TEMPORARY_SOURCE_Q 
      TO SP$_DMQ_SOURCE_Q   OF ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP

    MOVE "BDR         " 
      TO SP$_DMQ_SUBSYS	    OF ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP

    CALL 'ICA_DMQ_ATTACH_QUE
_AND_SBS'  USING  ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP

    IF SP$_SYSPRO_STATUS  OF ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP  IS FAILURE
       MOVE SP$_SYSPRO_STATUS OF ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE SP$_SYSPRO_STATUS_AUXIL IN ICA_DMQ_ATTACH_QUE_AND_SBS_WKSP 
         TO SP$_ACW_FORM_AUX_STATUS

       MOVE "SEC:BD-ATTACH-DMQ RTN:ICA_DMQ_ATTACH_QUE_AND_SBS" 
         TO SP$_ACW_FREE_TEXT

       DISPLAY SP$_ACW_FREE_TEXT

       MOVE SP$_SYSPRO_STATUS_AUXIL IN ICA_DMQ
_ATTACH_QUE_AND_SBS_WKSP 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF

    SET L$_CONN_DMQ	TO TRUE
    MOVE 1		TO L$_DMQ_CONN_STATUS
    .
 BD-EXIT.     EXIT.
*----------------------------------------------------------------------
 BE-CONNECT-DB			    SECTION.
*----------------------------------------------------------------------
 BE-00.

*   connecting to d.b. sets
    MOVE DP$_CONST_IMAGE_LIST 
      TO DP$_CONNECTION_LIST	OF ICD_CONNECT_DBW

    MOVE DP$_SYM_CONNECT 
      TO DP$_ACT
ION		OF ICD_CONNECT_DBW
	 
    CALL 'ICD_CONNECT_DBA'  USING  ICD_CONNECT_DBW

    IF DP$_STATUS  OF UTL_DBA_UNIV_WKSP  OF ICD_CONNECT_DBW  IS FAILURE
       DISPLAY "Error:: SEC:BE-CONNECT-DB  RTN:ICD_CONNECT_DBA"
       DISPLAY "STTS  =", DP$_STATUS        OF ICD_CONNECT_DBW CONVERSION
       DISPLAY "STTS.A=", DP$_STATUS_AUXIL  OF ICD_CONNECT_DBW CONVERSION

       MOVE DP$_STATUS		  IN ICD_CONNECT_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_CONNECT_DBW 
         T
O SP$_ACW_FORM_MSG

       MOVE "Sec:BE-CONNECT-DB  Rtn:ICD_CONNECT_DBA  Par:CONNECT" 
         TO SP$_ACW_FREE_TEXT

       MOVE DP$_STATUS IN ICD_CONNECT_DBW 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF

    MOVE 1  TO L$_DB_CONN_STATUS
    .
 BE-EXIT.     EXIT.
*----------------------------------------------------------------------
 BF-UPDATE-OPER			    SECTION.
*----------------------------------------------------------------------
* Purpose: Read & Update operational parameters.
* D
escription: Update ctrl status in oper table.
*----------------------------------------------------------------------
 BF-00.

*   Get ctrl status code.
    MOVE SYM$_OPER_ID 
      TO OPER_ID	    IN ICA_SVF_HANDLE_OPER_WKSP

    SET P$_READ_MODE	    IN ICA_SVF_HANDLE_OPER_WKSP  TO TRUE

    MOVE "BATCH" 
      TO DP$_WORK_CONTEXT   IN ICA_SVF_HANDLE_OPER_WKSP

    PERFORM BFA-CALL-SVF-HANDLE-OPER-RTN

*   Set ctrl status code to EXECUTE.
    MOVE V$_BATCH_EXECUTE 
      TO CTRL_STATUS_CODE   IN ICA_SVF_HAN
DLE_OPER_WKSP

    MOVE SYM$_OPER_ID 
      TO OPER_ID	    IN ICA_SVF_HANDLE_OPER_WKSP

    SET P$_UPDATE_MODE	    IN ICA_SVF_HANDLE_OPER_WKSP  TO TRUE

    MOVE "BATCH" 
      TO DP$_WORK_CONTEXT   IN ICA_SVF_HANDLE_OPER_WKSP

    PERFORM BFA-CALL-SVF-HANDLE-OPER-RTN
    .
 BF-EXIT.     EXIT.
*----------------------------------------------------------------------
 BFA-CALL-SVF-HANDLE-OPER-RTN			    SECTION.
*----------------------------------------------------------------------
 BFA-00.

    CALL 'ICA_SVF_
HANDLE_OPER'  USING  ICA_SVF_HANDLE_OPER_WKSP
				       UTL_CONTROL_ACW

    IF P$_ACW_PROC_AUX_STATUS  IN ICA_SVF_HANDLE_OPER_WKSP  NOT = SP$_MSG_NORMAL
       MOVE SP$_MSG_ERROR
         TO SP$_ACW_PROC_AUX_STATUS	IN UTL_CONTROL_ACW

       MOVE P$_ACW_PROC_AUX_STATUS	IN ICA_SVF_HANDLE_OPER_WKSP 
	 TO SP$_ACW_FORM_AUX_STATUS	IN UTL_CONTROL_ACW

       MOVE SP$_MSG_ERROR 
         TO SP$_EXIT_STATUS

       DISPLAY "Error in Sec:BFA-UPDATE-OPER  Rtn:ICA_SVF_HANDLE_OPER"

       PERFORM X-ERRORS
    END-IF

    .
 BFA-EXIT.     EXIT.
*----------------------------------------------------------------------
 BG-GET-CURRENT-TIME			    SECTION.
*----------------------------------------------------------------------
 BG-00.

    INITIALIZE  UTL_GET_DATE_TIME_WKSP

    CALL 'UTL_GET_DATE_TIME'  USING  UTL_GET_DATE_TIME_WKSP

    IF SP$_SYSPRO_STATUS  IN UTL_GET_DATE_TIME_WKSP  IS SUCCESS
       MOVE SP$_DATE_TIME_BINARY    IN UTL_GET_DATE_TIME_WKSP 
	 TO L$_SYSTEM_DATE_START
    ELSE
       DISPLAY "Error in Sec:BG-
GET-CURRENT-TIME  Rtn:UTL_GET_DATE_TIME"

       MOVE SP$_SYSPRO_STATUS	    IN UTL_GET_DATE_TIME_WKSP
         TO SP$_ACW_PROC_AUX_STATUS

       MOVE SP$_MSG_ERROR  
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF

    MOVE SP$_DATE_TIME_BINARY	IN UTL_GET_DATE_TIME_WKSP
      TO SP$_DATE_TIME_BINARY   IN UTL_CVT_DATE_DDMMYYYY_WKSP

    CALL 'UTL_CVT_DATE_DDMMYYYY'  USING  UTL_CVT_DATE_DDMMYYYY_WKSP

    IF SP$_SYSPRO_STATUS OF UTL_CVT_DATE_DDMMYYYY_WKSP = SP$_MSG_NORMAL
       MOVE SP$_DATE
_DDMMYYYY  
         TO L$_WS01_CURRENT_DATE
    ELSE
       DISPLAY "Error in Sec:BG-GET-CURRENT-TIME  Rtn:UTL_APPLY_DELTA_TIME_V2"

       MOVE SP$_MSG_ERROR  
         TO SP$_EXIT_STATUS
            SP$_ACW_PROC_AUX_STATUS

       PERFORM X-ERRORS
    END-IF  
    .
 BG-EXIT.     EXIT.
*----------------------------------------------------------------------
 BI-CHECK-519-FILE-RUNNING	    SECTION.
*----------------------------------------------------------------------
 BI-00.

    SET L$_SW01_600_DATA_NOT_
FOUND    TO TRUE

    PERFORM X1-START-TRANS-READ-ONLY

    PERFORM BIA-OPEN-CURSOR-600

    PERFORM UNTIL L$_SW01_600_DATA_FOUND  OR  L$_SW01_600_DATA_END
       PERFORM BIB-FETCH-CURSOR-600 
    END-PERFORM

    PERFORM BIC-CLOSE-CURSOR-600

    PERFORM X3-COMMIT
    .
 BI-EXIT.    EXIT.
*----------------------------------------------------------------------
 BIA-OPEN-CURSOR-600	    SECTION.
*----------------------------------------------------------------------
 BIA-00.

    INITIALIZE  ICD_TRT_600_TRF_M
GNT_DBW

    MOVE 519 
      TO FILEIN_TAPE_CODE   IN ICD_TRT_600_TRF_MGNT_PRW
			    IN ICD_TRT_600_TRF_MGNT_DBW

*   Decrease 1 day from start-date.
    INITIALIZE UTL_APPLY_DELTA_TIME_V2_WKSP

    MOVE L$_SYSTEM_DATE_START 
      TO SP$_DATE_TIME_1    OF UTL_APPLY_DELTA_TIME_V2_WKSP

    MOVE "C$_DAYS"  
      TO SP$_DELTA_TYPE	    OF UTL_APPLY_DELTA_TIME_V2_WKSP

    MOVE 1          
      TO SP$_TIME	    OF UTL_APPLY_DELTA_TIME_V2_WKSP

    MOVE "-"        
      TO SP$_MATH_OPERATOR  OF UTL_APPLY_DELT
A_TIME_V2_WKSP

    CALL 'UTL_APPLY_DELTA_TIME_V2' USING UTL_APPLY_DELTA_TIME_V2_WKSP

    IF SP$_SYSPRO_STATUS OF UTL_APPLY_DELTA_TIME_V2_WKSP NOT = SP$_MSG_NORMAL
       DISPLAY "Error!!!: SEC:BIA-OPEN-CURSOR-600  RTN:UTL_APPLY_DELTA_TIME_V2"

       MOVE SP$_MSG_ERROR  
         TO SP$_EXIT_STATUS
            SP$_ACW_PROC_AUX_STATUS

       PERFORM X-ERRORS
    END-IF  

    MOVE SP$_DATE_TIME_2    OF UTL_APPLY_DELTA_TIME_V2_WKSP
      TO TRNF_INPUTING_TMSP IN ICD_TRT_600_TRF_MGNT_PRW
			    IN ICD_TRT_6
00_TRF_MGNT_DBW

    MOVE 007  
      TO DP$_KEY_SEQUENCE IN ICD_TRT_600_TRF_MGNT_DBW

*   600.CUR007:
*   ... WHERE	FILEIN_TAPE_CODE    = PRM...	    &
*		TRNF_INPUTING_TMSP >= PRM...

       ADD DP$_SYM_OPEN_CURSOR 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRT_600_TRF_MGNT_DBW

    CALL 'ICD_TRT_600_TRF_MGNT_DBA'  USING  ICD_TRT_600_TRF_MGNT_DBW

    EVALUATE DP$_STATUS IN ICD_TRT_600_TRF_MGNT_DBW
       WHEN SP$_MSG_NORMAL
	  CONTINUE

       WHEN OTHER
          MOVE CORR UTL_DBA_UNIV_
WKSP	IN ICD_TRT_600_TRF_MGNT_DBW
            TO ICA_ERROR_HANDLE_WKSP

          CALL 'ICA_ICF_ERROR_HANDLE'  USING  ICA_ERROR_HANDLE_WKSP
                                              UTL_CONTROL_ACW
                                      GIVING  SP$_ACW_PROC_AUX_STATUS

          IF P$_FOUND_MSG_SEVERITY IN ICA_ERROR_HANDLE_WKSP = "W"
             CALL 'ICA_ICF_XLATE_MSG'  USING  UTL_CONTROL_ACW
				              ICA_XLATE_MSG_WKSP
          ELSE
             MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRT_600_TRF_M
GNT_DBW
               TO SP$_ACW_FORM_MSG

             MOVE "SEC:BIA-OPEN-CURSOR-600  RTN:ICD_TRT_600_TRF_MGNT_DBA"
               TO SP$_ACW_FREE_TEXT
          END-IF

          DISPLAY "Error in SEC:BIA-OPEN-CURSOR-600 "
                  "RTN:ICD_TRT_600_TRF_MGNT_DBA"

          MOVE DP$_STATUS_AUXIL	    IN ICD_TRT_600_TRF_MGNT_DBW 
            TO SP$_EXIT_STATUS

          MOVE DP$_STATUS IN ICD_TRT_600_TRF_MGNT_DBW
            TO SP$_ACW_PROC_AUX_STATUS

          DISPLAY SP$_ACW_FORM_MSG

         
 PERFORM X-ERRORS
    END-EVALUATE
    .
 BIA-EXIT.    EXIT.
*----------------------------------------------------------------------
 BIB-FETCH-CURSOR-600		    SECTION.
*----------------------------------------------------------------------
 BIB-00.

    MOVE DP$_SYM_FETCH 
      TO DP$_ACTION	    OF ICD_TRT_600_TRF_MGNT_DBW

    CALL 'ICD_TRT_600_TRF_MGNT_DBA'  USING  ICD_TRT_600_TRF_MGNT_DBW

    EVALUATE  DP$_STATUS    OF ICD_TRT_600_TRF_MGNT_DBW
       WHEN SP$_MSG_NORMAL
	  PERFORM BIBA-CHECK-036

    
   WHEN SP$_MSG_END_OF_DATA_SET
	  PERFORM BIBA-CHECK-036

	  IF L$_SW01_600_DATA_NOT_FOUND
	     SET L$_SW01_600_DATA_END TO TRUE
	  END-IF

       WHEN SP$_MSG_NO_DATA_FOUND
	  SET L$_SW01_600_DATA_END    TO TRUE

       WHEN OTHER
          MOVE CORR UTL_DBA_UNIV_WKSP	IN ICD_TRT_600_TRF_MGNT_DBW 
            TO ICA_ERROR_HANDLE_WKSP

          CALL 'ICA_ICF_ERROR_HANDLE'  USING  ICA_ERROR_HANDLE_WKSP
                                              UTL_CONTROL_ACW
                                      GIVIN
G  SP$_ACW_PROC_AUX_STATUS

          IF P$_FOUND_MSG_SEVERITY IN ICA_ERROR_HANDLE_WKSP = "W"
	     CALL 'ICA_ICF_XLATE_MSG' USING	UTL_CONTROL_ACW
						ICA_XLATE_MSG_WKSP
          ELSE
             MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRT_600_TRF_MGNT_DBW
               TO SP$_ACW_FORM_MSG

             MOVE "SEC:BIB-FETCH-CURSOR-600  RTN:ICD_TRT_600_TRF_MGNT_DBA"
               TO SP$_ACW_FREE_TEXT
          END-IF

          DISPLAY "Error in SEC:BIB-FETCH-CURSOR-600 "
                  "RTN:ICD_TRT_600_TR
F_MGNT_DBA"

          MOVE DP$_STATUS_AUXIL	    IN ICD_TRT_600_TRF_MGNT_DBW 
            TO SP$_EXIT_STATUS

          MOVE DP$_STATUS    OF ICD_TRT_600_TRF_MGNT_DBW
            TO SP$_ACW_PROC_AUX_STATUS

          DISPLAY SP$_ACW_FORM_MSG

          PERFORM X-ERRORS
    END-EVALUATE
    .
 BIB-EXIT.    EXIT.
*----------------------------------------------------------------------
 BIBA-CHECK-036			    SECTION.
*----------------------------------------------------------------------
 BIBA-00.

    INITIALIZ
E ICD_SVT_036_OPER_DBW

    MOVE 000  
      TO DP$_KEY_SEQUENCE	IN ICD_SVT_036_OPER_DBW

    MOVE OPER_ID IN ICD_TRT_600_TRF_MGNT IN ICD_TRT_600_TRF_MGNT_DBW  
      TO OPER_ID IN ICD_SVT_036_OPER_PRW IN ICD_SVT_036_OPER_DBW

       ADD DP$_SYM_INQUIRE 
        TO DP$_SYM_ACCESS_RDB
    GIVING DP$_ACTION		IN ICD_SVT_036_OPER_DBW

    CALL 'ICD_SVT_036_OPER_DBA' USING ICD_SVT_036_OPER_DBW

    EVALUATE DP$_STATUS IN ICD_SVT_036_OPER_DBW
       WHEN SP$_MSG_NORMAL
	  IF (CTRL_STATUS_CODE	IN ICD_SVT_036_OPER
				IN ICD_SVT_036_OPER_DBW = V0010$WORKING)  AND
	     (OPER_ID IN ICD_SVT_036_OPER IN ICD_SVT_036_OPER_DBW  NOT =
              SYM$_OPER_ID)
	  THEN
             SET L$_SW01_600_DATA_FOUND TO TRUE

	     DISPLAY "  FOUND OLD WORKING 036.OPER_ID=",
		     OPER_ID	IN ICD_SVT_036_OPER_PRW
				IN ICD_SVT_036_OPER_DBW CONVERSION
	  END-IF

       WHEN SP$_MSG_NO_DATA_FOUND
	  CONTINUE

       WHEN OTHER
          MOVE CORR UTL_DBA_UNIV_WKSP IN ICD_SVT_036_OPER_DBW TO
               ICA_ERROR_HANDLE_WKSP

    
      CALL 'ICA_ICF_ERROR_HANDLE' USING  ICA_ERROR_HANDLE_WKSP
                                             UTL_CONTROL_ACW
                                     GIVING  SP$_ACW_PROC_AUX_STATUS
          IF P$_FOUND_MSG_SEVERITY  IN ICA_ERROR_HANDLE_WKSP = "W"
	     CALL 'ICA_ICF_XLATE_MSG' USING	UTL_CONTROL_ACW
						ICA_XLATE_MSG_WKSP
          ELSE
             MOVE DP$_STATUS_AUXIL_TEXT IN ICD_SVT_036_OPER_DBW
               TO SP$_ACW_FORM_MSG

             MOVE "SEC:BIBA-CHECK-036 RTN:ICD_SVT_036_OPER_
DBA"
               TO SP$_ACW_FREE_TEXT
          END-IF

          DISPLAY "Error in SEC:BIBA-CHECK-036 RTN:ICD_SVT_036_OPER_DBA"

          MOVE DP$_STATUS IN ICD_SVT_036_OPER_DBW
            TO SP$_ACW_PROC_AUX_STATUS

          MOVE DP$_STATUS_AUXIL	    IN ICD_SVT_036_OPER_DBW 
            TO SP$_EXIT_STATUS

          DISPLAY SP$_ACW_FORM_MSG

          PERFORM X-ERRORS
    END-EVALUATE
    .
 BIBA-EXIT.    EXIT.
*----------------------------------------------------------------------
 BIC-CLOSE-CURSOR
-600		    SECTION.
*----------------------------------------------------------------------
 BIC-00.

       ADD DP$_SYM_CLOSE_CURSOR 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRT_600_TRF_MGNT_DBW

    CALL 'ICD_TRT_600_TRF_MGNT_DBA' USING ICD_TRT_600_TRF_MGNT_DBW

    IF DP$_STATUS IN ICD_TRT_600_TRF_MGNT_DBW  NOT = SP$_MSG_NORMAL
       DISPLAY "Error in Sec:BIC-CLOSE-CURSOR-600 "
               "Rtn:ICD_PRT_362_HST_PR_UPD_DBA"

       MOVE DP$_STATUS_AUXIL IN ICD_TRT_600_TRF_MGNT_DBW 
 
        TO SP$_EXIT_STATUS
            SP$_ACW_PROC_AUX_STATUS

       DISPLAY SP$_ACW_FORM_MSG

       PERFORM X-ERRORS
    END-IF
    .
 BIC-EXIT.     EXIT.
*----------------------------------------------------------------------
 BJ-GET-ENTITY-INFO		    SECTION.
*----------------------------------------------------------------------
 BJ-00.

    PERFORM X1-START-TRANS-READ-ONLY

       ADD DP$_SYM_INQUIRE 
        TO DP$_SYM_ACCESS_CTT 
    GIVING DP$_ACTION	    IN ICD_SVT_030_ENTITY_DBW

    MOVE SYM$_EN
TITY_CODE 
      TO ENTITY_CODE	    IN ICD_SVT_030_ENTITY_PRW
    
    CALL 'ICD_SVT_030_ENTITY_DBA'  USING  ICD_SVT_030_ENTITY_DBW

    IF DP$_STATUS IN ICD_SVT_030_ENTITY_DBW  NOT = SP$_MSG_NORMAL
       IF DP$_STATUS IN ICD_SVT_030_ENTITY_DBW  NOT FAILURE
          MOVE "<No data found for request.>" 
            TO SP$_ACW_FORM_MSG

	  MOVE SP$_MSG_ERROR 
            TO SP$_EXIT_STATUS
       ELSE
          MOVE DP$_STATUS_AUXIL_TEXT  IN  ICD_SVT_030_ENTITY_DBW
	    TO SP$_ACW_FORM_MSG

	  MOVE DP$_STAT
US_AUXIL	      IN  ICD_SVT_030_ENTITY_DBW 
            TO SP$_EXIT_STATUS
       END-IF

       MOVE SP$_MSG_ERROR 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE "Sec:BJ-GET-ENTITY-INFO  Rtn:ICD_SVT_030_ENTITY_DBA" 
         TO SP$_ACW_FREE_TEXT

       DISPLAY SP$_ACW_FREE_TEXT
       DISPLAY "  SP$_MSG_ERROR=", SP$_MSG_ERROR CONVERSION
       DISPLAY "  DP$_STATUS_AUXIL=",
                DP$_STATUS_AUXIL IN ICD_SVT_030_ENTITY_DBW CONVERSION
       DISPLAY "  DP$_STATUS_AUXIL_TEXT=",
                D
P$_STATUS_AUXIL_TEXT IN ICD_SVT_030_ENTITY_DBW CONVERSION

       PERFORM X-ERRORS
    END-IF

*   get entity oper information. (File Name).
       ADD DP$_SYM_INQUIRE 
        TO DP$_SYM_ACCESS_CTT 
    GIVING DP$_ACTION IN ICD_SVT_035_OPER_PRM_DBW

    MOVE SYM$_ENTITY_CODE 
      TO ENTITY_CODE	    IN ICD_SVT_035_OPER_PRM_PRW
    
    CALL 'ICD_SVT_035_OPER_PRM_DBA'  USING  ICD_SVT_035_OPER_PRM_DBW

    IF DP$_STATUS  IN ICD_SVT_035_OPER_PRM_DBW  IS FAILURE
       MOVE DP$_STATUS		    IN ICD_SVT_035_OPER
_PRM_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN ICD_SVT_035_OPER_PRM_DBW 
         TO SP$_ACW_FORM_MSG

       MOVE "Sec:BJ-GET-ENTITY-INFO Rtn:ICD_SVT_035_OPER_PRM_DBA" 
         TO SP$_ACW_FREE_TEXT

       DISPLAY SP$_ACW_FREE_TEXT
       DISPLAY "  DP$_STATUS=",
                DP$_STATUS IN ICD_SVT_035_OPER_PRM_DBW CONVERSION
       DISPLAY "  DP$_STATUS_AUXIL_TEXT=",
                DP$_STATUS_AUXIL_TEXT IN ICD_SVT_035_OPER_PRM_DBW CONVERSION

       MOVE DP$_STA
TUS_AUXIL IN ICD_SVT_035_OPER_PRM_DBW 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF

    PERFORM X3-COMMIT

    IF DP$_STATUS  IN ICD_SVT_035_OPER_PRM_DBW = SP$_MSG_NO_DATA_FOUND 
       MOVE SPACES  
         TO L$_FILE_DESC

       MOVE ZEROES  
         TO L$_FILE_DESC_LEN
    ELSE
       MOVE FILE_NAME  IN ICD_SVT_035_OPER_PRM 
         TO L$_FILE_DESC

       PERFORM VARYING L$_IX01_IND
		  FROM 1 BY 1
		 UNTIL (L$_IX01_IND > 11)                  OR
		       (L$_FILE_DESC(L$_IX01_IND
:1) = ' ')
	
	    CONTINUE
       END-PERFORM

       SUBTRACT 1 FROM L$_IX01_IND  GIVING  L$_FILE_DESC_LEN
    END-IF

*   Read entity param values.
    MOVE SYM$_OPER_ID 
      TO OPER_ID	    IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP

    MOVE SYM$_ENTITY_CODE 
      TO ENTITY_CODE	    IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP

    SET P$_READ_MODE	    IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP TO TRUE

    MOVE "BATCH" 
      TO DP$_WORK_CONTEXT   IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP

    CALL 'ICA_SVF_HANDLE_PARAM_VALUE'  USIN
G  ICA_SVF_HANDLE_PARAM_VALUE_WKSP
					      UTL_CONTROL_ACW

    IF P$_ACW_PROC_AUX_STATUS  IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP  NOT = 
       SP$_MSG_NORMAL
    THEN
       MOVE SP$_MSG_ERROR
         TO SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW

       MOVE P$_ACW_PROC_AUX_STATUS   IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP  
	 TO SP$_ACW_FORM_AUX_STATUS  IN UTL_CONTROL_ACW

       MOVE SP$_MSG_ERROR 
         TO SP$_EXIT_STATUS

       DISPLAY "Error in Sec:BJ-GET-ENTITY-INFO "
               "Rtn:ICA_SVF_HAN
DLE_PARAM_VALUE" 
       DISPLAY "STTS=",
	       P$_ACW_PROC_AUX_STATUS IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP
			    CONVERSION
       PERFORM X-ERRORS
    END-IF

    PERFORM BJA-GET-PARAMS-VALUE
    .
 BJ-EXIT.     EXIT.
*----------------------------------------------------------------------
 BJA-GET-PARAMS-VALUE			SECTION.
*----------------------------------------------------------------------
 BJA-00.

    PERFORM VARYING L$_IX01_IND
	       FROM 1 BY 1
	      UNTIL L$_IX01_IND > P$_PARAM_NUM IN 
        
                            ICA_SVF_HANDLE_PARAM_VALUE_WKSP

       DISPLAY " ", L$_IX01_IND CONVERSION, ". ",
               PRM_ENT_NAME IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP(L$_IX01_IND)

       EVALUATE TRUE
          WHEN PRM_ENT_NAME IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP(L$_IX01_IND) 
                                            = "TRNF_ID"
   	     MOVE NUMERIC_PRM_VALUE_1  IN
		     ICA_SVF_HANDLE_PARAM_VALUE_WKSP (L$_IX01_IND)
               TO TRNF_ID		IN ICD_TRT_600_TRF_MGNT_PRW
					IN ICD_TRT_600_TRF_
MGNT_DBW
                  L$_WS01_TRNF_ID	IN L$_K_TRB71I_ADC_NAME 
                  L$_WS01_TRNF_ID       IN L$_P_TRB71I_875_NAME_X
                  L$_WS01_TRNF_ID	IN L$_P_TRB71I_ATR_R_NAME_X
                  L$_WS01_TRNF_ID	IN L$_P_TRB71I_ATR_F_NAME
                  L$_WS01_TRNF_ID	IN L$_P_TRB71I_MSM_SEND_MSG_NAME
                  L$_WS01_TRNF_ID	IN L$_P_TRB71I_MSM_SUM_MSG_NAME
	          L$_WS01_TRNF_ID	IN ICA_TRB71I_MSM_MGS_WKSP

             MOVE L$_P_TRB71I_875_NAME_X
               TO L$_P_TRB7
1I_875_NAME

             MOVE L$_P_TRB71I_ATR_R_NAME_X
               TO L$_P_TRB71I_ATR_R_NAME

*         Check if param oper_id exists, if so then this is a recovery process.
*         Field numeric_prm_value_1 contains param oper_id.
	  WHEN PRM_ENT_NAME IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP(L$_IX01_IND)
				    = "OPER_ID"
	     INITIALIZE ICA_SVF_HANDLE_OPER_WKSP

	     MOVE NUMERIC_PRM_VALUE_1 
                        IN ICA_SVF_HANDLE_PARAM_VALUE_WKSP(L$_IX01_IND) 
               TO OPER_ID	    IN ICA_
SVF_HANDLE_OPER_WKSP

	     SET P$_READ_MODE	    IN ICA_SVF_HANDLE_OPER_WKSP TO TRUE

	     MOVE "BATCH" 
               TO DP$_WORK_CONTEXT  IN ICA_SVF_HANDLE_OPER_WKSP

	     CALL 'ICA_SVF_HANDLE_OPER'  USING  ICA_SVF_HANDLE_OPER_WKSP
						UTL_CONTROL_ACW

	     IF P$_ACW_PROC_AUX_STATUS IN ICA_SVF_HANDLE_OPER_WKSP  NOT = 
                SP$_MSG_NORMAL
             THEN
		MOVE SP$_MSG_ERROR
		  TO SP$_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW

		MOVE P$_ACW_PROC_AUX_STATUS IN ICA_SVF_HANDLE_OPER_WKSP 
		  
TO SP$_ACW_FORM_AUX_STATUS IN UTL_CONTROL_ACW

		MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
		DISPLAY " ERR>> STTS",
			P$_ACW_PROC_AUX_STATUS IN ICA_SVF_HANDLE_OPER_WKSP
					CONVERSION
                PERFORM X-ERRORS
             END-IF

	  WHEN OTHER
	     MOVE SP$_MSG_ERROR 
               TO SP$_EXIT_STATUS

	     MOVE SP$_MSG_ERROR 
               TO SP$_ACW_PROC_AUX_STATUS 

	     MOVE "ERROR: parameter was not extracted" 
               TO SP$_ACW_FORM_MSG

	     DISPLAY SP$_ACW_FORM_MSG

             S
TRING
	        "Error in Sec:BJA-GET-PARAMS-VALUE "  DELIMITED BY SIZE
                "Rtn:ICA_SVF_HANDLE_PARAM_VALUE_WKSP" DELIMITED BY SIZE
	        INTO SP$_ACW_FREE_TEXT
             END-STRING

             PERFORM X-ERRORS
       END-EVALUATE
    END-PERFORM

*   Set OPER ID symbol for TRB71I job
    MOVE "TRB71I_OPER_ID" 
      TO SP$_SYMBOL IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE SYM$_OPER_ID	
      TO SP$_SYMBOL_VALUE   IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE "GLOBAL"
      TO SP$_SYMBOL_TABLE   IN 
UTL_SET_SYMBOL_VALUE_WKSP

    CALL "UTL_SET_SYMBOL_VALUE"  USING  UTL_SET_SYMBOL_VALUE_WKSP

    IF SP$_SYSPRO_STATUS_AUXIL  IN UTL_SET_SYMBOL_VALUE_WKSP  IS FAILURE
       SET SP$_EXIT_STATUS_FAILURE TO TRUE

       MOVE SP$_MSG_ERROR 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE " Symbol TRB71I_OPER_ID not created" 
         TO SP$_ACW_FREE_TEXT

       DISPLAY SP$_ACW_FORM_MSG
       DISPLAY SP$_ACW_FREE_TEXT

       PERFORM X-ERRORS
    END-IF
    .
 BJA-EXIT.     EXIT.
*--------------------------
--------------------------------------------
 BK-GET-FILEIN-600-INFO		    SECTION.
*----------------------------------------------------------------------
 BK-00.

*   600 הלבטמ סנכנ ץבוק יטרפ תאירק 
    PERFORM X1-START-TRANS-READ-ONLY

    INITIALIZE ICA_SVM_FILEIN_UPDATE_WKSP
	       ICA_SVM_CHECK_DUP_BALANC_WKSP
	       ICA_XLATE_MSG_WKSP

    MOVE TRNF_ID    IN ICD_TRT_600_TRF_MGNT_PRW
		    IN ICD_TRT_600_TRF_MGNT_DBW
      TO TRNF_ID    IN ICA_SVM_CHECK_DUP_BALANC_WKSP
         TRNF_ID    IN ICA_SVM_
FILEIN_UPDATE_WKSP

    MOVE V$_HEB_YES 
      TO CALL_IN_BATCH	   IN ICA_SVM_CHECK_DUP_BALANC_WKSP 
         CALL_IN_BATCH	   IN ICA_SVM_FILEIN_UPDATE_WKSP 

    MOVE ZERO            
      TO DP$_KEY_SEQUENCE   OF ICD_TRT_600_TRF_MGNT_DBW
    
       ADD DP$_SYM_INQUIRE 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRT_600_TRF_MGNT_DBW

    CALL 'ICD_TRT_600_TRF_MGNT_DBA' USING ICD_TRT_600_TRF_MGNT_DBW

    IF DP$_STATUS IN ICD_TRT_600_TRF_MGNT_DBW = SP$_MSG_NORMAL
       DISPLAY "Source of
 the input file :" 
               TRNF_SRC_CODE	IN ICD_TRT_600_TRF_MGNT
				IN ICD_TRT_600_TRF_MGNT_DBW  WITH CONVERSION
    ELSE
       DISPLAY "  ERROR ON READING 600."
       DISPLAY "  TXT=", 
               DP$_STATUS_AUXIL_TEXT IN ICD_TRT_600_TRF_MGNT_DBW
       DISPLAY "  STTS=", 
               DP$_STATUS	     IN ICD_TRT_600_TRF_MGNT_DBW CONVERSION
       DISPLAY "  STTSA=", 
               DP$_STATUS_AUXIL	     IN ICD_TRT_600_TRF_MGNT_DBW CONVERSION

       IF DP$_STATUS IN ICD_TRT_600_TRF_MGNT_DB
W NOT FAILURE
          MOVE "<No data found for request.>" TO SP$_ACW_FORM_MSG
	  MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
       ELSE
          MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRT_600_TRF_MGNT_DBW
	  TO   SP$_ACW_FORM_MSG
	  MOVE DP$_STATUS_AUXIL IN ICD_TRT_600_TRF_MGNT_DBW TO SP$_EXIT_STATUS
       END-IF

       MOVE SP$_MSG_ERROR 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE "Sec:BK-GET-FILEIN-600-INFO  Rtn:ICD_TRT_600_TRF_MGNT_DBA" 
         TO SP$_ACW_FREE_TEXT

       PERFORM X-ERRORS
    END-IF


    PERFORM X3-COMMIT 
    .  
 BK-EXIT.     EXIT.
*----------------------------------------------------------------------
 BL-CHECK-FILEIN-STAGE		    SECTION.
*----------------------------------------------------------------------
 BL-00.

    DISPLAY ">>> Current input file stage:"
	    INPUT_FILE_STEP_CODE    IN ICD_TRT_600_TRF_MGNT
				    IN ICD_TRT_600_TRF_MGNT_DBW CONVERSION

    IF INPUT_FILE_STEP_CODE   IN ICD_TRT_600_TRF_MGNT
                              IN ICD_TRT_600_TRF_MGNT_DBW = ZERO
    T
HEN
*      ץבוקב םינותנה תוניקת תקידב
       PERFORM BLA-CHECK-FILE-VALUES

*      ןוזיאו תוליפכ תקידב
       PERFORM BLB-CHECK-DUP-BALANCE
    END-IF

*   הטילקה בלשב אצמנ םא הקידב
    IF INPUT_FILE_STEP_CODE	    IN ICD_TRT_600_TRF_MGNT <= 1
       INITIALIZE  L$_CT01_INP_REC_COUNT

       OPEN INPUT K_TRB71I_ADC
       DISPLAY "Open input file: ", L$_K_TRB71I_ADC_NAME

       SET L$_SW01_EOF_INP_FILE_NO  TO TRUE

       PERFORM BLD-UPDATE-FILEIN-STATUS

       PERFORM BZ-OPEN-OUTPUT-FILES

       PERFORM 
C-READ-INPUT-FILE
    ELSE
       SET L$_SW01_EOF_INP_FILE_YES  TO TRUE 

       MOVE RECS_NUM_CHKP	    IN ICD_TRT_600_TRF_MGNT
				    IN ICD_TRT_600_TRF_MGNT_DBW
         TO L$_CT01_INP_REC_COUNT

       MOVE INPUT_FILE_STEP_CODE    IN ICD_TRT_600_TRF_MGNT
         TO INPUT_FILE_STEP_CODE    IN ICA_SVM_FILEIN_UPDATE_WKSP
    END-IF
    .  
 BL-EXIT.     EXIT.
*----------------------------------------------------------------------
 BLA-CHECK-FILE-VALUES				SECTION.
*----------------------------------------
------------------------------
 BLA-00.

    OPEN INPUT K_TRB71I_ADC

    DISPLAY "Open input file: ", L$_K_TRB71I_ADC_NAME
            " for checking file values & duplication."

    INITIALIZE  L$_CT01_INP_REC_COUNT

    SET L$_SW01_EOF_INP_FILE_NO   TO TRUE

    PERFORM C-READ-INPUT-FILE

    MOVE K_TRB71I_ADC_REC
      TO TRNF_FIRST_DATA_REC IN ICA_SVM_CHECK_DUP_BALANC_WKSP

    PERFORM UNTIL L$_SW01_EOF_INP_FILE_YES
       EVALUATE TRUE
          WHEN L$_WS01_REC_TYPE_HDR	IN ICA_TRB71I_ACCUM_INTRF_REC
					IN K_TRB71I_ADC_REC

             ADD 1   TO L$_CT01_HDR_TRL

          WHEN L$_WS01_REC_TYPE_TRL	IN ICA_TRB71I_ACCUM_INTRF_REC
					IN K_TRB71I_ADC_REC

             PERFORM BLAB-HANDLE-TRAILER

          WHEN L$_WS01_REC_TYPE_DATA	IN ICA_TRB71I_ACCUM_INTRF_REC
					IN K_TRB71I_ADC_REC

             PERFORM BLAC-HANDLE-DATA

          WHEN OTHER
             CONTINUE
       END-EVALUATE

       PERFORM C-READ-INPUT-FILE
    END-PERFORM

*   תמויסה תומושר תומכמ הנוש תרתוכה תומושר תומכו הדימב
    IF L$_
CT01_HDR_TRL  NOT = ZEROES
       MOVE SP$_MSG_ERROR 
         TO SP$_EXIT_STATUS

       DISPLAY "Error in Sec:BLA-CHECK-FILE-VALUES "
       DISPLAY "Number of header records is different from trailer records."

       PERFORM X-ERRORS
    END-IF

    MOVE L$_CT01_INP_REC_COUNT  TO
	 TOT_RECS_NUM		IN ICA_SVM_CHECK_DUP_BALANC_WKSP
         TOT_RECS_NUM		IN ICD_TRT_600_TRF_MGNT
                                IN ICD_TRT_600_TRF_MGNT_DBW

    SET L$_SW01_EOF_INP_FILE_NO  TO TRUE

    CLOSE K_TRB71I_ADC

    
DISPLAY "Close input file: ", L$_K_TRB71I_ADC_NAME
            " for checking file values & duplication."
    .
 BLA-EXIT.     EXIT.
*----------------------------------------------------------------------
 BLAB-HANDLE-TRAILER		SECTION.
*----------------------------------------------------------------------
 BLAB-00.

    IF (L$_WS01_FILE_NUM_RECORDS    IN ICA_TRB71I_ACCUM_INTRF_TRL
				    IN K_TRB71I_ADC_REC 
                                     = L$_CT01_NUM_TNUOT)	        
    THEN
       MOVE ZEROES
   
      TO L$_CT01_NUM_TNUOT

       SUBTRACT 1   FROM L$_CT01_HDR_TRL
    END-IF
    .
 BLAB-EXIT.     EXIT.
*----------------------------------------------------------------------
 BLAC-HANDLE-DATA		SECTION.
*----------------------------------------------------------------------
 BLAC-00.

    ADD 1   TO L$_CT01_NUM_TNUOT
    .
 BLAC-EXIT.     EXIT.
*----------------------------------------------------------------------
 BLB-CHECK-DUP-BALANCE		    SECTION.
*--------------------------------------------------
--------------------
*    Open  the file for inputing. 
*    If the records number in tab.600 = 0:
*       Read a first record or/and a header record.
*       Calculate a records number. 
*       Check a balance problem.
*       Write fields in ICA_SVM_CHECK_DUP_BALANC_WKSP
*       Call the routine "Ica_svm_filein_check_dup_balanc"
*       If a duplicate problem or a balanc problem exists --> 
*                                     
*----------------------------------------------------------------------
 BLB
-00.

    DISPLAY "ןוזיאו תוליפכ תקידב..."

    MOVE "ל"  TO  IND_UNBALANCED_FILE IN ICA_SVM_CHECK_DUP_BALANC_WKSP

    MOVE "ל"  TO  TRNF_DUPL_IND IN ICA_SVM_CHECK_DUP_BALANC_WKSP

    MOVE TRNF_ID    IN ICD_TRT_600_TRF_MGNT_PRW
                    IN ICD_TRT_600_TRF_MGNT_DBW  TO
         TRNF_ID    IN ICA_SVM_CHECK_DUP_BALANC_WKSP

    CALL 'ICA_SVM_FILEIN_CHECK_DUP_BALANC' 
                                        USING ICA_SVM_CHECK_DUP_BALANC_WKSP
 			                      UTL_CONTROL_ACW
 	            
                   GIVING SP$_ACW_PROC_AUX_STATUS.

    IF SP$_ACW_PROC_AUX_STATUS  IS FAILURE
       MOVE SP$_MSG_ERROR 
         TO SP$_EXIT_STATUS

       DISPLAY "Error in Sec:BLB-CHECK-DUP-BALANCE "
               "Rtn:ICA_SVM_FILEIN_CHECK_DUP_BALANC"
       DISPLAY "    STTS=", SP$_ACW_PROC_AUX_STATUS CONVERSION
       DISPLAY "    TEST=", SP$_ACW_FORM_MSG OF UTL_CONTROL_ACW

       PERFORM X-ERRORS
    ELSE
       DISPLAY "Rtn: ICA_SVM_FILEIN_CHECK_DUP_BALANC reported success."
    END-IF
    .
 BLB-
EXIT.     EXIT.
*----------------------------------------------------------------------
 BLD-UPDATE-FILEIN-STATUS		    SECTION.
*----------------------------------------------------------------------
 BLD-00.

    MOVE V2002$INPUTING_IN_PROCESS  
      TO TRNF_INPUTING_STS_CODE  IN ICA_SVM_FILEIN_UPDATE_WKSP

*   הטילק - 1 בלש
    MOVE 1
      TO INPUT_FILE_STEP_CODE	    IN ICA_SVM_FILEIN_UPDATE_WKSP

    PERFORM UA-UPDATE-FILEIN-STATUS
    .
 BLD-EXIT.     EXIT.
*-------------------------------------------
---------------------------
 BZ-OPEN-OUTPUT-FILES			    SECTION.
*----------------------------------------------------------------------
 BZ-00.

    OPEN OUTPUT P_TRB71I_875
    DISPLAY "Open Output File: " L$_P_TRB71I_875_NAME

    OPEN OUTPUT P_TRB71I_ATR_R
    DISPLAY "Open Output File: " L$_P_TRB71I_ATR_R_NAME

    OPEN OUTPUT P_TRB71I_ATR_F
    DISPLAY "Open Output File: " L$_P_TRB71I_ATR_F_NAME

    OPEN OUTPUT P_TRB71I_MSM_SEND_MSG
    DISPLAY "Open Output file: " L$_P_TRB71I_MSM_SEND_MSG_NAME

    
OPEN OUTPUT P_TRB71I_MSM_SUM_MSG
    DISPLAY "Open Output file: " L$_P_TRB71I_MSM_SUM_MSG_NAME
    .
 BZ-EXIT.     EXIT.
*----------------------------------------------------------------------
 C-READ-INPUT-FILE          SECTION.
*----------------------------------------------------------------------
 C-00.

    READ K_TRB71I_ADC 
       AT END  
	  SET L$_SW01_EOF_INP_FILE_YES  TO TRUE 
  
       NOT AT END
          ADD 1 TO L$_CT01_INP_REC_COUNT

          MOVE L$_WS01_REC_TYPE		    IN ICA_TRB71I_ACCUM_I
NTRF_REC
                                            IN K_TRB71I_ADC_REC
            TO L$_MH01_CURR_REC_TYPE

          MOVE L$_WS01_CUST_EXT_ID_TYP      IN K_TRB71I_ADC_REC
            TO L$_MH01_CURR_CUST_EXT_ID_TYP

          MOVE L$_WS01_CUST_PASSPORT_NUM    IN K_TRB71I_ADC_REC
            TO L$_MH01_CURR_CUST_PASSPORT

          IF L$_WS01_REC_TYPE_DATA	    IN ICA_TRB71I_ACCUM_INTRF_REC
                                            IN K_TRB71I_ADC_REC
          THEN
             PERFORM CA-GET-NUMERIC-F
IELDS-VALUES
          END-IF
    END-READ

    MOVE SP$_MSG_NORMAL  
      TO SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW
    .
 C-EXIT.     EXIT.
*----------------------------------------------------------------------
 CA-GET-NUMERIC-FIELDS-VALUES            SECTION.
*----------------------------------------------------------------------
 CA-00.

    MOVE L$_WS01_TRX_AMOUNT		IN K_TRB71I_ADC_REC
      TO L$_WS01_TRX_AMOUNT_9

    MOVE L$_WS01_TOTAL_OPEN_BALANCE	IN K_TRB71I_ADC_REC
      TO L$_WS01_TOTAL_OP
EN_BALANCE_9

    MOVE L$_WS01_TOTAL_BAD_DEBT		IN K_TRB71I_ADC_REC
      TO L$_WS01_TOTAL_BAD_DEBT_9
    .
 CA-EXIT.     EXIT.
*----------------------------------------------------------------------
 DA-START-CUST            SECTION.
*----------------------------------------------------------------------
 DA-00.

    SET L$_SW01_EOF_INP_FILE_NO
        L$_SW01_DATA_REC_NOT_OK	    TO TRUE

    MOVE L$_MH01_CURR_CUST
      TO L$_MH01_PREV_CUST

*   תיחכונה המושרה תרימש
    MOVE K_TRB71I_ADC_REC
      TO L$_WS
01_PREV_K_TRB71I_ADC_REC

    MOVE ZEROES
      TO L$_CT01_SUM_ADC_TRN

    MOVE ICA_TRB71I_SET_DBCR_STOP_INIT
      TO ICA_TRB71I_SET_DBCR_STOP_WKSP

    EVALUATE TRUE
       WHEN L$_WS01_REC_TYPE_DATA   IN ICA_TRB71I_ACCUM_INTRF_REC
                                    IN K_TRB71I_ADC_REC

          PERFORM DAA-CHECK-INP-REC-FIELDS

          IF L$_SW01_DATA_REC_OK
             COMPUTE L$_WS01_MIN_CUST_ADC_BALANCE = 
                     L$_WS01_TOTAL_OPEN_BALANCE_9 -
                     L$_WS01_TOTAL_BAD
_DEBT_9

             PERFORM DAB-GET-500-DATA
          END-IF

       WHEN L$_WS01_REC_TYPE_HDR    IN ICA_TRB71I_ACCUM_INTRF_REC
                                    IN K_TRB71I_ADC_REC
 
          MOVE L$_WS01_BATCH_SEQ    IN ICA_TRB71I_ACCUM_INTRF_HDR
                                    IN K_TRB71I_ADC_REC
            TO L$_WS01_ATR_BATCH_SEQ

       WHEN OTHER
          CONTINUE
    END-EVALUATE
    .
 DA-EXIT.     EXIT.
*----------------------------------------------------------------------
 DAA-CHECK-
INP-REC-FIELDS            SECTION.
*----------------------------------------------------------------------
 DAA-00.

    INSPECT L$_WS01_CUST_EXT_ID_TYP  IN K_TRB71I_ADC_REC
       REPLACING LEADING SPACES BY ZEROES

    INSPECT L$_WS01_CUST_INT_ID	     IN K_TRB71I_ADC_REC
       REPLACING LEADING SPACES BY ZEROES

    IF (L$_WS01_CUST_EXT_ID_TYP	 IN K_TRB71I_ADC_REC	IS NUMERIC)  AND
       (L$_WS01_CUST_INT_ID	 IN K_TRB71I_ADC_REC	IS NUMERIC)  AND
       (L$_WS01_TRX_AMOUNT_9				IS NUMERIC)  AND
       (L$
_WS01_TOTAL_OPEN_BALANCE_9			IS NUMERIC)  AND
       (L$_WS01_TOTAL_BAD_DEBT_9			IS NUMERIC)
    THEN 
       SET L$_SW01_DATA_REC_OK		    TO TRUE
    ELSE 
       SET L$_SW01_DATA_REC_NOT_OK	    TO TRUE

       SET L$_WS01_REC_STAT_DESC_INP	    TO TRUE

       PERFORM MA-WRITE-MSM-DATA-REC
    END-IF
    .
 DA-EXIT.     EXIT.
*----------------------------------------------------------------------
 DAB-GET-500-DATA            SECTION.
*----------------------------------------------------------------------
 
DAB-00.

    PERFORM X1-START-TRANS-READ-ONLY

    MOVE ICD_CST_500_CST_CATLOG_DBW_INIT
      TO ICD_CST_500_CST_CATLOG_DBW

*   ימינפ חוקל ההזמ הדשב ךרע ןיא םא
    IF L$_WS01_CUST_INT_ID  IN K_TRB71I_ADC_REC = ZEROES
       MOVE L$_WS01_CUST_EXT_ID_TYP     IN K_TRB71I_ADC_REC
         TO EXT_ID_NUM_TYPE_CODE	IN ICD_CST_500_CST_CATLOG_PRW
					IN ICD_CST_500_CST_CATLOG_DBW

       MOVE L$_WS01_CUST_EXT_ID		IN K_TRB71I_ADC_REC
         TO CUST_EXT_ID			IN ICD_CST_500_CST_CATLOG_PRW
					IN ICD_CST_500_CST_CA
TLOG_DBW

       MOVE V$_HEB_YES 
         TO MAIN_CUST_FLAG		IN ICD_CST_500_CST_CATLOG_PRW
					IN ICD_CST_500_CST_CATLOG_DBW

       MOVE 1 
         TO DP$_KEY_SEQUENCE		IN ICD_CST_500_CST_CATLOG_DBW
    ELSE
       MOVE L$_WS01_CUST_INT_ID		IN K_TRB71I_ADC_REC
         TO CUST_INT_ID			IN ICD_CST_500_CST_CATLOG_PRW
					IN ICD_CST_500_CST_CATLOG_DBW
    END-IF

       ADD DP$_SYM_INQUIRE
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION			IN ICD_CST_500_CST_CATLOG_DBW

    CALL 'ICD_CST_500_CST_CATLO
G_DBA'  USING  ICD_CST_500_CST_CATLOG_DBW

    EVALUATE DP$_STATUS	    IN ICD_CST_500_CST_CATLOG_DBW
       WHEN SP$_MSG_NORMAL
          SET L$_SW01_DATA_REC_OK	    TO TRUE

       WHEN SP$_MSG_NO_DATA_FOUND
          SET L$_SW01_DATA_REC_NOT_OK	    TO TRUE

          SET L$_WS01_REC_STAT_DESC_500	    TO TRUE

          PERFORM MA-WRITE-MSM-DATA-REC

       WHEN OTHER
          MOVE DP$_STATUS   IN ICD_CST_500_CST_CATLOG_DBW
            TO SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW

	  MOVE SP$_MSG_ERROR 

            TO SP$_EXIT_STATUS

          DISPLAY "Error in Sec:DAB-GET-500-DATA "
                  "Rtn:ICD_CST_500_CST_CATLOG_DBA"

          DISPLAY "For EXT_ID_NUM_TYPE_CODE: " 
                  EXT_ID_NUM_TYPE_CODE IN ICD_CST_500_CST_CATLOG_PRW
				       IN ICD_CST_500_CST_CATLOG_DBW  CONVERSION
          DISPLAY "For CUST_EXT_ID: " 
                  CUST_EXT_ID	       IN ICD_CST_500_CST_CATLOG_PRW
			               IN ICD_CST_500_CST_CATLOG_DBW  CONVERSION

          PERFORM X-ERRORS
    END-EVAL
UATE

    PERFORM X3-COMMIT 
    .
 DAB-EXIT.     EXIT.
*----------------------------------------------------------------------
 DB-HANDLE-CUST-DETAILS            SECTION.
*----------------------------------------------------------------------
 DB-00.

    DISPLAY "Proccess record #",L$_CT01_INP_REC_COUNT	WITH CONVERSION

*   תיחכונה המושרה תרימש
    MOVE K_TRB71I_ADC_REC
      TO L$_WS01_PREV_K_TRB71I_ADC_REC

    EVALUATE TRUE 
       WHEN L$_WS01_REC_TYPE_DATA   IN ICA_TRB71I_ACCUM_INTRF_REC
            
                        IN K_TRB71I_ADC_REC

*         דובא בוח לש העונת
          IF L$_WS01_BAD_DEBT_YES   IN ICA_TRB71I_ACCUM_INTRF_REC
                                    IN K_TRB71I_ADC_REC
          THEN
             DISPLAY "Bad Debt Movement, Ignore."
          ELSE
             PERFORM DBA-HANDLE-CUST-DATA
          END-IF

          PERFORM DBB-UPDATE-REC-COUNTERS

          IF L$_SW01_DATA_REC_OK
             MOVE SPACES   
               TO L$_WS01_REC_STAT_DESC   IN ICA_TRB71I_MSM_MGS_REC

    
         PERFORM MA-WRITE-MSM-DATA-REC
          END-IF

       WHEN L$_WS01_REC_TYPE_HDR    IN ICA_TRB71I_ACCUM_INTRF_REC
                                    IN K_TRB71I_ADC_REC

          ADD 1  TO L$_CT01_INP_HDR_REC

       WHEN L$_WS01_REC_TYPE_TRL    IN ICA_TRB71I_ACCUM_INTRF_REC
                                    IN K_TRB71I_ADC_REC

          ADD 1  TO L$_CT01_INP_TRL_REC

       WHEN OTHER
          CONTINUE
    END-EVALUATE

    IF L$_CT01_INP_REC_COUNT > RECS_NUM_CHKP	 IN ICD_TRT_600_TRF_MGNT
  
                                               IN ICD_TRT_600_TRF_MGNT_DBW
    THEN
       MOVE L$_CT01_INP_REC_COUNT 
         TO RECS_NUM_CHKP	IN ICA_SVM_FILEIN_UPDATE_WKSP

       PERFORM UA-UPDATE-FILEIN-STATUS
    END-IF
    .
 DB-EXIT.     EXIT.
*----------------------------------------------------------------------
 DBA-HANDLE-CUST-DATA		    SECTION.
*----------------------------------------------------------------------
 DBA-00.

*   הייבגב חוקלה לש תילמינימה בוחה תרתי
    IF L$_WS01_MIN_CUST_ADC_BA
LANCE > 
       L$_WS01_TOTAL_OPEN_BALANCE_9 -
       L$_WS01_TOTAL_BAD_DEBT_9	     
    THEN
       COMPUTE L$_WS01_MIN_CUST_ADC_BALANCE = 
               L$_WS01_TOTAL_OPEN_BALANCE_9 -
               L$_WS01_TOTAL_BAD_DEBT_9	     
    END-IF

*   הייבגב עציב חוקלהש תועונתה םוכיס
    ADD L$_WS01_TRX_AMOUNT_9
     TO L$_CT01_SUM_ADC_TRN
    .
 DBA-EXIT.     EXIT.
*----------------------------------------------------------------------
 DBB-UPDATE-REC-COUNTERS		    SECTION.
*----------------------------------
------------------------------------
 DBB-00.

*   הניקת םינותנ המושר
    IF L$_SW01_DATA_REC_OK
       ADD 1  TO L$_CT01_INP_DATA_REC_OK 

       ADD L$_WS01_TRX_AMOUNT_9
        TO L$_CT01_INP_REC_SUM

       IF L$_WS01_BAD_DEBT_YES   IN ICA_TRB71I_ACCUM_INTRF_REC
                                 IN K_TRB71I_ADC_REC
       THEN
          ADD 1  TO L$_CT01_INP_DATA_REC_BAD_DEBT 

          ADD L$_WS01_TRX_AMOUNT_9
           TO L$_CT01_INP_REC_SUM_BAD_DEBT
       END-IF
    ELSE
       ADD 1  TO L$_CT01_IN
P_DATA_REC_NOT_OK 

       ADD L$_WS01_TRX_AMOUNT_9
        TO L$_CT01_INP_REC_SUM_BAD
    END-IF
    .
 DBB-EXIT.     EXIT.
*----------------------------------------------------------------------
 DC-BREAK-ON-CUST		    SECTION.
*----------------------------------------------------------------------
 DC-00.

    IF NOT L$_SW01_EOF_INP_FILE_YES
       IF L$_MH01_CURR_CUST  NOT = L$_MH01_PREV_CUST
          SET L$_SW01_EOF_INP_FILE_CUST_YES  TO TRUE
       END-IF
    END-IF
 
*   ATR-ל המושר תביתכ
    IF (L$_
MH01_PREV_REC_TYPE_DATA)	    AND
       (NOT L$_SW01_EOF_INP_FILE_CUST_YES)
    THEN
       PERFORM DCA-WRITE-PREV-ATR-REC
    END-IF
    .
 DC-EXIT.     EXIT.
*----------------------------------------------------------------------
 DCA-WRITE-PREV-ATR-REC		    SECTION.
*----------------------------------------------------------------------
 DCA-00.

    MOVE ICA_TRB71I_WRITE_ATR_REC_INIT
      TO ICA_TRB71I_WRITE_ATR_REC_WKSP

    MOVE TRNF_ID		    IN ICD_TRT_600_TRF_MGNT_PRW
  				    IN ICD_TRT_600_TRF_MG
NT_DBW
      TO TRNF_ID		    IN ICA_TRB71I_WRITE_ATR_REC_INP

    MOVE TRNF_SRC_CODE		    IN ICD_TRT_600_TRF_MGNT
				    IN ICD_TRT_600_TRF_MGNT_DBW 
      TO TRNF_SRC_CODE		    IN ICA_TRB71I_WRITE_ATR_REC_INP

    MOVE BATCH_SEQ		    IN ICD_ATR_6699_ADC_IN_FIL
				    IN ICD_ATR_6699_ADC_IN_FIL_DBW
      TO BATCH_SEQ		    IN ICA_TRB71I_WRITE_ATR_REC_INP

    IF L$_SW01_DATA_REC_OK
       IF P$_COUNT_RECS	    IN ICA_875_PRD_STRC
			    IN ICA_TRB71I_SET_DBCR_STOP_OUT = ZEROES
       THEN
          SET L$_W
S01_CAL_STATUS_COMPLETED  IN L$_WS01_PREV_K_TRB71I_ADC_REC  
	   TO TRUE
       ELSE
          SET L$_WS01_CAL_STATUS_DEDUCTED   IN L$_WS01_PREV_K_TRB71I_ADC_REC  
	   TO TRUE

          MOVE TOTAL_TRN_AMT		    IN ICA_875_PRD_STRC
					    IN ICA_TRB71I_SET_DBCR_STOP_OUT
            TO L$_WS01_SUM_REC		    IN ICA_TRB71I_MSM_MGS_CUST_DEDUCT

          MOVE P$_COUNT_RECS		    IN ICA_875_PRD_STRC
					    IN ICA_TRB71I_SET_DBCR_STOP_OUT
            TO L$_WS01_NUM_OF_REC	    IN ICA_TRB71I_MSM_MGS_CUST_DEDUCT

 
         MOVE ICA_TRB71I_MSM_MGS_CUST_DEDUCT
            TO L$_WS01_CAL_ERROR_MSG	    IN L$_WS01_PREV_K_TRB71I_ADC_REC
       END-IF
    ELSE
       SET L$_WS01_CAL_STATUS_ERROR	 IN L$_WS01_PREV_K_TRB71I_ADC_REC  
        TO TRUE

       MOVE L$_WS01_REC_STAT_DESC
         TO L$_WS01_CAL_ERROR_MSG	 IN L$_WS01_PREV_K_TRB71I_ADC_REC
    END-IF

    MOVE ICD_CST_500_CST_CATLOG	    IN ICD_CST_500_CST_CATLOG_DBW
      TO ICD_CST_500_CST_CATLOG     IN ICA_TRB71I_WRITE_ATR_REC_INP

    CALL 'ICA_TRB71I_WRITE_ATR_R
EC'  USING  UTL_CONTROL_ACW
                                            ICA_TRB71I_WRITE_ATR_REC_WKSP
                                            L$_WS01_PREV_K_TRB71I_ADC_REC

    IF SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW = SP$_MSG_NORMAL
       ADD P$_COUNT_RECS	IN ICA_TRB71I_WRITE_ATR_REC_OUT
        TO L$_CT01_ATR_REC_COUNT
    ELSE
       DISPLAY "Error in Sec:DCA-WRITE-PREV-ATR-REC "
               "Rtn:ICA_TRB71I_WRITE_ATR_REC"

       DISPLAY "  STTSA: " 
               SP$_ACW_PROC_AUX_STATUS 
 OF UTL_CONTROL_ACW CONVERSION

       MOVE SP$_MSG_ERROR  
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 DCA-EXIT.     EXIT.
*----------------------------------------------------------------------
 DD-END-CUST		    SECTION.
*----------------------------------------------------------------------
 DD-00.

    EVALUATE TRUE
*      היבגב חוקלל בויח/יוכיז םייקו ח"וכ-ב חוקל םייק םא
       WHEN (L$_MH01_PREV_REC_TYPE_DATA)  AND
            (L$_SW01_DATA_REC_OK)	  AND
*           Start Cha
nge #0001
            (L$_CT01_SUM_ADC_TRN  NOT = ZERO)

          IF L$_CT01_SUM_ADC_TRN > ZEROES
*            היבגב חוקלה לש יחכונה בוחה תרתי
             MOVE L$_WS01_MIN_CUST_ADC_BALANCE
               TO L$_WS01_ORG_CUST_ADC_BAL
*            End Change #0001
          ELSE
*            היבגב חוקלה לש ירוקמה בוחה תרתי
             COMPUTE L$_WS01_ORG_CUST_ADC_BAL =
                     L$_WS01_MIN_CUST_ADC_BALANCE - L$_CT01_SUM_ADC_TRN
          END-IF

*         םירבוצה תורתי ןוכדע
          PERFORM DD
A-SET-DBCR-STOP-AMOUNTS

       WHEN OTHER
          CONTINUE
    END-EVALUATE

*   ATR-ל המושר תביתכ
    IF (L$_MH01_PREV_REC_TYPE_DATA)  
       PERFORM DCA-WRITE-PREV-ATR-REC
    END-IF
    .
 DD-EXIT.     EXIT.
*----------------------------------------------------------------------
 DDA-SET-DBCR-STOP-AMOUNTS		    SECTION.
*----------------------------------------------------------------------
 DDA-00.

    MOVE ICA_TRB71I_SET_DBCR_STOP_INIT
      TO ICA_TRB71I_SET_DBCR_STOP_WKSP

    MOVE TRNF_ID		IN IC
D_TRT_600_TRF_MGNT_PRW
				IN ICD_TRT_600_TRF_MGNT_DBW
      TO TRNF_ID		IN ICA_TRB71I_SET_DBCR_STOP_INP

    MOVE TRNF_SRC_CODE		IN ICD_TRT_600_TRF_MGNT
                                IN ICD_TRT_600_TRF_MGNT_DBW
      TO TRNF_SRC_CODE		IN ICA_TRB71I_SET_DBCR_STOP_INP

    MOVE SP$_DATE_TIME_BINARY   IN UTL_GET_DATE_TIME_WKSP 
      TO SP$_DATE_TIME_BINARY   IN ICA_TRB71I_SET_DBCR_STOP_INP

*   יבויח רפסמכ גצוימ היבגב חוקלה לש יוכיזה םוכס 
*   ילילש רפסמכ גצוימ היבגב חוקלה לש בויחה םוכס 
    COMPUTE TRN_CR
EDIT_AMOUNT   IN ICA_TRB71I_SET_DBCR_STOP_INP = 
            L$_CT01_SUM_ADC_TRN * (-1)

    MOVE L$_WS01_ORG_CUST_ADC_BAL
      TO BALANCE_AMT		IN ICA_TRB71I_SET_DBCR_STOP_INP

    MOVE ICD_CST_500_CST_CATLOG	IN ICD_CST_500_CST_CATLOG_DBW
      TO ICD_CST_500_CST_CATLOG	IN ICA_TRB71I_SET_DBCR_STOP_INP

    CALL 'ICA_TRB71I_SET_DBCR_STOP'  USING  UTL_USER_ACW
                                            UTL_CONTROL_ACW
                                            ICA_TRB71I_SET_DBCR_STOP_WKSP
					    
    EV
ALUATE SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW
       WHEN SP$_MSG_NORMAL
          ADD P$_COUNT_RECS     IN ICA_875_PRD_STRC
				IN ICA_TRB71I_SET_DBCR_STOP_OUT
	   TO L$_CT01_875_REC_COUNT

          ADD TOTAL_TRN_AMT	IN ICA_875_PRD_STRC
                                IN ICA_TRB71I_SET_DBCR_STOP_OUT
           TO L$_CT01_875_SUM_COUNT

       WHEN SP$_MSG_NO_DATA_FOUND
          DISPLAY "No data found by Rtn:ICA_TRB71I_SET_DBCR_STOP "
          DISPLAY "for CUST_INT_ID: "
                  CUST_INT_I
D	IN ICD_CST_500_CST_CATLOG
                                IN ICD_CST_500_CST_CATLOG_DBW	CONVERSION

          SET L$_SW01_DATA_REC_NOT_OK		TO TRUE

          SET L$_WS01_REC_STAT_DESC_755		TO TRUE

          PERFORM MA-WRITE-MSM-DATA-REC
            
       WHEN OTHER
          DISPLAY "Error in Sec:DDA-SET-DBCR-STOP-AMOUNTS "
                  "Rtn:ICA_TRB71I_SET_DBCR_STOP "

          DISPLAY "for CUST_INT_ID: "
                  CUST_INT_ID	IN ICD_CST_500_CST_CATLOG
                                IN I
CD_CST_500_CST_CATLOG_DBW	CONVERSION
          
	  DISPLAY "  STTSA: " 
                  SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW CONVERSION

	  MOVE SP$_MSG_ERROR  
            TO SP$_EXIT_STATUS

	  PERFORM X-ERRORS
    END-EVALUATE
    .
 DDA-EXIT.     EXIT.
*----------------------------------------------------------------------
 I-DUMMY-CALL		    SECTION.
*----------------------------------------------------------------------
 I-00.

    CALL 'ICA_TRB71I_EXIT_HANDLER'

    DISPLAY TX$_LEVEL         
  CONVERSION
    .
 I-EXIT.     EXIT.
*----------------------------------------------------------------------
 MA-WRITE-MSM-DATA-REC		    SECTION.
*----------------------------------------------------------------------
 MA-00.

*   ל"אודב העדוהה ץבוק תנכה
    IF L$_SW01_MSM_TITLE_NO     AND 
       L$_SW01_DATA_REC_NOT_OK 
    THEN
*      תרתוכ תרוש
       WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM ICA_TRB71I_MSM_MGS_HDR

       SET L$_SW01_MSM_TITLE_YES    TO TRUE
    END-IF

    PERFORM MAA-HANDLE-MSM-COMMON-DA
TA

*   םינותנ תרוש
    WRITE P_TRB71I_MSM_SEND_MSG_REC    FROM ICA_TRB71I_MSM_MGS_REC

    IF L$_SW01_DATA_REC_NOT_OK 
*      היוגש םינותנ תרוש
       WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM ICA_TRB71I_MSM_MGS_REC
    END-IF
    .
 MA-EXIT.     EXIT.
*----------------------------------------------------------------------
 MAA-HANDLE-MSM-COMMON-DATA		    SECTION.
*----------------------------------------------------------------------
 MAA-00.

    IF L$_SW01_DATA_REC_NOT_OK
       SET L$_WS01_REC_STAT_NOT_OK  
IN ICA_TRB71I_MSM_MGS_REC  TO TRUE
    ELSE
       SET L$_WS01_REC_STAT_OK	    IN ICA_TRB71I_MSM_MGS_REC  TO TRUE
    END-IF

    MOVE L_WS01_BPEL_INTRF_REC_ID	IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_RECORD_ID		IN ICA_TRB71I_MSM_MGS_REC

    MOVE L$_WS01_CUST_EXT_ID_TYP	IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_EXT_ID_NUM_TYPE_CODE	IN ICA_TRB71I_MSM_MGS_REC

    MOVE L$_WS01_CUST_PASSPORT_NUM	IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_CUST_EXT_ID		IN ICA_TRB71I_MSM_MGS_REC

    MOVE
 L$_WS01_TRX_AMOUNT		IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_MONEY_AMOUNT		IN ICA_TRB71I_MSM_MGS_REC

    MOVE L$_WS01_TRX_CLASS		IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_TRX_CLASS		IN ICA_TRB71I_MSM_MGS_REC

    MOVE L$_WS01_TRX_DATE_DD		IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_VALUE_DATE_DD		IN ICA_TRB71I_MSM_MGS_REC
    MOVE L$_WS01_TRX_DATE_MM		IN L$_WS01_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_VALUE_DATE_MM		IN ICA_TRB71I_MSM_MGS_REC
    MOVE L$_WS01_TRX_DATE_YYYY		IN L$_WS01
_PREV_K_TRB71I_ADC_REC
      TO L$_WS01_VALUE_DATE_YYYY	IN ICA_TRB71I_MSM_MGS_REC
    .
 MAA-EXIT.     EXIT.
*----------------------------------------------------------------------
 MB-WRITE-MSM-SUMMERY-REC            SECTION.
*----------------------------------------------------------------------
 MB-00.

    INITIALIZE P_TRB71I_MSM_SUM_MSG_REC

*   םיחוור תרוש
    WRITE P_TRB71I_MSM_SUM_MSG_REC

*   טלקה ץבוקב תמויסו החיתפ תורוש םוכיס
    COMPUTE L$_WS01_NUM_OF_REC  IN L$_WS01_SUM_HDR_REC_LINE = 
        
    L$_CT01_INP_HDR_REC + L$_CT01_INP_TRL_REC

    WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_SUM_HDR_REC_LINE
    DISPLAY L$_WS01_SUM_HDR_REC_LINE

*   תוניקת תומושר םוכיס
    IF L$_CT01_INP_DATA_REC_OK > ZERO
       MOVE L$_CT01_INP_REC_SUM
         TO L$_WS01_SUM_REC	    IN L$_WS01_SUM_REC_LINE

       MOVE L$_CT01_INP_DATA_REC_OK
         TO L$_WS01_NUM_OF_REC	    IN L$_WS01_SUM_REC_LINE

       SET L$_WS01_SUM_REC_TYPE_OK    
           L$_WS01_NUM_REC_TYPE_OK    TO TRUE

       WRITE P_TRB71I_MSM_SU
M_MSG_REC  FROM L$_WS01_SUM_REC_LINE
       DISPLAY L$_WS01_SUM_REC_LINE
    END-IF

*   תויוגש תומושר םוכיס
    IF L$_CT01_INP_DATA_REC_NOT_OK > ZERO
       SET L$_WS01_SUM_REC_TYPE_BAD    
           L$_WS01_NUM_REC_TYPE_BAD    TO TRUE

       MOVE L$_CT01_INP_REC_SUM_BAD
         TO L$_WS01_SUM_REC	    IN L$_WS01_SUM_REC_LINE

       MOVE L$_CT01_INP_DATA_REC_NOT_OK
         TO L$_WS01_NUM_OF_REC	    IN L$_WS01_SUM_REC_LINE

       WRITE P_TRB71I_MSM_SUM_MSG_REC	FROM L$_WS01_SUM_REC_LINE
       DISPLAY L
$_WS01_SUM_REC_LINE
    END-IF

*   דובא בוח תומושר םוכיס
    IF L$_CT01_INP_DATA_REC_BAD_DEBT > ZERO
       MOVE L$_CT01_INP_REC_SUM_BAD_DEBT
         TO L$_WS01_SUM_REC	    IN L$_WS01_SUM_REC_LINE

       MOVE L$_CT01_INP_DATA_REC_BAD_DEBT
         TO L$_WS01_NUM_OF_REC	    IN L$_WS01_SUM_REC_LINE

       SET L$_WS01_SUM_REC_TYPE_BAD_DEBT    
           L$_WS01_NUM_REC_TYPE_BAD_DEBT    TO TRUE

       WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_SUM_REC_LINE
       DISPLAY L$_WS01_SUM_REC_LINE
    END-IF

*   וזזוקש םירבוצ םוכיס
    MOVE L$_CT01_875_SUM_COUNT
      TO L$_WS01_SUM_REC	IN L$_WS01_SUM_ACCUM_LINE

    MOVE L$_CT01_875_REC_COUNT
      TO L$_WS01_NUM_OF_REC	IN L$_WS01_SUM_ACCUM_LINE

    WRITE P_TRB71I_MSM_SUM_MSG_REC	FROM L$_WS01_SUM_ACCUM_LINE
    DISPLAY L$_WS01_SUM_ACCUM_LINE

*   הצרה תביבס יטרפ
    EVALUATE TRUE
       WHEN L$_ICA$ENV = "PROD"
          SET L$_WS01_ENV_PROD	    TO TRUE

       WHEN L$_ICA$ENV = "TEST"
          SET L$_WS01_ENV_TEST	    TO TRUE

       WHEN OTHER
          S
ET L$_WS01_ENV_DEV	    TO TRUE
    END-EVALUATE

    WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_ENV_RUN_LINE
    DISPLAY L$_WS01_ENV_RUN_LINE

*   טלק ץבוק יטרפ
    MOVE L$_WS01_TRNF_ID    IN L$_K_TRB71I_ADC_NAME
      TO L$_WS01_TRNF_ID    IN L$_WS01_FILE_LINE

    WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_FILE_LINE
    DISPLAY L$_WS01_FILE_LINE

*   הווצא יטרפ
    MOVE L$_WS01_ATR_BATCH_SEQ
      TO L$_WS01_BATCH_ID    IN L$_WS01_BATCH_LINE

    WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_BATCH_LINE

    DISPLAY L$_WS01_BATCH_LINE

*   הציר יטרפ
    MOVE SYM$_OPER_ID
      TO L$_WS01_OPER_ID    IN L$_WS01_JOB_LINE

    WRITE P_TRB71I_MSM_SUM_MSG_REC  FROM L$_WS01_JOB_LINE
    DISPLAY L$_WS01_JOB_LINE
    .
 MB-EXIT.     EXIT.
*----------------------------------------------------------------------
 UA-UPDATE-FILEIN-STATUS            SECTION.
*----------------------------------------------------------------------
 UA-00.

    MOVE SP$_DATE_TIME_BINARY	    IN UTL_GET_DATE_TIME_WKSP
      TO TRNF_INPUTING_
TMSP	    IN ICA_SVM_FILEIN_UPDATE_WKSP

    CALL 'ICA_SVM_FILEIN_UPDATE' USING ICA_SVM_FILEIN_UPDATE_WKSP
       		                       UTL_CONTROL_ACW
                                GIVING SP$_ACW_PROC_AUX_STATUS

    IF SP$_ACW_PROC_AUX_STATUS  NOT = SP$_MSG_NORMAL
       DISPLAY "Error in Sec:UA-UPDATE-FILEIN-STATUS " 
               "Rtn:ICA_SVM_FILEIN_UPDATE"
       DISPLAY "STTS =" 
               SP$_ACW_PROC_AUX_STATUS	CONVERSION

       MOVE SP$_MSG_ERROR 
         TO SP$_EXIT_STATUS
    END-IF
    . 
 UA-EXIT.     EXIT.
*----------------------------------------------------------------------
 UJ-PROC-SEND-INTR-MAIL	    SECTION.
*----------------------------------------------------------------------
 UJ-00.

    MOVE UTL_SPAWN_PROCESS_WKSP_INIT 
      TO UTL_SPAWN_PROCESS_WKSP

    MOVE "@ICA_PROC:ICA_SEND_INTERNAL_MAIL " 
      TO SP$_COMMAND_LINE	    IN  UTL_SPAWN_PROCESS_WKSP(1:33)

    MOVE L$_WS01_MSM_FILE_NAME 
      TO SP$_COMMAND_LINE	    IN  UTL_SPAWN_PROCESS_WKSP(34:58)

    CALL "UTL_SPA
WN_PROCESS"  USING  UTL_SPAWN_PROCESS_WKSP

    IF SP$_SYSPRO_STATUS IN UTL_SPAWN_PROCESS_WKSP = SP$_MSG_NORMAL
    THEN
       DISPLAY "Procedure ICA_SEND_INTERNAL_MAIL ended successfuly."
    ELSE
       DISPLAY "Procedure ICA_SEND_INTERNAL_MAIL ended with errors!!!"
    END-IF
    .
 UJ-EXIT.    EXIT.
*----------------------------------------------------------------------
 UM-MSG-ENV-DETAILS		SECTION.
*----------------------------------------------------------------------
 UM-00.

*   הצרה תביבס יטרפ
   
 EVALUATE TRUE
       WHEN L$_ICA$ENV = "PROD"
          SET L$_WS01_ENV_PROD	    TO TRUE

       WHEN L$_ICA$ENV = "TEST"
          SET L$_WS01_ENV_TEST	    TO TRUE

       WHEN OTHER
          SET L$_WS01_ENV_DEV	    TO TRUE
    END-EVALUATE

    WRITE P_TRB71I_MSM_MSG_REC  FROM L$_WS01_ENV_RUN_LINE
    DISPLAY L$_WS01_ENV_RUN_LINE

*   טלק ץבוק יטרפ
    MOVE L$_WS01_TRNF_ID    IN L$_K_TRB71I_ADC_NAME
      TO L$_WS01_TRNF_ID    IN L$_WS01_FILE_LINE

    WRITE P_TRB71I_MSM_MSG_REC  FROM L$_WS01_FILE_LINE
    DISPLAY L$_WS01_FILE_LINE

*   הציר יטרפ
    MOVE SYM$_OPER_ID
      TO L$_WS01_OPER_ID    IN L$_WS01_JOB_LINE

    WRITE P_TRB71I_MSM_MSG_REC  FROM L$_WS01_JOB_LINE
    DISPLAY L$_WS01_JOB_LINE
    .
 UM-EXIT.     EXIT.
*----------------------------------------------------------------------
 US-CALL-UPDATE-SYMBOL-RTN		SECTION.
*----------------------------------------------------------------------
 US-00.

    CALL "UTL_SET_SYMBOL_VALUE"  USING  UTL_SET_SYMBOL_VALUE_WKSP

    IF SP$_SYSPRO_STATUS  OF U
TL_SET_SYMBOL_VALUE_WKSP IS FAILURE
       DISPLAY "Error in Sec:US-CALL-UPDATE-SYMBOL-RTN "
               "Rtn:UTL_SET_SYMBOL_VALUE "
       DISPLAY "Status:"
               SP$_SYSPRO_STATUS    IN UTL_SET_SYMBOL_VALUE_WKSP 
               
       DISPLAY "for SP$_SYMBOL:"
               SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP
       DISPLAY "for SP$_SYMBOL_VALUE:"
               SP$_SYMBOL_VALUE	    IN UTL_SET_SYMBOL_VALUE_WKSP

       MOVE SP$_SYSPRO_STATUS	    IN UTL_SET_SYMBOL_VALUE_WKSP 
        
 TO SP$_ACW_PROC_AUX_STATUS

       MOVE SP$_SYSPRO_STATUS_AUXIL IN UTL_SET_SYMBOL_VALUE_WKSP
         TO SP$_ACW_FORM_AUX_STATUS
            SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 US-EXIT.     EXIT.
*----------------------------------------------------------------------
 X1-START-TRANS-READ-ONLY			SECTION.
*----------------------------------------------------------------------
 X1-00.

    MOVE "BATCH" 
      TO DP$_WORK_CONTEXT	    IN ICD_COMMON_DBW

    MOVE DP$_SYM_TRAN_READ 
      T
O DP$_ACTION		    IN ICD_COMMON_DBW

    CALL 'ICD_COMMON_DBA'  USING  ICD_COMMON_DBW

    IF DP$_STATUS  IN ICD_COMMON_DBW  IS FAILURE
       MOVE DP$_STATUS		    IN ICD_COMMON_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG

       DISPLAY "Error in Sec:X1-START-TRANS-READ-ONLY "
               "RTN:ICD_COMMON_DBA" 
       DISPLAY " STTS--->"
                DP$_STATUS	    IN ICD_COMMON_DBW   CONVERSION

       MOVE DP$_STATUS_
AUXIL	    IN ICD_COMMON_DBW 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 X1-EXIT.     EXIT.
*----------------------------------------------------------------------
 X2-START-TRANS-READ-WRITE		SECTION.
*----------------------------------------------------------------------
 X2-00.

    MOVE "BATCH" 
      TO DP$_WORK_CONTEXT	    IN ICD_COMMON_DBW

    MOVE DP$_SYM_TRAN_READ_WRITE
      TO DP$_ACTION		    IN ICD_COMMON_DBW

    CALL 'ICD_COMMON_DBA'  USING  ICD_COMMON_DBW

    IF DP
$_STATUS  IN ICD_COMMON_DBW  IS FAILURE
       MOVE DP$_STATUS		    IN ICD_COMMON_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG

       DISPLAY "Error in Sec:X2-START-TRANS-READ-WRITE "
               "RTN:ICD_COMMON_DBA" 
       DISPLAY " STTS--->"
                DP$_STATUS	    IN ICD_COMMON_DBW   CONVERSION

       MOVE DP$_STATUS_AUXIL	    IN ICD_COMMON_DBW 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
  
  .
 X2-EXIT.     EXIT.
*----------------------------------------------------------------------
 X3-COMMIT                         SECTION.
*----------------------------------------------------------------------
 X3-00.

    IF TX$_LEVEL = ZERO
       GO TO X3-EXIT
    END-IF. 

    MOVE DP$_SYM_COMMIT 
      TO DP$_ACTION		    IN ICD_COMMON_DBW

    CALL 'ICD_COMMON_DBA'  USING  ICD_COMMON_DBW

    IF DP$_STATUS  IN ICD_COMMON_DBW  IS FAILURE
       MOVE DP$_STATUS		    IN ICD_COMMON_DBW 
         TO SP$_A
CW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG

       DISPLAY "Error in Sec:X3-COMMIT "
               "RTN:ICD_COMMON_DBA" 
       DISPLAY " STTS--->"
                DP$_STATUS	    IN ICD_COMMON_DBW   CONVERSION

       MOVE DP$_STATUS_AUXIL	    IN ICD_COMMON_DBW 
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 X3-EXIT.     EXIT.
*----------------------------------------------------------------------
 X4-ROLLBACK		    SECTIO
N.
*----------------------------------------------------------------------
 X4-00.

    IF TX$_LEVEL = ZERO
       GO TO X4-EXIT
    END-IF

    MOVE DP$_SYM_ROLLBACK 
      TO DP$_ACTION		    IN ICD_COMMON_DBW

    CALL 'ICD_COMMON_DBA'  USING  ICD_COMMON_DBW

    IF DP$_STATUS  IN ICD_COMMON_DBW  IS FAILURE
       MOVE DP$_STATUS		    IN ICD_COMMON_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG

       DISPLAY "Error in Sec:X4
-ROLLBACK "
               "RTN:ICD_COMMON_DBA" 
       DISPLAY " STTS--->"
                DP$_STATUS	    IN ICD_COMMON_DBW   CONVERSION

       MOVE DP$_STATUS_AUXIL	    IN ICD_COMMON_DBW 
         TO SP$_EXIT_STATUS
    END-IF
    .
 X4-EXIT.     EXIT.
*----------------------------------------------------------------------
 X-ERRORS	    SECTION.
*----------------------------------------------------------------------
 X-00.

    PERFORM X4-ROLLBACK

    DISPLAY "Attention: Program ICA_TRB71I_APPL.COB ende
d with errors!!!"
    PERFORM XA-SEND-FAIL-OUTLOOK

    PERFORM ZZ-EXIT-APPL
    .
 X-EXIT.     EXIT.
*----------------------------------------------------------------------
 XA-SEND-FAIL-OUTLOOK		SECTION.
*----------------------------------------------------------------------
 XA-00.

    SET L$_WS01_MSG_TYPE_PROC_ERR   TO TRUE

    OPEN OUTPUT P_TRB71I_MSM_MSG
    DISPLAY "Open Output file:" L$_P_TRB71I_MSM_MSG_NAME

    PERFORM UM-MSG-ENV-DETAILS

    CLOSE P_TRB71I_MSM_MSG
    DISPLAY "Close Output file
:" L$_P_TRB71I_MSM_MSG_NAME

    MOVE "ICA_DATA:ICA_TRB71I_C2.DAT"
      TO L$_WS01_MSM_FILE_NAME

    PERFORM UJ-PROC-SEND-INTR-MAIL
    .
 XA-EXIT.     EXIT.
*----------------------------------------------------------------------
 Z-FINISH	    SECTION.
*----------------------------------------------------------------------
 Z-00.

    PERFORM E-PROCCESSING-COMPLETE

    PERFORM ZZ-EXIT-APPL
    .
 Z-EXIT.     EXIT.
*----------------------------------------------------------------------
 E-PROCCESSING-COMP
LETE	    SECTION.
*----------------------------------------------------------------------
 E-00.

*   הטילקה בלשב אצמנ םא הקידב
    IF INPUT_FILE_STEP_CODE	    IN ICD_TRT_600_TRF_MGNT <= 1
       DISPLAY '<<< ץבוקה תטילק םויס - 1 בלש ליחתה >>>'

*      ATR ץבוקל םינותנ תמושר תביתכ
       PERFORM EA-WRITE-ATR-F-TRAILER

       PERFORM EB-CLOSE-FILES

       PERFORM EC-BUILD-OUTLOOK-MSG

       PERFORM EE-UPDATE-SYMBOLS

       DISPLAY L$_CT01_INP_HDR_REC         " :טלקה ץבוקב תרתוכ תומושר כהס"
       DISPLAY
 L$_CT01_INP_TRL_REC         " :טלקה ץבוקב תמויס תומושר כהס"
       DISPLAY L$_CT01_INP_DATA_REC_OK     " :טלקה ץבוקב תוניקת םינותנ תומושר כהס"
       DISPLAY L$_CT01_INP_DATA_REC_NOT_OK " :טלקה ץבוקב תויוגש םינותנ תומושר כהס"

       MOVE 2
         TO INPUT_FILE_STEP_CODE	    IN ICA_SVM_FILEIN_UPDATE_WKSP

       PERFORM UA-UPDATE-FILEIN-STATUS
    END-IF

    IF INPUT_FILE_STEP_CODE	    IN ICD_TRT_600_TRF_MGNT <= 2
       DISPLAY '<<< ATR םינותנ תואלבט ןוכדע - 2 בלש ליחתה >>>'

       PERFORM EH-UPDATE-T
ABLES

       MOVE 3
         TO INPUT_FILE_STEP_CODE	    IN ICA_SVM_FILEIN_UPDATE_WKSP

       PERFORM UA-UPDATE-FILEIN-STATUS
    END-IF

    IF INPUT_FILE_STEP_CODE	    IN ICD_TRT_600_TRF_MGNT <= 3
       DISPLAY '<<< RDB םינותנ תואלבט ןוכדע - 3 בלש ליחתה >>>'

       PERFORM EH-UPDATE-TABLES

       MOVE 4
         TO INPUT_FILE_STEP_CODE	    IN ICA_SVM_FILEIN_UPDATE_WKSP

       PERFORM UA-UPDATE-FILEIN-STATUS
    END-IF

    IF INPUT_FILE_STEP_CODE	    IN ICD_TRT_600_TRF_MGNT <= 4
       DISPLAY '<<< 
ל"אוד תועדוהו תוחוד - 4 בלש ליחתה >>>'

       SET L$_WS01_MSG_TYPE_SUM_FILES	TO TRUE

       MOVE L$_P_TRB71I_MSM_MSG_NAME
         TO L$_WS01_MSM_FILE_NAME

       PERFORM UJ-PROC-SEND-INTR-MAIL

*      המייתסה ץבוק תטילק - 99 בלש
       MOVE 99
         TO INPUT_FILE_STEP_CODE	    IN ICA_SVM_FILEIN_UPDATE_WKSP

       PERFORM UA-UPDATE-FILEIN-STATUS
    END-IF
    .
 E-EXIT.     EXIT.
*------------------------------------------------------------------------
 EA-WRITE-ATR-F-TRAILER	    SECTION.
*---------
--------------------------------------------------------------------
 EA-00.

*   הלעפה ההזמ
    MOVE SYM$_OPER_ID
      TO OPER_ID		    IN P_TRB71I_ATR_F_REC

*   סנכנ ץבוק ההזמ
    MOVE TRNF_ID		    IN ICD_TRT_600_TRF_MGNT_PRW
  				    IN ICD_TRT_600_TRF_MGNT_DBW
      TO TRNF_ID		    IN P_TRB71I_ATR_F_REC

*   ץבוקה רוקמ
    MOVE TRNF_SRC_CODE		    IN ICD_TRT_600_TRF_MGNT
  				    IN ICD_TRT_600_TRF_MGNT_DBW
      TO TRNF_SRC_CODE		    IN P_TRB71I_ATR_F_REC

*   ץבוקה םש
    MOVE L$_K_TRB71I_ADC_NAME
 
     TO FILE_NAME		    IN P_TRB71I_ATR_F_REC

*   הציר רוטרמונ
    MOVE L$_WS01_ATR_BATCH_SEQ
      TO BATCH_SEQ		    IN P_TRB71I_ATR_F_REC

*   ץבוקב םינותנ תומושר תומכ
    COMPUTE IO_IN_TRN_IN_FILE	    IN P_TRB71I_ATR_F_REC =
            L$_CT01_INP_DATA_REC_OK +
            L$_CT01_INP_DATA_REC_NOT_OK

    MOVE L$_CT01_INP_DATA_REC_OK
      TO IO_OUT_TRN_COMPLETED	    IN P_TRB71I_ATR_F_REC

    MOVE L$_CT01_INP_DATA_REC_NOT_OK
      TO IO_OUT_TRN_BAD_INCOMPLETE  IN P_TRB71I_ATR_F_REC

*   ץבוקה תריצי ךיר
את
    MOVE L$_SYSTEM_DATE_START
      TO CREATE_DATE		    IN P_TRB71I_ATR_F_REC
*         RECEIVE_DATE		    IN P_TRB71I_ATR_F_REC

*    MOVE ZEROES
*      TO SENDING_DATE		    IN P_TRB71I_ATR_F_REC
         
*   ח"וכ-ב ץבוקה לש דוביע סוטטס
    IF L$_CT01_INP_DATA_REC_NOT_OK > ZEROES
       MOVE 99
         TO CTRL_STATUS_CODE	    IN P_TRB71I_ATR_F_REC

       STRING
          'File loaded with '		DELIMITED BY SIZE
          L$_CT01_INP_DATA_REC_NOT_OK   DELIMITED BY SIZE
          ' error records.'		DELIMI
TED BY SIZE
          INTO ERROR_DESCRIPTION    IN P_TRB71I_ATR_F_REC
       END-STRING
    ELSE
       MOVE 20
         TO CTRL_STATUS_CODE	    IN P_TRB71I_ATR_F_REC

       MOVE SPACES
         TO ERROR_DESCRIPTION	    IN P_TRB71I_ATR_F_REC
    END-IF


    WRITE P_TRB71I_ATR_F_REC
    .
 EA-EXIT.     EXIT.
*----------------------------------------------------------------------
 EB-CLOSE-FILES	    SECTION.
*----------------------------------------------------------------------
 EB-00.

    CLOSE K_TRB71I_
ADC
    DISPLAY "Close Input File: " L$_K_TRB71I_ADC_NAME

    CLOSE P_TRB71I_875
    DISPLAY "Close Output File: " L$_P_TRB71I_875_NAME

    CLOSE P_TRB71I_ATR_R
    DISPLAY "Close Output File: " L$_P_TRB71I_ATR_R_NAME

    CLOSE P_TRB71I_ATR_F
    DISPLAY "Close Output File: " L$_P_TRB71I_ATR_F_NAME
    .
 EB-EXIT.     EXIT.
*----------------------------------------------------------------------
 EC-BUILD-OUTLOOK-MSG	    SECTION.
*----------------------------------------------------------------------
 EC-
00.

*   OutLook תעדוה תנכה   
    PERFORM MB-WRITE-MSM-SUMMERY-REC

    PERFORM ECA-SET-MSM-SUBJECT

    PERFORM ECB-SET-MSM-FILE-NAMES

    PERFORM ECC-CLOSE-MAIL-FILES
    .
 EC-EXIT.     EXIT.
*----------------------------------------------------------------------
 ECA-SET-MSM-SUBJECT	    SECTION.
*----------------------------------------------------------------------
 ECA-00.

    SET L$_WS01_MSG_TYPE_SUM_SUB	TO TRUE

    OPEN I-O P_TRB71I_MSM_MSG
    DISPLAY "Open Input-Output file:" L$_P_TRB71I_MSM_M
SG_NAME

*   תויוגש תומושר תומייק םא הקידב
    IF L$_CT01_INP_DATA_REC_NOT_OK > ZERO
       SET L$_WS01_SUB_PROC_STATUS_FAILED   TO TRUE

       IF L$_CT01_INP_DATA_REC_OK > ZERO
          SET L$_WS01_SUB_PROC_STATUS_P_FAIL
              L$_WS01_SUB_COMMENT_ON		TO TRUE

          MOVE L$_CT01_INP_DATA_REC_OK
            TO L$_WS01_NUM_OF_REC	IN ICA_TRB71I_MSM_MGS_SUB
       ELSE
          MOVE SPACES 
            TO L$_WS01_SUB_COMMENT	IN ICA_TRB71I_MSM_MGS_SUB

          MOVE ZEROES
            TO L$_WS01_
NUM_OF_REC	IN ICA_TRB71I_MSM_MGS_SUB
       END-IF
    ELSE
       SET L$_WS01_SUB_PROC_STATUS_OK	    
           L$_WS01_SUB_COMMENT_ON	    TO TRUE

       COMPUTE L$_WS01_NUM_OF_REC	IN ICA_TRB71I_MSM_MGS_SUB = 
               L$_CT01_INP_DATA_REC_OK + 
               L$_CT01_INP_HDR_REC     + L$_CT01_INP_TRL_REC
    END-IF

    READ P_TRB71I_MSM_MSG

    REWRITE P_TRB71I_MSM_MSG_REC  FROM ICA_TRB71I_MSM_MGS_SUB

    DISPLAY "Outlook msg subject: " ICA_TRB71I_MSM_MGS_SUB

    CLOSE P_TRB71I_MSM_MSG
    DIS
PLAY "Close Output file:" L$_P_TRB71I_MSM_MSG_NAME
    .
 ECA-EXIT.     EXIT.
*----------------------------------------------------------------------
 ECB-SET-MSM-FILE-NAMES	    SECTION.
*----------------------------------------------------------------------
 ECB-00.

    SET L$_WS01_MSG_TYPE_SUM_FILES	TO TRUE

    OPEN I-O P_TRB71I_MSM_MSG
    DISPLAY "Open Input-Output file:" L$_P_TRB71I_MSM_MSG_NAME

    READ P_TRB71I_MSM_MSG

    SET L$_WS01_MSG_TYPE_SUM_SUB	TO TRUE

    STRING
       L$_P_TRB71I_MSM_SU
M_MSG_NAME	DELIMITED BY SIZE
       ' '				DELIMITED BY SIZE
       L$_P_TRB71I_MSM_MSG_NAME		DELIMITED BY SIZE
       INTO  P_TRB71I_MSM_MSG_REC
    END-STRING

    SET L$_WS01_MSG_TYPE_SUM_FILES	TO TRUE

    REWRITE P_TRB71I_MSM_MSG_REC

    CLOSE P_TRB71I_MSM_MSG
    DISPLAY "Close Output file:" L$_P_TRB71I_MSM_MSG_NAME
    .
 ECB-EXIT.     EXIT.
*----------------------------------------------------------------------
 ECC-CLOSE-MAIL-FILES	    SECTION.
*----------------------------------------------------
------------------
 ECC-00.

    CLOSE P_TRB71I_MSM_SEND_MSG
    DISPLAY "Close Output file: " L$_P_TRB71I_MSM_SEND_MSG_NAME

    CLOSE P_TRB71I_MSM_SUM_MSG
    DISPLAY "Close Output file: " L$_P_TRB71I_MSM_SUM_MSG_NAME
    .
 ECC-EXIT.     EXIT.
*----------------------------------------------------------------------
 EE-UPDATE-SYMBOLS		SECTION.
*----------------------------------------------------------------------
 EE-00.

    PERFORM X2-START-TRANS-READ-WRITE

*   Set file name symbol. (file to print).
 
   MOVE "SYM$_FILE_NAME_1" 
      TO SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE L$_FILE_NAME 
      TO SP$_SYMBOL_VALUE   IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set copy number symbol.
    MOVE "SYM$_COPY_NUM_1" 
      TO SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE COPY_NUM	    IN ICA_SVF_HANDLE_OPER_WKSP 
      TO SP$_SYMBOL_VALUE   IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set queue name symbol.
    MOVE "SYM$_PRINT_QUEU
E_1" 
      TO SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE PRINT_QUEUE_NAME_1 IN ICA_SVF_HANDLE_OPER_WKSP 
      TO SP$_SYMBOL_VALUE   IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Inverse REMARK field.
    MOVE 100 
      TO P$_LENGTH	    IN ICA_INVERSE_STRING_WKSP

    MOVE REMARK		    IN ICA_SVF_HANDLE_OPER_WKSP
      TO P$_STRING IN ICA_INVERSE_STRING_WKSP

    CALL 'ICA_ICF_INVERSE_STRING'  USING  ICA_INVERSE_STRING_WKSP

    MOVE P$_STRING	    IN ICA_INVERSE_STRING
_WKSP 
      TO REMARK		    IN ICA_SVF_HANDLE_OPER_WKSP

*   Set print note symbol.
    MOVE "SYM$_PRINT_NOTE_1" 
      TO SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE REMARK		    IN ICA_SVF_HANDLE_OPER_WKSP 
      TO SP$_SYMBOL_VALUE   IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set save last oper param symbol.
    MOVE "SYM$_SAVE_LAST_OPER_PARAM" 
      TO SP$_SYMBOL	    IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE SAVE_LAST_OPER_PARM	IN ICD_SVT_030_ENTITY 
      TO SP$_SYM
BOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set print form name symbol.
    MOVE "SYM$_PRINT_FORM_NAME_1" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    IF FORM_NAME    IN ICD_SVT_035_OPER_PRM = SPACES
       MOVE "DEFAULT" 
         TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP
    ELSE
       MOVE FORM_NAME		IN ICD_SVT_035_OPER_PRM
         TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP
    END-IF

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set delete file after print symbol.
    MOVE "SYM$_DEL_FILE_AFTER_PRINT_1" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE DELETE_AFTER_PRINT	IN ICD_SVT_035_OPER_PRM
      TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set send message symbol.
    MOVE "SYM$_SEND_MSG" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE S
END_TERMINATE_MSG	IN ICD_SVT_035_OPER_PRM
      TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set page header symbol.
    MOVE "SYM$_PAGE_HEADER" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE PAGE_HEADER		IN ICD_SVT_035_OPER_PRM
      TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMB
OL-RTN

*   Set entity type symbol.
    MOVE "SYM$_ENTITY_TYPE" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE ENTITY_TYPE		IN ICD_SVT_030_ENTITY
      TO SP$_SYMBOL_VALUE	IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

*   Set output device symbol.
    MOVE "SYM$_OUTPUT_DEVICE" 
      TO SP$_SYMBOL		IN UTL_SET_SYMBOL_VALUE_WKSP

    MOVE OUTPUT_DEVICE		IN ICD_SVT_035_OPER_PRM
      TO SP$_SYMBOL_VALUE	IN U
TL_SET_SYMBOL_VALUE_WKSP

    MOVE 1 
      TO SP$_SYMBOL_LEN		IN UTL_SET_SYMBOL_VALUE_WKSP

    PERFORM US-CALL-UPDATE-SYMBOL-RTN

    PERFORM X3-COMMIT
    .
 EE-EXIT.     EXIT.
*----------------------------------------------------------------------
 EH-UPDATE-TABLES	    SECTION.
*----------------------------------------------------------------------
 EH-00.

    MOVE ICA_TRB71I_UPD_TABLES_WKSP_INIT
      TO ICA_TRB71I_UPD_TABLES_WKSP

    MOVE L$_WS01_TRNF_ID        IN L$_K_TRB71I_ADC_NAME
      TO TRNF_
ID		IN ICA_TRB71I_UPD_TABLES_INP

    MOVE INPUT_FILE_STEP_CODE	IN ICA_SVM_FILEIN_UPDATE_WKSP
      TO INPUT_FILE_STEP_CODE	IN ICA_TRB71I_UPD_TABLES_INP

    MOVE SP$_DATE_TIME_BINARY	IN UTL_GET_DATE_TIME_WKSP
      TO SP$_DATE_TIME_BINARY	IN ICA_TRB71I_UPD_TABLES_INP

    CALL 'ICA_TRB71I_UPD_TABLES' USING  UTL_CONTROL_ACW
 					ICA_TRB71I_UPD_TABLES_WKSP

    IF SP$_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW  NOT = SP$_MSG_NORMAL
       DISPLAY "Error in Sec:EH-UPDATE-TABLES "
               "RTN:ICA_TRB71I_U
PD_TABLES "

       DISPLAY "  STTSA: " 
               SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW CONVERSION

       MOVE SP$_MSG_ERROR  
         TO SP$_EXIT_STATUS

       PERFORM X-ERRORS
    END-IF
    .
 EH-EXIT.     EXIT.
*----------------------------------------------------------------------
 ZZ-EXIT-APPL	    SECTION.
*----------------------------------------------------------------------
 ZZ-00.

*   (לשכנ/החלצהב םייתסה) הטילקה ךילהתב ץבוק סוטטס ןוכדע:
    PERFORM ZZA-UPDATE-INPUTING-STATUS


    
CALL 'ICD_EXIT_TX' GIVING L$_STATUS

    IF L$_STATUS IS FAILURE
       MOVE L$_STATUS 
         TO SP$_ACW_PROC_AUX_STATUS
	    SP$_EXIT_STATUS
    END-IF

*   Move status to l$_exit_status for checking in exit handler routine.
    MOVE SP$_ACW_PROC_AUX_STATUS 
      TO L$_EXIT_STATUS

    IF SP$_ACW_PROC_AUX_STATUS IS FAILURE
       CALL 'ICA_ICF_GENERAL_EXCEPTION' USING UTL_CONTROL_ACW
					      UTL_USER_ACW
					      ICA_TASK_STANDARD_WKSP
    END-IF

    PERFORM ZZC-DISCONNECT-DB

*   disconnect from
 dmq queues.
    IF L$_CONN_DMQ
       CALL 'ICA_DMQ_EXIT' USING ICA_DMQ_EXIT_WKSP
       IF SP$_SYSPRO_STATUS OF ICA_DMQ_EXIT_WKSP IS NOT SUCCESS
          MOVE SP$_SYSPRO_STATUS_AUXIL IN ICA_DMQ_EXIT_WKSP TO SP$_EXIT_STATUS
       END-IF
       MOVE 0 TO L$_DMQ_CONN_STATUS
    END-IF.

    DISPLAY "****************************************************"
    DISPLAY "******** End of Program: ICA_TRB71I_APPL.COB *******"
    DISPLAY "****************************************************"



*# 12-NOV-2012 14:1
5:00.57 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_APPL.INC".

    CALL 'UTL_EXIT_ROUTINE' USING BY REFERENCE UTL_EXIT_ROUTINE_WKSP
    .
 ZZ-EXIT.     EXIT.
*----------------------------------------------------------------------
 ZZA-UPDATE-INPUTING-STATUS		SECTION.
*----------------------------------------------------------------------
 ZZA-00.
   
    IF (SP$_ACW_PROC_AUX_STATUS  OF UTL_CONTROL_ACW  = SP$_MSG_NORMAL)  AND
       (SP$_EXIT_STATUS_SUCCESS)       
    THEN
       MOVE V20
02$INPUTING_COMPLETED 
         TO TRNF_INPUTING_STS_CODE   IN ICA_SVM_FILEIN_UPDATE_WKSP
    ELSE
       MOVE V2002$CONTROL_PROBLEM 
	 TO TRNF_INPUTING_STS_CODE   IN ICA_SVM_FILEIN_UPDATE_WKSP
    END-IF

    MOVE L$_CT01_INP_REC_COUNT 
      TO RECS_NUM_CHKP		     IN ICA_SVM_FILEIN_UPDATE_WKSP

    PERFORM UA-UPDATE-FILEIN-STATUS

    IF TRNF_INPUTING_STS_CODE     IN ICA_SVM_FILEIN_UPDATE_WKSP 
                                   = V2002$CONTROL_PROBLEM
    THEN
       MOVE SP$_MSG_ERROR 
         TO SP$_A
CW_PROC_AUX_STATUS
            SP$_EXIT_STATUS
    END-IF   
    .
 ZZA-EXIT.     EXIT.
*----------------------------------------------------------------------
 ZZC-DISCONNECT-DB		SECTION.
*----------------------------------------------------------------------
 ZZC-00.

    MOVE DP$_SYM_DISCONNECT_ALL 
      TO DP$_ACTION OF ICD_CONNECT_DBW
	 
    CALL 'ICD_CONNECT_DBA'  USING  ICD_CONNECT_DBW

    IF DP$_STATUS  IN UTL_DBA_UNIV_WKSP  IN ICD_CONNECT_DBW  IS NOT SUCCESS
       MOVE DP$_STATUS		    IN UTL_DBA
_UNIV_WKSP 
				    IN ICD_CONNECT_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 

       MOVE DP$_STATUS_AUXIL_TEXT   IN UTL_DBA_UNIV_WKSP 
                                    IN ICD_CONNECT_DBW 
	 TO SP$_ACW_FORM_MSG

       MOVE "Error in Sec:ZZC-DISCONNECT-DB Rtn:ICD_CONNECT_DBA PAR:DISCONNECT" 
         TO SP$_ACW_FREE_TEXT

       MOVE DP$_STATUS_AUXIL	    IN UTL_DBA_UNIV_WKSP 
				    IN ICD_CONNECT_DBW 
         TO SP$_EXIT_STATUS
    END-IF

    MOVE ZEROES 
      TO L$_DB_CONN_STATUS
    .
 ZZC-EXIT.  
   EXIT.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
