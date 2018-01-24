 *######### PROGRAM_NAME: ICA_TRB711_EDIT_BENLEUMI.COB  #############*
*#                                                                 #*           
*#                                היצרגטניא - תועונת TR : תכרעמ תת #*           
*#                             ICA_TRB711_EDIT_BENLEUMI :לודומה םש #*
*#                                                                 #*           
*#                                                :ילנויצקנופ רואת #*
*#                                                                 #*           
*#                               היצרגטניאל RMS ץבוק תארוק תינכותה #*           
*#                     .ימואלניבה קנבה תשירד יפל תועונתה תא תכרועו #*           
*#                      --------------                             #*           
*#                                                                 #*           
*#                                                         :םיחתפמ #*           
*#         ןורחא ןוכדע .ת        ךיראת       עצבמ םש       בלש     #*           
*#                            11-SEP-2003    TP_ELEVY      בוציע   #*           
*#                            11-SEP-2003    TP_ELEVY      תונכת   #*
*#                                                                 #*           
*#                                                        :םירטמרפ #*           
*#                                         [    ] :עדימ תינבת     #*           
*#         (שומיש ,םיכרע ,רבסה) רואת   פ/ק           הדש םש        #*           
*#                            [    ]  [  ]           [    ]        #*           
*#                                                                 #*           
*#                                                     :עדימ ירגאמ #*           
*#               תירבעב םש / רואת    פ/ק             הלבט/ץבוק     #*           
*#                         [    ]  [     ]           [       ]     #*
*#                                                                 #*           
*#                                                  :םישגדהו תורעה #*           
*#                                                      [    ]     #*           
*#                                                                 #*           
*#                                          :םייונישו םינוכדע בקעמ #*           
*#               יונישה רואת     עצבמ םש       ךיראת       CID     #*           
*# -------------------------   -------------    -----------   ---- #*
*#                 יטרפ גתומ        TP_ELEVY     07-09-2004   1001 #*
*# -------------------------   -------------    -----------   ---- #*
*#     תיעבטמ בר הקילס תפסוה         יול ןרע    14-SEP-2005   1009 #*
*# -------------------------   -------------    -----------   ---- #*
*#     תיעבטמ בר הקילס תפסוה         יול ןרע    06-AUG-2006   1023 #*
*# -------------------------   -------------    -----------   ---- #*
*#              U-BANK תפסוה     ןוקוא ןוריל    05-OCT-2008   2000 #*
*# -------------------------   -------------    -----------   ---- #*
*#   ץבוקב ןוזיא תקידב תפסוה	י'גרפ תינליא    28-MAR-2012   2001 #*
*#             ךילהתה תלשכהו					   #*
*# -------------------------   -------------    -----------   ---- #*
*#    םיצבק לש םושיר עצבל אל       ןסינ ןרוא    25-Dec-2012   2002 #*
*# םיאצוי םיצבק תכרעמל םיקיר                                       #*
*###################################################################*           

*----------------------------------------------------*
IDENTIFICATION	 	 	 	     DIVISION.
*----------------------------------------------------*
PROGRAM-ID. ICA_TRB711_EDIT_BENLEUMI.
AUTHOR. TP_ELEVY.
*----------------------------------------------------*
ENVIRONMENT	 	 	 	     DIVISION.                          
*----------------------------------------------------*
*----------------------------------------------------*
INPUT-OUTPUT                                 SECTION.
*----------------------------------------------------*
FILE-CONTROL.
*-----------*

    SELECT ICA_INTGR_INPUT_BEFORE ASSIGN TO "SYS$DISK"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS SP$_COBRMS_FILE_STATUS.

    SELECT ICA_INTGR_INPUT_FILE_3 ASSIGN TO "SYS$DISK"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS SP$_COBRMS_FILE_STATUS.

    SELECT ICA_BANK_BENLEUMI_FILE ASSIGN TO "SYS$DISK"
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS SP$_COBRMS_FILE_STATUS.

    SELECT  SORT_FILE
	ASSIGN	TO  "SRTFIL".

* Change #1023 - Start.
    SELECT ICA_MSG_OUTPUT_FILE ASSIGN TO ICA_MSG_OUTPUT_FILE
	   ORGANIZATION IS SEQUENTIAL
	   ACCESS IS SEQUENTIAL
	   FILE STATUS IS SP$_COBRMS_FILE_STATUS.
* Change #1023 - End.
***************************************
I-O-CONTROL.

    APPLY	EXTENSION   1000
	PREALLOCATION 2000  ON	ICA_INTGR_INPUT_FILE_3

    APPLY
    	EXTENSION 1000
    	CONTIGUOUS-BEST-TRY PREALLOCATION 2000
    	ON ICA_BANK_BENLEUMI_FILE.

**************************************
*----------------------------------------------------*
DATA	 	 	 	 	     DIVISION.                          
*----------------------------------------------------*
FILE SECTION.
*------------

FD ICA_INTGR_INPUT_BEFORE
   VALUE OF ID IS L$_INTGR_BEFORE_NAME.
   COPY "ICA_CDD_WKSP:ICA_TRB711_OUTPUT_670"    FROM DICTIONARY
      REPLACING ==ICA_TRB711_OUTPUT_670== BY ==ICA_INTGR_BEFORE_REC==. 
                                                                                
FD ICA_INTGR_INPUT_FILE_3
   VALUE OF ID IS L$_INTGR_INPUT_NAME.
   COPY "ICA_CDD_WKSP:ICA_TRB711_OUTPUT_670"    FROM DICTIONARY. 
                                                                                
FD ICA_BANK_BENLEUMI_FILE
   VALUE OF ID IS L$_BANK_BENLEUMI_NAME.
01 BENLEUMI_REC  PIC X(220).

SD  SORT_FILE.
   01  SORT_REC.
    COPY "ICA_CDD_WKSP:ICA_TRB711_OUTPUT_670"    FROM DICTIONARY
        REPLACING ==ICA_TRB711_OUTPUT_670== BY ==COPY_REC==
                  ==01==                     BY ==02==
                  ==02==                     BY ==03==
                  ==03==                     BY ==04==.
    COPY "ICD_CDD_FIELD:VALUE_DATE"               FROM DICTIONARY
        REPLACING ==VALUE_DATE==  BY  ==NEW_VALUE_DATE==
                  ==01==          BY  ==02==.

* Change #1023 - Start.
FD ICA_MSG_OUTPUT_FILE
    VALUE OF ID IS L$_MSG_FILE_NAME.
    COPY    "ICA_CDD_WKSP:ICA_TRB711_MESSAGE_WKSP"          FROM DICTIONARY
      REPLACING ==ICA_TRB711_MESSAGE_WKSP== 
             BY ==ICA_MSG_RMS==. 
* Change #1023 - End.
*----------------------------------------------------*
WORKING-STORAGE 	 	 	     SECTION.                           
*# 13-NOV-2012 18:29:22.15 - TP_TSARSU - include the TLG wksp.
COPY "ICA_CDD_WKSP:ICA_TLG_WKSP"             FROM DICTIONARY.

*----------------------------------------------------*

* VAL COPIES *
**************
COPY "ICA_CDD_WKSP:ICA_PRT_346_EXT_ID_N_TP_VAL"	       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_1007_VAL"       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_PRT_300_PROD_TYPES_VAL"	       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_PRT_327_BANKS_VAL"	       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TRT_669_FIN_STTLMNT_VAL"        FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TRT_674_CURNCY_TYP_VAL"	       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVT_072_FILEOUT_DEF_VAL"        FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVM_FILEOUT_INSERT_WKSP"        FROM DICTIONARY.

* GENERAL COPIES *
******************
COPY "ICA_CDD_WKSP:ICA_RMS_MSG_WKSP"                   FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_XLATE_MSG_WKSP"                 FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_INVERSE_STRING_WKSP"            FROM DICTIONARY.
* Change #1009 - Start.
COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".
COPY "ICD_CDD_WKSP:ICD_PRT_316_ACC_TYPES_DBW"          FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_CONSTANT_VALUES"                FROM DICTIONARY.
* Change #1009 - End.
* Change #1023 - Start.
COPY "ICD_CDD_WKSP:ICD_TRT_674_CURNCY_TYP_MPW"   FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_TRT_674_CURNCY_TYP_OPW"   FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_TRT_674_CURNCY_TYP_OPW"    FROM DICTIONARY
     REPLACING ICD_TRT_674_CURNCY_TYP_OPW
            BY ICD_TRT_674_CURNCY_TYP_INIT.
COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_2892_VAL" FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TRB711_MESSAGE_WKSP"      FROM DICTIONARY
       REPLACING ==ICA_TRB711_MESSAGE_WKSP==
  	      BY ==INIT_TRB711_MESSAGE_WKSP==.
* Change #1023 - End.

* UTL COPIES.                                     
***************
COPY "UTL_CDD_WKSP:UTL_COBRMS_VALUE_WKSP"              FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_EXIT_ROUTINE_WKSP"              FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_BINARY_WKSP"      FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_GET_DATEYYYY_TIME_WKSP"	       FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DELTA_TIME_WKSP"            FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_APPLY_DELTA_TIME_WKSP"          FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_WKSP"         FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_TIME_WKSP"             FROM DICTIONARY.
*COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_WKSP"	       FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVF_APTUSER_CALL_WKSP"	       FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_DIFFER_DATES_WKSP"              FROM DICTIONARY.
COPY "UTL_SOURCE:UTL_MESSAGE.INC".

* FD COPIES *
*************
COPY "ICA_CDD_WKSP:ICA_TRB711_BNL_HEADER_RECORD"       FROM DICTIONARY. 
COPY "ICA_CDD_WKSP:ICA_TRB711_BNL_TRN_RECORD"          FROM DICTIONARY. 
COPY "ICA_CDD_WKSP:ICA_TRB711_BNL_TRAILER_RECORD"      FROM DICTIONARY. 

*#----- GENERAL FIELDS -----#*
01 L$_RECORD_COUNTER
   PIC S9(7) COMP EXTERNAL.

01 L$_PROCESS_TYPE		    PIC 9 VALUE 0. 
   88 L$_SW_VISA		    VALUE 0.
   88 L$_SW_DELEK		    VALUE 1.

01 L$_MSG_TEXT                  PIC X(28) VALUE 
                                ' XXXXXX תינכותב - XXXX ךלהמב'.

01 L$_INTGR_BEFORE_NAME		PIC X(70) VALUE SPACES.

01 L$_INTGR_INPUT_NAME          PIC X(37) VALUE 
                                 "ICA_IN_DAT_DIR:ICA_TRB711_BNL_SRT.DAT".

01 L$_BANK_BENLEUMI_NAME.
   03 FILLER			PIC X(15) VALUE "ICA_T2_DAT_DIR:".
   03 CURRENCY_IND              PIC X(2).
   03 FILLER                    PIC X(1)  VALUE "_".
   03 L$_BENLEUMI_PHASE_NUM	PIC 9(04).
   03 L$_BENLEUMI_PHASE_DATE    PIC 9(08).
   03 FILLER			PIC X(04) VALUE ".DAT".

* Change #1023 - Start.
01 L$_MSG_FILE_NAME.        
    03 FILLER                     PIC X(15) VALUE "ICA_IN_DAT_DIR:".
    03 FILLER                     PIC X(14) VALUE "ICA_TRB711_MSG".
    03 L$_OPER_AND_PHASE          PIC X(26).
* Change #1023 - End.

01 L$_NUM_MSG_ERRORS		PIC 9(2) VALUE 0.

01 L$_RETURN_STATUS		PIC S9(9) COMP.
01 L$_TIME_STRING               PIC X(23).

01 L$_PREV_BANK			PIC 9(02) COMP.

01 L$_VISA_TEXT.
   03 L$_VISA_BANK		PIC 9(02) VALUE 31.
   03 L$_VISA_SNIF		PIC 9(03) VALUE 046.
   03 L$_VISA_CODE_CHESBON	PIC 9(04) VALUE 0000.
   03 L$_VISA_SUG_CHESBON	PIC 9(03) VALUE 409.
   03 L$_VISA_CHESBON	        PIC 9(06) VALUE 318272.

* Chg #2000 - Start.
01 L$_VISA_TEXT_U.
   03 L$_VISA_BANK_U            PIC 9(02) VALUE 26.
   03 L$_VISA_SNIF_U            PIC 9(03) VALUE 101.
   03 L$_VISA_CODE_CHESBON_U    PIC 9(06) VALUE 000000.
   03 L$_VISA_SUG_CHESBON_U     PIC 9     VALUE 2.
   03 L$_VISA_CHESBON_U         PIC 9(06) VALUE 852708.
* Chg #2000 - End.

01 L$_PROD_EXT_ID            	PIC X(20).
01 L$_PROD_RED1  REDEFINES  L$_PROD_EXT_ID.
   03  L$_PROD_EXT_ID_007       PIC X(07).
   03  L$_PROD_EXT_ID_013       PIC X(13).
   03  L$_PROD_EXT_RED  REDEFINES  L$_PROD_EXT_ID_013.
       05  L$_PROD_EXT_ID_07    PIC X(07).
       05  L$_PROD_EXT_ID_06    PIC X(06).
01 L$_PROD_RED2  REDEFINES  L$_PROD_EXT_ID.
   03  L$_PROD_EXT_ID_7         PIC X(07).
   03  L$_PROD_EXT_ID_13        PIC X(13).

*#1001 Start.
01 L$_EXT_ID_NUM_TYPE_CODE	PIC 9(02).
*#1001 End.
* Change #1023 - Start.
01 L$_TYPE_OF_MESSAGE            PIC 9(2) VALUE 0. 
   88 L$_SW_DATE_ZERO	          VALUE 1.
   88 L$_SW_NO_ACCUM	          VALUE 2.
   88 L$_SW_NO_SIMUCHIN	          VALUE 3.
   88 L$_SW_PROGRAM_STOP	  VALUE 4.
   88 L$_SW_NO_CAL_ACC	          VALUE 5.
   88 L$_SW_NO_668_REC	          VALUE 6.
   88 L$_SW_WRONG_670_REC	  VALUE 7.
   88 L$_SW_ALPHA_TRN	          VALUE 8.
   88 L$_SW_STOPPED_AMT	          VALUE 9.
   88 L$_SW_737_669               VALUE 10.   
   88 L$_SW_CURRENCY_CODE         VALUE 11.   
   88 L$_SW_STOPPED_AMT_NO_IND	  VALUE 12.
   88 L$_SW_NO_CURRENCY_CODE      VALUE 13.   
   88 L$_SW_NO_132_REC	          VALUE 14.
   88 L$_SW_NO_947_REC	          VALUE 15.
   88 L$_SW_DIF_CURRENCY_CODE     VALUE 16.   
   88 L$_SW_NO_CURR_CONV_SRC_IND  VALUE 17.
   88 L$_SW_BANK_CURRENCY_CODE    VALUE 18.   
   88 L$_SW_BANK_CURRENCY_IND     VALUE 19.   

01 L$_OPER_AND_PHASE_TMP       	PIC X(26) EXTERNAL.
* Change #1023 - End.

01 L$_SUG_RESHUMA_KOTERET	PIC 9(01) VALUE 0.
01 L$_SUG_RESHUMA_TNUA   	PIC 9(01) VALUE 1.
01 L$_SUG_RESHUMA_SIUM  	PIC 9(01) VALUE 9.
01 L$_SUG_SERET          	PIC 9(01) VALUE 0.
01 L$_SUG_ZIHUI          	PIC 9(01) VALUE 0.
01 L$_MIS_ZIHUI          	PIC X(09) VALUE SPACES.
01 L$_ASMACTA_CODE       	PIC 9(01) VALUE 7.

01 L$_DEBIT_SIDE		PIC 9(03) VALUE 504.
01 L$_CREDIT_SIDE		PIC 9(03) VALUE 006.
01 L$_DEBIT_IZUN		PIC 9(03) VALUE 601.
01 L$_CREDIT_IZUN		PIC 9(03) VALUE 154.
01 L$_DEBIT_PEULA      		PIC 9(02) VALUE 01.
01 L$_CREDIT_PEULA      	PIC 9(02) VALUE 04.

* Start Change #2000.
*01 L$_CODE_BANK			PIC 9(02) VALUE 31.
01 L$_CODE_BANK_UBANK           PIC 9(2) VALUE 26.
01 L$_CODE_BANK_BNL             PIC 9(2) VALUE 31.
* End Change #2000.

01 L$_CODE_CHESBON       	PIC 9(04) VALUE 0000.
01 L$_CODE_SIDURI		PIC 9(02) VALUE 01.
01 L$_CODE_MOSSAD      		PIC 9(08) VALUE 13508999.
01 L$_DINERS_MOSSAD     	PIC X(10) VALUE "    סרנייד".
01 L$_VISA_MOSSAD     		PIC X(10) VALUE "     ל.א.כ".
01 L$_CODE_MELLEL     		PIC 9(03) VALUE 000.
01 L$_DINERS_MELLEL     	PIC X(10) VALUE "    סרנייד".
01 L$_VISA_MELLEL     		PIC X(10) VALUE "     ל.א.כ".
01 L$_IZUN_RECORD      		PIC X(20) VALUE "99999999999999999999".

01 L$_CODE_MOSSAD_DEB_NO_ELEC   PIC 9(08) VALUE 08547994.
01 L$_CODE_MOSSAD_DEB_ELEC      PIC 9(08) VALUE 19271287.
01 L$_CODE_MOSSAD_CRD_NO_ELEC   PIC 9(08) VALUE 13508205.
01 L$_CODE_MOSSAD_CRD_ELEC      PIC 9(08) VALUE 22656623.
01 L$_CODE_MOSSAD_AMEX          PIC 9(08) VALUE 19268283.

01 L$_MOSSAD_SOC_CRD_DYNR      PIC 9(08) VALUE 13322201.
01 L$_MOSSAD_LAK_CRD_DYNR      PIC 9(08) VALUE 13352281.
01 L$_MOSSAD_LAK_DEB_DYNR      PIC 9(08) VALUE 00721696.

01 L$_SHEKEL     		PIC 9(02) VALUE 00.
01 L$_DOLLAR     		PIC 9(02) VALUE 01.

01 L$_DEB_SCHUM_BANK		PIC S9(15)V99 COMP.
01 L$_CRD_SCHUM_BANK		PIC S9(15)V99 COMP.

01 L$_SCHUM			PIC 9(15)V99 COMP.

01 L$_DEB_TNUOT_BANK		PIC 9(07).
01 L$_CRD_TNUOT_BANK		PIC 9(07).

01 L$_OUTPUT_OPEN_FLAG		PIC 9.
   88 L$_OUTPUT_CLOSE		VALUE 0.
   88 L$_OUTPUT_OPEN		VALUE 1.

01 L$_EOF_INPUT_FLAG		PIC 9. 
   88 L$_NOT_EOF_INPUT		VALUE 0.
   88 L$_EOF_INPUT		VALUE 1.

01 L$_EOF_OUTPUT_FLAG		PIC 9. 
   88 L$_NOT_EOF_OUTPUT		VALUE 0.
   88 L$_EOF_OUTPUT		VALUE 1.

01 L$_FILE_FLAG			PIC 9 VALUE 0. 
   88 L$_EOF_FILE		VALUE 9.
   88 L$_BREAK_FILE		VALUE 6 9.

01 L$_FIRST_OUT_FLAG	 PIC 9  VALUE 0. 
   88 L$_FIRST_OUT		VALUE 0.
   88 L$_NOT_FIRST_OUT          VALUE 1.

* Change #2001 -Start.
01 L$_TOTAL_CRD_AMT		PIC S9(12)V99 COMP.
01 L$_TOTAL_DEB_AMT		PIC S9(12)V99 COMP.
* Change #2001 -End.
*-------------------------------------------------------------------------
*                            יחכונ ךיראת

01 L$_DATE_YYMMDD_TODAY         PIC 9(8).
01 L$_VALUE_DATE		PIC 9(8).

*-------------------------------------------------------------------------
*                  תועש ילב הריבשל ךיראת

01 L$_DATE_YYMMDD_CHECK        PIC 9(8).

*-------------------------------------------------------------------------
*                        היצרגטניאל ךיראת

01 L$_DATE_YYMMDD_INTEGR       PIC 9(8).

*-------------------------------------------------------------------------
*                           המושרב ךיראת

01 L$_VALUE_DATE_CURRENT       PIC 9(8).

*-------------------------------------------------------------------------
01 L$_DATE_YYMMDD_27	       PIC 9(8).
*-------------------------------------------------------------------------
*              TRB714 תינכותמ עיגמה עבטמ                       
01 L$_CURRENCY			PIC 9(2) VALUE 03.
*#1009 Start.
01 L$_CURRENCY_IND	        PIC 9(1).
   88 L$_CURRENCY_SHEKEL	VALUE 3.
   88 L$_FOREIGNER_CURRENCY	VALUE 1.

01 L$_RELEASE_IND	        PIC 9(1).
   88 L$_RELEASE   	        VALUE 0.
   88 L$_NOT_RELEASE   	        VALUE 1.
*#1009 End.
*----------------------------------------------------*
01 L$_OPER_ID			PIC 9(16).
01 L$_SW			PIC 9(1).
   88 L$_SW_ATIDI		VALUE 1.
01 L$_FILE_NAME			PIC X(17).
01 L$_TOT_RESHUMOT		PIC  Z(6).
01 LD$_AMT			PIC ZZZ,ZZZ,ZZZ,ZZZ.99.
01 LX$_AMT			PIC X(18).
01 L$_DATE_X8			PIC X(8).
01 L$_DATE_X8_RED REDEFINES L$_DATE_X8.
   03 L$_DATE_X8_DD		PIC 99.
   03 L$_X8_1			PIC X.
   03 L$_DATE_X8_MM		PIC 99.
   03 L$_X8_2			PIC X.
   03 L$_DATE_X8_YY		PIC 99.
*----------------------------------------------------*
LINKAGE 	 	 	 	     SECTION.                           
*----------------------------------------------------*
  COPY "ICA_CDD_WKSP:ICA_TRJ711_JOB_DATA"	 FROM DICTIONARY
        REPLACING ICA_TRJ711_JOB_DATA BY ICA_TRJ711_JOB_DATA_INW.
  COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"            FROM DICTIONARY.
************************************************************************
 PROCEDURE  DIVISION       USING  ICA_TRJ711_JOB_DATA_INW
                                  UTL_CONTROL_ACW
 		          GIVING  SP$_ACW_PROC_AUX_STATUS.
************************************************************************


DECLARATIVES.
*-----------------------------------------------------------*
001-I-O-PROBLEM				            SECTION.
*-----------------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON ICA_INTGR_INPUT_FILE_3.

001.

    DISPLAY "** ERROR HANDLING IN INPUT FILE : " L$_INTGR_INPUT_NAME
    DISPLAY "** SP$_COBRMS_FILE_STATUS : " SP$_COBRMS_FILE_STATUS
    DISPLAY "** SEC:001-I-O-PROBLEM ICA_INTGR_INPUT_FILE_3:ץבוק "

    MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
    MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV

    CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP

    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)

    SET SP$_EXIT_STATUS_FAILURE TO TRUE

    MOVE SP$_MSG_ERROR                   TO SP$_ACW_PROC_AUX_STATUS 
    MOVE SP$_COBRMS_FILE_STATUS          TO  L$_NUM_MSG_ERRORS
    MOVE L$_NUM_MSG_ERRORS               TO  SP$_ACW_FREE_TEXT(1:2)
    MOVE MSG_TEXT IN ICA_RMS_MSG_WKSP(1) TO  SP$_ACW_FREE_TEXT(4:66)



*# 13-NOV-2012 18:29:23.17 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.


*-----------------------------------------------------------*
002-I-O-PROBLEM				            SECTION.
*-----------------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON ICA_INTGR_INPUT_BEFORE.

002.

    DISPLAY "** ERROR HANDLING IN INPUT FILE : " L$_INTGR_BEFORE_NAME
    DISPLAY "** SP$_COBRMS_FILE_STATUS : " SP$_COBRMS_FILE_STATUS
    DISPLAY "** SEC:002-I-O-PROBLEM L$_INTGR_BEFORE_NAME:ץבוק "

    MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
    MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV

    CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP

    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)

    SET SP$_EXIT_STATUS_FAILURE TO TRUE

    MOVE SP$_MSG_ERROR                   TO SP$_ACW_PROC_AUX_STATUS 
    MOVE SP$_COBRMS_FILE_STATUS          TO  L$_NUM_MSG_ERRORS
    MOVE L$_NUM_MSG_ERRORS               TO  SP$_ACW_FREE_TEXT(1:2)
    MOVE MSG_TEXT IN ICA_RMS_MSG_WKSP(1) TO  SP$_ACW_FREE_TEXT(4:66)



*# 13-NOV-2012 18:29:23.17 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.

* Change #1023 - Start.
*-----------------------------------------------------------*
003-I-O-PROBLEM				            SECTION.
*-----------------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON ICA_MSG_OUTPUT_FILE.

003.

    DISPLAY "** ERROR HANDLING IN INPUT FILE : " L$_MSG_FILE_NAME
    DISPLAY "** SP$_COBRMS_FILE_STATUS : " SP$_COBRMS_FILE_STATUS
    DISPLAY "** SEC:003-I-O-PROBLEM L$_MSG_FILE_NAME:ץבוק "

    MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
    MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV

    CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP

    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)

    SET SP$_EXIT_STATUS_FAILURE TO TRUE

    MOVE SP$_MSG_ERROR                   TO SP$_ACW_PROC_AUX_STATUS 
    MOVE SP$_COBRMS_FILE_STATUS          TO  L$_NUM_MSG_ERRORS
    MOVE L$_NUM_MSG_ERRORS               TO  SP$_ACW_FREE_TEXT(1:2)
    MOVE MSG_TEXT IN ICA_RMS_MSG_WKSP(1) TO  SP$_ACW_FREE_TEXT(4:66)



*# 13-NOV-2012 18:29:23.17 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.
* Change #1023 - End.

*-----------------------------------------------------------*
OUTPUT-ERROR				            SECTION.
*-----------------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON ICA_BANK_BENLEUMI_FILE.

OUT-ERR.

    DISPLAY "** ERROR HANDLING IN OUTPUT FILE : " L$_BANK_BENLEUMI_NAME
    DISPLAY "** SP$_COBRMS_FILE_STATUS : ", SP$_COBRMS_FILE_STATUS
    DISPLAY "** SEC:DECLARATIVES-OUT-ERR ICA_BANK_BENLEUMI_FILE:ץבוק "

    MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
    MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV

    CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP

    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
    DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)

    SET SP$_EXIT_STATUS_FAILURE TO TRUE

    MOVE SP$_MSG_ERROR                    TO  SP$_ACW_PROC_AUX_STATUS 
    MOVE SP$_COBRMS_FILE_STATUS           TO  L$_NUM_MSG_ERRORS
    MOVE L$_NUM_MSG_ERRORS                TO  SP$_ACW_FREE_TEXT(1:2)
    MOVE MSG_TEXT IN ICA_RMS_MSG_WKSP(1)  TO  SP$_ACW_FREE_TEXT(4:66)



*# 13-NOV-2012 18:29:23.17 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.

END DECLARATIVES.
                                                                                

A-MAIN SECTION.
*#-----------------------------------------------------------------------------
*# Section: A-MAIN
*# Description:
*#                         --------
*#                         ישאר עטק
*#                         --------
*#        םיצבק תחיתפו םינתשמ לוחתא
*#      ימואלניבה קנבל תועונת תכירע
*#                             םויס
*#
*#-----------------------------------------------------------------------------
A-00.

    PERFORM S-SORT-INPUT.

    PERFORM B-INIT.
    
    PERFORM T-PROCESS UNTIL L$_EOF_FILE.

    PERFORM Z-FINISH.

A-EXIT.
     EXIT.


T-PROCESS   SECTION.
*#-----------------------------------------------------------------------------
*# Section: T-PROCESS
*# Purpose: 
*# Description:
*#
*#-----------------------------------------------------------------------------
T-00.
*#  הביתכו קנבל תרתוכ תמושר תכירע
    PERFORM D-BUILD-BENLEUMI-KOT-DATA.

*# הריבש ,האירק ,העונת תמושר תינב לופיט
    PERFORM U-TIPUL UNTIL L$_BREAK_FILE.

*#  הביתכו קנבל םויס תמושר תכירע
    PERFORM F-BUILD-BENLEUMI-SIUM-DATA.
T-EXIT.
     EXIT.

U-TIPUL   SECTION.
*#-----------------------------------------------------------------------------
*# Section: U-TIPUL
*# Purpose: 
*# Description:
*#
*#-----------------------------------------------------------------------------
U-00.

*#           טלפ ץבוק תמושר תביתכ
            PERFORM E-BUILD-BANK-RECORD.

*#           השדח טלק תמושר תאירק
            PERFORM I-READ-INTGR-INPUT.

*#           הריבש תקידב
            PERFORM K-BREAK.

U-EXIT.
     EXIT.

S-SORT-INPUT        SECTION.
*#-----------------------------------------------------------------------------
*# Section: S-SORT-INPUT
*# Purpose: Making cobol sort for the input FILE
*# Description:
*#
*#-----------------------------------------------------------------------------
S-00.

    MOVE P$_TRB714_FILE_NAME IN  ICA_TRJ711_JOB_DATA_INW(56:2)
      TO L$_CURRENCY.

* Change #1009 - Start.
    IF  L$_CURRENCY NOT = V674$SHEKEL
        SET L$_FOREIGNER_CURRENCY TO TRUE
    ELSE
        SET L$_CURRENCY_SHEKEL    TO TRUE 	
    END-IF.
* Change #1009 - End.

* Change #1023 - Start.
    MOVE L$_OPER_AND_PHASE_TMP 
      TO L$_OPER_AND_PHASE IN L$_MSG_FILE_NAME.
    INITIALIZE  ICD_TRT_674_CURNCY_TYP_INIT.
* Change #1023 - End.
    MOVE P$_TRB714_FILE_NAME IN ICA_TRJ711_JOB_DATA_INW(1:54)
      TO L$_INTGR_BEFORE_NAME.

    CALL 'UTL_GET_DATE_TIME_BINARY'  USING  UTL_GET_DATE_TIME_BINARY_WKSP.

    IF  SP$_SYSPRO_STATUS    IN UTL_GET_DATE_TIME_BINARY_WKSP  IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL   IN  UTL_GET_DATE_TIME_BINARY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_GET_DATE_TIME_BINARY SEC:S-SORT-INPUT'
          TO  SP$_ACW_FREE_TEXT  IN  UTL_CONTROL_ACW(1:40)
        MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
        PERFORM  Z-FINISH
    END-IF.

* Change #1009 - Start.
*    SORT SORT_FILE
*         ON ASCENDING 
*         KEY DEB_CRD_CURRENCY_CODE IN SORT_REC
*	     NEW_VALUE_DATE  IN SORT_REC
*             INTG_BANK_NUM   IN SORT_REC
*             ACC_BANK_NUM    IN SORT_REC
*             ACC_BRANCH_NUM  IN SORT_REC
*             ACC_TYPE_CODE   IN SORT_REC
*             ACC_NUM         IN SORT_REC
*         WITH   DUPLICATES IN ORDER
*	 INPUT   PROCEDURE SA-ADD-NEW-VALUE-DATE
*	 OUTPUT  PROCEDURE SB-DELETE-NEW-VALUE-DATE.

    SORT SORT_FILE
         ON ASCENDING 
         KEY 
	     NEW_VALUE_DATE        IN SORT_REC
	     DEB_CRD_CURRENCY_CODE IN SORT_REC
             INTG_BANK_NUM         IN SORT_REC
             ACC_BANK_NUM          IN SORT_REC
             ACC_BRANCH_NUM        IN SORT_REC
             ACC_TYPE_CODE         IN SORT_REC
             ACC_NUM               IN SORT_REC
         WITH   DUPLICATES IN ORDER
	 INPUT   PROCEDURE SA-ADD-NEW-VALUE-DATE
	 OUTPUT  PROCEDURE SB-DELETE-NEW-VALUE-DATE.
* Change #1009 - End.
S-EXIT.
     EXIT.


SA-ADD-NEW-VALUE-DATE          SECTION.
*#-----------------------------------------------------------------------------
*# Section: SA-ADD-NEW-VALUE-DATE
*# Description:  IF value_date > to_day ---> put new_value_date = to_day,
*#               else                        put new_value_date = value_date
*#-----------------------------------------------------------------------------
SA-00.

    OPEN INPUT ICA_INTGR_INPUT_BEFORE ALLOWING ALL.

    SET L$_NOT_EOF_INPUT TO TRUE

    PERFORM UNTIL L$_EOF_INPUT

         READ ICA_INTGR_INPUT_BEFORE	
            AT END
    	       CLOSE ICA_INTGR_INPUT_BEFORE
 	       SET L$_EOF_INPUT TO TRUE
         END-READ

* Change #1009 - Start.
*         IF L$_NOT_EOF_INPUT
*            MOVE CORR ICA_INTGR_BEFORE_REC TO COPY_REC
*            IF DEB_CRD_CURRENCY_CODE IN ICA_INTGR_BEFORE_REC = L$_CURRENCY
*               IF VALUE_DATE IN ICA_INTGR_BEFORE_REC <= 
*                  SP$_DATE_TIME_BINARY IN  UTL_GET_DATE_TIME_BINARY_WKSP
*                  MOVE SP$_DATE_TIME_BINARY IN  UTL_GET_DATE_TIME_BINARY_WKSP
*                    TO NEW_VALUE_DATE
*               ELSE
*                  MOVE VALUE_DATE IN ICA_INTGR_BEFORE_REC
*                    TO NEW_VALUE_DATE
*               END-IF
*               RELEASE SORT_REC
*            END-IF
*         END-IF

         IF L$_NOT_EOF_INPUT
            MOVE CORR ICA_INTGR_BEFORE_REC TO COPY_REC
            IF L$_CURRENCY_SHEKEL
	       IF DEB_CRD_CURRENCY_CODE IN ICA_INTGR_BEFORE_REC = V674$SHEKEL
	          SET L$_NOT_RELEASE TO TRUE
               ELSE 
	          SET L$_RELEASE  TO TRUE
               END-IF
            ELSE 
	       IF DEB_CRD_CURRENCY_CODE IN ICA_INTGR_BEFORE_REC = V674$SHEKEL
	          SET L$_RELEASE TO TRUE
               ELSE 
	          SET L$_NOT_RELEASE TO TRUE
               END-IF        
            END-IF
         END-IF

         IF L$_NOT_EOF_INPUT
            IF L$_NOT_RELEASE
               IF VALUE_DATE IN ICA_INTGR_BEFORE_REC <= 
                  SP$_DATE_TIME_BINARY IN  UTL_GET_DATE_TIME_BINARY_WKSP
                  MOVE SP$_DATE_TIME_BINARY IN  UTL_GET_DATE_TIME_BINARY_WKSP
                    TO NEW_VALUE_DATE
               ELSE
                  MOVE VALUE_DATE IN ICA_INTGR_BEFORE_REC
                    TO NEW_VALUE_DATE
               END-IF
               RELEASE SORT_REC
            END-IF
         END-IF
* Change #1009 - End.

    END-PERFORM.

SA-EXIT.
     EXIT.


SB-DELETE-NEW-VALUE-DATE             SECTION.
*#-----------------------------------------------------------------------------
*# Section: SB-DELETE-NEW-VALUE-DATE.
*# Description:  
*#-----------------------------------------------------------------------------
SB-00.

    SET L$_NOT_EOF_OUTPUT TO TRUE.

    OPEN OUTPUT ICA_INTGR_INPUT_FILE_3.

    PERFORM UNTIL L$_EOF_OUTPUT
       RETURN SORT_FILE
          AT END CLOSE ICA_INTGR_INPUT_FILE_3
              SET L$_EOF_OUTPUT TO TRUE

          NOT AT END
              MOVE CORR COPY_REC TO ICA_TRB711_OUTPUT_670
              WRITE ICA_TRB711_OUTPUT_670
       END-RETURN
    END-PERFORM.

SB-EXIT.
     EXIT.


B-INIT SECTION.
*#-----------------------------------------------------------------------------
*# Section: B-INIT
*# Description:
*#                         -------------------------
*#                         םיצבק תחיתפו םינתשמ לוחתא
*#                         -------------------------
*#                                      םינתשמ לוחתא
*#                                 יחכונ ךיראת בושיח
*#                                 הריבש ךיראת בושיח
*#           (יחכונ ךיראתמ תוחפ םימי 27) ךיראת בושיח
*#                                 ךלהמ ךיראת  בושיח
*#           םירטמרפה חטשמ םש יפלע -  טלק ץבוק תחיתפ 
*#                                 הנושר המושר תאירק
*#-----------------------------------------------------------------------------
B-00.
*# 13-NOV-2012 18:29:22.15 - TP_TSARSU - Move program name to TLG wksp.
    MOVE "ICA_TRB711_EDIT_BENLEUMI" 
      TO P$_PROG_NAME IN ICA_TLG_WKSP.

*# 13-NOV-2012 18:29:22.15 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_INIT_PROG.INC".


*   םינתשמ לוחתא
*   ------------

    MOVE 0 TO L$_SW.
    SET L$_OUTPUT_CLOSE  TO TRUE.
    SET L$_NOT_EOF_INPUT TO TRUE.
    SET L$_EOF_OUTPUT    TO TRUE.
    SET L$_FIRST_OUT     TO TRUE.
    MOVE 0 TO L$_FILE_FLAG.
    MOVE 0 TO L$_RECORD_COUNTER.
    MOVE 01 TO L$_CODE_SIDURI.
    INITIALIZE  ICA_RMS_MSG_WKSP
                ICA_XLATE_MSG_WKSP
                UTL_CONTROL_ACW
                UTL_CVT_DELTA_TIME_WKSP
                UTL_APPLY_DELTA_TIME_WKSP
* Start Change #2000.
                L$_FIRST_OUT_FLAG
* End Change #2000.

*#  Define that is a delek process
    IF L$_DBCR_PROCESS_TYPE_DESC IN ICA_TRJ711_JOB_DATA_INW(1:5) = "DELEK"
       SET L$_SW_DELEK TO TRUE
    ELSE
       SET L$_SW_VISA TO TRUE
    END-IF.

    MOVE "  " TO CURRENCY_IND IN L$_BANK_BENLEUMI_NAME.

    MOVE SP$_MSG_NORMAL
      TO SP$_ACW_PROC_AUX_STATUS  IN  UTL_CONTROL_ACW.

    MOVE 'TRB711' TO L$_MSG_TEXT(2:6).

    MOVE DEB_CRD_PHASE_NUM IN ICA_TRJ711_JOB_DATA_INW
      TO L$_MSG_TEXT(19:4).


*   ךלהמ ךיראת תאבה
*   --------------------
    MOVE DBCR_PHASE_TMSP IN ICA_TRJ711_JOB_DATA_INW
      TO SP$_DATE_TIME_BINARY OF UTL_CVT_DATE_DDMMYYYY_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY' USING UTL_CVT_DATE_DDMMYYYY_WKSP.
    IF  SP$_SYSPRO_STATUS  IN UTL_CVT_DATE_DDMMYYYY_WKSP IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL  IN UTL_CVT_DATE_DDMMYYYY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DATE_YYMMDD SEC:B-INIT' 
                         TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:39)
        MOVE L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(41:28)
        DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
        PERFORM  Z-FINISH
    END-IF.
    
*# YYMMDD היצרגטניא ךיראת תריצי
    INSPECT SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
            REPLACING ALL " " BY "0".
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
      TO L$_DATE_YYMMDD_INTEGR(7:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
      TO L$_DATE_YYMMDD_INTEGR(5:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
      TO L$_DATE_YYMMDD_INTEGR(1:4).
*--------------
    MOVE L$_DATE_YYMMDD_INTEGR TO L$_DATE_YYMMDD_TODAY,
                                  L$_BENLEUMI_PHASE_DATE.

    MOVE   L$_DATE_YYMMDD_TODAY TO L$_DATE_YYMMDD_CHECK.
*--------------


*   (יחכונ ךיראתמ תוחפ םימי 27-ו יחכונ ךיראת) ךיראת בושיח
*   -----------------------------------------------------
    PERFORM  BA-CAL-CURR-DATE-27.

*   טלק ץבוק תחיתפ
    OPEN INPUT ICA_INTGR_INPUT_FILE_3.

    PERFORM I-READ-INTGR-INPUT.

    IF  L$_EOF_INPUT 
        MOVE  SP$_MSG_NO_DATA_FOUND             
          TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'קיר ימואלניב ץבוק SEC:I-READ-INTGR-INPUT '
          TO  SP$_ACW_FREE_TEXT
        PERFORM  Z-FINISH
    END-IF.


B-EXIT.
     EXIT.
                                                                                
BA-CAL-CURR-DATE-27 SECTION.
*#-----------------------------------------------------------------------------
*# Section: BA-CAL-CURR-DATE-27.
*# Description: יחכונ ךיראתמ תוחפ םימי 27-ו יחכונ ךיראת בושיח
*#-----------------------------------------------------------------------------
BA-00.

    MOVE  SP$_DATE_TIME_BINARY          IN  UTL_CVT_DATE_DDMMYYYY_WKSP
      TO  SP$_DATE_TIME_1               IN  UTL_APPLY_DELTA_TIME_WKSP.

*   יחכונ ךיראתמ תוחפ םימי 27 - ךיראת בושיח
*   ----------------------------------------
*   יראניבל םימי רפסמ תכיפה

    MOVE  "0027 00:00:00.00"
      TO  L$_TIME_STRING.

    MOVE  L$_TIME_STRING
      TO  SP$_DELTA_TIME_C23            IN  UTL_CVT_DELTA_TIME_WKSP.

    CALL 'UTL_CVT_DELTA_TIME'  USING    UTL_CVT_DELTA_TIME_WKSP.

    IF  SP$_SYSPRO_STATUS    IN UTL_CVT_DELTA_TIME_WKSP  IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL   IN UTL_CVT_DELTA_TIME_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DELTA_TIMESEC :BA-CAL-CURR  ' 
          TO  SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:40)
        MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
        PERFORM  Z-FINISH
    END-IF.

    MOVE  SP$_DELTA_TIME                IN  UTL_CVT_DELTA_TIME_WKSP 
      TO  SP$_DELTA_TIME                IN  UTL_APPLY_DELTA_TIME_WKSP.

    MOVE  "-"
      TO  SP$_MATH_OPERATOR             IN  UTL_APPLY_DELTA_TIME_WKSP.

    CALL 'UTL_APPLY_DELTA_TIME'      USING  UTL_APPLY_DELTA_TIME_WKSP.

    IF  SP$_SYSPRO_STATUS    IN UTL_APPLY_DELTA_TIME_WKSP  IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL   IN  UTL_APPLY_DELTA_TIME_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_APPLY_DELTA_TIME SEC:BA-CAL-CUR ' 
          TO  SP$_ACW_FREE_TEXT  IN  UTL_CONTROL_ACW(1:40)
        MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
        PERFORM  Z-FINISH
    END-IF.

    MOVE  SP$_DATE_TIME_2               IN  UTL_APPLY_DELTA_TIME_WKSP 
      TO  SP$_DATE_TIME_BINARY          IN  UTL_CVT_DATE_DDMMYYYY_WKSP.


*   'YYMMDD'-ל יראניבה ךיראתה תכיפה
*   --------------------------------
    CALL 'UTL_CVT_DATE_DDMMYYYY' USING UTL_CVT_DATE_DDMMYYYY_WKSP.
    IF  SP$_SYSPRO_STATUS  IN UTL_CVT_DATE_DDMMYYYY_WKSP IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL  IN UTL_CVT_DATE_DDMMYYYY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DATE_DDMMYYYY SEC:E-OPEN         ' 
                         TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:39)
        MOVE L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(41:28)
        DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
        PERFORM  Z-FINISH
    END-IF.
    
    INSPECT SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
            REPLACING ALL " " BY "0".
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
      TO L$_DATE_YYMMDD_27(7:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
      TO L$_DATE_YYMMDD_27(5:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
      TO L$_DATE_YYMMDD_27(1:4).

BA-EXIT.
     EXIT.
                                                                                
C-OPEN-OUTPUT-FILE      SECTION.
*#-----------------------------------------------------------------------------
*# Section: C-OPEN-OUTPUT-FILE.
*# Description: טלפ ץבוק תחיתפ
*#          קנבל םינתשמ לוחתיא
*#-----------------------------------------------------------------------------
C-00.

    MOVE  DEB_CRD_PHASE_NUM IN ICA_TRJ711_JOB_DATA_INW
      TO  L$_BENLEUMI_PHASE_NUM.

*------------
    MOVE 0 TO SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
    MOVE VALUE_DATE IN ICD_TRT_670_CUR_CRDB IN ICA_TRB711_OUTPUT_670
      TO SP$_DATE_TIME_BINARY OF UTL_CVT_DATE_DDMMYYYY_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY' USING UTL_CVT_DATE_DDMMYYYY_WKSP.
    IF  SP$_SYSPRO_STATUS  IN UTL_CVT_DATE_DDMMYYYY_WKSP IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL  IN UTL_CVT_DATE_DDMMYYYY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DATE_YYMMDD SEC:K-READ' 
                         TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:39)
        MOVE L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(41:28)
        DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
        PERFORM  Z-FINISH
    END-IF.
    
    INSPECT SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
            REPLACING ALL " " BY "0".
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
      TO L$_VALUE_DATE(7:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
      TO L$_VALUE_DATE(5:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
      TO L$_VALUE_DATE(1:4).
   
    IF L$_VALUE_DATE NOT > L$_DATE_YYMMDD_TODAY
       MOVE L$_DATE_YYMMDD_TODAY TO L$_DATE_YYMMDD_CHECK
    ELSE
       SET L$_SW_ATIDI TO TRUE
       MOVE L$_VALUE_DATE
         TO L$_DATE_YYMMDD_CHECK
*# YYYYMMDD ידיתע ךיראת רשאכ  ץבוקה םשל ךיראת תריצי
       MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
         TO L$_BENLEUMI_PHASE_DATE(7:2)
       MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
         TO L$_BENLEUMI_PHASE_DATE(5:2)
       MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
         TO L$_BENLEUMI_PHASE_DATE(1:4)
    END-IF.
    
    IF (DEB_CRD_CURRENCY_CODE IN ICD_TRT_670_CUR_CRDB 
                              IN ICA_TRB711_OUTPUT_670 = V674$SHEKEL) 
       IF L$_SW_DELEK
         MOVE "DL" TO CURRENCY_IND
       ELSE
* Start Change #2000.
*         THEN
*             MOVE "IL"
*               TO CURRENCY_IND
*       END-IF
         IF ACC_BANK_NUM      IN ICD_TRT_670_CUR_CRDB
                              IN ICA_TRB711_OUTPUT_670 = V327$INTG_BNL
         THEN
             MOVE "IL"
               TO CURRENCY_IND

         ELSE

             MOVE "JA"
               TO CURRENCY_IND

         END-IF
* End Change #2000.

       END-IF
    ELSE

* Start Change #2000.
         IF ACC_BANK_NUM      IN ICD_TRT_670_CUR_CRDB
                              IN ICA_TRB711_OUTPUT_670 = V327$INTG_BNL
         THEN
             MOVE "IY" TO CURRENCY_IND

         ELSE
             MOVE "IW" TO CURRENCY_IND

         END-IF
* End Change #2000.

    END-IF

    OPEN  OUTPUT ICA_BANK_BENLEUMI_FILE.

*   קנבל םינתשמ לוחתיא

    SET   L$_OUTPUT_OPEN  TO  TRUE.

    MOVE  ZEROS  TO  L$_DEB_SCHUM_BANK  L$_CRD_SCHUM_BANK
                     L$_DEB_TNUOT_BANK  L$_CRD_TNUOT_BANK.

    SET L$_NOT_EOF_OUTPUT TO TRUE.

C-EXIT.
     EXIT.


D-BUILD-BENLEUMI-KOT-DATA SECTION.
*#-----------------------------------------------------------------------------
*# Section: D-BUILD-BENLEUMI-KOT-DATA.
*# Description:
*#             -----------------------------------------
*#             ימואלניבה קנבל - הביתכו תרתוכ תמושר תכירע
*#             -----------------------------------------  
*#
*#-----------------------------------------------------------------------------
D-00.

*#  טלפ ץבוק תחיתפ 
    PERFORM C-OPEN-OUTPUT-FILE.

    MOVE 0 TO L$_FILE_FLAG.

*   ימואלניבה קנבל - תרתוכ תמושר תכירע
*   -----------------------------------
    INITIALIZE BENLEUMI_REC
               ICA_TRB711_BNL_HEADER_RECORD.

*   SUG-RESHUMA
    MOVE  L$_SUG_RESHUMA_KOTERET
      TO  P$_BENLEUMI_SUG_RESHUMA   IN ICA_TRB711_BNL_HEADER_RECORD.

*   CODE-MOSSAD
    MOVE  L$_CODE_MOSSAD
      TO  P$_BENLEUMI_CODE_MOSSAD   IN ICA_TRB711_BNL_HEADER_RECORD.

*   CODE-BANK
* Start Change #2000.
*    MOVE  L$_CODE_BANK
*      TO P$_BENLEUMI_BANK IN ICA_TRB711_BNL_HEADER_RECORD

    IF ACC_BANK_NUM           IN ICD_TRT_670_CUR_CRDB
                              IN ICA_TRB711_OUTPUT_670 = V327$KLALI
    THEN
        MOVE L$_CODE_BANK_UBANK
          TO P$_BENLEUMI_BANK IN ICA_TRB711_BNL_HEADER_RECORD

    ELSE

        MOVE L$_CODE_BANK_BNL
          TO P$_BENLEUMI_BANK IN ICA_TRB711_BNL_HEADER_RECORD

    END-IF

* End Change #2000.

*   TAARICH-MAHALACH = YYMMDD
    MOVE  L$_DATE_YYMMDD_INTEGR(3:6)
      TO  P$_BENLEUMI_TAR_MAHALACH  IN ICA_TRB711_BNL_HEADER_RECORD.

*   TAARICH-SERET = YYMMDD
*Y2K
    MOVE  L$_DATE_YYMMDD_INTEGR(3:6)
      TO  P$_BENLEUMI_TAARICH_SERET IN ICA_TRB711_BNL_HEADER_RECORD.

*   CODE-SIDURI
    MOVE  L$_CODE_SIDURI
      TO  P$_BENLEUMI_CODE_SIDURI   IN ICA_TRB711_BNL_HEADER_RECORD.

*   SUG-SERET
    MOVE  L$_SUG_SERET
      TO  P$_BENLEUMI_SUG_SERET     IN ICA_TRB711_BNL_HEADER_RECORD.

    MOVE DEB_CRD_PHASE_NUM IN ICA_TRJ711_JOB_DATA_INW
      TO P$_BENLEUMI_INTG IN ICA_TRB711_BNL_HEADER_RECORD.

*   ימואלניבה קנבל - תרתוכ תמושר הביתכ
*   ---------------------------------
    WRITE BENLEUMI_REC  FROM  ICA_TRB711_BNL_HEADER_RECORD.

    ADD 1 TO L$_RECORD_COUNTER.
* Change #2001 - Start.
    MOVE 0 TO L$_TOTAL_CRD_AMT .
    MOVE 0 TO L$_TOTAL_DEB_AMT .
* Change #2001 - End.

D-EXIT.
     EXIT.


E-BUILD-BANK-RECORD SECTION.
*#-----------------------------------------------------------------------------
*# Section: E-BUILD-BANK-RECORD
*# Description: ימואלניבה קנבל העונת תכירע
*#               --------------------------
*#      העונת תמושר תכירעו טלק תמושר דוביע
*#                       העונת תמושר תביתכ
*#                   תפסונ טלק תמושר תאירק
*#
*#-----------------------------------------------------------------------------
E-00.

*   העונת תמושר תכירעו טלק תמושר דוביע
*   INIT

    INITIALIZE  BENLEUMI_REC
                ICA_TRB711_BNL_TRN_RECORD.

*   SUG-RESHUMA

    MOVE  L$_SUG_RESHUMA_TNUA
      TO  P$_BENLEUMI_SUG_RESHUMA   IN ICA_TRB711_BNL_TRN_RECORD.

    IF CREDIT_COMPANY_CODE IN ICD_TRT_670_CUR_CRDB
                           IN ICA_TRB711_OUTPUT_670 = V1007$DINERS_COMPANY
       IF EXT_ID_NUM_TYPE_CODE IN ICD_TRT_670_CUR_CRDB
                          IN ICA_TRB711_OUTPUT_670 = V346$MERCHANT_NUM_DINERS
                                                   OR V346$CHAIN_DEAL_DINERS
          MOVE L$_MOSSAD_SOC_CRD_DYNR
            TO P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
       ELSE
          IF DEB_CRD_CODE IN ICD_TRT_670_CUR_CRDB
                          IN ICA_TRB711_OUTPUT_670 = V669$CREDIT_SIDE
             MOVE L$_MOSSAD_LAK_CRD_DYNR
               TO P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
          ELSE
             MOVE L$_MOSSAD_LAK_DEB_DYNR
               TO P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
          END-IF
       END-IF
    ELSE
    IF  DEB_CRD_CODE		    IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
        = V669$CREDIT_SIDE
        MOVE  L$_CODE_MOSSAD_CRD_NO_ELEC
          TO  P$_BENLEUMI_CODE_MOSSAD  IN ICA_TRB711_BNL_TRN_RECORD
        IF  PROD_TYPE_CODE          IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
            = V300$ELECTRON_VISA
            MOVE  L$_CODE_MOSSAD_CRD_ELEC
              TO  P$_BENLEUMI_CODE_MOSSAD  IN ICA_TRB711_BNL_TRN_RECORD
        END-IF
        IF  PROD_TYPE_CODE          IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
            = V300$BUSINESS_AMEX
            MOVE  L$_CODE_MOSSAD_AMEX
              TO  P$_BENLEUMI_CODE_MOSSAD  IN ICA_TRB711_BNL_TRN_RECORD
        END-IF
    ELSE
        IF  DEB_CRD_CODE            IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
            = V669$DEBIT_SIDE
            MOVE  L$_CODE_MOSSAD_DEB_NO_ELEC
              TO  P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
            IF  PROD_TYPE_CODE      IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
                = V300$ELECTRON_VISA
                MOVE  L$_CODE_MOSSAD_DEB_ELEC
                  TO  P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
            END-IF
            IF  PROD_TYPE_CODE      IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
                = V300$BUSINESS_AMEX
                MOVE  L$_CODE_MOSSAD_AMEX
                  TO  P$_BENLEUMI_CODE_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
            END-IF
        ELSE
	    MOVE  SP$_MSG_ERROR
	      TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
            MOVE 'םיאתמ וניא דסומ דוק SEC:D-BUILD-BENLEUM '
               TO  SP$_ACW_FREE_TEXT  IN UTL_CONTROL_ACW(1:40)
            MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
            PERFORM  Z-FINISH
        END-IF
    END-IF.

*   CODE-MATBEA

* Change #1009 - Start.
*    IF (DEB_CRD_CURRENCY_CODE IN ICD_TRT_670_CUR_CRDB 
*                              IN ICA_TRB711_OUTPUT_670 = V674$SHEKEL) 
*          MOVE  L$_SHEKEL
*            TO  P$_BENLEUMI_MATBEA    IN ICA_TRB711_BNL_TRN_RECORD
*    ELSE
*          MOVE  L$_DOLLAR
*            TO  P$_BENLEUMI_MATBEA    IN ICA_TRB711_BNL_TRN_RECORD
*    END-IF.

    MOVE DEB_CRD_CURRENCY_CODE      IN ICD_TRT_670_CUR_CRDB 
                                    IN ICA_TRB711_OUTPUT_670 
      TO P$_BENLEUMI_MATBEA         IN ICA_TRB711_BNL_TRN_RECORD.
* Change #1009 - End.
*    IF DEB_CRD_CURRENCY_CODE IN ICA_TRB711_OUTPUT_670 = V674$SHEKEL
*       MOVE 0
*         TO P$_BENLEUMI_MATBEA         IN ICA_TRB711_BNL_TRN_RECORD
*    ELSE 
*       PERFORM GD-GET-CURRENCY-CODE-674
*       MOVE BNL_BANK_CURRENCY_CODE  IN ICD_TRT_674_CURNCY_TYP
*                                      IN ICD_TRT_674_CURNCY_TYP_MPW(1)
*         TO P$_BENLEUMI_MATBEA        IN ICA_TRB711_BNL_TRN_RECORD
*    END-IF
* Change #1023 - End.

*   SUG-TNUA

    IF  DEB_CRD_CODE                IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
        = V669$CREDIT_SIDE
        IF  PROD_EXT_ID             IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
          = L$_IZUN_RECORD
          MOVE  L$_CREDIT_IZUN
            TO  P$_BENLEUMI_SUG_TNUA  IN ICA_TRB711_BNL_TRN_RECORD
        ELSE
          MOVE  L$_CREDIT_SIDE
            TO  P$_BENLEUMI_SUG_TNUA  IN ICA_TRB711_BNL_TRN_RECORD
        END-IF
    ELSE
        IF  DEB_CRD_CODE            IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
            = V669$DEBIT_SIDE
            IF  PROD_EXT_ID         IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
                = L$_IZUN_RECORD
                MOVE  L$_DEBIT_IZUN
                  TO  P$_BENLEUMI_SUG_TNUA IN ICA_TRB711_BNL_TRN_RECORD
            ELSE
                MOVE  L$_DEBIT_SIDE
                  TO  P$_BENLEUMI_SUG_TNUA IN ICA_TRB711_BNL_TRN_RECORD
            END-IF
        ELSE
	    MOVE  SP$_MSG_ERROR
	      TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
            MOVE 'םיאתמ וניא העונת גוס SE:D-BUILD-BENLEUM '
              TO  SP$_ACW_FREE_TEXT        IN UTL_CONTROL_ACW(1:40)
            MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
            PERFORM  Z-FINISH
        END-IF
    END-IF.

*   TAARICH-ERECH-SLIKA = YYMMDD
*Y2K
    MOVE  L$_DATE_YYMMDD_TODAY(3:6)
      TO  P$_BENLEUMI_TAR_ERECH_SLIKA  IN ICA_TRB711_BNL_TRN_RECORD.

*   SCHUM
    MOVE  TOT_XFER_AMT		       IN ICD_TRT_670_CUR_CRDB
                                       IN ICA_TRB711_OUTPUT_670
      TO  P$_BENLEUMI_SCHUM            IN ICA_TRB711_BNL_TRN_RECORD.

*   PIRTEI-CHESBON-YAAD
*   -------------------
*   BANK

    MOVE  ACC_BANK_NUM		       IN ICD_TRT_670_CUR_CRDB
                                       IN ICA_TRB711_OUTPUT_670
      TO  P$_BENLEUMI_BANK             IN ICA_TRB711_BNL_TRN_RECORD.

*   SNIF

    MOVE  ACC_BRANCH_NUM               IN ICD_TRT_670_CUR_CRDB
                                       IN ICA_TRB711_OUTPUT_670
      TO  P$_BENLEUMI_SNIF 	       IN ICA_TRB711_BNL_TRN_RECORD.

*   CODE-CHESBON

    MOVE  L$_CODE_CHESBON
      TO  P$_BENLEUMI_CODE_CHESBON     IN ICA_TRB711_BNL_TRN_RECORD.

*   SUG-CHESBON

* Change #1009 - Start.
*    MOVE  ACC_TYPE_CODE	  	       IN ICD_TRT_670_CUR_CRDB
*                                       IN ICA_TRB711_OUTPUT_670
*      TO  P$_BENLEUMI_SUG_CHESBON      IN ICA_TRB711_BNL_TRN_RECORD.
    IF  DEB_CRD_CURRENCY_CODE	IN ICD_TRT_670_CUR_CRDB
                                IN ICA_TRB711_OUTPUT_670 = V674$SHEKEL OR
				                         = V674$DOLLAR
         MOVE  ACC_TYPE_CODE                IN ICD_TRT_670_CUR_CRDB 
					    IN ICA_TRB711_OUTPUT_670
           TO  P$_BENLEUMI_SUG_CHESBON      IN ICA_TRB711_BNL_TRN_RECORD
    ELSE
         PERFORM  EB-GET-ACC-TYPE-CODE        
    END-IF.
* Change #1009 - End.

*   CHESBON

    MOVE  ACC_NUM		       IN ICD_TRT_670_CUR_CRDB
                                       IN ICA_TRB711_OUTPUT_670
      TO  P$_BENLEUMI_CHESBON	       IN ICA_TRB711_BNL_TRN_RECORD.

*   PIRTEI-CHESBON-VISA

* Chg #2000 - Start.

    IF ACC_BANK_NUM           IN ICD_TRT_670_CUR_CRDB
                              IN ICA_TRB711_OUTPUT_670 = V327$KLALI 
    THEN
        MOVE  L$_VISA_TEXT_U
          TO  P$_BENLEUMI_PIRTEI_VISA     IN ICA_TRB711_BNL_TRN_RECORD

    ELSE

        MOVE  L$_VISA_TEXT
          TO  P$_BENLEUMI_PIRTEI_VISA     IN ICA_TRB711_BNL_TRN_RECORD

    END-IF

* Chg #2000 - End.

*   CODE-MELLEL

    MOVE  L$_CODE_MELLEL
      TO  P$_BENLEUMI_CODE_MELLEL     IN ICA_TRB711_BNL_TRN_RECORD.

    IF CREDIT_COMPANY_CODE IN ICD_TRT_670_CUR_CRDB
                           IN ICA_TRB711_OUTPUT_670 = V1007$DINERS_COMPANY
       MOVE  L$_DINERS_MOSSAD
         TO  P$_BENLEUMI_SHEM_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
       MOVE  L$_DINERS_MELLEL
         TO  P$_BENLEUMI_SHEM_MELLEL IN ICA_TRB711_BNL_TRN_RECORD
    ELSE
       MOVE  L$_VISA_MOSSAD
         TO  P$_BENLEUMI_SHEM_MOSSAD IN ICA_TRB711_BNL_TRN_RECORD
       MOVE  L$_VISA_MELLEL
         TO  P$_BENLEUMI_SHEM_MELLEL IN ICA_TRB711_BNL_TRN_RECORD
    END-IF.

*   SUG-ZIHUI

    MOVE  L$_SUG_ZIHUI
      TO  P$_BENLEUMI_SUG_ZIHUI       IN ICA_TRB711_BNL_TRN_RECORD.

*   MIS-ZIHUI

    MOVE  L$_MIS_ZIHUI
      TO  P$_BENLEUMI_MIS_ZIHUI       IN ICA_TRB711_BNL_TRN_RECORD.

*   SHEM-LAK-ESEK

    MOVE  40
      TO  P$_LENGTH       IN  ICA_INVERSE_STRING_WKSP.
    MOVE  FLAT_NAME       IN ICA_TRB711_OUTPUT_670
      TO  P$_STRING       IN  ICA_INVERSE_STRING_WKSP.
    CALL  'ICA_ICF_INVERSE_STRING'  USING  ICA_INVERSE_STRING_WKSP.
    MOVE  P$_STRING       IN  ICA_INVERSE_STRING_WKSP
      TO  FLAT_NAME       IN ICA_TRB711_OUTPUT_670.

    MOVE  FLAT_NAME          IN ICA_TRB711_OUTPUT_670 (25:16)
      TO  P$_BENLEUMI_SHEM   IN ICA_TRB711_BNL_TRN_RECORD.

*   PIRTEI-ASMACTA
*   --------------
*   ASMACTA-FILLER-X2

    MOVE  SPACES
      TO  P$_BENLEUMI_FILLER_X2       IN ICA_TRB711_BNL_TRN_RECORD.

*   ASMACTA-LAMED-VIZA

    MOVE  PROD_EXT_ID                 IN ICD_TRT_670_CUR_CRDB
                                      IN ICA_TRB711_OUTPUT_670
      TO  L$_PROD_EXT_ID.

    IF  PROD_TYPE_CODE                IN ICD_TRT_670_CUR_CRDB
                                      IN ICA_TRB711_OUTPUT_670
        = V300$BUSINESS  OR = V300$BUSINESS_AMEX
        MOVE  ALL ZEROS           TO  L$_PROD_EXT_ID_07
        MOVE  L$_PROD_EXT_ID_013  TO  P$_BENLEUMI_LAMED_VISA
                                      IN ICA_TRB711_BNL_TRN_RECORD
    ELSE          
        MOVE  L$_PROD_EXT_ID_13   TO  P$_BENLEUMI_LAMED_VISA
                                      IN ICA_TRB711_BNL_TRN_RECORD
    END-IF.

*#1001 Start.
*    MOVE  EXT_ID_NUM_TYPE_CODE     IN ICD_TRT_670_CUR_CRDB
*                                   IN ICA_TRB711_OUTPUT_670
*      TO P$_BENLEUMI_TYPE_CODE.
 
    MOVE  EXT_ID_NUM_TYPE_CODE     IN ICD_TRT_670_CUR_CRDB
                                   IN ICA_TRB711_OUTPUT_670
      TO  L$_EXT_ID_NUM_TYPE_CODE.

    MOVE  L$_EXT_ID_NUM_TYPE_CODE     
      TO  P$_BENLEUMI_TYPE_CODE.

*#1001 End.

    MOVE  PROD_EXT_ID              IN ICD_TRT_670_CUR_CRDB
                                   IN ICA_TRB711_OUTPUT_670
*#1023 Start.
*      TO P$_PROD_EXT_ID.
      TO P$_PROD_EXT_ID            IN ICA_TRB711_BNL_TRN_RECORD.
*#1023 End.

*   ASMACTA-FILLER-1

    MOVE  L$_ASMACTA_CODE
      TO  P$_BENLEUMI_FILLER_1      IN ICA_TRB711_BNL_TRN_RECORD.

*   ASMACTA-SUG-PEULA

    IF  DEB_CRD_CODE                IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
        = V669$CREDIT_SIDE
        MOVE  L$_CREDIT_PEULA
          TO  P$_BENLEUMI_SUG_PEULA IN ICA_TRB711_BNL_TRN_RECORD
    ELSE
        IF  DEB_CRD_CODE            IN ICD_TRT_670_CUR_CRDB
                                    IN ICA_TRB711_OUTPUT_670
            = V669$DEBIT_SIDE
            MOVE  L$_DEBIT_PEULA
              TO  P$_BENLEUMI_SUG_PEULA IN ICA_TRB711_BNL_TRN_RECORD
        ELSE
	    MOVE  SP$_MSG_ERROR
	      TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
            MOVE 'םיאתמ וניא הלועפ גוס SE:D-BUILD-BENLEUM '
              TO  SP$_ACW_FREE_TEXT        IN UTL_CONTROL_ACW(1:40)
            MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
            PERFORM  Z-FINISH
        END-IF
    END-IF.

*   ASMACTA-TAARICH-PEULA = YYMMDD

*------------
    MOVE 0 TO SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
    MOVE VALUE_DATE		IN ICD_TRT_670_CUR_CRDB
                                IN ICA_TRB711_OUTPUT_670 TO
         SP$_DATE_TIME_BINARY OF UTL_CVT_DATE_DDMMYYYY_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY' USING UTL_CVT_DATE_DDMMYYYY_WKSP.
    IF  SP$_SYSPRO_STATUS  IN UTL_CVT_DATE_DDMMYYYY_WKSP IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL  IN UTL_CVT_DATE_DDMMYYYY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DATE_YYMMDD SE:D-BUILD-BENL '
                         TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:39)
        MOVE L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(41:28)
        DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
        PERFORM  Z-FINISH
    END-IF.
    
    INSPECT SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
            REPLACING ALL " " BY "0".
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
      TO L$_VALUE_DATE(7:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
      TO L$_VALUE_DATE(5:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
      TO L$_VALUE_DATE(1:4).

*Y2K
    IF L$_VALUE_DATE <  L$_DATE_YYMMDD_27
        MOVE  L$_DATE_YYMMDD_27(3:6)
          TO  P$_BENLEUMI_TAARICH_PEULA IN ICA_TRB711_BNL_TRN_RECORD
    ELSE
        MOVE  L$_VALUE_DATE(3:6)
          TO  P$_BENLEUMI_TAARICH_PEULA IN ICA_TRB711_BNL_TRN_RECORD
    END-IF.

*   TKUFA

    MOVE  SPACES
      TO  P$_BENLEUMI_TKUFA           IN ICA_TRB711_BNL_TRN_RECORD.

*   FILLER-12

    MOVE  ZEROS 
      TO  P$_BENLEUMI_FILLER_12       IN ICA_TRB711_BNL_TRN_RECORD.

*   TAARICH-RIBIT = YYMMDD
*Y2K
    MOVE  L$_DATE_YYMMDD_TODAY(3:6)
      TO  P$_BENLEUMI_TAARICH_RIBIT   IN ICA_TRB711_BNL_TRN_RECORD.

*   FILLER-2

    MOVE  ZEROS 
      TO  P$_BENLEUMI_FILLER_2        IN ICA_TRB711_BNL_TRN_RECORD.

*# טלחומ ךרעל םוכס תרבעה

    MOVE TOT_XFER_AMT IN ICD_TRT_670_CUR_CRDB
	 	      IN ICA_TRB711_OUTPUT_670 TO L$_SCHUM.

    IF P$_BENLEUMI_SUG_PEULA IN ICA_TRB711_BNL_TRN_RECORD = L$_DEBIT_PEULA
       ADD L$_SCHUM TO L$_DEB_SCHUM_BANK
       ADD 1 TO L$_DEB_TNUOT_BANK
* Change #2001 -Start.
       ADD L$_SCHUM TO L$_TOTAL_DEB_AMT
* Change #2001 -End.
    ELSE
       ADD L$_SCHUM TO L$_CRD_SCHUM_BANK
       ADD 1 TO L$_CRD_TNUOT_BANK
* Change #2001 -Start.
       ADD L$_SCHUM TO L$_TOTAL_CRD_AMT
* Change #2001 -End.
    END-IF.

*   העונת תמושר תביתכ
    WRITE  BENLEUMI_REC  FROM  ICA_TRB711_BNL_TRN_RECORD.

    ADD 1 TO L$_RECORD_COUNTER.

E-EXIT.
     EXIT.



F-BUILD-BENLEUMI-SIUM-DATA SECTION.
*#-----------------------------------------------------------------------------
*# Section: F-BUILD-BENLEUMI-SIUM-DATA.
*# Description:
*#             -----------------------------------------
*#             ימואלניבה קנבל - הביתכו םויס תמושר תכירע
*#             -----------------------------------------  
*#
*#-----------------------------------------------------------------------------
F-00.

*   ימואלניבה קנבל - םויס תמושר תכירע
*   -----------------------------------
    INITIALIZE  BENLEUMI_REC
                ICA_TRB711_BNL_TRAILER_RECORD.

*   SUG-RESHUMA

    MOVE  L$_SUG_RESHUMA_SIUM
      TO  P$_BENLEUMI_SUG_RESHUMA   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   CODE-MOSSAD

    MOVE  L$_CODE_MOSSAD
      TO  P$_BENLEUMI_CODE_MOSSAD   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   CODE-BANK
* Start Change #2000.
*    MOVE  L$_CODE_BANK
*      TO  P$_BENLEUMI_BANK          IN ICA_TRB711_BNL_TRAILER_RECORD.

    IF ACC_BANK_NUM           IN ICD_TRT_670_CUR_CRDB
                              IN ICA_TRB711_OUTPUT_670 = V327$KLALI
    THEN
        MOVE L$_CODE_BANK_UBANK
          TO P$_BENLEUMI_BANK IN ICA_TRB711_BNL_TRAILER_RECORD

    ELSE

        MOVE L$_CODE_BANK_BNL
          TO P$_BENLEUMI_BANK IN ICA_TRB711_BNL_TRAILER_RECORD

    END-IF

* End Change #2000.

*   TAARICH-MAHALACH = YYMMDD
*Y2K
    MOVE  L$_DATE_YYMMDD_INTEGR(3:6)
      TO  P$_BENLEUMI_TAR_MAHALACH  IN ICA_TRB711_BNL_TRAILER_RECORD.

*   TAARICH-SERET = YYMMDD
*Y2K
    MOVE  L$_DATE_YYMMDD_INTEGR(3:6)
      TO  P$_BENLEUMI_TAARICH_SERET    IN ICA_TRB711_BNL_TRAILER_RECORD.

*   CODE-SIDURI
    MOVE  L$_CODE_SIDURI
      TO  P$_BENLEUMI_CODE_SIDURI   IN ICA_TRB711_BNL_TRAILER_RECORD.

    ADD 1 TO L$_CODE_SIDURI.

*   SUG-SERET
    MOVE  L$_SUG_SERET
      TO  P$_BENLEUMI_SUG_SERET     IN ICA_TRB711_BNL_TRAILER_RECORD.

*   SCHUM-ZIKUI
    MOVE  L$_CRD_SCHUM_BANK  
      TO  P$_BENLEUMI_SCHUM_ZIKUI   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   SCHUM-CHIUV

    MOVE  L$_DEB_SCHUM_BANK  
      TO  P$_BENLEUMI_SCHUM_CHIUV   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   TNUOT-ZIKUI

    MOVE  L$_CRD_TNUOT_BANK  
      TO  P$_BENLEUMI_TNUOT_ZIKUI   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   TNUOT-CHIUV

    MOVE  L$_DEB_TNUOT_BANK  
      TO  P$_BENLEUMI_TNUOT_CHIUV   IN ICA_TRB711_BNL_TRAILER_RECORD.

*   ימואלניבה קנבל - םויס תמושר הביתכ
    WRITE BENLEUMI_REC  FROM  ICA_TRB711_BNL_TRAILER_RECORD.

    ADD 1 TO L$_RECORD_COUNTER.

    CLOSE  ICA_BANK_BENLEUMI_FILE.

    IF L$_SW_ATIDI
       PERFORM  H-REPORT-ATIDI
    END-IF.

* Change #2001 -Start.
* Change #2002 -Start.
*   PERFORM  G-WRITE-OUTPUT-FILE.

    IF (L$_TOTAL_CRD_AMT = L$_TOTAL_DEB_AMT)  AND
       (L$_RECORD_COUNTER > ZEROES)
    THEN
       PERFORM G-WRITE-OUTPUT-FILE
    ELSE
       DISPLAY "Attantion! Empty or unbalanced file."
       DISPLAY "File ",L$_BANK_BENLEUMI_NAME ," was not registered." 
       DISPLAY "L$_TOTAL_CRD_AMT :",L$_TOTAL_CRD_AMT
       DISPLAY "L$_TOTAL_DEB_AMT :",L$_TOTAL_DEB_AMT
       DISPLAY "L$_RECORD_COUNTER:",L$_RECORD_COUNTER

       PERFORM M-SEND-A-MESSAGE
    END-IF .
* Change #2002 -End.
* Change #2001 -End.

F-EXIT.
     EXIT.

* Change #1009 - Start.
*------------------------------
EB-GET-ACC-TYPE-CODE   SECTION.
*------------------------------
*# Section: EB-GET-ACC-TYPE-CODE   
*# Purpose: Get type code . 
*# Description: 1. Read from ctt tab 316 by bank and type for balal
*#-----------------------------------------------------------------------------
EB-00.
********

    MOVE ACC_BANK_NUM  IN ICD_TRT_670_CUR_CRDB 
                       IN ICA_TRB711_OUTPUT_670
      TO ACC_BANK_NUM  IN ICD_PRT_316_ACC_TYPES_PRW

    MOVE ACC_TYPE_CODE IN ICD_TRT_670_CUR_CRDB 
                       IN ICA_TRB711_OUTPUT_670
      TO ACC_TYPE_CODE IN ICD_PRT_316_ACC_TYPES_PRW

       ADD DP$_SYM_INQUIRE 
        TO DP$_SYM_ACCESS_CTT 
    GIVING DP$_ACTION     IN ICD_PRT_316_ACC_TYPES_DBW
    MOVE 0 
      TO DP$_KEY_SEQUENCE IN ICD_PRT_316_ACC_TYPES_DBW

    CALL 'ICD_PRT_316_ACC_TYPES_DBA' USING ICD_PRT_316_ACC_TYPES_DBW

    IF DP$_STATUS IN ICD_PRT_316_ACC_TYPES_DBW = SP$_MSG_NORMAL
       IF FICTIVE_TYPE_IND IN ICD_PRT_316_ACC_TYPES 
                           IN ICD_PRT_316_ACC_TYPES_DBW = V$_HEB_YES
          MOVE  ORIGINAL_ACC_TYPE_CODE       IN ICD_PRT_316_ACC_TYPES
		 			     IN ICD_PRT_316_ACC_TYPES_DBW
            TO  P$_BENLEUMI_SUG_CHESBON      IN ICA_TRB711_BNL_TRN_RECORD
       ELSE
          MOVE  ACC_TYPE_CODE                IN ICD_TRT_670_CUR_CRDB 
		 			     IN ICA_TRB711_OUTPUT_670
            TO  P$_BENLEUMI_SUG_CHESBON      IN ICA_TRB711_BNL_TRN_RECORD
       END-IF
    END-IF.

*-----------------
EB-EXIT.
         EXIT.
*-----------------
* Change #1009 - End.
* Change #1023 - Start.
*--------------------------------------------------
GD-GET-CURRENCY-CODE-674		    SECTION.
*--------------------------------------------------
GD-00.


    MOVE ICD_TRT_674_CURNCY_TYP_INIT
      TO ICD_TRT_674_CURNCY_TYP_OPW.

    MOVE DEB_CRD_CURRENCY_CODE IN ICD_TRT_670_CUR_CRDB  
                               IN ICA_TRB711_OUTPUT_670
      TO       CURRENCY_CODE   IN ICD_TRT_674_CURNCY_TYP_PRW 
	   	               IN ICD_TRT_674_CURNCY_TYP_MPW.

    MOVE "EQ" 
      TO       CURRENCY_CODE_$ IN ICD_TRT_674_CURNCY_TYP_OPW.

    MOVE ICD_TRT_674_CURNCY_TYP_OPW 
      TO DP$_OPC_IN_VEC      IN UTL_DBA_UNIV_WKSP 
			     IN ICD_TRT_674_CURNCY_TYP_MPW.

    MOVE DP$_SYM_FETCH_NEXT_PAGE   
      TO DP$_ACTION          IN ICD_TRT_674_CURNCY_TYP_MPW.
     ADD DP$_SYM_ACCESS_CTT_FILTER 
      TO DP$_ACTION          IN ICD_TRT_674_CURNCY_TYP_MPW.
    MOVE ZERO 	
      TO DP$_KEY_SEQUENCE    IN ICD_TRT_674_CURNCY_TYP_MPW.
    MOVE ZERO         
      TO CT_PAGE_PTR         IN ICD_TRT_674_CURNCY_TYP_CTW.

    CALL 'ICD_TRT_674_CURNCY_TYP_MPQ' USING ICD_TRT_674_CURNCY_TYP_MPW.

    EVALUATE DP$_STATUS      IN ICD_TRT_674_CURNCY_TYP_MPW
 	WHEN SP$_MSG_NORMAL
	WHEN SP$_MSG_END_OF_DATA_SET
             IF CURR_CONV_SRC_IND IN ICD_TRT_674_CURNCY_TYP
                                  IN ICD_TRT_674_CURNCY_TYP_MPW(1) NOT = V$_HEB_YES
               IF BNL_BANK_CURRENCY_CODE IN ICD_TRT_674_CURNCY_TYP
			                 IN ICD_TRT_674_CURNCY_TYP_MPW(1) = 0
                  
	          SET L$_SW_BANK_CURRENCY_IND TO TRUE  
                  PERFORM  L-SEND-A-MESSAGE
	          MOVE  SP$_MSG_ERROR
	            TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
                  PERFORM  Z-FINISH
               ELSE
	          SET L$_SW_NO_CURR_CONV_SRC_IND TO TRUE  
                  PERFORM  L-SEND-A-MESSAGE
	       END-IF
             ELSE 
               IF BNL_BANK_CURRENCY_CODE IN ICD_TRT_674_CURNCY_TYP
			                 IN ICD_TRT_674_CURNCY_TYP_MPW(1) = 0
                  
	          SET L$_SW_BANK_CURRENCY_CODE TO TRUE  
                  PERFORM  L-SEND-A-MESSAGE
	          MOVE  SP$_MSG_ERROR
	            TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
                  PERFORM  Z-FINISH
               ELSE 
                  CONTINUE
               END-IF                     
             END-IF
        WHEN OTHER
	      MOVE  SP$_MSG_ERROR
	        TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
              MOVE  DEB_CRD_CURRENCY_CODE    IN ICD_TRT_670_CUR_CRDB
                                             IN ICA_TRB711_OUTPUT_670
                TO  SP$_ACW_FREE_TEXT        IN UTL_CONTROL_ACW(1:2)
              MOVE 'יקוח אל עבטמ SEC:C-ONE-RECORD '
                TO  SP$_ACW_FREE_TEXT        IN UTL_CONTROL_ACW(4:41)
              MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(42:28)
           PERFORM  L-SEND-A-MESSAGE
           PERFORM  Z-FINISH
    END-EVALUATE.


GD-EXIT.
    EXIT.

L-SEND-A-MESSAGE SECTION.
*#-----------------------------------------------------------------------------
*# Section: L-SEND-A-MESSAGE 
*# Purpose: .
*# Description:
*#
*#-----------------------------------------------------------------------------
L-00.


    MOVE INIT_TRB711_MESSAGE_WKSP
      TO ICA_MSG_RMS.

    MOVE "מזהה ח/ז"
      TO P$_FILLER08            IN ICA_MSG_RMS.   

    MOVE DEB_CRD_INTR_ID        IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670
      TO P$_ICP_TRN_INT_ID      IN ICA_MSG_RMS.           


    MOVE "בנק"
      TO P$_VIC_FILLER_5        IN ICA_MSG_RMS.   

    MOVE ACC_BANK_NUM           IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_BANK            IN ICA_MSG_RMS.           

    MOVE "סניף"
      TO P$_VIC_FILLER_4_4      IN ICA_MSG_RMS.   

    MOVE ACC_BRANCH_NUM         IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_BRANCH          IN ICA_MSG_RMS.   

    MOVE "סוג חשבון"
      TO P$_VIC_FILLER_10       IN ICA_MSG_RMS.   

    MOVE ACC_TYPE_CODE          IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_ACC_TYPE        IN ICA_MSG_RMS.   

    MOVE "מטבע"
      TO P$_VIC_FILLER_4_3      IN ICA_MSG_RMS.   

    MOVE DEB_CRD_CURRENCY_CODE  IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_CURRENCY        IN ICA_MSG_RMS.   

    MOVE "חשבון"
      TO P$_FILLER5             IN ICA_MSG_RMS.   

    MOVE ACC_NUM                IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_ACC             IN ICA_MSG_RMS.   

    MOVE "מוצר"
      TO P$_VIC_FILLER_4_2      IN ICA_MSG_RMS.   

    MOVE PROD_EXT_ID            IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_PROD_EXT_ID         IN ICA_MSG_RMS.   

    MOVE "חברת אשראי"
      TO P$_FILLER_X12          IN ICA_MSG_RMS.   

    MOVE CREDIT_COMPANY_CODE    IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_DIS_CREDIT_COMPANY  IN ICA_MSG_RMS.   


    EVALUATE TRUE

      WHEN L$_SW_DATE_ZERO
        MOVE "^ תאריך ערך שווה אפס מועבר תאריך סימוכין  " 
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS


      WHEN L$_SW_NO_ACCUM
        MOVE "^ חשבון לא קיים בטבלה 677  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_SIMUCHIN
        MOVE "^ אין סימוכין פתוח  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_CAL_ACC
        MOVE "^אין רשומה בטבלה 666 או 667"
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_668_REC
        MOVE "^מזהה חיוב זיכוי לא קיים בטבלת 668"
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_STOPPED_AMT
        MOVE "^ סכום עצירה שגוי   "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_737_669
        MOVE "^ אין מזהה מקור בטבלה 346"
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_CURRENCY_CODE
        MOVE "^ מטבי העצירה שונה ממטבע המיני  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_STOPPED_AMT_NO_IND
        MOVE "^ אין אנידקציה לעצירה ויש סום לעצירה  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_CURRENCY_CODE
        MOVE "^אין שער המרה למטבע    ליום ערך "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_132_REC

        MOVE L$_MSG_TEXT	
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_947_REC

        MOVE L$_MSG_TEXT	
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_CURRENCY_CODE
        MOVE "^ מטבי העצירה שונה ממטבע המיני  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_DIF_CURRENCY_CODE
        MOVE "^ מטבי העצירה ומטבע ח/ז לא מתאימים  "
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_NO_CURR_CONV_SRC_IND

        MOVE "^ אינדיקציה חסרה בטבלה 674"	
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_BANK_CURRENCY_CODE
        MOVE "^  קוד מטבע חסרים בטבלה 674"
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

      WHEN L$_SW_BANK_CURRENCY_IND
        MOVE "^ קוד מטבע ואינדיקציה חסרים בטבלה 674"
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS
      WHEN OTHER

        MOVE SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
	  TO P$_FREE_TEXT_1  IN ICA_MSG_RMS

    END-EVALUATE.
        
    OPEN EXTEND ICA_MSG_OUTPUT_FILE .
	WRITE	ICA_MSG_RMS.
    CLOSE       ICA_MSG_OUTPUT_FILE .


*#-----------
L-EXIT.
      EXIT.
*#-----------

* Change #1023 - End.

* Change #2001 -Start.
*#-----------------------------------------------------------------------------
M-SEND-A-MESSAGE SECTION.
*#-----------------------------------------------------------------------------
M-00.

    MOVE INIT_TRB711_MESSAGE_WKSP
      TO ICA_MSG_RMS.

    MOVE "בנק"
      TO P$_VIC_FILLER_5        IN ICA_MSG_RMS.   

    MOVE ACC_BANK_NUM           IN ICD_TRT_670_CUR_CRDB
		                IN ICA_TRB711_OUTPUT_670 
      TO P$_INT_BANK            IN ICA_MSG_RMS.           

    MOVE "^ לא נשלח קובץ לבנק, קובץ לא מאוזן"
      TO P$_FREE_TEXT_1  IN ICA_MSG_RMS(1:34)
    MOVE L$_BANK_BENLEUMI_NAME
      TO P$_FREE_TEXT_1  IN ICA_MSG_RMS(36:35).

    OPEN EXTEND ICA_MSG_OUTPUT_FILE .
    WRITE ICA_MSG_RMS.
    CLOSE ICA_MSG_OUTPUT_FILE .

M-EXIT.
      EXIT.
* Change #2001 -End.

H-REPORT-ATIDI        SECTION.
*-------------------------------------
*# Section: H-REPORT-ATIDI
*# Purpose: תחילש
*# Description: הניטור י"ע םיאצוי םיצבקב ץבוקה םושיר
*#-----------------------------------------------------------------------------
H-00.

    DISPLAY " תידיתע היצרגטניא TR5822 חוד הליחתה ". 

    MOVE "TR5822" TO ENTITY_CODE IN ICA_SVF_APTUSER_CALL_WKSP.

    MOVE L$_BANK_BENLEUMI_NAME(16:17) TO L$_FILE_NAME.

    MOVE "APT:CONSTANT_FIELDS::VAL_FILE" TO PRM_ENT_NAME
                                            IN ICA_SVF_APTUSER_CALL_WKSP(1).

    MOVE "EQ" TO PARM_OPERATOR IN ICA_SVF_APTUSER_CALL_WKSP(1).

    MOVE L$_FILE_NAME TO TEXT_PRM_VALUE_1 IN ICA_SVF_APTUSER_CALL_WKSP(1).

    PERFORM DFA-CALL-APTUSER-REPORTS.
    DISPLAY "!!! TR5822 תינכות המייתסה ". 

H-EXIT.
     EXIT.

*--------------------------------------
DFA-CALL-APTUSER-REPORTS     SECTION.
*--------------------------------------
*# Section: DFA-CALL-APTUSER-REPORTS             
*# Purpose: Handle REPORTS by APTUSER.
*# Description: 1. 
*#-----------------------------------------------------------------------------
DFA-00.
    MOVE OPER_ID IN ICA_TRJ711_JOB_DATA_INW TO L$_OPER_ID.
    MOVE L$_OPER_ID TO OPER_ID IN ICA_SVF_APTUSER_CALL_WKSP.
    SET P$_APT_TYPE_FILE IN ICA_SVF_APTUSER_CALL_WKSP TO TRUE.
    MOVE 1 TO P$_PARAM_NUM IN ICA_SVF_APTUSER_CALL_WKSP.
    SET P$_APT_NO_PRINT IN ICA_SVF_APTUSER_CALL_WKSP TO TRUE.
    SET P$_APT_PRINT_QUEUE IN ICA_SVF_APTUSER_CALL_WKSP TO TRUE.

    CALL 'ICA_SVF_APTUSER_CALL' USING ICA_SVF_APTUSER_CALL_WKSP 
				      UTL_CONTROL_ACW.

    IF SP$_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW IS FAILURE
       MOVE "SEC:DFA-CALL-APTUSER-REPORTS RTN:ICA_SVF_APTUSER_CALL PAR:"
                                   TO SP$_ACW_FREE_TEXT(1:60)
       MOVE ENTITY_CODE OF ICA_SVF_APTUSER_CALL_WKSP
                                   TO SP$_ACW_FREE_TEXT(60:6)
       DISPLAY "!!! TR5822 תינכותב היעב בל םיש "
       DISPLAY SP$_ACW_FREE_TEXT
    END-IF.

*-------------
DFA-EXIT.
        EXIT.
*-------------


G-WRITE-OUTPUT-FILE        SECTION.
*-------------------------------------
*# Section: G-WRITE-OUTPUT-FILE    
*# Purpose: Write output files by FILEOUT_INSERT.
*# Description: הניטור י"ע םיאצוי םיצבקב ץבוקה םושיר
*#-----------------------------------------------------------------------------
G-00.

    INITIALIZE ICA_SVM_FILEOUT_INSERT_WKSP.

    IF L$_SW_VISA
* Start Change #2000.

*       IF CURRENCY_IND = "IL"
    THEN
        EVALUATE TRUE

            WHEN CURRENCY_IND = "IL"

                MOVE V072$BENLEUMI_INTGR
                  TO FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP

            WHEN CURRENCY_IND = "JA"

                MOVE V072$UBANK_SHEKEL
                  TO FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP

            WHEN CURRENCY_IND = "IY"

                MOVE V072$BENLEUMI_DOLLAR_INTGR
                  TO FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP

            WHEN CURRENCY_IND = "IW"

                MOVE V072$UBANK_DOLLAR
                  TO FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP

        END-EVALUATE

*          MOVE V072$BENLEUMI_INTGR TO
*               FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP
*       ELSE
*          MOVE V072$BENLEUMI_DOLLAR_INTGR TO
*               FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP
*       END-IF
* End Change #2000.

    ELSE
       MOVE V072$BENLEUMI_DLK_INTGR
         TO FILEOUT_TYPE_CODE IN ICA_SVM_FILEOUT_INSERT_WKSP
    END-IF.

    MOVE OPER_ID IN ICA_TRJ711_JOB_DATA_INW TO
         OPER_ID IN ICA_SVM_FILEOUT_INSERT_WKSP.

    MOVE L$_BANK_BENLEUMI_NAME TO
         FILEOUT_NAME IN ICA_SVM_FILEOUT_INSERT_WKSP.

    MOVE L$_RECORD_COUNTER
      TO TOT_RECS_NUM IN ICA_SVM_FILEOUT_INSERT_WKSP.
    
    CALL 'ICA_SVM_FILEOUT_INSERT' USING ICA_SVM_FILEOUT_INSERT_WKSP
                                        UTL_CONTROL_ACW.

    IF SP$_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW NOT = SP$_MSG_NORMAL
* Change #2001 -Start.
*       MOVE SP$_MSG_ERROR
*	    TO  SP$_ACW_PROC_AUX_STATUS  IN UTL_CONTROL_ACW
*       MOVE 'FILEOUT_INSERT; SEC:G-WRITE-OUTPUT-FILE'
*            TO  SP$_ACW_FREE_TEXT        IN UTL_CONTROL_ACW(1:33)
*       MOVE  L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(35:28)
*       DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
*       PERFORM  Z-FINISH

	PERFORM M-SEND-A-MESSAGE
* Change #2001 -End.
    END-IF.

    MOVE 0 TO L$_RECORD_COUNTER.
*------------
G-EXIT.
       EXIT.
*------------



I-READ-INTGR-INPUT SECTION.
*#-----------------------------------------------------------------------------
*# Section: I-READ-INTGR-INPUT
*# Description:
*#             ---------------
*#             טלק תמושר תאירק
*#       המושרה  תוניקת  תקידב
*#             ---------------
*#
*#-----------------------------------------------------------------------------
I-00.
    
        READ  ICA_INTGR_INPUT_FILE_3
          AT END
          MOVE 9 TO L$_FILE_FLAG
        END-READ.


I-EXIT.
     EXIT.


K-BREAK SECTION.
*#-----------------------------------------------------------------------------
*# Section: K-BREAK
*# Description:
*#             --------
*#             הריבש עטק
*#             --------
*#         
*#-----------------------------------------------------------------------------
K-00.

    IF L$_EOF_FILE
       GO TO K-EXIT
    END-IF.

    MOVE 0 TO SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
    MOVE VALUE_DATE		IN ICD_TRT_670_CUR_CRDB
                                IN ICA_TRB711_OUTPUT_670 TO
         SP$_DATE_TIME_BINARY OF UTL_CVT_DATE_DDMMYYYY_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY' USING UTL_CVT_DATE_DDMMYYYY_WKSP.
    IF  SP$_SYSPRO_STATUS  IN UTL_CVT_DATE_DDMMYYYY_WKSP IS FAILURE
	MOVE  SP$_SYSPRO_STATUS_AUXIL  IN UTL_CVT_DATE_DDMMYYYY_WKSP
	  TO  SP$_ACW_PROC_AUX_STATUS
        MOVE 'RTN:UTL_CVT_DATE_YYMMDD SEC:K-READ' 
                         TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(1:39)
        MOVE L$_MSG_TEXT TO SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW(41:28)
        DISPLAY SP$_ACW_FREE_TEXT IN UTL_CONTROL_ACW
        PERFORM  Z-FINISH
    END-IF.
    
    INSPECT SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP
            REPLACING ALL " " BY "0".
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(1:2)
      TO L$_VALUE_DATE_CURRENT(7:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(4:2)
      TO L$_VALUE_DATE_CURRENT(5:2).
    MOVE SP$_DATE_DDMMYYYY IN UTL_CVT_DATE_DDMMYYYY_WKSP(7:4)
      TO L$_VALUE_DATE_CURRENT(1:4).

*Y2K
    IF L$_VALUE_DATE_CURRENT > L$_DATE_YYMMDD_CHECK
       MOVE L$_VALUE_DATE_CURRENT TO L$_DATE_YYMMDD_CHECK
       MOVE 6 TO L$_FILE_FLAG
    END-IF.

* Change #1009 - Start.
*    IF ((DEB_CRD_CURRENCY_CODE IN ICD_TRT_670_CUR_CRDB 
*                               IN ICA_TRB711_OUTPUT_670 = V674$SHEKEL) 
*        AND (CURRENCY_IND IN L$_BANK_BENLEUMI_NAME = "IY")) OR
*           ((DEB_CRD_CURRENCY_CODE IN ICD_TRT_670_CUR_CRDB 
*                                   IN ICA_TRB711_OUTPUT_670 = V674$DOLLAR) 
*        AND (CURRENCY_IND IN L$_BANK_BENLEUMI_NAME = "IL"))
*
*       MOVE 6 TO L$_FILE_FLAG
*    END-IF.
* Change #1009 - End.

K-EXIT.
     EXIT.



Z-FINISH SECTION.
*#-----------------------------------------------------------------------------
*# Section: Z-FINISH
*# Description:
*#             --------
*#             םויס עטק
*#             --------
*#         
*#-----------------------------------------------------------------------------
Z-00.

    IF  SP$_ACW_PROC_AUX_STATUS   IN UTL_CONTROL_ACW   = SP$_MSG_NORMAL
        MOVE  L$_BANK_BENLEUMI_NAME
          TO  P$_TRB714_FILE_NAME IN ICA_TRJ711_JOB_DATA_INW
    END-IF.

    CLOSE  ICA_INTGR_INPUT_FILE_3.



*# 13-NOV-2012 18:29:23.17 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.

Z-EXIT.
     EXIT.
                                                                                            
