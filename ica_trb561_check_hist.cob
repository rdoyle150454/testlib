*############# PROGRAM_NAME:  [ICA_TRB561_CHECK_HIST.COB] #####################* 
*#                                                                            #*           
*#                                                              TR : תכרעמ תת #*           
*#                                         [ICA_TRB561_CHECK_HIST] :לודומה םש #*     
*#                                                                            #*     
*#                                                                            #*           
*#                                                           :ילנויצקנופ רואת #*           
*#                                  הירוטסהה ילולסמל תויטמוטוא תוקידב תינכות  #*
*#                         הירוטסההמ הפילש תלעפהל םירטמרפ םילבקתמ טלקה ץבוקב  #*
*#     1. הלבטב תושייה ןוכדע 6060 הנשיה הטישב הצירל                         #*
*#     2. ץבוקב תואצותה תרימשו הפילשה תלעפה ,הפילשל טלקה ינותנ תרבעה          #*
*#     3. הלבטב תושייה ןוכדע 6060 השדחה הטישב הצירל                         #*
*#     4. ץבוקב תואצותה תרימשו הפילשה תלעפה ,הפילשל טלקה ינותנ תרבעה          #*
*#     5. םיטלפה תאוושהל תינכת תלעפהל,םיסנכנ םיצבקל הלעפהה ההזמ םע ץבוק םושיר #*
*#                                                                            #*
*#                                               תרציימ תינכתה 2 טלפ יצבק:  #*
*#           1. טלקב המושר לכל הירוטסההמ ופלשנש תומושרה לכ תא ליכיש ץבוק      #*
*#           2. טלקב המושר לכל ופלשנש תומושרה תומכ תא ליכיש ץבוק              #*
*#                                                                            #*     
*#                                                                    :םיחתפמ #*                          
*#                                                TP_NHELER   20-OCT-2013     #*       
*#                                                                            #*     
*#                                                                            #*      
*#                                                     :םייונישו םינוכדע בקעמ #*           
*#                                   יונישה רואת     עצבמ םש     ךיראת   CID  #*           
*#                                                                            #*    
*##############################################################################*           

*###################################################################*           
IDENTIFICATION	 	 	 	     DIVISION.
*###################################################################*           
*====================================================================
PROGRAM-ID. ICA_TRB561_CHECK_HIST.
                                                            
AUTHOR. TP_NHELER.                                                             

*###################################################################*           
ENVIRONMENT	 	 	 	     DIVISION.
*###################################################################*           
*-----------------------
INPUT-OUTPUT	SECTION.
*-----------------------
FILE-CONTROL.

    SELECT I_CHECK_FILE     
           ASSIGN TO FILE_A
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS SP$_COBRMS_FILE_STATUS
           RESERVE 10 AREAS.
              
    SELECT O_DATA_FILE
           ASSIGN TO FILE_A
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS SP$_COBRMS_FILE_STATUS
           RESERVE 10 AREAS.           

    SELECT O_COUNT_FILE
           ASSIGN TO FILE_A
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS SP$_COBRMS_FILE_STATUS
           RESERVE 10 AREAS.

    SELECT O_OPER_FILE
           ASSIGN TO FILE_A
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS SP$_COBRMS_FILE_STATUS
           RESERVE 10 AREAS.
           
    SELECT O_MAIL_FILE
           ASSIGN TO FILE_A
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE  IS SEQUENTIAL
           FILE STATUS  IS SP$_COBRMS_FILE_STATUS
           RESERVE 10 AREAS.           

*###################################################################*           
DATA	 	 	 	 	     DIVISION.
*###################################################################*           
*---------------
FILE	SECTION.
*---------------                           
FD  I_CHECK_FILE    
    VALUE OF ID IS L$_I_CHECK_FILE_NAME.
    COPY "ICA_CDD_WKSP:ICA_TRB561_IN_WKSP"	FROM DICTIONARY
          REPLACING ICA_TRB561_IN_WKSP  BY I_CHECK_REC.

FD  O_DATA_FILE
    VALUE OF ID IS L$_O_DATA_FILE_NAME.
    COPY "ICA_CDD_WKSP:ICA_TRB561_OUT_WKSP"      FROM DICTIONARY
          REPLACING ICA_TRB561_OUT_WKSP BY O_DATA_REC.

FD  O_COUNT_FILE    
    VALUE OF ID IS L$_O_COUNT_FILE_NAME.
    COPY "ICA_CDD_WKSP:ICA_TRB561_OUT_CNT_WKSP"  FROM DICTIONARY
          REPLACING ICA_TRB561_OUT_CNT_WKSP BY O_COUNT_REC.          
          
FD  O_OPER_FILE    
    VALUE OF ID IS L$_O_OPER_FILE_NAME.
01  O_OPER_REC.
    03 L$_REC           PIC X(100).              

FD  O_MAIL_FILE
    VALUE OF ID IS L$_O_MAIL_FILE_NAME.
01  O_MAIL_REC.
    03 MSG              PIC X(200).
    
*====================================================================
WORKING-STORAGE 	 	 	     SECTION.  
*====================================================================         
COPY "ICA_CDD_WKSP:ICA_TLG_WKSP"             FROM DICTIONARY.

* םיצבק תומש *
*------------*
01 L$_I_CHECK_FILE_NAME.
   03 FILLER                    PIC X(15) VALUE "ICA_TR_DAT_DIR:".
   03 FILLER                    PIC X(14) VALUE "ICA_TRB561_IN_".
   03 L$_OPER_ID                PIC 9(16).
   03 FILLER                    PIC X(4) VALUE ".DAT".
   
01 L$_O_DATA_FILE_NAME.
   03 FILLER                    PIC X(15) VALUE "ICA_TR_DAT_DIR:".
   03 FILLER                    PIC X(15) VALUE "ICA_TRB561_OUT_".
   03 L$_TYPE                   PIC X(4).
      88 L$_OLD_TYPE            VALUE "OLD_".
      88 L$_NEW_TYPE            VALUE "NEW_".
   03 L$_OPER_ID                PIC 9(16).
   03 FILLER                    PIC X(4) VALUE ".DAT".
   
01 L$_O_COUNT_FILE_NAME.
   03 FILLER                    PIC X(15) VALUE "ICA_TR_DAT_DIR:".
   03 FILLER                    PIC X(15) VALUE "ICA_TRB561_OUT_".
   03 L$_TYPE                   PIC X(4).
      88 L$_OLD_TYPE            VALUE "OLD_".
      88 L$_NEW_TYPE            VALUE "NEW_".
   03 FILLER                    PIC X(4) VALUE "CNT_".
   03 L$_OPER_ID                PIC 9(16).
   03 FILLER                    PIC X(4) VALUE ".DAT".
   
01 L$_O_OPER_FILE_NAME.
   03 FILLER                    PIC X(15) VALUE "ICA_TR_DAT_DIR:".
   03 FILLER                    PIC X(16) VALUE "ICA_TRB561_OPER_".
   03 L$_OPER_ID                PIC 9(16).
   03 FILLER                    PIC X(4) VALUE ".DAT".
   
01 L$_O_MAIL_FILE_NAME.
   03 FILLER                    PIC X(15) VALUE "ICA_TR_DAT_DIR:".
   03 FILLER                    PIC X(16) VALUE "ICA_TRB561_MAIL_".
   03 L$_OPER_ID                PIC 9(16).
   03 FILLER                    PIC X(4) VALUE ".DAT".

COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_BINARY_WKSP"  	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_WKSP"       	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_WKSP"       	FROM DICTIONARY
      REPLACING UTL_CVT_DATE_DDMMYYYY_WKSP BY INIT_CVT_DATE_DDMMYYYY_WKSP.
             
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_DBW"                     FROM DICTIONARY
    REPLACING ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_DBW"                     FROM DICTIONARY
    REPLACING ICD_TRN_HGQ_DBW
           BY ICD_TRN_HGQ_DBW_INIT
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRT_6060_DB_DCISION_DBW"         FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TRB561_FILTER_WKSP"              FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICA_CDD_WKSP:ICA_TRB561_FILTER_WKSP"              FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==
              ==ICA_TRB561_FILTER_WKSP==
           BY ==ICA_TRB561_FILTER_WKSP_INIT==.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_2_WKSP"        FROM DICTIONARY
    REPLACING ==UTL_CVT_DATE_DDMMYYYY_2_WKSP==
           BY ==INIT_CVT_DATE_DDMMYYYY_2_WKSP==.  
COPY "ICA_CDD_WKSP:ICA_SVM_FILEIN_INSERT_AUTO_WKSP"     FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_TRT_608_IN_OUT_SRC_VAL"          FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVT_022_COD_TBL_41_VAL"          FROM DICTIONARY.


*** לולסמ לכ רובע 
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH01_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH02_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH03_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH04_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH05_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH06_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH07_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH08_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH09_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH10_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH11_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH12_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH13_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH14_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH15_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH16_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH17_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH18_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH19_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH50_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH51_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH52_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH53_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH54_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH55_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH57_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH58_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH63_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH64_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH67_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH68_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH72_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH73_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH74_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH81_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH82_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH83_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH84_WKSP"                     FROM DICTIONARY
    REPLACING ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.


COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH01_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH01_WKSP== BY ==ICD_TRN_HGQ_PATH01_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH02_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH02_WKSP== BY ==ICD_TRN_HGQ_PATH02_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH03_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH03_WKSP== BY ==ICD_TRN_HGQ_PATH03_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH04_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH04_WKSP== BY ==ICD_TRN_HGQ_PATH04_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH05_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH05_WKSP== BY ==ICD_TRN_HGQ_PATH05_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH06_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH06_WKSP== BY ==ICD_TRN_HGQ_PATH06_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH07_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH07_WKSP== BY ==ICD_TRN_HGQ_PATH07_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH08_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH08_WKSP== BY ==ICD_TRN_HGQ_PATH08_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH09_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH09_WKSP== BY ==ICD_TRN_HGQ_PATH09_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH10_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH10_WKSP== BY ==ICD_TRN_HGQ_PATH10_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH11_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH11_WKSP== BY ==ICD_TRN_HGQ_PATH11_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH12_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH12_WKSP== BY ==ICD_TRN_HGQ_PATH12_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH13_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH13_WKSP== BY ==ICD_TRN_HGQ_PATH13_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH14_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH14_WKSP== BY ==ICD_TRN_HGQ_PATH14_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH15_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH15_WKSP== BY ==ICD_TRN_HGQ_PATH15_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH16_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH16_WKSP== BY ==ICD_TRN_HGQ_PATH16_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH17_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH17_WKSP== BY ==ICD_TRN_HGQ_PATH17_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH18_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH18_WKSP== BY ==ICD_TRN_HGQ_PATH18_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH19_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH19_WKSP== BY ==ICD_TRN_HGQ_PATH19_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH50_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH50_WKSP== BY ==ICD_TRN_HGQ_PATH50_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH51_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH51_WKSP== BY ==ICD_TRN_HGQ_PATH51_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH52_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH52_WKSP== BY ==ICD_TRN_HGQ_PATH52_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH53_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH53_WKSP== BY ==ICD_TRN_HGQ_PATH53_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH54_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH54_WKSP== BY ==ICD_TRN_HGQ_PATH54_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH55_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH55_WKSP== BY ==ICD_TRN_HGQ_PATH55_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH57_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH57_WKSP== BY ==ICD_TRN_HGQ_PATH57_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH58_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH58_WKSP== BY ==ICD_TRN_HGQ_PATH58_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH63_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH63_WKSP== BY ==ICD_TRN_HGQ_PATH63_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH64_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH64_WKSP== BY ==ICD_TRN_HGQ_PATH64_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH67_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH67_WKSP== BY ==ICD_TRN_HGQ_PATH67_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH68_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH68_WKSP== BY ==ICD_TRN_HGQ_PATH68_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH72_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH72_WKSP== BY ==ICD_TRN_HGQ_PATH72_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH73_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH73_WKSP== BY ==ICD_TRN_HGQ_PATH73_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH74_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH74_WKSP== BY ==ICD_TRN_HGQ_PATH74_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH81_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH81_WKSP== BY ==ICD_TRN_HGQ_PATH81_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH82_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH82_WKSP== BY ==ICD_TRN_HGQ_PATH82_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH83_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH83_WKSP== BY ==ICD_TRN_HGQ_PATH83_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    
COPY "ICD_CDD_WKSP:ICD_TRN_HGQ_PATH84_WKSP"                     FROM DICTIONARY
    REPLACING ==ICD_TRN_HGQ_PATH84_WKSP== BY ==ICD_TRN_HGQ_PATH84_WKSP_INIT==
              ==VALUE IS== BY ==. 88 DUMMY_REPL VALUE IS==
              ==VALUE IS      0==   BY == ==
              ==VALUE IS SPACES==   BY == ==.    

COPY "ICA_CDD_WKSP:ICA_TRB561_OUT_WKSP"      FROM DICTIONARY
    REPLACING ==ICA_TRB561_OUT_WKSP== BY ==ICA_TRB561_OUT_WKSP_INIT==.

COPY "ICA_CDD_WKSP:ICA_TRB561_OUT_CNT_WKSP"  FROM DICTIONARY
    REPLACING ==ICA_TRB561_OUT_CNT_WKSP== BY ==ICA_TRB561_OUT_CNT_WKSP_INIT==.
    
*#   Include files.
********************
COPY "UTL_SOURCE:UTL_MESSAGE.INC".
COPY "ICA_SOURCE:ICA_MESSAGE.INC".
COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".
COPY "ICD_SOURCE:ICD_CONSTANT_DBA.INC".

*#   UTL copies.
*****************
COPY "UTL_CDD_WKSP:UTL_APPLY_DELTA_TIME_V2_WKSP"      	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_V2_WKSP"       	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_GET_DATE_TIME_WKSP"         	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_COBRMS_VALUE_WKSP"            	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_EXIT_ROUTINE_WKSP"              	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_VMS_MSG_TRANSLATE_WKSP"     	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_RENAME_FILE_WKSP"               	FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_COPY_FILE_V2_WKSP"               FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_SPAWN_PROCESS_WKSP"              FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_PROC_WAIT_WKSP"                  FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DELTA_TIME_WKSP"             FROM DICTIONARY.
COPY "UTL_CDD_WKSP:UTL_CVT_DATE_DDMMYYYY_2_WKSP"        FROM DICTIONARY.   

*#   Dba copies.
*****************
COPY "ICD_CDD_WKSP:ICD_COMMON_DBW"		     	FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVF_HANDLE_OPER_WKSP"          	FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_XLATE_MSG_WKSP"                 	FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_ERROR_HANDLE_WKSP"   		FROM DICTIONARY.

*#   General copies.
*********************
COPY "ICA_CDD_WKSP:ICA_RMS_MSG_WKSP"                 	FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_SVF_PUT_ENTITY_STS_WKSP"         FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_CONNECT_DBW"         		FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_HSF_FETCH_HANDLE_WKSP"         	FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_CONSTANT_VALUES"                 FROM DICTIONARY.

01 ICA_ICF_EXCEPTION_HANDLER		PIC S9(9) COMP
		          VALUE EXTERNAL ICA_ICF_EXCEPTION_HANDLER.

01 SYM$_OPER_ID				PIC 9(16) EXTERNAL.

01 L$_DB_CONN_STATUS			PIC 9 EXTERNAL.
01 L$_DMQ_CONN_STATUS			PIC 9 EXTERNAL.

01 L$_SW_TYPE                           PIC S9(4) COMP.
   88 L$_TYPE_OLD                       VALUE 1.
   88 L$_TYPE_NEW                       VALUE 2.

01 L$_SW_HIST_OUT                       PIC S9(4) COMP. 
   88 L$_END_HIST_OUT                   VALUE 1.
   88 L$_NOT_END_HIST_OUT               VALUE 0.

01 L$_SW_IN_FILE                        PIC S9(4) COMP.  
   88 L$_EOF_IN_FILE                    VALUE 1.
   88 L$_NOT_EOF_IN_FILE                VALUE 0.

*# הייהש ןמז תרדגה - 2 תוקד
01 L$_DELAY     PIC X(23) VALUE "0 00:02:00.00".

COPY "UTL_CDD_FIELD:SP$_DELTA_TIME" FROM DICTIONARY
    REPLACING SP$_DELTA_TIME BY L$_DELTA_TIME.

01 L$_DATE.
   03 L$_DD                             PIC X(2).
   03 FILLER                            PIC X VALUE '/'.
   03 L$_MM                             PIC X(2).
   03 FILLER                            PIC X VALUE '/'.
   03 L$_YYYY                           PIC X(4).
        

COPY "ICD_CDD_FIELD:DEB_CRD_DATE"       FROM DICTIONARY
    REPLACING ==DEB_CRD_DATE==  BY ==L$_FROM_DEB_CRD_DATE==.
COPY "ICD_CDD_FIELD:PURCHASE_DATE"      FROM DICTIONARY
    REPLACING ==PURCHASE_DATE== BY ==L$_FROM_PURCHASE_DATE==.
COPY "ICD_CDD_FIELD:TRN_DEPOSIT_DATE"   FROM DICTIONARY
    REPLACING ==TRN_DEPOSIT_DATE== BY ==L$_FROM_TRN_DEPOSIT_DATE==.
COPY "ICD_CDD_FIELD:DBCR_BANK_TMSP"     FROM DICTIONARY
    REPLACING ==DBCR_BANK_TMSP== BY ==L$_FROM_DBCR_BANK==.            
COPY "ICD_CDD_FIELD:DEB_CRD_DATE"       FROM DICTIONARY
    REPLACING ==DEB_CRD_DATE==  BY ==L$_TO_DEB_CRD_DATE==.
COPY "ICD_CDD_FIELD:PURCHASE_DATE"      FROM DICTIONARY
    REPLACING ==PURCHASE_DATE== BY ==L$_TO_PURCHASE_DATE==.
COPY "ICD_CDD_FIELD:TRN_DEPOSIT_DATE"   FROM DICTIONARY
    REPLACING ==TRN_DEPOSIT_DATE== BY ==L$_TO_TRN_DEPOSIT_DATE==.
COPY "ICD_CDD_FIELD:DBCR_BANK_TMSP"     FROM DICTIONARY
    REPLACING ==DBCR_BANK_TMSP== BY ==L$_TO_DBCR_BANK==.            


01 L$_BATCH                             PIC 9(1) VALUE 2.

01 L$_MSG                               PIC X(200).

01 L$_IN_REC_NUMBER                     PIC 9(7).
01 L$_OUT_REC_NUMBER                    PIC 9(7).



*====================================================================
 LINKAGE 	 	 	 	     SECTION.                           
*====================================================================
01 L$_IN_FILE_NAME      PIC X(100).
COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW" FROM DICTIONARY.
                                                                                
*###################################################################*           
PROCEDURE DIVISION  USING   L$_IN_FILE_NAME,
                            UTL_CONTROL_ACW,
		    GIVING  SP$_ACW_PROC_AUX_STATUS.
*###################################################################*    
DECLARATIVES.
*-----------*
*--------------------------------------------------*
001-I-O-PROBLEM                             SECTION.
*--------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON I_CHECK_FILE.
001.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
    THEN
    
       DISPLAY "I_CHECK_FILE ERROR: " L$_I_CHECK_FILE_NAME
       MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV
       CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)
       SET SP$_EXIT_STATUS_FAILURE TO TRUE
       MOVE "SEC:DECLARATIVES 001-I-O-PROBLEM"  TO  SP$_ACW_FREE_TEXT

*# 13-NOV-2012 16:08:38.76 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE' USING BY  REFERENCE UTL_EXIT_ROUTINE_WKSP

    END-IF.

*--------------------------------------------------*
002-I-O-PROBLEM                             SECTION.
*--------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON O_DATA_FILE.
002.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
    THEN
    
       DISPLAY "O_DATA_FILE ERROR: " L$_O_DATA_FILE_NAME
       MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV
       CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)
       SET SP$_EXIT_STATUS_FAILURE TO TRUE
       MOVE "SEC:DECLARATIVES 002-I-O-PROBLEM"  TO  SP$_ACW_FREE_TEXT

*# 13-NOV-2012 16:08:38.76 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE' USING BY  REFERENCE UTL_EXIT_ROUTINE_WKSP

    END-IF.

*--------------------------------------------------*
003-I-O-PROBLEM                             SECTION.
*--------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON O_COUNT_FILE.
003.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
    THEN
    
       DISPLAY "O_COUNT_FILE ERROR: " L$_O_COUNT_FILE_NAME
       MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV
       CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)
       SET SP$_EXIT_STATUS_FAILURE TO TRUE
       MOVE "SEC:DECLARATIVES 003-I-O-PROBLEM"  TO  SP$_ACW_FREE_TEXT

*# 13-NOV-2012 16:08:38.76 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE' USING BY  REFERENCE UTL_EXIT_ROUTINE_WKSP

    END-IF.

*--------------------------------------------------*
004-I-O-PROBLEM                             SECTION.
*--------------------------------------------------*
    USE AFTER STANDARD ERROR PROCEDURE ON O_OPER_FILE.
004.
****
    IF NOT SP$_COBRMS_SUCCESS IN SP$_COBRMS_FILE_STATUS
    THEN
    
       DISPLAY "O_OPER_FILE ERROR: " L$_O_OPER_FILE_NAME
       MOVE RMS-CURRENT-STS TO P$_RMS_CURRENT_STS
       MOVE RMS-CURRENT-STV TO P$_RMS_CURRENT_STV
       CALL 'ICA_ICF_XLATE_RMS_STS' USING ICA_RMS_MSG_WKSP
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(1)
       DISPLAY MSG_TEXT IN ICA_RMS_MSG_WKSP(2)
       SET SP$_EXIT_STATUS_FAILURE TO TRUE
       MOVE "SEC:DECLARATIVES 004-I-O-PROBLEM"  TO  SP$_ACW_FREE_TEXT

*# 13-NOV-2012 16:08:38.76 - TP_TSARSU - Report to Splunk.
       CALL 'UTL_TERM_PROG_FUNCTION'
       CALL 'UTL_EXIT_ROUTINE' USING BY  REFERENCE UTL_EXIT_ROUTINE_WKSP

    END-IF.

END DECLARATIVES.     
  
*#-----------------------------------------------------------------------------
A-MAIN							    SECTION.
*#-----------------------------------------------------------------------------
*# Section: A-MAIN
*# Purpose: Control program flow.
*# Description:
*#-----------------------------------------------------------------------------
A-00.

    PERFORM B-INIT.
    
    PERFORM C-HANDLE.
    
    PERFORM Z-FINISH.

A-EXIT.
    EXIT.       
        
*#-----------------------------------------------------------------------------
B-INIT					    SECTION.
*#-----------------------------------------------------------------------------
*# Section: B-INIT.
*# Purpose: 
*# Description:
*#-----------------------------------------------------------------------------
B-00.
*# 13-NOV-2012 20:12:03.78 - TP_TSARSU - Move program name to TLG wksp.
    MOVE "ICA_TRB561_CHECK_HIST" 
      TO P$_PROG_NAME IN ICA_TLG_WKSP.

*# 13-NOV-2012 20:12:03.78 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_INIT_PROG.INC".

    DISPLAY "!!! START ICA_TRB561_CHECK_HIST !!! ".

    CALL "UTL_GET_DATE_TIME"    USING UTL_GET_DATE_TIME_WKSP.
    DISPLAY SP$_DATE_TIME_C23   IN UTL_GET_DATE_TIME_WKSP.
    
*   Initialize Exit status.
    SET SP$_EXIT_STATUS_SUCCESS TO TRUE.

    MOVE SP$_MSG_NORMAL					
      TO SP$_ACW_PROC_AUX_STATUS	IN UTL_CONTROL_ACW.

    INITIALIZE ICD_TRN_HGQ_DBW_INIT
               ICA_TRB561_FILTER_WKSP_INIT
               INIT_CVT_DATE_DDMMYYYY_2_WKSP
               ICA_TRB561_OUT_WKSP_INIT
               ICA_TRB561_OUT_CNT_WKSP_INIT.
               
               
    INITIALIZE PATH01_FIELDS    IN ICD_TRN_HGQ_PATH01_WKSP_INIT
               PATH02_FIELDS    IN ICD_TRN_HGQ_PATH02_WKSP_INIT
               PATH03_FIELDS    IN ICD_TRN_HGQ_PATH03_WKSP_INIT
               PATH04_FIELDS    IN ICD_TRN_HGQ_PATH04_WKSP_INIT
               PATH05_FIELDS    IN ICD_TRN_HGQ_PATH05_WKSP_INIT
               PATH06_FIELDS    IN ICD_TRN_HGQ_PATH06_WKSP_INIT
               PATH07_FIELDS    IN ICD_TRN_HGQ_PATH07_WKSP_INIT
               PATH08_FIELDS    IN ICD_TRN_HGQ_PATH08_WKSP_INIT
               PATH09_FIELDS    IN ICD_TRN_HGQ_PATH09_WKSP_INIT
               PATH10_FIELDS    IN ICD_TRN_HGQ_PATH10_WKSP_INIT
               PATH11_FIELDS    IN ICD_TRN_HGQ_PATH11_WKSP_INIT
               PATH12_FIELDS    IN ICD_TRN_HGQ_PATH12_WKSP_INIT
               PATH13_FIELDS    IN ICD_TRN_HGQ_PATH13_WKSP_INIT
               PATH14_FIELDS    IN ICD_TRN_HGQ_PATH14_WKSP_INIT
               PATH15_FIELDS    IN ICD_TRN_HGQ_PATH15_WKSP_INIT
               PATH16_FIELDS    IN ICD_TRN_HGQ_PATH16_WKSP_INIT
               PATH17_FIELDS    IN ICD_TRN_HGQ_PATH17_WKSP_INIT
               PATH18_FIELDS    IN ICD_TRN_HGQ_PATH18_WKSP_INIT
               PATH19_FIELDS    IN ICD_TRN_HGQ_PATH19_WKSP_INIT
               PATH50_FIELDS    IN ICD_TRN_HGQ_PATH50_WKSP_INIT
               PATH51_FIELDS    IN ICD_TRN_HGQ_PATH51_WKSP_INIT
               PATH52_FIELDS    IN ICD_TRN_HGQ_PATH52_WKSP_INIT
               PATH53_FIELDS    IN ICD_TRN_HGQ_PATH53_WKSP_INIT
               PATH54_FIELDS    IN ICD_TRN_HGQ_PATH54_WKSP_INIT
               PATH55_FIELDS    IN ICD_TRN_HGQ_PATH55_WKSP_INIT
               PATH57_FIELDS    IN ICD_TRN_HGQ_PATH57_WKSP_INIT
               PATH58_FIELDS    IN ICD_TRN_HGQ_PATH58_WKSP_INIT
               PATH63_FIELDS    IN ICD_TRN_HGQ_PATH63_WKSP_INIT
               PATH64_FIELDS    IN ICD_TRN_HGQ_PATH64_WKSP_INIT
               PATH67_FIELDS    IN ICD_TRN_HGQ_PATH67_WKSP_INIT
               PATH68_FIELDS    IN ICD_TRN_HGQ_PATH68_WKSP_INIT
               PATH72_FIELDS    IN ICD_TRN_HGQ_PATH72_WKSP_INIT
               PATH73_FIELDS    IN ICD_TRN_HGQ_PATH73_WKSP_INIT
               PATH74_FIELDS    IN ICD_TRN_HGQ_PATH74_WKSP_INIT
               PATH81_FIELDS    IN ICD_TRN_HGQ_PATH81_WKSP_INIT
               PATH82_FIELDS    IN ICD_TRN_HGQ_PATH82_WKSP_INIT
               PATH83_FIELDS    IN ICD_TRN_HGQ_PATH83_WKSP_INIT
               PATH84_FIELDS    IN ICD_TRN_HGQ_PATH84_WKSP_INIT.
    
    MOVE SYM$_OPER_ID
      TO L$_OPER_ID                     IN L$_I_CHECK_FILE_NAME
         L$_OPER_ID                     IN L$_O_DATA_FILE_NAME
         L$_OPER_ID                     IN L$_O_COUNT_FILE_NAME
         L$_OPER_ID                     IN L$_O_OPER_FILE_NAME
         L$_OPER_ID                     IN L$_O_MAIL_FILE_NAME.    

    PERFORM BA-COPY-IN-FILE.
    
    PERFORM BB-OPEN-INPUT-FILE.

B-EXIT.
     EXIT.

*#-----------------------------------------------------------------------------
BA-COPY-IN-FILE                                 SECTION.
*#-----------------------------------------------------------------------------
BA-00.

    INITIALIZE UTL_COPY_FILE_V2_WKSP.
    
    MOVE L$_IN_FILE_NAME
      TO SP$_INFILENAME IN UTL_COPY_FILE_V2_WKSP.

    MOVE L$_I_CHECK_FILE_NAME
      TO SP$_OUTFILENAME IN UTL_COPY_FILE_V2_WKSP.

    CALL 'UTL_COPY_FILE_V2' USING UTL_COPY_FILE_V2_WKSP.

    EVALUATE SP$_SYSPRO_STATUS IN UTL_COPY_FILE_V2_WKSP
        WHEN SP$_MSG_NORMAL
            DISPLAY "COPY INPUT FILE: " L$_IN_FILE_NAME
            DISPLAY "TO NAME : "  L$_I_CHECK_FILE_NAME

        WHEN OTHER
            DISPLAY " ERROR COPY INPUT FILE !!! "
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            MOVE SP$_MSG_ERROR TO SP$_ACW_PROC_AUX_STATUS 
            PERFORM Z-FINISH

    END-EVALUATE.   

BA-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------    
BB-OPEN-INPUT-FILE                                   SECTION.
*#-----------------------------------------------------------------------------
BB-00.
  
    OPEN INPUT I_CHECK_FILE.
    
    MOVE ZERO
      TO L$_IN_REC_NUMBER.
      
*# FIRST READ    
    SET L$_NOT_EOF_IN_FILE TO TRUE.
    PERFORM R-READ-IN-FILE.
          
BB-EXIT.
    EXIT.
              
*#-----------------------------------------------------------------------------
C-HANDLE    				        SECTION.
*#-----------------------------------------------------------------------------
C-00.

*#  הנשיה הטישב הצירל תושייה ןוכדע
    SET L$_TYPE_OLD TO TRUE.   
    PERFORM D1-UPDATE-6060.
    PERFORM D2-OPEN-FILES.

*#  ומויסל דע טלקה ץבוק לע האלול    
    PERFORM UNTIL L$_EOF_IN_FILE   
*#  רטליפ תניטורב לופיטו םירטמרפ תרבעה - הירוטסהל האירקה תנכה        
        PERFORM G-PREPARE-HIST-PARAMS
*#  הירוטסההמ תומושר תפילשל רוסרק תחיתפ
        PERFORM MA-OPEN-CURSOR-HGA
                    
*#  הירוטסההמ ףלשנש טלפה םויסל דע האלול        
        PERFORM UNTIL L$_END_HIST_OUT
*#  טלפ תמושר תפילש        
            PERFORM MB-FETCH-HGA          
*#  טלפל המושר בותכל םאה הקידב (שקבתהש תומושר רפסמ לש לובגל דע)
            PERFORM O-CHECK-END-OUT     
*#  טלפה ץבוקל הירוטסההמ הלבקתהש טלפה תמושר תביתכ            
            IF L$_NOT_END_HIST_OUT
            THEN
                PERFORM N-WRITE-DATA-FILE
            END-IF
        END-PERFORM    
      
*#  הירוטסההמ הפילשל רוסרק תריגס        
        PERFORM MC-CLOSE-CURSOR-HGA
*#  הלבקתהש תומושרה תומכ לש טלפה ץבוקל הביתכ
        PERFORM P-WRITE-COUNT-FILE
*#  טלקה ץבוקמ המושר תאירק    
        PERFORM R-READ-IN-FILE
        
    END-PERFORM.  
         
    PERFORM D3-CLOSE-FILES.


*#  השדחה הטישב הצירל תושייה ןוכדע
    SET L$_TYPE_NEW TO TRUE.   
    PERFORM D1-UPDATE-6060.
    PERFORM D2-OPEN-FILES.

*#  ומויסל דע טלקה ץבוק לע האלול    
    PERFORM UNTIL L$_EOF_IN_FILE   
*#  רטליפ תניטורב לופיטו םירטמרפ תרבעה - הירוטסהל האירקה תנכה        
        PERFORM G-PREPARE-HIST-PARAMS
*#  הירוטסההמ תומושר תפילשל רוסרק תחיתפ
        PERFORM MA-OPEN-CURSOR-HGA
                    
*#  הירוטסההמ ףלשנש טלפה םויסל דע האלול        
        PERFORM UNTIL L$_END_HIST_OUT
*#  טלפ תמושר תפילש        
            PERFORM MB-FETCH-HGA          
*#  טלפל המושר בותכל םאה הקידב (שקבתהש תומושר רפסמ לש לובגל דע)
            PERFORM O-CHECK-END-OUT     
*#  טלפה ץבוקל הירוטסההמ הלבקתהש טלפה תמושר תביתכ            
            IF L$_NOT_END_HIST_OUT
            THEN
                PERFORM N-WRITE-DATA-FILE
            END-IF
        END-PERFORM    
      
*#  הירוטסההמ הפילשל רוסרק תריגס        
        PERFORM MC-CLOSE-CURSOR-HGA
*#  הלבקתהש תומושרה תומכ לש טלפה ץבוקל הביתכ
        PERFORM P-WRITE-COUNT-FILE
*#  טלקה ץבוקמ המושר תאירק    
        PERFORM R-READ-IN-FILE
        
    END-PERFORM.  
         
    PERFORM D3-CLOSE-FILES.

    PERFORM V-WRITE-OPER-FILE-IN. 
    
C-EXIT.
    EXIT.
 
*#----------------------------------------------------------------------------- 
D1-UPDATE-6060                                   SECTION.
*#-----------------------------------------------------------------------------
D1-00.

*# ןוכדע עוציב םוקמב ,הפסוה כ"חאו הקיחמ עוציב
*# תושייל תמייק המושר ןיאש םירקמ תוסכל

    PERFORM X2-START-TRANS-READ-WRITE.

*# הלבטהמ תושייה לש המושרה תקיחמ
    INITIALIZE ICD_TRT_6060_DB_DCISION_DBW.
    
    MOVE 'TRJ561'
      TO ENTITY_CODE    IN ICD_TRT_6060_DB_DCISION_PRW 
                        IN ICD_TRT_6060_DB_DCISION_DBW.
                        
    MOVE L$_BATCH
      TO ENTITY_TYPE    IN ICD_TRT_6060_DB_DCISION_PRW 
                        IN ICD_TRT_6060_DB_DCISION_DBW.
                                                                
    ADD DP$_SYM_DELETE  TO DP$_SYM_ACCESS_RDB
    GIVING DP$_ACTION   IN UTL_DBA_UNIV_WKSP IN ICD_TRT_6060_DB_DCISION_DBW.
    
    MOVE ZERO 
      TO DP$_KEY_SEQUENCE       IN UTL_DBA_UNIV_WKSP
                                IN ICD_TRT_6060_DB_DCISION_DBW.

    CALL 'ICD_TRT_6060_DB_DCISION_DBA' USING ICD_TRT_6060_DB_DCISION_DBW.

    EVALUATE DP$_STATUS IN ICD_TRT_6060_DB_DCISION_DBW

        WHEN SP$_MSG_NORMAL
        WHEN SP$_MSG_NO_DATA_FOUND
            CONTINUE

        WHEN OTHER
            DISPLAY "ERROR! ROUTINE: ICD_TRT_6060_DB_DCISION_DBA - DELETE"
            DISPLAY "ENTITY_CODE: " ENTITY_CODE IN ICD_TRT_6060_DB_DCISION_PRW 
                                                IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "ENTITY_TYPE: " ENTITY_TYPE IN ICD_TRT_6060_DB_DCISION_PRW 
                                                IN ICD_TRT_6060_DB_DCISION_DBW
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            PERFORM F-ROLLBACK
            PERFORM Z-FINISH
            
    END-EVALUATE.
                                                                

*# הלבטל תושייה לש המושר תפסוה
    INITIALIZE ICD_TRT_6060_DB_DCISION_DBW.
    
    MOVE 'TRJ561'
      TO ENTITY_CODE    IN ICD_TRT_6060_DB_DCISION 
                        IN ICD_TRT_6060_DB_DCISION_DBW.

    MOVE L$_BATCH
      TO ENTITY_TYPE    IN ICD_TRT_6060_DB_DCISION 
                        IN ICD_TRT_6060_DB_DCISION_DBW.

    MOVE 1
      TO PRIORITY_LEVEL IN ICD_TRT_6060_DB_DCISION 
                        IN ICD_TRT_6060_DB_DCISION_DBW.
      
    MOVE L$_SW_TYPE
      TO QUERY_TYPE     IN ICD_TRT_6060_DB_DCISION 
                        IN ICD_TRT_6060_DB_DCISION_DBW.                     
                                                
    ADD DP$_SYM_INSERT  TO DP$_SYM_ACCESS_RDB
    GIVING DP$_ACTION   IN UTL_DBA_UNIV_WKSP IN ICD_TRT_6060_DB_DCISION_DBW.
    
    MOVE ZERO 
      TO DP$_KEY_SEQUENCE       IN UTL_DBA_UNIV_WKSP
                                IN ICD_TRT_6060_DB_DCISION_DBW.

    CALL 'ICD_TRT_6060_DB_DCISION_DBA' USING ICD_TRT_6060_DB_DCISION_DBW.

    EVALUATE DP$_STATUS IN ICD_TRT_6060_DB_DCISION_DBW

        WHEN SP$_MSG_NORMAL
            DISPLAY "INSERT 6060 VALUES: "
            DISPLAY "ENTITY_CODE : " ENTITY_CODE        IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "ENTITY_TYPE : " ENTITY_TYPE        IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "PRIORITY_LEVEL : " PRIORITY_LEVEL  IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "QUERY_TYPE : " QUERY_TYPE          IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
        WHEN OTHER
            DISPLAY "ERROR! ROUTINE: ICD_TRT_6060_DB_DCISION_DBA - INSERT"
            DISPLAY "ENTITY_CODE : " ENTITY_CODE        IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "ENTITY_TYPE : " ENTITY_TYPE        IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "PRIORITY_LEVEL : " PRIORITY_LEVEL  IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            DISPLAY "QUERY_TYPE : " QUERY_TYPE          IN ICD_TRT_6060_DB_DCISION 
                                                        IN ICD_TRT_6060_DB_DCISION_DBW
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            PERFORM F-ROLLBACK
            PERFORM Z-FINISH
            
    END-EVALUATE.
    
    PERFORM E-COMMIT.
    
    PERFORM D10-UPDATE-6060-CTT.
    
D1-EXIT.
    EXIT.

*#----------------------------------------------------------------------------- 
D10-UPDATE-6060-CTT                             SECTION.
*#----------------------------------------------------------------------------- 
D10-00.

*# הלבטב שומיש שי תינכתה ךשמהב 6060 המ הפילש יפ לע CTT
*# הש תנמ לע CTT ןכדעתי
*# ל תינדי ןוכדע תדוקפ ץירהל שיCTT
*# לש המוזי הנתמה רוצילו 2 תוקד
    
    STRING "CTT$UPDATE_TABLE ICD_TRT_6060_DB_DCISION " 
      DELIMITED BY SIZE 
      INTO SP$_COMMAND_LINE     IN UTL_SPAWN_PROCESS_WKSP
    END-STRING.

    CALL 'UTL_SPAWN_PROCESS' USING UTL_SPAWN_PROCESS_WKSP.
    IF SP$_SYSPRO_STATUS IN UTL_SPAWN_PROCESS_WKSP IS FAILURE
    THEN
        DISPLAY "ERROR: D10-UPDATE-6060-CTT !!! "
        MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
        PERFORM Z-FINISH
    END-IF.

*# לש הנתמה תריצי 2 תוקד
    INITIALIZE UTL_PROC_WAIT_WKSP.
    INITIALIZE UTL_CVT_DELTA_TIME_WKSP.

    MOVE L$_DELAY
      TO SP$_DELTA_TIME_C23     IN UTL_CVT_DELTA_TIME_WKSP.

    MOVE ZERO
      TO SP$_DELTA_TIME OF UTL_CVT_DELTA_TIME_WKSP.

    CALL 'UTL_CVT_DELTA_TIME' USING UTL_CVT_DELTA_TIME_WKSP.

    MOVE SP$_DELTA_TIME IN UTL_CVT_DELTA_TIME_WKSP
      TO L$_DELTA_TIME.
			
    MOVE L$_DELTA_TIME
      TO SP$_DELTA_TIME IN UTL_PROC_WAIT_WKSP.

    CALL 'UTL_PROC_WAIT' USING UTL_PROC_WAIT_WKSP.


D10-EXIT.
    EXIT.
    
*#----------------------------------------------------------------------------- 
D2-OPEN-FILES                               SECTION.
*#----------------------------------------------------------------------------- 
D2-00.

*#  IN FILE
    OPEN INPUT I_CHECK_FILE.
    
    MOVE ZERO
      TO L$_IN_REC_NUMBER.
      
*# FIRST READ    
    SET L$_NOT_EOF_IN_FILE TO TRUE.
    PERFORM R-READ-IN-FILE.
    
    
*#  OUT FILE    
    EVALUATE TRUE
    
        WHEN L$_TYPE_OLD
            SET L$_OLD_TYPE IN L$_O_DATA_FILE_NAME  TO TRUE  
            SET L$_OLD_TYPE IN L$_O_COUNT_FILE_NAME TO TRUE      
        
        WHEN L$_TYPE_NEW    
            SET L$_NEW_TYPE IN L$_O_DATA_FILE_NAME  TO TRUE  
            SET L$_NEW_TYPE IN L$_O_COUNT_FILE_NAME TO TRUE                      
        
    END-EVALUATE.    

    OPEN OUTPUT O_DATA_FILE
                O_COUNT_FILE.

D2-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------    
D3-CLOSE-FILES                                  SECTION.
*#-----------------------------------------------------------------------------
D3-00.

    CLOSE I_CHECK_FILE
          O_DATA_FILE
          O_COUNT_FILE.

D3-EXIT.
    EXIT.
    
*#----------------------------------------------------------------------------- 
G-PREPARE-HIST-PARAMS                           SECTION.
*#-----------------------------------------------------------------------------
G-00.
*# הירוטסהל האירקה תנכה         
*# רטליפ תניטורב לופיט ללוכ , טלקה ץבוקמ םירטמרפ תרבעה
    MOVE ICD_TRN_HGQ_DBW_INIT
      TO ICD_TRN_HGQ_DBW.
      
*#  הפילשל םירטמרפה תרבעהו הפילשל לולסמ לש הטלחה
    PERFORM H-HGA-PATH
        
*#  תואלבטמ םינותנ תמלשהל תויצקידניא תרבעה        
    PERFORM I-HGA-TABLES-IND
    
*#  רטליפה תניטורל םינותנ תרבעה        
    PERFORM J-HGA-FILTER-FIELDS    

*#  הפילש תמרב םיסופיא
    SET L$_NOT_END_HIST_OUT TO TRUE.
    MOVE ZERO
      TO L$_OUT_REC_NUMBER.

G-EXIT.
    EXIT.
     
*#-----------------------------------------------------------------------------
H-HGA-PATH                                      SECTION.
*#-----------------------------------------------------------------------------
H-00.

    PERFORM HA-CONVERT-DATE-FIELDS.
    
    EVALUATE P$_PATH_NUM IN I_CHECK_REC
        WHEN 1
            PERFORM H1-PATH
        WHEN 2
            PERFORM H2-PATH        
        WHEN 3
            PERFORM H3-PATH        
        WHEN 4
            PERFORM H4-PATH        
        WHEN 5
            PERFORM H5-PATH        
        WHEN 6
            PERFORM H6-PATH        
        WHEN 7
            PERFORM H7-PATH        
        WHEN 8
            PERFORM H8-PATH        
        WHEN 9
            PERFORM H9-PATH        
        WHEN 10
            PERFORM H10-PATH        
        WHEN 11
            PERFORM H11-PATH        
        WHEN 12
            PERFORM H12-PATH        
        WHEN 13
            PERFORM H13-PATH        
        WHEN 14
            PERFORM H14-PATH        
        WHEN 15
            PERFORM H15-PATH        
        WHEN 16
        WHEN 17
            STRING " LINE NUMBER : ", L$_IN_REC_NUMBER,
                   " IN FILE , PATH : ", P$_PATH_NUM IN I_CHECK_REC,
                   " CAN'T CHECK 16 & 17 PATH !!"
              DELIMITED BY SIZE
              INTO L$_MSG
            END-STRING 
            DISPLAY L$_MSG
            PERFORM T-SEND-OUTLOOK
        WHEN 18
            PERFORM H18-PATH        
        WHEN 19
            PERFORM H19-PATH        
        WHEN 50
            PERFORM H50-PATH        
        WHEN 51
            PERFORM H51-PATH        
        WHEN 52
            PERFORM H52-PATH        
        WHEN 53
            PERFORM H53-PATH        
        WHEN 54
            PERFORM H54-PATH        
        WHEN 55
            PERFORM H55-PATH        
        WHEN 57
            PERFORM H57-PATH        
        WHEN 58
            PERFORM H58-PATH        
        WHEN 63
            PERFORM H63-PATH        
        WHEN 64
            PERFORM H64-PATH        
        WHEN 67
            PERFORM H67-PATH        
        WHEN 68
            PERFORM H68-PATH        
        WHEN 72
            PERFORM H72-PATH        
        WHEN 73
            PERFORM H73-PATH        
        WHEN 74
            PERFORM H74-PATH        
        WHEN 81
            PERFORM H81-PATH        
        WHEN 82        
            PERFORM H82-PATH        
        WHEN 83        
            PERFORM H83-PATH        
        WHEN 84            
            PERFORM H84-PATH                                                                          
        WHEN OTHER
            STRING " LINE NUMBER : ",
                     L$_IN_REC_NUMBER,
                   " IN FILE , PATH NOT EXIST : ", 
                     P$_PATH_NUM IN I_CHECK_REC
              DELIMITED BY SIZE
              INTO L$_MSG
            END-STRING 
            DISPLAY L$_MSG
            PERFORM T-SEND-OUTLOOK
    END-EVALUATE.
       
       
H-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
HA-CONVERT-DATE-FIELDS                  SECTION.        
*#-----------------------------------------------------------------------------
HA-00.

*# FROM FIELDS
    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_DEB_CRD_DATE                IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_DEB_CRD_DATE                IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_DEB_CRD_DATE                IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_FROM_DEB_CRD_DATE.


    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_PURCHASE_DATE               IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_PURCHASE_DATE               IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_PURCHASE_DATE               IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_FROM_PURCHASE_DATE.

      
    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_FROM_TRN_DEPOSIT_DATE.
      

    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_MT_DBCR_BANK_TMSP           IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_MT_DBCR_BANK_TMSP           IN FROM_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_MT_DBCR_BANK_TMSP           IN FROM_FIELDS
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_FROM_DBCR_BANK.            


*# TO FIELDS
    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_DEB_CRD_DATE                IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_DEB_CRD_DATE                IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_DEB_CRD_DATE                IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_TO_DEB_CRD_DATE.


    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_PURCHASE_DATE               IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_PURCHASE_DATE               IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_PURCHASE_DATE               IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_TO_PURCHASE_DATE.

      
    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_TRN_DEPOSIT_DATE_8          IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_TO_TRN_DEPOSIT_DATE.
      

    MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
      TO UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE P$_MT_DBCR_BANK_TMSP           IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(1:2)
      TO L$_DD                          IN L$_DATE.
    MOVE P$_MT_DBCR_BANK_TMSP           IN TO_FIELDS 
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(3:2)
      TO L$_MM                          IN L$_DATE.
    MOVE P$_MT_DBCR_BANK_TMSP           IN TO_FIELDS
                                        IN INPUT_FIELDS 
                                        IN I_CHECK_REC(5:4)
      TO L$_YYYY                        IN L$_DATE.
    MOVE L$_DATE
      TO SP$_DATE_DDMMYYYY              IN UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    CALL 'UTL_CVT_DATE_DDMMYYYY_2'      USING UTL_CVT_DATE_DDMMYYYY_2_WKSP.
    MOVE SP$_DATE_TIME_BINARY           IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
      TO L$_TO_DBCR_BANK.            


HA-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H1-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H1-00.

    MOVE DP$_CONST_PATH_01
      TO DP$_PATH_NUMBER                IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH01_WKSP_INIT
      TO ICD_TRN_HGQ_PATH01_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL                       
                                        IN PATH01_FIELDS
                                        IN ICD_TRN_HGQ_PATH01_WKSP.
                                

    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL 
                                        IN PATH01_FIELDS
                                        IN ICD_TRN_HGQ_PATH01_WKSP.                                   

    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH01_FIELDS
                                        IN ICD_TRN_HGQ_PATH01_WKSP.                       
                                
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH01_FIELDS
                                        IN ICD_TRN_HGQ_PATH01_WKSP.  


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH01_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.


*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

H1-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H2-PATH                                 SECTION.                               
*#-----------------------------------------------------------------------------
H2-00.

    MOVE DP$_CONST_PATH_02
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH02_WKSP_INIT
      TO ICD_TRN_HGQ_PATH02_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                        
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_FROM
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_TO
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_FROM
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                
    MOVE P$_TRN_TYPE_2                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_TO
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                
    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN P_VAL
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
                                
    MOVE P$_TRN_TYPE_3                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_3                     IN P_VAL
                                        IN PATH02_FIELDS
                                        IN ICD_TRN_HGQ_PATH02_WKSP.
  

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH02_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
    
    
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                                                        
H2-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
H3-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H3-00.

    MOVE DP$_CONST_PATH_03
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH03_WKSP_INIT
      TO ICD_TRN_HGQ_PATH03_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL                       
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP.
                                
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL 
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP.                                   

    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP. 
                                         
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP.
                                         
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP. 
      
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP. 

    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP.    

    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH03_FIELDS
                                        IN ICD_TRN_HGQ_PATH03_WKSP.  


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH03_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                        
H3-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H4-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H4-00.

    MOVE DP$_CONST_PATH_04
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                
    MOVE ICD_TRN_HGQ_PATH04_WKSP_INIT
      TO ICD_TRN_HGQ_PATH04_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.
                                        
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH04_FIELDS
                                        IN ICD_TRN_HGQ_PATH04_WKSP.


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH04_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                        
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                                                                    
H4-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H5-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H5-00.

    MOVE DP$_CONST_PATH_05
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH05_WKSP_INIT
      TO ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.
      
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH05_FIELDS
                                        IN ICD_TRN_HGQ_PATH05_WKSP.     


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH05_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

H5-EXIT.
    EXIT.
    
*#----------------------------------------------------------------------------- 
H6-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H6-00.

    MOVE DP$_CONST_PATH_06
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                 
    MOVE ICD_TRN_HGQ_PATH06_WKSP_INIT
      TO ICD_TRN_HGQ_PATH06_WKSP.
                     
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE P$_DEPO_PACKET_EXT_ID_8        IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PACKET_REFERENCE_NUM           IN P_VAL
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
      
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.
    
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH06_FIELDS
                                        IN ICD_TRN_HGQ_PATH06_WKSP.                     
                                          

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH06_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
                       
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                          
H6-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H7-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H7-00.

    MOVE DP$_CONST_PATH_07
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH07_WKSP_INIT
      TO ICD_TRN_HGQ_PATH07_WKSP.
                 
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.
                                        
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH07_FIELDS
                                        IN ICD_TRN_HGQ_PATH07_WKSP.                 
                                          

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH07_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
  
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                          
H7-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H8-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H8-00.

    MOVE DP$_CONST_PATH_08
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH08_WKSP_INIT
      TO ICD_TRN_HGQ_PATH08_WKSP.
                                                                                              
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                         
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH08_FIELDS
                                        IN ICD_TRN_HGQ_PATH08_WKSP.
                                                               
                                          
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH08_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                  
                                          
H8-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H9-PATH                                 SECTION.
*#-----------------------------------------------------------------------------
H9-00.

    MOVE DP$_CONST_PATH_09
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
    
    MOVE ICD_TRN_HGQ_PATH09_WKSP_INIT
      TO ICD_TRN_HGQ_PATH09_WKSP.
            

    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.

    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.

                                        
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_FROM
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_TO
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_FROM
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                
    MOVE P$_TRN_TYPE_2                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_TO
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                
    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN P_VAL
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.
                                
    MOVE P$_TRN_TYPE_3                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_3                     IN P_VAL
                                        IN PATH09_FIELDS
                                        IN ICD_TRN_HGQ_PATH09_WKSP.                                    
                   
                   
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH09_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.  
           
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.           
                                                         
H9-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H10-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H10-00.

    MOVE DP$_CONST_PATH_10
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH10_WKSP_INIT
      TO ICD_TRN_HGQ_PATH10_WKSP.
               

    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.

    MOVE P$_DEPO_PACKET_EXT_ID_8        IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PACKET_REFERENCE_NUM           IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.

    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.

                                        
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_FROM
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                        
    MOVE P$_TRN_TYPE_1                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN RANGE_TO
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN FROM_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_FROM
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                
    MOVE P$_TRN_TYPE_2                  IN TO_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN RANGE_TO
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                
    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                        
    MOVE P$_TRN_TYPE_2                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_2                     IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.
                                
    MOVE P$_TRN_TYPE_3                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_3                     IN P_VAL
                                        IN PATH10_FIELDS
                                        IN ICD_TRN_HGQ_PATH10_WKSP.       
                                        

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH10_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                        
       
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.       
                                          
H10-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H11-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H11-00.

    MOVE DP$_CONST_PATH_11
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH11_WKSP_INIT
      TO ICD_TRN_HGQ_PATH11_WKSP.
         
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN P_VAL
                                IN PATH11_FIELDS
                                IN ICD_TRN_HGQ_PATH11_WKSP.
                                        
                                          
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH11_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW. 

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
*** העונת ההזמ יפ לע
    MOVE 0
      TO DP$_DATE_TYPE          IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                        
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                                                      
                                                                                 
H11-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H12-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H12-00.

    MOVE DP$_CONST_PATH_12
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH12_WKSP_INIT
      TO ICD_TRN_HGQ_PATH12_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH12_FIELDS
                                        IN ICD_TRN_HGQ_PATH12_WKSP.
      
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH12_FIELDS
                                        IN ICD_TRN_HGQ_PATH12_WKSP.
      
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH12_FIELDS
                                        IN ICD_TRN_HGQ_PATH12_WKSP.


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH12_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                        
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                                            
                                        
H12-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H13-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H13-00.

    MOVE DP$_CONST_PATH_13
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH13_WKSP_INIT
      TO ICD_TRN_HGQ_PATH13_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH13_FIELDS
                                        IN ICD_TRN_HGQ_PATH13_WKSP.
      
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH13_FIELDS
                                        IN ICD_TRN_HGQ_PATH13_WKSP.
  
    MOVE P$_DEPO_PACKET_EXT_ID_8        IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PACKET_REFERENCE_NUM           IN P_VAL
                                        IN PATH13_FIELDS
                                        IN ICD_TRN_HGQ_PATH13_WKSP.

    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH13_FIELDS
                                        IN ICD_TRN_HGQ_PATH13_WKSP.

                                        
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH13_FIELDS
                                        IN ICD_TRN_HGQ_PATH13_WKSP.                                        
                                        

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH13_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                  
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                         
                                        
H13-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H14-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H14-00.

    MOVE DP$_CONST_PATH_14
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH14_WKSP_INIT
      TO ICD_TRN_HGQ_PATH14_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH14_FIELDS
                                        IN ICD_TRN_HGQ_PATH14_WKSP.
      
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH14_FIELDS
                                        IN ICD_TRN_HGQ_PATH14_WKSP.

    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH14_FIELDS
                                        IN ICD_TRN_HGQ_PATH14_WKSP.

                                        
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH14_FIELDS
                                        IN ICD_TRN_HGQ_PATH14_WKSP.   


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH14_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
         
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                
                                        
H14-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H15-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H15-00.

    MOVE DP$_CONST_PATH_15
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                   
    MOVE ICD_TRN_HGQ_PATH15_WKSP_INIT
      TO ICD_TRN_HGQ_PATH15_WKSP.


    MOVE P$_PROD_INTR_ID_9              IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_INTR_ID                   IN P_VAL
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.

    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.
                                         
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH15_FIELDS
                                        IN ICD_TRN_HGQ_PATH15_WKSP.


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH15_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.               

H15-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H18-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H18-00.

    MOVE DP$_CONST_PATH_18
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH18_WKSP_INIT
      TO ICD_TRN_HGQ_PATH18_WKSP.
                                          
                                        
                  
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.
                                         
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH18_FIELDS
                                        IN ICD_TRN_HGQ_PATH18_WKSP.


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH18_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                               
                                                                                  
H18-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H19-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H19-00.

    MOVE DP$_CONST_PATH_19
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH19_WKSP_INIT
      TO ICD_TRN_HGQ_PATH19_WKSP.


    MOVE P$_PROD_INTR_ID_9              IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_INTR_ID                   IN P_VAL
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.

    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.
                                         
    MOVE L$_TO_DBCR_BANK
      TO DBCR_BANK_TMSP                 IN RANGE_TO
                                        IN PATH19_FIELDS
                                        IN ICD_TRN_HGQ_PATH19_WKSP.


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH19_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DBCR_BANK
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DBCR_BANK
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DBCR_BANK
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                               
                                                                                                            
H19-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H50-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H50-00.

    MOVE DP$_CONST_PATH_50
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH50_WKSP_INIT
      TO ICD_TRN_HGQ_PATH50_WKSP.
          
             
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN P_VAL
                                IN PATH50_FIELDS
                                IN ICD_TRN_HGQ_PATH50_WKSP.
                                    

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH50_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                    
                                    
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
*** העונת ההזמ יפ לע
    MOVE 0
      TO DP$_DATE_TYPE          IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                        
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                                                    
H50-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H51-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H51-00.

    MOVE DP$_CONST_PATH_51
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH51_WKSP_INIT
      TO ICD_TRN_HGQ_PATH51_WKSP.


    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN P_VAL
                                IN PATH51_FIELDS
                                IN ICD_TRN_HGQ_PATH51_WKSP. 
      

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH51_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
*** העונת ההזמ יפ לע
    MOVE 0
      TO DP$_DATE_TYPE          IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                        
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                      
H51-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H52-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H52-00.

    MOVE DP$_CONST_PATH_52
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH52_WKSP_INIT
      TO ICD_TRN_HGQ_PATH52_WKSP.
          
        
    MOVE P$_TRN_INT_ID                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_INT_ID                     IN P_VAL
                                        IN PATH52_FIELDS
                                        IN ICD_TRN_HGQ_PATH52_WKSP. 
                                
    MOVE P$_MT_TRN_BY_PROD_NUMERATOR    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_BY_PROD_NUMERATOR          IN P_VAL
                                        IN PATH52_FIELDS
                                        IN ICD_TRN_HGQ_PATH52_WKSP. 
                                

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH52_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                
            
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
*** העונת ההזמ יפ לע
    MOVE 0
      TO DP$_DATE_TYPE          IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.    
                                        
    MOVE P$_TRN_INT_ID          IN VAL_FIELDS
                                IN INPUT_FIELDS
                                IN I_CHECK_REC
      TO TRN_INT_ID             IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.                
                                
H52-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H53-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H53-00.

    MOVE DP$_CONST_PATH_53
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH53_WKSP_INIT
      TO ICD_TRN_HGQ_PATH53_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
                                        
    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.
      
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH53_FIELDS
                                        IN ICD_TRN_HGQ_PATH53_WKSP.     
      

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH53_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
H53-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H54-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H54-00.

    MOVE DP$_CONST_PATH_54
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH54_WKSP_INIT
      TO ICD_TRN_HGQ_PATH54_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.

    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH54_FIELDS
                                        IN ICD_TRN_HGQ_PATH54_WKSP.
                                          

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH54_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
                                
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
                                          
H54-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H55-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H55-00.

    MOVE DP$_CONST_PATH_55
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH55_WKSP_INIT
      TO ICD_TRN_HGQ_PATH55_WKSP.


    MOVE P$_PROD_INTR_ID_9              IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_INTR_ID                   IN P_VAL
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.

    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.
                                        
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH55_FIELDS
                                        IN ICD_TRN_HGQ_PATH55_WKSP.
      
      
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH55_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.          
                                              
H55-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H57-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H57-00.

    MOVE DP$_CONST_PATH_57
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH57_WKSP_INIT
      TO ICD_TRN_HGQ_PATH57_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
                                        
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.
      
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH57_FIELDS
                                        IN ICD_TRN_HGQ_PATH57_WKSP.     
                                          

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH57_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
        
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                
                                          
H57-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H58-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H58-00.

    MOVE DP$_CONST_PATH_58
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH58_WKSP_INIT
      TO ICD_TRN_HGQ_PATH58_WKSP.
      
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.
      
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH58_FIELDS
                                        IN ICD_TRN_HGQ_PATH58_WKSP.   
      
      
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH58_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.               
      
H58-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H63-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H63-00.

    MOVE DP$_CONST_PATH_63
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH63_WKSP_INIT
      TO ICD_TRN_HGQ_PATH63_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
                                        
    MOVE P$_ORIG_DEB_PROD_INTR_ID       IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO DEB_PROD_INTR_ID               IN P_VAL
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
      
    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.
      
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH63_FIELDS
                                        IN ICD_TRN_HGQ_PATH63_WKSP.           
                                                    
                                                    
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH63_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                                                      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                                                      
                                                                                            
H63-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H64-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H64-00.

    MOVE DP$_CONST_PATH_64
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH64_WKSP_INIT
      TO ICD_TRN_HGQ_PATH64_WKSP.
          
   
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
    MOVE P$_ORIG_DEB_PROD_INTR_ID       IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO DEB_PROD_INTR_ID               IN P_VAL
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.

    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH64_FIELDS
                                        IN ICD_TRN_HGQ_PATH64_WKSP.
                                        
                                        
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH64_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                            
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                                     
                                                                                                                                 
H64-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H67-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H67-00.

    MOVE DP$_CONST_PATH_67
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH67_WKSP_INIT
      TO ICD_TRN_HGQ_PATH67_WKSP.
                        
   
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
  
    MOVE P$_ORIG_DEB_PROD_INTR_ID       IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO DEB_PROD_INTR_ID               IN P_VAL
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
   
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.
      
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH67_FIELDS
                                        IN ICD_TRN_HGQ_PATH67_WKSP.   
                                                             
                                                             
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH67_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
        
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                
                                                  
                                                                                                                            
H67-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H68-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H68-00.

    MOVE DP$_CONST_PATH_68
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH68_WKSP_INIT
      TO ICD_TRN_HGQ_PATH68_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.
                                        
    MOVE P$_ACC_BANK_NUM_9_N            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BANK_NUM                   IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.
                                        
    MOVE P$_ACC_BRANCH_NUM_9            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_BRANCH_NUM                 IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.
                                        
    MOVE P$_ACC_TYPE_CODE_NUM           IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_TYPE_CODE                  IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.
                                        
    MOVE P$_ACC_NUM_9                   IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO ACC_NUM                        IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.

    MOVE P$_CRD_PROD_INTR_ID            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO CRD_PROD_INTR_ID               IN P_VAL
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.

    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH68_FIELDS
                                        IN ICD_TRN_HGQ_PATH68_WKSP.           


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH68_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.            

H68-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H72-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H72-00.

    MOVE DP$_CONST_PATH_72
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH72_WKSP_INIT
      TO ICD_TRN_HGQ_PATH72_WKSP.
                                
    
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH72_FIELDS
                                        IN ICD_TRN_HGQ_PATH72_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH72_FIELDS
                                        IN ICD_TRN_HGQ_PATH72_WKSP.    
                                 
    MOVE P$_MT_TRN_CURR_CODE            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_CURRENCY_CODE              IN P_VAL
                                        IN PATH72_FIELDS
                                        IN ICD_TRN_HGQ_PATH72_WKSP.

    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH72_FIELDS
                                        IN ICD_TRN_HGQ_PATH72_WKSP.                                              
                                          

    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH72_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                          
              
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                          
                                          
H72-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H73-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H73-00.

    MOVE DP$_CONST_PATH_73
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH73_WKSP_INIT
      TO ICD_TRN_HGQ_PATH73_WKSP.
                         

    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.                          

    MOVE P$_DEPO_PACKET_EXT_ID_8        IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PACKET_REFERENCE_NUM           IN P_VAL
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.

    MOVE P$_MT_TRN_CURR_CODE            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_CURRENCY_CODE              IN P_VAL
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.
                                                                
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.
    
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH73_FIELDS
                                        IN ICD_TRN_HGQ_PATH73_WKSP.           
                                          
                                          
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH73_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.  
                                                                   
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                                                      
                                                                                
H73-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H74-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H74-00.

    MOVE DP$_CONST_PATH_74
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH74_WKSP_INIT
      TO ICD_TRN_HGQ_PATH74_WKSP.
              
    
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH74_FIELDS
                                        IN ICD_TRN_HGQ_PATH74_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH74_FIELDS
                                        IN ICD_TRN_HGQ_PATH74_WKSP.                          

    MOVE P$_MT_TRN_CURR_CODE            IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_CURRENCY_CODE              IN P_VAL
                                        IN PATH74_FIELDS
                                        IN ICD_TRN_HGQ_PATH74_WKSP.    
 
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH74_FIELDS
                                        IN ICD_TRN_HGQ_PATH74_WKSP.
    
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH74_FIELDS
                                        IN ICD_TRN_HGQ_PATH74_WKSP.                                              


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH74_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.   

H74-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H81-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H81-00.

    MOVE DP$_CONST_PATH_81
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH81_WKSP_INIT
      TO ICD_TRN_HGQ_PATH81_WKSP.


    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH81_FIELDS
                                        IN ICD_TRN_HGQ_PATH81_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH81_FIELDS
                                        IN ICD_TRN_HGQ_PATH81_WKSP.              
      
    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH81_FIELDS
                                        IN ICD_TRN_HGQ_PATH81_WKSP.

    MOVE L$_FROM_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_FROM
                                        IN PATH81_FIELDS
                                        IN ICD_TRN_HGQ_PATH81_WKSP.                       
                                
    MOVE L$_TO_PURCHASE_DATE
      TO PURCHASE_DATE                  IN RANGE_TO
                                        IN PATH81_FIELDS
                                        IN ICD_TRN_HGQ_PATH81_WKSP.  
      
      
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH81_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_PURCHASE_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_PURCHASE_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_PURCHASE
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.      
      
H81-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H82-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H82-00.

    MOVE DP$_CONST_PATH_82
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.

    MOVE ICD_TRN_HGQ_PATH82_WKSP_INIT
      TO ICD_TRN_HGQ_PATH82_WKSP.
      
    
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH82_FIELDS
                                        IN ICD_TRN_HGQ_PATH82_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH82_FIELDS
                                        IN ICD_TRN_HGQ_PATH82_WKSP.              
      
    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH82_FIELDS
                                        IN ICD_TRN_HGQ_PATH82_WKSP.
  
    MOVE L$_TO_DEB_CRD_DATE
      TO DEB_CRD_DATE                   IN RANGE_TO
                                        IN PATH82_FIELDS
                                        IN ICD_TRN_HGQ_PATH82_WKSP.
                                                            
                                                            
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH82_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                                            
                         
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_DEB_CRD_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_DEB_CRD_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEB_CRD
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                     
                                                                                  
H82-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H83-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H83-00.

    MOVE DP$_CONST_PATH_83
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH83_WKSP_INIT
      TO ICD_TRN_HGQ_PATH83_WKSP.
   
   
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.          

    MOVE P$_DEPO_PACKET_EXT_ID_8        IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PACKET_REFERENCE_NUM           IN P_VAL
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.   

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.
 
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.
    
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH83_FIELDS
                                        IN ICD_TRN_HGQ_PATH83_WKSP.    
                                    
    
    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH83_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                    
                                    
*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.                                    
                                    
H83-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
H84-PATH                                SECTION.
*#-----------------------------------------------------------------------------
H84-00.

    MOVE DP$_CONST_PATH_84
      TO DP$_PATH_NUMBER        IN ICD_TRN_HGQ_PRW
                                IN ICD_TRN_HGQ_DBW.
                                    
    MOVE ICD_TRN_HGQ_PATH84_WKSP_INIT
      TO ICD_TRN_HGQ_PATH84_WKSP.
      
    MOVE PROD_EXT_ID                    IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO PROD_EXT_ID                    IN P_VAL
                                        IN PATH84_FIELDS
                                        IN ICD_TRN_HGQ_PATH84_WKSP.
                                        
    MOVE P$_EXT_ID_NUM_TYPE_CODE_9      IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO EXT_ID_NUM_TYPE_CODE           IN P_VAL
                                        IN PATH84_FIELDS
                                        IN ICD_TRN_HGQ_PATH84_WKSP.          

    MOVE P$_TRN_TYPE_1                  IN VAL_FIELDS
                                        IN INPUT_FIELDS
                                        IN I_CHECK_REC
      TO TRN_TYPE_1                     IN P_VAL
                                        IN PATH84_FIELDS
                                        IN ICD_TRN_HGQ_PATH84_WKSP.
 
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_FROM
                                        IN PATH84_FIELDS
                                        IN ICD_TRN_HGQ_PATH84_WKSP.
    
    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO TRN_DEPOSIT_DATE               IN RANGE_TO
                                        IN PATH84_FIELDS
                                        IN ICD_TRN_HGQ_PATH84_WKSP.    


    MOVE DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PATH84_WKSP      
      TO DP$_ARG_FOR_SELECT_AREA        IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

*** הזיאמ הפילשל םיעבוק םיכיראת תרדגה DB
    MOVE L$_FROM_TRN_DEPOSIT_DATE
      TO DP$_FROM_DATE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE L$_TO_TRN_DEPOSIT_DATE
      TO DP$_TO_DATE                    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

    MOVE DP$_CONST_TYPE_DEPOSIT
      TO DP$_DATE_TYPE                  IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

H84-EXIT.
    EXIT.
    
                
*#-----------------------------------------------------------------------------
I-HGA-TABLES-IND                                SECTION.
*#-----------------------------------------------------------------------------
I-00.

*# תואלבטמ םינותנ תמלשהל תויצקידניא תרבעה

    MOVE P$_623_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T623).

    MOVE P$_624_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T624).
                                        
    MOVE P$_628_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T628).
                                        
    MOVE P$_633_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T633).
                                        
    MOVE P$_638_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T638).
                                        
    MOVE P$_640_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T640).
                                        
    MOVE P$_643_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T643).
                                        
    MOVE P$_644_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T644).
                                        
    MOVE P$_646_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T646).
                                        
    MOVE P$_653_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T653).
                                        
    MOVE P$_655_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T655).
                                        
    MOVE P$_669_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T669).
                                        
    MOVE P$_668_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T668).
                                        
    MOVE P$_670_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T670).
                                        
    MOVE P$_634_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T634).
                                        
    MOVE P$_652_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T652).
                                        
    MOVE P$_635_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T635).
                                        
    MOVE P$_697_IND                     IN I_CHECK_REC
      TO DP$_TABLE_OUTPUT_FLAG          IN ICD_OUTPUT_TABLES
                                        IN ICD_TRN_HGQ_PRW       
                                        IN ICD_TRN_HGQ_DBW(DP$_CONST_IDX_T697).

I-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
J-HGA-FILTER-FIELDS                             SECTION.
*#-----------------------------------------------------------------------------
J-00.

*# MOVE FIELDS TO FILTER

    MOVE ICA_TRB561_FILTER_WKSP_INIT
      TO ICA_TRB561_FILTER_WKSP.

    IF P$_TRB670_TRN_TYPE               IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_TRB670_TRN_TYPE         IN I_CHECK_REC            
          TO TRN_TYPE                   IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_MT_TRN_CURR_CODE              IN FILTER_FIELDS IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_MT_TRN_CURR_CODE        IN FILTER_FIELDS IN I_CHECK_REC   
          TO TRN_CURRENCY_CODE          IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF TRN_DEL_IND                      IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE TRN_DEL_IND                IN I_CHECK_REC
          TO TRN_DEL_IND                IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_TIS_GOODS_CODE                IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_TIS_GOODS_CODE          IN I_CHECK_REC
          TO GOODS_CODE                 IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_CREDIT_COMPANY_CODE           IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_CREDIT_COMPANY_CODE     IN I_CHECK_REC
          TO CREDIT_COMPANY_CODE        IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP 
    END-IF.
    
    IF P$_DEBIT_DAY_NUM                 IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_DEBIT_DAY_NUM           IN I_CHECK_REC
          TO DEBIT_DAY                  IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP 
    END-IF.
    
    IF CRD_PROD_EXT_ID                  IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE CRD_PROD_EXT_ID            IN I_CHECK_REC
          TO CRD_PROD_EXT_ID            IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_TRM144_TRN_EXEC_WAY           IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_TRM144_TRN_EXEC_WAY     IN I_CHECK_REC
          TO TRN_EXEC_WAY               IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF DEB_PROD_EXT_ID                  IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE DEB_PROD_EXT_ID            IN I_CHECK_REC
          TO DEB_PROD_EXT_ID            IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_ISSUER_CODE_3                 IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_ISSUER_CODE_3           IN I_CHECK_REC
          TO ISSUER_CODE                IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_DRAFT_RESTORE_NUM             IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE P$_DRAFT_RESTORE_NUM       IN I_CHECK_REC
          TO DRAFT_RESTORE_NUM          IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_DEB_CRD_DATE                  IN FILTER_FIELDS IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
          TO UTL_CVT_DATE_DDMMYYYY_2_WKSP
        MOVE P$_DEB_CRD_DATE            IN FILTER_FIELDS IN I_CHECK_REC (1:2)
          TO L$_DD                      IN L$_DATE
        MOVE P$_DEB_CRD_DATE            IN FILTER_FIELDS IN I_CHECK_REC (3:2)
          TO L$_MM                      IN L$_DATE
        MOVE P$_DEB_CRD_DATE            IN FILTER_FIELDS IN I_CHECK_REC (5:4)
          TO L$_YYYY                    IN L$_DATE
        MOVE L$_DATE
          TO SP$_DATE_DDMMYYYY          IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
        CALL 'UTL_CVT_DATE_DDMMYYYY_2'  USING UTL_CVT_DATE_DDMMYYYY_2_WKSP
        MOVE SP$_DATE_TIME_BINARY       IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
          TO P$_FROM_DEB_CRD_DATE       IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_DEB_CRD_DATE_A1               IN FILTER_FIELDS IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE INIT_CVT_DATE_DDMMYYYY_2_WKSP
          TO UTL_CVT_DATE_DDMMYYYY_2_WKSP
        MOVE P$_DEB_CRD_DATE_A1         IN FILTER_FIELDS IN I_CHECK_REC (1:2)
          TO L$_DD                      IN L$_DATE
        MOVE P$_DEB_CRD_DATE_A1         IN FILTER_FIELDS IN I_CHECK_REC (3:2)
          TO L$_MM                      IN L$_DATE
        MOVE P$_DEB_CRD_DATE_A1         IN FILTER_FIELDS IN I_CHECK_REC (5:4)
          TO L$_YYYY                    IN L$_DATE
        MOVE L$_DATE
          TO SP$_DATE_DDMMYYYY          IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
        CALL 'UTL_CVT_DATE_DDMMYYYY_2'  USING UTL_CVT_DATE_DDMMYYYY_2_WKSP
        MOVE SP$_DATE_TIME_BINARY       IN UTL_CVT_DATE_DDMMYYYY_2_WKSP
          TO P$_TILL_DEB_CRD_DATE       IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.    
      
    IF P$_PAYMENT_ADTNL_REF             IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_PAYMENT_ADTNL_REF       IN I_CHECK_REC
          TO PAYMENT_ADTNL_REF          IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_MT_TRN_STS                    IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_MT_TRN_STS              IN I_CHECK_REC
          TO TRN_STS                    IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_MT_INDEX_CURRENCY_CODE        IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_MT_INDEX_CURRENCY_CODE  IN I_CHECK_REC
          TO INDEX_CURRENCY_CODE        IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_MT_TRN_STS_REASON             IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_MT_TRN_STS_REASON       IN I_CHECK_REC
          TO TRN_STS_REASON             IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF MICROFILM                        IN I_CHECK_REC NOT= SPACES
    THEN
        MOVE MICROFILM                  IN I_CHECK_REC
          TO MICROFILM                  IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    IF P$_DEB_CRD_PHASE_NUM             IN I_CHECK_REC NOT= ZERO
    THEN
        MOVE P$_DEB_CRD_PHASE_NUM       IN I_CHECK_REC
          TO DEB_CRD_PHASE_NUM          IN FILTER_FIELDS_RECORD
                                        IN ICA_TRB561_FILTER_WKSP
    END-IF.
    
    MOVE DP$_FILTER_ROUTINE_ARG_AREA    IN ICA_TRB561_FILTER_WKSP
      TO DP$_FILTER_ROUTINE_ARG_AREA    IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.
                                      
*# MOVE FILTER ROUTINE NAME
    MOVE "ICA_TRB561_HGA_FILTER"
      TO DP$_FILTER_ROUTINE_NAME_AREA   IN ICD_TRN_HGQ_PRW
                                        IN ICD_TRN_HGQ_DBW.

J-EXIT.
    EXIT.
    

*#-----------------------------------------------------------------------------
MA-OPEN-CURSOR-HGA                              SECTION.
*#-----------------------------------------------------------------------------
MA-00.

    MOVE ZERO 
      TO DP$_ACTION IN ICD_TRN_HGQ_DBW.
       ADD DP$_SYM_OPEN_CURSOR 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRN_HGQ_DBW.

    CALL 'ICD_TRN_HGA' USING ICD_TRN_HGQ_DBW.

    EVALUATE DP$_STATUS IN ICD_TRN_HGQ_DBW
        WHEN SP$_MSG_NORMAL
            CONTINUE
        WHEN OTHER
            MOVE DP$_STATUS            IN ICD_TRN_HGQ_DBW                 
              TO SP$_ACW_PROC_AUX_STATUS
            MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRN_HGQ_DBW     
              TO SP$_ACW_FORM_MSG
            MOVE "SEC:MA-OPEN-CURSOR-HGA RTN:ICD_TRN_HGA OPEN  ERROR!!"   
              TO SP$_ACW_FREE_TEXT
            DISPLAY SP$_ACW_FREE_TEXT
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            PERFORM F-ROLLBACK
            PERFORM Z-FINISH
    END-EVALUATE.


MA-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
MB-FETCH-HGA                                    SECTION.
*#-----------------------------------------------------------------------------
MB-00.

    MOVE ZERO 
      TO DP$_ACTION IN ICD_TRN_HGQ_DBW.
       ADD DP$_SYM_FETCH 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRN_HGQ_DBW.

    CALL 'ICD_TRN_HGA' USING ICD_TRN_HGQ_DBW.

    EVALUATE DP$_STATUS    IN ICD_TRN_HGQ_DBW
        WHEN SP$_MSG_NORMAL
            ADD 1 
             TO L$_OUT_REC_NUMBER
            
        WHEN SP$_MSG_NO_DATA_FOUND
        WHEN SP$_MSG_END_OF_DATA_SET
            SET L$_END_HIST_OUT TO TRUE
            
        WHEN OTHER
            MOVE DP$_STATUS            IN ICD_TRN_HGQ_DBW                 
              TO SP$_ACW_PROC_AUX_STATUS
            MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRN_HGQ_DBW     
              TO SP$_ACW_FORM_MSG
            MOVE "SEC:MB-FETCH-HGA RTN:ICD_TRN_HGA FETCH  ERROR!!"   
              TO SP$_ACW_FREE_TEXT
            DISPLAY SP$_ACW_FREE_TEXT
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            PERFORM F-ROLLBACK
            PERFORM Z-FINISH
    END-EVALUATE.



MB-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
MC-CLOSE-CURSOR-HGA                             SECTION.
*#-----------------------------------------------------------------------------
MC-00.

    MOVE ZERO 
      TO DP$_ACTION IN ICD_TRN_HGQ_DBW.
       ADD DP$_SYM_CLOSE_CURSOR 
        TO DP$_SYM_ACCESS_RDB 
    GIVING DP$_ACTION IN ICD_TRN_HGQ_DBW.

    CALL 'ICD_TRN_HGA' USING ICD_TRN_HGQ_DBW.

    EVALUATE DP$_STATUS IN ICD_TRN_HGQ_DBW
        WHEN SP$_MSG_NORMAL
             CONTINUE
        WHEN OTHER
            MOVE DP$_STATUS            IN ICD_TRN_HGQ_DBW                 
              TO SP$_ACW_PROC_AUX_STATUS
            MOVE DP$_STATUS_AUXIL_TEXT IN ICD_TRN_HGQ_DBW     
              TO SP$_ACW_FORM_MSG
            MOVE "SEC:MC-CLOSE-CURSOR-HGA RTN:ICD_TRN_HGA CLOSE  ERROR!!"   
              TO SP$_ACW_FREE_TEXT
            DISPLAY SP$_ACW_FREE_TEXT
            MOVE SP$_MSG_ERROR TO SP$_EXIT_STATUS
            PERFORM F-ROLLBACK
            PERFORM Z-FINISH
    END-EVALUATE.


MC-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
N-WRITE-DATA-FILE                               SECTION.
*#-----------------------------------------------------------------------------
N-00.

    MOVE ICA_TRB561_OUT_WKSP_INIT
      TO O_DATA_REC.
                
*#  טלקה ץבוקמ המושרל רופסמ
    MOVE L$_IN_REC_NUMBER
      TO P$_RECORD_COUNTER      IN O_DATA_REC.  

*#  טלפה תומושר רופסמ 
    MOVE L$_OUT_REC_NUMBER
      TO P$_COUNT_RECS          IN O_DATA_REC. 
      
*#  הירוטסההמ לבקתהש טלפה               
    MOVE ICD_TRN_HGQ_SLCT_DATA_WKSP     IN SINGLE_DATA_RECORD
                                        IN ICD_TRN_HGQ_DBW
      TO ICD_TRN_HGQ_SLCT_DATA_WKSP     IN O_DATA_REC.

    WRITE O_DATA_REC.
    
N-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
O-CHECK-END-OUT                                 SECTION.
*#-----------------------------------------------------------------------------
O-00.

*#  המושר בותכל ןיא - שרדנל עיגה ופלשנש תומושרה רפסמ םא
    IF L$_OUT_REC_NUMBER > P$_RECORD_COUNT    IN I_CHECK_REC
    THEN
        SET L$_END_HIST_OUT TO TRUE
    END-IF.
    
O-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
P-WRITE-COUNT-FILE                              SECTION.
*#-----------------------------------------------------------------------------
P-00.

    MOVE ICA_TRB561_OUT_CNT_WKSP_INIT
      TO O_COUNT_REC.
                
*#  טלקה ץבוקמ המושרל רופסמ
    MOVE L$_IN_REC_NUMBER
      TO P$_RECORD_COUNTER      IN O_COUNT_REC.  

*#  טלפה תומושר רפסמ 
    MOVE L$_OUT_REC_NUMBER
      TO P$_COUNT_RECS          IN O_COUNT_REC. 
      
    WRITE O_COUNT_REC.


P-EXIT.
    EXIT.
    
*#-----------------------------------------------------------------------------
R-READ-IN-FILE                          SECTION.
*#-----------------------------------------------------------------------------
R-00.

    READ I_CHECK_FILE
        AT END
            SET L$_EOF_IN_FILE TO TRUE  
    END-READ.
    
    ADD 1
     TO L$_IN_REC_NUMBER.
     
R-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
V-WRITE-OPER-FILE-IN                    SECTION.
*#-----------------------------------------------------------------------------
V-00.

*#  הצירה לש הלעפהה ההזמ לש ןכות םע ץבוק תריצי
*#  שדח לומ ןשי םיצבקה לש םיטלפ תאוושה תינכת רובע
    OPEN OUTPUT O_OPER_FILE.
    
    MOVE SYM$_OPER_ID
      TO L$_REC        IN O_OPER_REC.

    WRITE O_OPER_REC.
    
    MOVE L$_IN_FILE_NAME
      TO L$_REC        IN O_OPER_REC.
      
    WRITE O_OPER_REC.   
    
    CLOSE O_OPER_FILE.
    
*#  םיסנכנ םיצבקל ץבוקה םושיר
    INITIALIZE ICA_SVM_FILEIN_INSERT_AUTO_WKSP.

    MOVE V$_HEB_YES
      TO CALL_IN_BATCH    IN ICA_SVM_FILEIN_INSERT_AUTO_WKSP.

    MOVE V608$INNER_PROCESS
      TO TRNF_SRC_CODE    IN ICA_SVM_FILEIN_INSERT_AUTO_WKSP.

    MOVE V41$CHECK_DUP_TRN
      TO FILEIN_TAPE_CODE IN ICA_SVM_FILEIN_INSERT_AUTO_WKSP.

    MOVE L$_O_OPER_FILE_NAME
      TO TRNF_DIR         IN ICA_SVM_FILEIN_INSERT_AUTO_WKSP.

    CALL 'ICA_SVM_FILEIN_INSERT_AUTO' USING ICA_SVM_FILEIN_INSERT_AUTO_WKSP
                                            UTL_CONTROL_ACW.

    IF SP$_ACW_PROC_AUX_STATUS IN UTL_CONTROL_ACW NOT = ICA_S_0088
    THEN
        DISPLAY "Error > ICA_SVM_FILEIN_INSERT_AUTO "
        DISPLAY "FILE NAME: " L$_O_OPER_FILE_NAME
        SET SP$_EXIT_STATUS_FAILURE TO TRUE
        PERFORM F-ROLLBACK
        PERFORM Z-FINISH
    ELSE
        DISPLAY "CREATE FILE IN - TRNF_ID : "
        DISPLAY TRNF_ID IN ICA_SVM_FILEIN_INSERT_AUTO_WKSP
    END-IF.
    

V-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
S-DUMMY-CALL                                SECTION.
*#-----------------------------------------------------------------------------
*# Section: E-DUMMY-CALL
*# Purpose: Dummy call for filters routines (for link) - never execute!
*#-----------------------------------------------------------------------------
S-00.

*** FOR LINK -- DUMMY CALL TO ICA_TRB561_HGA_FILTER
*** THE ROUTINE EXEC IN HGA CALL
    CALL "ICA_TRB561_HGA_FILTER" USING ICA_TRB561_FILTER_WKSP.

S-EXIT.
    EXIT.


*#-----------------------------------------------------------------------------
T-SEND-OUTLOOK                          SECTION.
*#-----------------------------------------------------------------------------
T-00.

    INITIALIZE UTL_SPAWN_PROCESS_WKSP.

    OPEN OUTPUT O_MAIL_FILE.

    MOVE L$_MSG
      TO MSG IN O_MAIL_REC.

    WRITE O_MAIL_REC.

    CLOSE O_MAIL_FILE.

    STRING "@ICA_PROC:ICA_SEND_INTERNAL_MAIL ",
           L$_O_MAIL_FILE_NAME
           DELIMITED BY SIZE
      INTO SP$_COMMAND_LINE         IN UTL_SPAWN_PROCESS_WKSP
    END-STRING  

     CALL 'UTL_SPAWN_PROCESS'       USING UTL_SPAWN_PROCESS_WKSP

     IF SP$_SYSPRO_STATUS           IN UTL_SPAWN_PROCESS_WKSP IS NOT SUCCESS
     THEN
        DISPLAY "**Error at T-SEND-OUTLOOK - UTL_SPAWN_PROCESS "
        DISPLAY "  Command : ",
                SP$_COMMAND_LINE    IN UTL_SPAWN_PROCESS_WKSP
     END-IF.

T-EXIT.
    EXIT.

*#-----------------------------------------------------------------------------
F-ROLLBACK				SECTION.
*#-----------------------------------------------------------------------------
F-00.

    MOVE DP$_SYM_ROLLBACK 
      TO DP$_ACTION IN ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

F-EXIT.
     EXIT.
     
*#-----------------------------------------------------------------------------
E-COMMIT				SECTION.
*#-----------------------------------------------------------------------------
E-00.

    MOVE DP$_SYM_COMMIT 
      TO DP$_ACTION IN ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

E-EXIT.
    EXIT.  
      
*#-----------------------------------------------------------------------------
X1-START-TRANS-READ-ONLY                  SECTION.
*#-----------------------------------------------------------------------------
X1-00.

*   Start transaction.
    MOVE "BATCH" TO DP$_WORK_CONTEXT IN ICD_COMMON_DBW.
    MOVE DP$_SYM_TRAN_READ TO DP$_ACTION IN ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

    IF DP$_STATUS IN ICD_COMMON_DBW IS FAILURE
    THEN
       MOVE DP$_STATUS IN ICD_COMMON_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 
       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG
       MOVE "SEC:X1-START-TRANS-READ-ONLY RTN:ICD_COMMON_DBA" 
         TO SP$_ACW_FREE_TEXT
       MOVE DP$_STATUS_AUXIL IN ICD_COMMON_DBW TO SP$_EXIT_STATUS
       PERFORM Z-FINISH
    END-IF.

X1-EXIT.
     EXIT.    
     
*#-----------------------------------------------------------------------------
X2-START-TRANS-READ-WRITE                  SECTION.
*#-----------------------------------------------------------------------------
X2-00.

*   Start transaction.
    MOVE "BATCH" TO DP$_WORK_CONTEXT IN ICD_COMMON_DBW.
    MOVE DP$_SYM_TRAN_READ_WRITE TO DP$_ACTION IN ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

    IF DP$_STATUS IN ICD_COMMON_DBW IS FAILURE
    THEN
       MOVE DP$_STATUS IN ICD_COMMON_DBW 
         TO SP$_ACW_PROC_AUX_STATUS 
       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_COMMON_DBW 
         TO SP$_ACW_FORM_MSG
       MOVE "SEC:X2-START-TRANS-READ-WRITE RTN:ICD_COMMON_DBA" 
         TO SP$_ACW_FREE_TEXT
       MOVE DP$_STATUS_AUXIL IN ICD_COMMON_DBW TO SP$_EXIT_STATUS
       PERFORM F-ROLLBACK
       PERFORM Z-FINISH
    END-IF.

X2-EXIT.
     EXIT.    
    
*#-----------------------------------------------------------------------------
Z-FINISH 				SECTION.
*#-----------------------------------------------------------------------------
*# Section: Z-FINISH
*# Purpose: Exit program with status.
*# Description:
*#-----------------------------------------------------------------------------
Z-00.
    
    DISPLAY "---- Z-FINISH   ICA_TRB561_CHECK_HIST ----".
    CALL "UTL_GET_DATE_TIME"    USING UTL_GET_DATE_TIME_WKSP.
    DISPLAY SP$_DATE_TIME_C23   IN UTL_GET_DATE_TIME_WKSP.

    DISPLAY "STATUS = ",SP$_ACW_PROC_AUX_STATUS with conversion.

*# 13-NOV-2012 20:12:04.70 - TP_TSARSU - Report to Splunk.
    COPY "ICA_SOURCE:ICA_TERM_PROG.INC".

    EXIT PROGRAM.

Z-EXIT.
    EXIT.

*============================================================
*               END OF PROGRAM ICA_TRB561_CHECK_HIST	    *
*============================================================

                                                                                                                                                                                                                                                                                                                                                                                                       
