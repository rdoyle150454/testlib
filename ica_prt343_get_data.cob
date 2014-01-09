*###### PROGRAM_NAME:  [ICA_PRT343_GET_DATA.COB        ] ###########*           
*#                                                                 #*           
*#                   [                                ] : תכרעמ תת #*
*#                                [ICA_PRT343_GET_DATA] :לודומה םש #*
*#                                                                 #*           
*#                                                :ילנויצקנופ רואת #*           
*#                                [DATA PANEL ל 
"הרוש" תאירק ]     #*           
*#                                                                 #*           
*#                                                         :םיחתפמ #*           
*#         ןורחא ןוכדע .ת        ךיראת       עצבמ םש       בלש     #*           
*#                 [    ]    [22-MAR-1994] [TP_ASAKA]     בוציע   #*           
*#                 [    ]    [22-MAR-1994] [TP_ASAKA]     תונכת   #*           
*#                                                                 #*       
    
*#                                                        :םירטמרפ #*           
*#                                          [    ] :עדימ תינבת     #*           
*#         (שומיש ,םיכרע ,רבסה) רואת   פ/ק           הדש םש        #*           
*#                            [    ]  [  ]           [    ]        #*           
*#                                                                 #*           
*#                                                     :עדימ ירגאמ #*           
*#               תירב
עב םש / רואת    פ/ק             הלבט/ץבוק     #*           
*#                         [    ]   [  ]                [    ]     #*           
*#                                                                 #*           
*#                                                  :םישגדהו תורעה #*           
*#                                                      [    ]     #*           
*#                                                                 #*           
*#                                          :םי
יונישו םינוכדע בקעמ #*           
*#               יונישה רואת     עצבמ םש       ךיראת       CID     #*           
*#                    [    ]      [    ]      [    ]     [   ]     #*           
*###################################################################*           
*#                                                                              
*#                                                                              
*#                                                                       
       
*#                                                                              
*#                                                                              
*#                                                                              

******************************************************************************* 
* (2) OTHER ROUTINES:
* --------------------
* 
*
*    2.1 ICA_ICf_XLATE_MSG - message handling routine.
*
*
*           Input:  ICA_XLATE_MSG_WKSP
*                   UTL_CONTRO
L_ACW
*           Output: UTL_CONTROL_ACW
*
*
******************************************************************************* 
* MODIFICATION HISTORY:    Chg #     Date        Author     Description
*                          ----------------------------------------------- 
*
*
********************************************************************************

IDENTIFICATION DIVISION.
************************
PROGRAM-ID. ICA_PRT343_GET_DATA.
******************************



DATA DIVISION.
**************

*-
----------------------

WORKING-STORAGE SECTION.

*#  3-NOV-1998 21:20:30.89 - TP_GAHARO - include the TLG wksp
     COPY "ICA_CDD_WKSP:ICA_TLG_WKSP" FROM DICTIONARY.

*-----------------------
01 ICA_ICF_EXCEPTION_HANDLER
   PIC S9(9) COMP VALUE EXTERNAL ICA_ICF_EXCEPTION_HANDLER.

COPY "ICD_CDD_WKSP:ICD_PRT_343_CLB_MEM_BAS_DBW"     FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA_ERROR_HANDLE_WKSP"           FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_COMMON_DBW"                  FROM DICTIONARY.
COPY "ICA_CDD_WKSP:ICA
_XLATE_MSG_WKSP"              FROM DICTIONARY.
COPY "ICA_SOURCE:ICA_OPCODE_TYPES.INC".
COPY "UTL_SOURCE:UTL_SYMBOLS_DBA.INC".
COPY "UTL_SOURCE:UTL_MESSAGE.INC".
COPY "ICA_SOURCE:ICA_MESSAGE.INC".


* This indicator indicates whether to perform ROLLBACK operation in database
* while handling errors. (Because there are cases where ROLLBACK is not needed,
* such as when START TRANSACTION operation fails, or when calling dba routine
* "ICD_DBT_MASTER_KEY_NXT" which performs ROLLBACK/COMMIT).

*---------------
L
INKAGE SECTION.
*---------------

COPY "ICA_CDD_WKSP:ICA_PRT343_KEY"      FROM DICTIONARY
	REPLACING ICA_PRT343_KEY BY ICA_PRT343_KEY_INW.
COPY "ICA_CDD_WKSP:ICA_PRT343_DATA"      FROM DICTIONARY
	REPLACING ICA_PRT343_DATA BY ICA_PRT343_DATA_OTW.
COPY "ICA_CDD_WKSP:ICA_PRT343_SCROLL_OTW"         FROM DICTIONARY.
COPY "ICD_CDD_WKSP:ICD_PRT_343_CLB_MEM_BAS"           FROM DICTIONARY
	REPLACING ICD_PRT_343_CLB_MEM_BAS BY ICD_PRT_343_CLB_MEM_BAS_ORW.
COPY "ICA_CDD_WKSP:ICA_MPQ_CTW"                 FROM DICTIONA
RY.
COPY "UTL_CDD_WKSP:UTL_CONTROL_ACW"             FROM DICTIONARY.


***************************************************************

PROCEDURE DIVISION USING ICA_PRT343_KEY_INW
                         ICA_PRT343_DATA_OTW
                         ICA_PRT343_SCROLL_OTW
			 ICD_PRT_343_CLB_MEM_BAS_ORW
			 ICA_MPQ_CTW
                         UTL_CONTROL_ACW
		   GIVING SP$_ACW_PROC_AUX_STATUS.

***************************************************************


*--------------------------------------------
------
A-MAIN					    SECTION.
*--------------------------------------------------
A-00.

    PERFORM B-INIT.
    
    PERFORM C-PROCESS.

    PERFORM Z-FINISH.

A-EXIT.
    EXIT.


*--------------------------------------------------
B-INIT					    SECTION.
*--------------------------------------------------
B-00.


*#  3-NOV-1998 21:20:30.89 - TP_GAHARO - Move prog name to TLG wksp
     MOVE "ICA_PRT343_GET_DATA" 
       TO P$_PROG_NAME IN ICA_TLG_WKSP.

*#  3-NOV-1998 21:20:30.89 - TP_GAHARO - Call to TL
G interface routine (PUSH)
     COPY "ICA_SOURCE:ICA_INIT_PROG.INC".


    COPY "ICA_SOURCE:ICA_MUS_INIT_GET_DATA.INC".

*   Establish condition EXCEPTION_HANDLER.
    CALL 'LIB$ESTABLISH' USING BY VALUE ICA_ICF_EXCEPTION_HANDLER.

* initialize ERROR HANDLING routine workspace. 

    INITIALIZE ICA_ERROR_HANDLE_WKSP.

    MOVE V$_DISPLAY TO P$_OPCODE IN ICA_ERROR_HANDLE_WKSP.

* initialize DBA routine workspace. Put success value in return status.

    INITIALIZE ICD_PRT_343_CLB_MEM_BAS_DBW,
	       ICD_PRT
_343_CLB_MEM_BAS_ORW,
	       ICA_PRT343_DATA_OTW.  		    

    MOVE SP$_MSG_NORMAL TO SP$_ACW_PROC_AUX_STATUS.
    MOVE "S" TO SP$_ACW_FORM_MSG_SEVERITY.

    IF P$_NUM_OF_LINES IN ICA_MPQ_CTW = 0   
       MOVE "פ" TO P$_OPCODE IN ICA_MPQ_CTW
       MOVE  1  TO P$_CURRENT_NUM_SAVED IN ICA_MPQ_CTW
    END-IF.
          

B-EXIT.
    EXIT.


*--------------------------------------------------
C-PROCESS				    SECTION.
*--------------------------------------------------
C-00.

    IF P$_OPCODE IN ICA_MPQ_CT
W NOT = "פ" 
       PERFORM CA-PREPARE-KEY
       PERFORM CB-GET-REC
       PERFORM CC-HANDLE-DATA
    END-IF.

C-EXIT.
    EXIT.


*--------------------------------------------------
CA-PREPARE-KEY				    SECTION.
*--------------------------------------------------
CA-00.

    
    MOVE CORR ICA_PRT343_SCROLL_WKSP IN ICA_PRT343_SCROLL_OTW(P$_CURRENT_NUM_SAVED)
     TO ICD_PRT_343_CLB_MEM_BAS_PRW.

    MOVE CORR ICA_PRT343_SCROLL_WKSP IN ICA_PRT343_SCROLL_OTW(P$_CURRENT_NUM_SAVED)
     TO ICA_PRT343_DATA_O
TW.

CA-EXIT.
    EXIT.


*--------------------------------------------------
CB-GET-REC				    SECTION.
*--------------------------------------------------
CB-00.

*   Compute DP$_ACTION out of an READ ACTION and a DATABASE ACCESS
    MOVE DP$_SYM_TRAN_READ TO DP$_ACTION OF ICD_COMMON_DBW.

    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

    IF DP$_STATUS IN ICD_COMMON_DBW IS FAILURE
       MOVE DP$_STATUS IN ICD_COMMON_DBW TO SP$_ACW_PROC_AUX_STATUS
       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_COMMON_DBW TO
 SP$_ACW_FORM_MSG
       MOVE "SEC:CB-GET-REC RTN:ICD_COMMON_DBA" TO SP$_ACW_FREE_TEXT
       PERFORM Z-FINISH
    END-IF.


    ADD DP$_SYM_INQUIRE TO  DP$_SYM_ACCESS_RDB GIVING
	DP$_ACTION IN ICD_PRT_343_CLB_MEM_BAS_DBW.
    CALL 'ICD_PRT_343_CLB_MEM_BAS_DBA' USING ICD_PRT_343_CLB_MEM_BAS_DBW.

    IF DP$_STATUS IN ICD_PRT_343_CLB_MEM_BAS_DBW = SP$_MSG_NO_DATA_FOUND
       MOVE ICA_W_0005 TO SP$_ACW_PROC_AUX_STATUS
       CALL 'ICA_ICF_XLATE_MSG' USING UTL_CONTROL_ACW
                                     
 ICA_XLATE_MSG_WKSP
       PERFORM E-ROLLBACK
       PERFORM Z-FINISH
    END-IF.

    IF DP$_STATUS IN ICD_PRT_343_CLB_MEM_BAS_DBW IS FAILURE
       MOVE DP$_STATUS IN ICD_PRT_343_CLB_MEM_BAS_DBW TO SP$_ACW_PROC_AUX_STATUS
       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_PRT_343_CLB_MEM_BAS_DBW TO SP$_ACW_FORM_MSG
       MOVE "SEC:CB-GET-REC RTN:ICD_PRT_343_CLB_MEM_BAS_DBA" TO SP$_ACW_FREE_TEXT
       PERFORM E-ROLLBACK
       PERFORM Z-FINISH
    END-IF.
    
CB-EXIT.
    EXIT.


*-------------------------------
-------------------
CC-HANDLE-DATA				    SECTION.
*--------------------------------------------------
CC-00.

    MOVE CORR ICD_PRT_343_CLB_MEM_BAS IN ICD_PRT_343_CLB_MEM_BAS_DBW TO 
						   ICA_PRT343_DATA_OTW.

    MOVE CORR ICD_PRT_343_CLB_MEM_BAS IN ICD_PRT_343_CLB_MEM_BAS_DBW TO
						   ICD_PRT_343_CLB_MEM_BAS_ORW.

    MOVE DP$_SYM_COMMIT TO DP$_ACTION OF ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA' USING ICD_COMMON_DBW.

    IF DP$_STATUS IN ICD_COMMON_DBW IS FAILURE
       MOVE DP$_STATUS IN ICD_COM
MON_DBW TO SP$_ACW_PROC_AUX_STATUS
       MOVE DP$_STATUS_AUXIL_TEXT IN ICD_COMMON_DBW TO SP$_ACW_FORM_MSG
       MOVE "SEC:CC-HANDLE-DATA RTN:ICD_COMMON_DBA" TO SP$_ACW_FREE_TEXT
       PERFORM E-ROLLBACK
       PERFORM Z-FINISH
    END-IF.

CC-EXIT.
    EXIT.


*--------------------------------------------------
E-ROLLBACK				    SECTION.
*--------------------------------------------------
E-00.

* Rollback transaction.

    MOVE DP$_SYM_ROLLBACK TO DP$_ACTION OF ICD_COMMON_DBW.
    CALL 'ICD_COMMON_DBA'
 USING ICD_COMMON_DBW.

E-EXIT.
    EXIT.


*--------------------------------------------------
Z-FINISH				    SECTION.
*--------------------------------------------------
Z-00.

    COPY "ICA_SOURCE:ICA_MUS_TERM_GET_DATA.INC".

    CALL 'ICA_ICF_GENERAL_EXIT_ROUTINE' USING UTL_CONTROL_ACW.

    EXIT PROGRAM.

Z-EXIT.
    EXIT.

                                                                                                                                                                                   
