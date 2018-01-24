2	&
	!	B A S E 6 4
	&
	!	Program  :	BASE64
		%Ident		"V1.0-01"
	!	Edit Date:	14-Nov-2011
	&
	&
	!	 Refer all Inquiries to:
	&
	!       Campus America, Inc.
	!	308 N Peters Rd, Suite 120
	!	Knoxville, TN  37922-2327
	!	Phone:  (865) 523-9506
	&
	&
	!*******************************************************************!
	!								    !
	!		   C O P Y R I G H T   N O T I C E		    !
	!								    !
	!								    !
	! Copyright (c) 2011                                                !
	!								    !
	! Campus America, Inc., 308 N Peters Rd, Suite 120, Knoxville, TN   !
	!								    !
	! All Rights Expressly Reserved					    !
	!								    !
	! This software is furnished under a license for use only on a      !
	! single computer system and may not be copied except under such    !
	! terms and conditions as may be authorized by Campus America or    !
	! any of its affiliates in writing. All copies shall include the    !
	! above copyright notice conspicuously displayed in a manner so as  !
	! to bring it to the attention of all users. This software and any  !
	! copies or portions thereof may not be provided or otherwise made  !
	! available or disclosed to any other person except for use on such !
	! system and only to one who agrees in writing to these license     !
	! terms. Title to and ownership of this software and all copies of  !
	! or any portion thereof shall at all times remain in               !
	! Campus America.                                                   !
	!*******************************************************************!
	!								    !
	!	  R E S T R I C T E D    R I G H T S    L E G E N D	    !
	!								    !
	! Use, duplication, or disclosure by the Government is subject to   !
	! restrictions as set forth in paragraph (b)(3)(B) of the Rights in !
	! Technical Data and Computer Software clause in DAR 7-104(a).      !
	!								    !
	! Campus America, Inc., 308 N Peters Rd, Suite 120, Knoxville, TN   !
	!*******************************************************************!
	%Page

10	! Structured for VAX BASIC V3

	&
	!	M O D I F I C A T I O N   H I S T O R Y
	&
	!   Date      Initials  Reason

	&
	!	S P E C I A L   P R O G R A M   F O R M A T S
	&
	!	P R O G R A M M I N G   N O T I C E S
	&
	!  Line number		Notice

200	%Page
	&
	!	B U I L D I N G   B L O C K S   U S E D
	&

201	! FORMAT	V6.2	Skeleton Program Format


300	%Page
	&
	!	D A T A   S T R U C T U R E   D E S C R I P T I O N S
	&

	%Page
	&
	!	C O M P I L A T I O N   C O N D I T I O N A L S
	&

	%Page
	&
	!	D A T A T Y P E   D E F I N I T I O N S
	&

	%Page
	&
	!	V A R I A B L E   D E C L A R A T I O N S
	&

	DECLARE STRING CONSTANT IDENT = "V1.0-01"
		! Version/Edit for header and errors

799	%Page
	&
	!	E X T E R N A L   D E C L A R A T I O N S
	&

	EXTERNAL LONG FUNCTION	GET_FILE_INFO()

840	EXTERNAL LONG FUNCTION &
		LIB$EXTZV, &
		LIB$GET_FOREIGN, &
		LIB$SPAWN, &
		LIB$SYS_GETMSG

849	%Page
	&
	!	C O M M O N / M A P   D E C L A R A T I O N S
	&

	COMMON (FILE_INFO)	LONG	FILE_ORG, &
				LONG	FILE_RFM, &
				LONG	FILE_RAT, &
				LONG	FILE_EBK, &
				LONG	FILE_FFB

	MAP (WRKMAP)	STRING	B64STR=64, &
			STRING	WRKSTR=4
	MAP (WRKMAP)	BYTE	B64TBL(63), &
			BYTE	WRKBYT(3)
	MAP (WRKMAP)	STRING	B64CHR(63)=1, &
			STRING	WRKCHR(3)=1

	%Page
	&
	!	D I M E N S I O N   S T A T E M E N T S
	&

950	! Utility DIMension Statements

999	%Page
	&
	!	B E G I N N I N G   O F   T H E   P R O G R A M
	&

1000	ON ERROR GOTO 19000
		! Set the standard error trap

1005	!GOTO 1039	IF ENTRY% <> 0%
		! ENTRY% = Entry parameter set on CHAIN or DCL entry
		! Skip the program name and such if special entry

1010	!PRINT "BASE64 ";IDENT;" - Base64 encoder/decoder ";
		! Print who we are

	!PRINT SP;TAB(60%);TIME$(0%)
	!PRINT "Copyright (c) 2011, Campus America, Inc."
		! Print the time

1039	&
	!	V A R I A B L E   I N I T I A L I Z A T I O N
	&

1040	B64STR = &
	    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	ZEROES$ = STRING$(4%, 0%)

1149	! Utility Constants

1199	%Page
	!	1 2 0 0   -   1 9 9 8
	&
	!	P R E L I M I N A R Y   L O G I C
	&

1200	CALL LIB$GET_FOREIGN(CMDLIN$)
	CMDLIN$ = EDIT$(CMDLIN$, 8%+16%+32%+128%)
	IF	CMDLIN$ = ""
	THEN	PRINT "?MCR command entry only"
		GOTO 9000
	END IF

	Z% = INSTR(1%, CMDLIN$, SP)
	CMD$ = LEFT(CMDLIN$, Z%-1%)
	CMDLIN$ = RIGHT(CMDLIN$, Z%+1%)
	SELECT	CMD$
	CASE "DECODE"
		!ENCDEC% = 0%
	CASE "ENCODE"
		ENCDEC% = 1%
	CASE ELSE
		PRINT "?Invalid operation - ";CMD$
		GOTO 9000
	END SELECT

	Z% = INSTR(1%, CMDLIN$, SP)
	INFILE$ = LEFT(CMDLIN$, Z%-1%)
	OUTFILE$ = RIGHT(CMDLIN$, Z%+1%)
	IF	INFILE$ = "" OR OUTFILE$ = ""
	THEN	PRINT "?Missing filespec - ";CMDLIN$
		GOTO 9000
	END IF

1900	GOTO ENCODE	IF ENCDEC%

1999	%Page
	!	2 0 0 0   -   8 9 9 9
	&
	!	M A I N   P R O G R A M   L O G I C
	&

2000	&
 DECODE:
	TXTOUT% = (RIGHT(OUTFILE$,LEN(OUTFILE$)-4%) = "/TEXT")
	OUTFILE$ = LEFT(OUTFILE$, LEN(OUTFILE$)-5%)	IF TXTOUT%
	WHEN ERROR IN
		OPEN INFILE$ FOR INPUT AS FILE 1%, ACCESS READ, &
			ORGANIZATION SEQUENTIAL VARIABLE, RECORDTYPE ANY
	USE
		PRINT ERT$(ERR);" opening ";INFILE$
		CONTINUE 9000
	END WHEN
	WHEN ERROR IN
		IF	TXTOUT%
		THEN	OPEN OUTFILE$ FOR OUTPUT AS FILE 2%, RECORDSIZE 32767%
		ELSE	OPEN OUTFILE$ FOR OUTPUT AS FILE 2%, &
				ORGANIZATION SEQUENTIAL FIXED, &
				RECORDTYPE NONE, RECORDSIZE 512%
		END IF
	USE
		PRINT ERT$(ERR);" creating ";OUTFILE$
		CONTINUE 9000
	END WHEN

2100	WHILE -1%
	WHEN ERROR IN
		LINPUT #1%, INDAT$
	USE
		CONTINUE 2200	IF ERR = 11%
		PRINT ERT$(ERR);" reading ";INFILE$
		CONTINUE 9000
	END WHEN
	DATBUF$ = DATBUF$ + INDAT$
	SEGS% = LEN(DATBUF$) / 4%
	ITERATE  IF SEGS% = 0%
	WRKLEN% = SEGS% * 4%

	FOR PTR% = 1% TO WRKLEN% STEP 4%
	WRKSTR = MID(DATBUF$, PTR%, 4%)
	B1% = INSTR(1%, B64STR, WRKCHR(0%)) - 1%
	B2% = INSTR(1%, B64STR, WRKCHR(1%)) - 1%
	O1$ = CHR$(B1% * 4% + B2% / 16%)
	IF	WRKBYT(2%) = A'='
	THEN	O2$, O3$ = ""
	ELSE	B3% = INSTR(1%, B64STR, WRKCHR(2%)) - 1%
		O2$ = CHR$((B2% AND 15%) * 16% + B3% / 4%)
		IF	WRKBYT(3%) = A'='
		THEN	O3$ = ""
		ELSE	B4% = INSTR(1%, B64STR, WRKCHR(3%)) - 1%
			O3$ = CHR$((B3% AND 3%) * 64% + B4%)
		END IF
	END IF
	OUTSTR$ = OUTSTR$ + O1$ + O2$ + O3$

	IF	TXTOUT%
	THEN	Z% = INSTR(1%, OUTSTR$, LF)
		IF	Z%
		THEN	PRINT #2%, LEFT(OUTSTR$,Z%);
			OUTSTR$ = RIGHT(OUTSTR$, Z%+1%)
		END IF
		ITERATE
	END IF

	IF	LEN(OUTSTR$) >= 512%
	THEN	MOVE TO #2%, OUTSTR$=512%
		WHEN ERROR IN
			PUT #2%
			OUTBLK% = OUTBLK% + 1%
		USE
			PRINT ERT$(ERR);" writing ";OUTFILE$
			CONTINUE 9000
		END WHEN
		OUTSTR$ = RIGHT(OUTSTR$, 513%)
	END IF

	NEXT PTR%
	DATBUF$ = RIGHT(DATBUF$, WRKLEN%+1%)

	NEXT

2200	GOTO 2900	IF LEN(OUTSTR$) = 0%
	IF	TXTOUT%
	THEN	PRINT #2%, OUTSTR$;
		GOTO 2900
	END IF

2400	WHILE LEN(OUTDAT$) >= 512%
	MOVE TO #2%, OUTSTR$=512%
	WHEN ERROR IN
		PUT #2%
		OUTBLK% = OUTBLK% + 1%
	USE
		PRINT ERT$(ERR);" writing ";OUTFILE$
		CONTINUE 9000
	END WHEN
	OUTSTR$ = RIGHT(OUTSTR$, 513%)
	NEXT

	REMAIN% = LEN(OUTSTR$)
	GOTO 2900	IF REMAIN% = 0%
	OUTSTR$ = OUTSTR$ + STRING$(REMAIN%, 0%)
	MOVE TO #2%, OUTSTR$=512%
	WHEN ERROR IN
		PUT #2%
		OUTBLK% = OUTBLK% + 1%
	USE
		PRINT ERT$(ERR);" writing ";OUTFILE$
		CONTINUE 9000
	END WHEN

	CLOSE 2%
        CMD$ = "SET FILE/ATTR=(EBK:"+NUM1$(OUTBLK%) + &
                                ",FFB:"+NUM1$(REMAIN%)+") " + OUTFILE$
        STS% = LIB$SPAWN(CMD$,,, 2%)
	IF	(STS% AND 7%) <> 1%
	THEN	CALL LIB$SYS_GETMSG(STS%,, Z$, 1%)
		PRINT Z$;" setting EBK/FFB on ";OUTFILE$
	END IF


2900	GOTO 9000
		
3000	&
 ENCODE:
	WHEN ERROR IN
		OPEN INFILE$ FOR INPUT AS FILE 1%, ACCESS READ
		TXTIN% = -1%
		GOTO 3100
	USE
		IF	ERR <> 160% AND ERR <> 228%
		THEN	PRINT ERT$(ERR);" opening ";INFILE$
			CONTINUE 9000
		END IF
	END WHEN
	WHEN ERROR IN
		OPEN INFILE$ FOR INPUT AS FILE 1%, ACCESS READ, &
			ORGANIZATION UNDEFINED, RECORDTYPE ANY, &
			USEROPEN GET_FILE_INFO
	USE
		PRINT ERT$(ERR);" opening ";INFILE$
		CONTINUE 9000
	END WHEN
	CHKEBK% = (FILE_EBK <> 0% AND FILE_FFB <> 0%)

3100	WHEN ERROR IN
		OPEN OUTFILE$ FOR OUTPUT AS FILE 2%, &
			ORGANIZATION SEQUENTIAL VARIABLE
	USE
		PRINT ERT$(ERR);" creating ";OUTFILE$
		CONTINUE 9000
	END WHEN

3200	WHILE -1%
	WHEN ERROR IN
		IF	TXTIN%
		THEN	INPUT LINE #1%, INDAT$
		ELSE	GET #1%
			MOVE FROM #1%, INDAT$=RECOUNT
			INBLK% = INBLK% + 1%
		END IF
	USE
		CONTINUE 3400	IF ERR = 11%
		PRINT ERT$(ERR);" reading ";INFILE$
		CONTINUE 9000
	END WHEN
	INDAT$ = LEFT(INDAT$, FILE_FFB)  IF INBLK% = FILE_EBK	IF CHKEBK%
	DATBUF$ = DATBUF$ + INDAT$

	SEGS% = MIN(LEN(DATBUF$),32767%) / 3%
	ITERATE  IF SEGS% = 0%
	WRKLEN% = SEGS% * 3%

	FOR PTR% = 1% TO WRKLEN% STEP 3%
	WRKSTR = MID(DATBUF$, PTR%, 3%)
	O1% = LIB$EXTZV(2%, 6%, WRKBYT(0%))
	O2% = LIB$EXTZV(4%, 4%, WRKBYT(1%))
	O2% = (O2% OR ((WRKBYT(0%) AND 3%) * 16%))
	O3% = LIB$EXTZV(6%, 2%, WRKBYT(2%))
	O3% = (O3% OR ((WRKBYT(1%) AND 15%) * 4%))
	O4% = (WRKBYT(2%) AND 63%)
	OUTSTR$ = OUTSTR$ + B64CHR(O1%)+B64CHR(O2%)+B64CHR(O3%)+B64CHR(O4%)
	IF	LEN(OUTSTR$) >= 72%
	THEN	PRINT #2%, LEFT(OUTSTR$, 72%)
		OUTSTR$ = RIGHT(OUTSTR$, 73%)
	END IF

	NEXT PTR%
	DATBUF$ = RIGHT(DATBUF$, WRKLEN%+1%)

	NEXT

3400	! Here with two or fewer chars left to encode
	WRKLEN% = LEN(DATBUF$)
	GOTO 3900	IF WRKLEN% = 0%
	WRKSTR = DATBUF$ + ZEROES$
	O1% = LIB$EXTZV(2%, 6%, WRKBYT(0%))
	O2% = LIB$EXTZV(4%, 4%, WRKBYT(1%))
	O2% = (O2% OR ((WRKBYT(0%) AND 3%) * 16%))
	O3% = LIB$EXTZV(6%, 2%, WRKBYT(2%))
	O3% = (O3% OR ((WRKBYT(1%) AND 15%) * 4%))
	OUTSTR$ = OUTSTR$ + B64CHR(O1%)+B64CHR(O2%)
	IF	WRKLEN% = 1%
	THEN	OUTSTR$ = OUTSTR$ + "=="
		GOTO 3900
	END IF
	OUTSTR$ = OUTSTR$ + B64CHR(O3%) + "="	

3900	WHILE LEN(OUTSTR$)
	PRINT #2%, LEFT(OUTSTR$, 72%)
	OUTSTR$ = RIGHT(OUTSTR$, 73%)
	NEXT

	! GOTO 9000

9000	%Page
	&
	!	E N D   O F   T H E   P R O G R A M
	&

9010	CLOSE 1%, 2%

9997	ON ERROR GO BACK
	!PRINT SP
	!PRINT "Finished    ";TIME$(0%)
		! Print the time and end

	GOTO 32000

9999	%Page
	!	1 0 0 0 0   -   1 6 9 9 8
	&
	!	U S E R   S U B R O U T I N E S
	&

16999	%Page
	!	1 7 0 0 0   -   1 8 9 9 9
	&
	!	U S E R   F U N C T I O N S
	&

19000	%Page
	!	S T A N D A R D   E R R O R   T R A P
	&
		! NOTE: 19001-19099 and 19501-19599 are
		!	reserved for utility routine traps.

19500	ERR$ = ERT$(ERR)
		! ERR$ = Text of the error that occurred

19990	PRINT ERR$;" at line";ERL;"in ";ERN$;SP;IDENT;BEL
	RESUME 9000
		! End of the error handler

19999	%Page
	!	2 0 0 0 0   -   2 2 9 9 8
	&
	!	P A C K A G E   S U B R O U T I N E S
	&

22999	%Page
	!	2 3 0 0 0   -   2 4 9 9 8
	&
	!	P A C K A G E   F U N C T I O N S
	&

24999	%Page
	!	2 5 0 0 0   -   2 6 9 9 8
	&
	!	U T I L I T Y   S U B R O U T I N E S
	&

26999	%Page
	!	2 7 0 0 0   -   2 9 9 9 8
	&
	!	U T I L I T Y   F U N C T I O N S
	&

32000	END

32100	FUNCTION LONG GET_FILE_INFO(FAB FAB, LONG RAB, LONG CHNL)

32110	RECORD	FAB
		BYTE	FAB$B_BID
		BYTE	FAB$B_BLN
		WORD	FAB$W_IFI
		LONG	FAB$L_FOP
		LONG	FAB$L_STS
		LONG	FAB$L_STV
		LONG	FAB$L_ALQ
		WORD	FAB$W_DEQ
		BYTE	FAB$B_FAC
		BYTE	FAB$B_SHR
		LONG	FAB$L_CTX
		BYTE	FAB$B_RTV
		BYTE	FAB$B_ORG
		BYTE	FAB$B_RAT
		BYTE	FAB$B_RFM
		LONG	FAB$L_JNL
		LONG	FAB$L_XAB
		LONG	FAB$L_NAM
		LONG	FAB$L_FNA
		LONG	FAB$L_DNA
		BYTE	FAB$B_FNS
		BYTE	FAB$B_DNS
		WORD	FAB$W_MRS
		LONG	FAB$L_MRN
		WORD	FAB$W_BLS
		BYTE	FAB$B_BKS
		BYTE	FAB$B_FSZ
		LONG	FAB$L_DEV
		LONG	FAB$L_SDC
		WORD	FAB$W_GBC
		BYTE	FAB$B_ACMODES
		BYTE	FAB$B_RCF
		LONG	FILL
	END RECORD FAB
	DECLARE LONG CONSTANT FAB$K_BLN = 80

	RECORD	XABFHC
		BYTE	XAB$B_COD
		BYTE	XAB$B_BLN
		WORD	FILL
		LONG	XAB$L_NXT
		BYTE	XAB$B_RFO
		BYTE	XAB$B_ATR
		WORD	XAB$W_LRL
		VARIANT
		CASE
			LONG	XAB$L_HBK
			LONG	XAB$L_EBK
		CASE
			WORD	XAB$W_HBK0
			WORD	XAB$W_HBK2
			WORD	XAB$W_EBK0
			WORD	XAB$W_EBK2
		END VARIANT
		WORD	XAB$W_FFB
		BYTE	FILL
		BYTE	XAB$B_HSZ
		WORD	XAB$W_MRZ
		WORD	XAB$W_DXQ
		WORD	XAB$W_GBC
		BYTE	FILL(8)
		WORD	XAB$W_VERLIMIT
		LONG	XAB$L_SBN
	END RECORD XABFHC
	DECLARE LONG CONSTANT XAB$K_FHCLEN = 44

	DECLARE XABFHC	XABFHC

	EXTERNAL LONG CONSTANT	RMS$_NORMAL, &
				SS$_NORMAL

	EXTERNAL BYTE CONSTANT	XAB$C_FHC

	EXTERNAL LONG FUNCTION &
		SYS$CONNECT, &
		SYS$OPEN

	COMMON (FILE_INFO)	LONG	FILE_ORG, &
				LONG	FILE_RFM, &
				LONG	FILE_RAT, &
				LONG	FILE_EBK, &
				LONG	FILE_FFB

32120	FILE_EBK = 0%
	FILE_FFB = 0%

32130	STS% = SYS$OPEN(FAB)
		! Open the file for input

	STS% = SYS$CONNECT(RAB)	IF STS% = RMS$_NORMAL
	GET_FILE_INFO = STS%
	EXIT FUNCTION	IF STS% <> RMS$_NORMAL
		! Connect stream if open was successful, exit if not

32140	FILE_ORG = FAB::FAB$B_ORG
	FILE_RFM = FAB::FAB$B_RFM
	FILE_RAT = FAB::FAB$B_RAT

32150	XABPTR% = FAB::FAB$L_XAB
 FIND_XABFHC:
	WHILE XABPTR%
	CALL LIB$MOVC3(8%, XABPTR% BY VALUE, XABFHC)
	GOTO 32160	IF XABFHC::XAB$B_COD = XAB$C_FHC
	XABPTR% = XABFHC::XAB$L_NXT
	NEXT
	EXIT FUNCTION
	
32160	CALL LIB$MOVC3(XAB$K_FHCLEN, XABPTR% BY VALUE, XABFHC)
	FILE_EBK = XABFHC::XAB$L_EBK
	FILE_FFB = XABFHC::XAB$W_FFB

32190	END FUNCTION
