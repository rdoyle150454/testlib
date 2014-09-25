2	FUNCTION LONG JXF_OPEN_REG

	&
	!	J X F _ O P E N _ R E G
	&
	!	Routine  :	JXF_OPEN_REG
		%Ident		"V2.1-03"
	!	Edit Date:	15-Mar-2005
	&
	&
	!	 Refer all Inquiries to:
	&
	!       Campus America, Inc.
	!	900 Hill Avenue, Suite 205
	!	Knoxville, TN  37915-2580
	!	Phone:  (865) 523-9506
	&
	&
	!*******************************************************************!
	!								    !
	!		   C O P Y R I G H T   N O T I C E		    !
	!								    !
	!								    !
	! Copyright (c) 2000, 2001, 2003 Campus America                     !
	!								    !
	! Campus America, Inc., 900 Hill Ave, Ste 205, Knoxville, TN  37915 !
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
	! Campus America, Inc., 900 Hill Ave, Ste 205, Knoxville, TN  37915 !
	!*******************************************************************!

10	! Structured for VAX BASIC V3

	&
	!	M O D I F I C A T I O N   H I S T O R Y
	&
	!   Date      Initials  Reason

100	&
	!	R O U T I N E   P A R A M E T E R S
	&
	! Input Parameters:
	&
	! None
	&
	! Routine Output:
	&
	! Result:		0 if no error
	!			-ERR if error in open or field info load
	&
	! Field info variables loaded in COMMON
	&

	&
	!	D A T A T Y P E   D E F I N I T I O N S
	&

	%Include "COMMON"
	%Include "CONSTANT"

	&
	!	V A R I A B L E   D E C L A R A T I O N S
	&

	DECLARE STRING CONSTANT	IDENT = "V2.1-03"
		! Version/edit for errors

	&
	!	E X T E R N A L   D E C L A R A T I O N S
	&

	EXTERNAL LONG FUNCTION &
		REG_NAME_TO_FLD(LONG,STRING,LONG,DOUBLE), &
		REG_OPEN_FILES(LONG,LONG), &
		REG_OPEN_REG(LONG)

840	EXTERNAL LONG FUNCTION &
		FIO_OPEN_DSC(LONG,STRING), &
		FIO_SET_OPTION(LONG,LONG)
	EXTERNAL STRING FUNCTION &
		FIO_ERT()

849	&
	!	C O M M O N / M A P   D E C L A R A T I O N S
	&

	COMMON (JXF___CMN_REGINFO)	STUDENTID_DSC, &
					COURSEID_DSC, &
					TERMCODE_DSC, &
					COURSE_DSC, &
					SECTION_DSC, &
					DEPARTMENT_DSC, &
					MASTERCOURSE_DSC, &
					COURSE_REC%

	&
	!	D I M E N S I O N   S T A T E M E N T S
	&

	&
	!	B E G I N N I N G   O F   T H E   R O U T I N E
	&

1000	ON ERROR GOTO 19000
		! Setup the standard error trap routine

	&
	!	V A R I A B L E   I N I T I A L I Z A T I O N
	&

	&
	!	1 2 0 0   -   1 9 9 7
	&
	!	P R E L I M I N A R Y   L O G I C
	&

1200	! Reg init
%If %VARIANT < 30 %Then
	Z% = FIO_OPEN_DSC(C$DS1%, C$STD$)
	IF	Z% < 0%
	THEN	PRINT FIO_ERT;" opening ";C$STD$;".DSC"
		JXF_OPEN_REG = Z%
	END IF
	Z% = FIO_OPEN_DSC(C$DS2%, C$CRS$)
	IF	Z% < 0%
	THEN	PRINT FIO_ERT;" opening ";C$CRS$;".DSC"
		JXF_OPEN_REG = Z%
	END IF
	JC$STD% = C$DS1%
	JC$CRS% = C$DS2%
		! Open description files, set channel numbers
		! Note: We leave these open for performance
%Else
	JC$STD% = STD_FILE
	JC$CRS% = CRS_FILE
		! Set file numbers
%End %If	

	&
	!	M A I N   R O U T I N E   L O G I C
	&

2000	Z9% = FIO_SET_OPTION(,)
	Z0% = REG_OPEN_FILES(1%+2%+16%+2048%, 1%+8192%)
	Z0% = REG_OPEN_REG(2%)	UNLESS Z0% < 0%
	JXF_OPEN_REG = Z0%
	GOTO 9000	IF Z0% < 0%
		! Open (read-only) STUDENT, COURSE and PATTERN (Reg 2.6 only);
		!   set up minor description info for Reg 2.6 (2048)
		! Open REGDAT, read-only

	STUDENTID_DSC = FNDSC(JC$STD%, ".STUDENTID")
	COURSEID_DSC = FNDSC(JC$CRS%, ".COURSEID")
	TERMCODE_DSC = FNDSC(JC$CRS%, ".TERMCODE")
	COURSE_DSC = FNDSC(JC$CRS%, ".COURSE")
	SECTION_DSC = FNDSC(JC$CRS%, ".SECTION")
	DEPARTMENT_DSC = FNDSC(JC$CRS%, ".DEPARTMENT")
		! Set field info needed by JXF_GET_REGDAT_FLD

	IF	FLDERR%
	THEN	JXF_OPEN_REG = FLDERR%
		GOTO 9000
	END IF

	CALL FIO_SET_OPTION(1%, 0%)	IF (Z9% AND 1%)
	MASTERCOURSE_DSC = FNODSC(JC$CRS%, ".MASTERCOURSE")
		! Set info for optional fields

9000	&
	!	E N D   O F   T H E   R O U T I N E
	&

	CALL FIO_SET_OPTION(1%, 0%)	IF (Z9% AND 1%)

	EXIT FUNCTION

	&
	!	S U B R O U T I N E S
	&

	&
	!	F U N C T I O N S
	&

	DEF FNDSC(FIL%, FLD$)
	Z% = REG_NAME_TO_FLD(FIL%, FLD$, 0%, Z)
	IF	Z% < 1%
	THEN	FLDERR% = MIN(Z%, -1%)	UNLESS FLDERR%
		FNDSC = 0.
	ELSE	FNDSC = Z
	END IF
	END DEF

	DEF FNODSC(FIL%, FLD$)
	Z% = REG_NAME_TO_FLD(FIL%, FLD$, 1%, Z)
	IF	Z% < 1%
	THEN	FNODSC = 0.
	ELSE	FNODSC = Z
	END IF
	END DEF

19000	&
	!	S T A N D A R D   E R R O R   T R A P
	&

	ER$ = ERT$(ERR) + " at line " + NUM1$(ERL) + " in " + ERN$+SP+IDENT
	PRINT ER$
	RESUME 9000
		! End of error handler
		! Return abort to caller

32767	END FUNCTION
