 *#################### ICA_INIT_APPL.INC (BEGIN OF INC) #############*
*#
*# FIRST COMMAND IN MAIN PROGRAM (Once per image)
*#
*###################################################################*

*#--------------------------------------------------------- 
*#  27/09/1998 tp_dnirel  
*#  
*#  TLG INIT - Initialization of Transaction logger's stack 
*#--------------------------------------------------------- 

	CALL 'UTL_INIT_ROUTINE_NAME_STACK'

*#--------------------------------------------------------- 

*#------------------------------------------------------------- 
*#  05/05/1999 tp_NREGUA  
*#  
*#  INIT - Initialization of external variable that will assure
*#         one sign-in and one sign-out in programs that use
*#	   ica_svf_aptuser_call (or utl_aptuser_call_interface)
*#-------------------------------------------------------------- 

*!!!	CALL 'ICA_SVF_INIT_FOR_SIGN_IN'

*#--------------------------------------------------------- 

*#################### ICA_INIT_APPL.INC (END   OF INC) ###############*
