set pf="Program Files"
set config=F:\ProjDir\Luisa_cocoa\cfg
set geodmsversion=GeoDMS17.2.5
set country1=GHA

rem "id";"LabelText"
rem 0;'SSP2_Cocoa_static'
rem 1;'SSP2_Cocoa_uncons_bau'
rem 2;'SSP2_EUDR_0'
rem 3;'SSP2_Cocoa_cons_BAU'
rem 4;'SSP2_EUDR_0_cons'
rem 5;'SSP2_Cocoa_cons_BAU_protected'
rem 6;'SSP2_EUDR_0_cons_no_nc'
rem 7;'SSP2_BAU_cons_no_nc'


set scenariolist=SSP2_Cocoa_cons_BAU SSP2_EUDR_0_cons SSP2_Cocoa_cons_BAU_protected SSP2_EUDR_0_cons_no_nc SSP2_BAU_cons_protected_no_nc

for %%s in (%scenariolist%) do (
	
	echo %%s
	set ScenarioName=%%s
	
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms /PerCountryGroup/%country1%/ModelIterations/GenButton/store_all
	C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms /PerCountryGroup/%country1%/ResultsCollection/gen_button/run

rem C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms PerCountryGroup/%country2%/VariablePreparation/Landuse
)

pause