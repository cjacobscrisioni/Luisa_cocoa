set pf="Program Files"
set config=F:\ProjDir\Luisa_cocoa\cfg
set geodmsversion=GeoDMS14.15.4
set country1=GHA
set country2=BFA

C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms /PerCountryGroup/%country1%/ModelIterations/y2050/ResultingState/cumu_store
C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms /PerCountryGroup/%country1%/ResultsCollection/store_cocoa_totals
rem C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms PerCountryGroup/%country2%/VariablePreparation/Landuse

pause