set pf="Program Files"
set config=G:\ProjDir\Africa\trunk\cfg
set geodmsversion=GeoDMS7166
set country1=GHA
set country2=BFA

rem C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms PerCountryGroup/%country1%/VariablePreparation/Landuse
C:\%pf%\ObjectVision\%geodmsversion%\GeoDmsRun.exe /Lbatchtrace.log %config%\Africa.dms PerCountryGroup/%country2%/VariablePreparation/Landuse

pause