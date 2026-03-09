set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.3-NC4-64\bin;%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.2\build\fortran;%PATH%
cd "%GITHUB_WORKSPACE%\modflow6\autotest"
pixi run autotest -m "%MARKERS%" -k "%FILTERS%" --netcdf --parallel