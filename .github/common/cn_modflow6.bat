echo "hi"

:: set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\;%PKG_CONFIG_PATH%
:: set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib\pkgconfig\;%PKG_CONFIG_PATH%
:: set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include;%PATH%
:: set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran;%PATH%
:: set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib;%PATH%
:: set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build;%PATH%
:: set LIBPATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib;%LIBPATH%
:: set LIBPATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran;%LIBPATH%

:: dir %GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build
:: dir %GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran

:: echo "%PKG_CONFIG_PATH%"
:: echo "%PATH%"

:: cd "%GITHUB_WORKSPACE%\netcdf"
:: ifort /include:%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include /include:%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran simple.f90 /link /libpath:%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran /libpath:%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib netcdff.lib netcdf.lib

:: simple.exe
:: dir .

:: where nc-config
:: where nf-config
:: nc-config
:: nf-config --all

set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
:: pixi run pytest -v --durations=0 --netcdf -k "test_netcdf"
