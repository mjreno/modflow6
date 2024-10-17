set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
dir ..
dir ..\netcdf
dir ..\netcdf\netCDF4.9.2-NC4-64
dir ..\netcdf\netCDF4.9.2-NC4-64\lib
dir ..\netcdf\netCDF4.9.2-NC4-64\bin
dir ..\netcdf\netcdf-fortran-4.6.1
dir ..\netcdf\netcdf-fortran-4.6.1\build
dir ..\netcdf\netcdf-fortran-4.6.1\build\fortran
pixi run setup -Dextended=true builddir
pixi run build builddir
pixi run test builddir
