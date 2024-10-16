set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
dir ..
dir ..\netcdf
dir ..\netcdf\netCDF4.9.2-NC4-64
dir ..\netcdf\netCDF4.9.2-NC4-64\include
dir ..\..
pixi run setup -Dextended=true builddir
pixi run build builddir
pixi run test builddir
