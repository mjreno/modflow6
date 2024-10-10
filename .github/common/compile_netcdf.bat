cd "%GITHUB_WORKSPACE%\netcdf"
dir netCDF4.9.2-NC4-64
dir netcdf-fortran-4.6.1_build

:: "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
dir "C:\Program Files (x86)\Intel\oneAPI"
"C:\Program Files (x86)\Intel\oneAPI\2024.1\oneapi-vars.bat"

ifort temp.f90 -o hello.exe
hello.exe

cd netcdf-fortran-4.6.1_build\build

where ifort
where ifx
cmake --version
:: cmake --help
:: where conda
:: where mamba
:: where micromamba

:: %GITHUB_WORKSPACE%
:: D:/a/modflow6/modflow6

:: BUILD/INSTALL STATIC LIBS
:: cmake --fresh -G "Visual Studio 17 2022" -A x64 -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="D:/a/modflow6/modflow6/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="D:/a/modflow6/modflow6/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
cmake –install .

:: BUILD/INSTALL SHARED LIBS
cmake --fresh -G "Visual Studio 17 2022" -A x64 -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=1 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
cmake –install .

set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\lib\pkgconfig\;%PKG_CONFIG_PATH%
set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib\pkgconfig\;%PKG_CONFIG_PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran\Release;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\bin;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\bin;%PATH%

set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
pixi run test builddir
pixi run pytest -v --durations=0 --keep-failed .failed --netcdf -k "test_netcdf"
