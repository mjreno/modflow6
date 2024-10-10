cd "%GITHUB_WORKSPACE%\netcdf"
dir netCDF4.9.2-NC4-64
dir netcdf-fortran-4.6.1_build

cd netcdf-fortran-4.6.1_build\build

where ifort
where conda
where mamba
where micromamba
cmake --version
cmake --help

ifort temp.f90 -o hello.exe
hello.exe

:: set CMAKE_Fortran_COMPILER=C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin\ifort.exe
:: set CMAKE_Fortran_COMPILER=ifort
set FC=ifort

:: FIRST BUILD/INSTALL STATIC LIBS
:: cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/lib/netcdf.lib -DNETCDF_C_INCLUDE_DIR=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/include -DBUILD_SHARED_LIBS=0 -DENABLE_TESTS=1 -DCMAKE_INSTALL_PREFIX=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netcdf-f/build ../netcdf-fortran-4.6.1

:: D:\a\modflow6\modflow6/netcdf
:: D:/a/modflow6/modflow6

:: cmake --fresh -A x64 -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1

cmake --fresh -G "Visual Studio 17 2022" -A x64 -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="D:/a/modflow6/modflow6/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="D:/a/modflow6/modflow6/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
:: cmake –install . --config Release
cmake –install .

:: NOW BUILD/INSTALL SHARED
:: cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/lib/netcdf.lib -DNETCDF_C_INCLUDE_DIR=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/include -DBUILD_SHARED_LIBS=1 -DENABLE_TESTS=1 -DCMAKE_INSTALL_PREFIX=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netcdf-f/build ../netcdf-fortran-4.6.1

cmake --fresh -G "Visual Studio 17 2022" -A x64 -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%/netcdf/netCDF4.9.2-NC4-64/include" -DBUILD_SHARED_LIBS=1 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.1_build/build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
:: cmake –install . --config Release
cmake –install .

:: set PKG_CONFIG_PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netcdf-f\build\lib\pkgconfig\;%PKG_CONFIG_PATH%
:: set PKG_CONFIG_PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netCDF4.9.2-NC4-64\lib\pkgconfig\;%PKG_CONFIG_PATH%
:: set PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netCDF4.9.2-NC4-64\include;%PATH%
:: set PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netcdf-f\build\fortran\Release;%PATH%
:: set PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netCDF4.9.2-NC4-64\bin;%PATH%
:: set PATH=C:\Users\mreno\local\netcdf\build-windows\builds\b4\build\netcdf-f\build\bin;%PATH%

set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\lib\pkgconfig\;%PKG_CONFIG_PATH%
set PKG_CONFIG_PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib\pkgconfig\;%PKG_CONFIG_PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\fortran\Release;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\bin;%PATH%
set PATH=%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build\bin;%PATH%

:: set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
pixi run test builddir
pixi run pytest -v --durations=0 --keep-failed .failed --netcdf -k "test_netcdf"
