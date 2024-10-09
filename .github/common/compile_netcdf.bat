cd "%GITHUB_WORKSPACE%\netcdf"
dir netCDF4.9.2-NC4-64
dir netcdf-fortran-4.6.1_build

cd netcdf-fortran-4.6.1_build\build

:: FIRST BUILD/INSTALL STATIC LIBS
:: cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/lib/netcdf.lib -DNETCDF_C_INCLUDE_DIR=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/include -DBUILD_SHARED_LIBS=0 -DENABLE_TESTS=1 -DCMAKE_INSTALL_PREFIX=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netcdf-f/build ../netcdf-fortran-4.6.1

cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib\netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
cmake –install . --config Release

:: NOW BUILD/INSTALL SHARED
:: cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/lib/netcdf.lib -DNETCDF_C_INCLUDE_DIR=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netCDF4.9.2-NC4-64/include -DBUILD_SHARED_LIBS=1 -DENABLE_TESTS=1 -DCMAKE_INSTALL_PREFIX=C:/Users/mreno/local/netcdf/build-windows/builds/b4/build/netcdf-f/build ../netcdf-fortran-4.6.1

cmake --fresh -G "Visual Studio 17 2022" -A x64 -DNETCDF_C_LIBRARY="%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\lib\netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%GITHUB_WORKSPACE%\netcdf\netCDF4.9.2-NC4-64\include" -DBUILD_SHARED_LIBS=1 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.1_build\build" ../netcdf-fortran-4.6.1
cmake --build . --config Release
cmake –install . --config Release

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

set FC=ifort
cd "%GITHUB_WORKSPACE%\modflow6"
pixi run setup -Dnetcdf=true builddir
pixi run build builddir
pixi run test builddir
pixi run pytest -v --durations=0 --keep-failed .failed --netcdf -k "test_netcdf"
