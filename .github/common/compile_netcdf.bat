cd "%GITHUB_WORKSPACE%\netcdf\netcdf-fortran-4.6.2\build"

:: build/install static libs
cmake --fresh -G Ninja -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DCMAKE_BUILD_TYPE=Release -DNETCDF_C_LIBRARY="%NETCDF_C_PREFIX%/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%NETCDF_C_PREFIX%/include" -DBUILD_SHARED_LIBS=0 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.2/build" ../netcdf-fortran-4.6.2
cmake --build .
cmake –-install .

:: build/install shared libs
cmake --fresh -G Ninja -DCMAKE_Fortran_COMPILER="C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/ifort.exe" -DCMAKE_BUILD_TYPE=Release -DNETCDF_C_LIBRARY="%NETCDF_C_PREFIX%/lib/netcdf.lib" -DNETCDF_C_INCLUDE_DIR="%NETCDF_C_PREFIX%/include" -DBUILD_SHARED_LIBS=1 -DCMAKE_INSTALL_PREFIX="%GITHUB_WORKSPACE%/netcdf/netcdf-fortran-4.6.2/build" ../netcdf-fortran-4.6.2
cmake --build .
cmake –-install .
