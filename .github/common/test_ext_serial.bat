cd "%GITHUB_WORKSPACE%\modflow6\autotest"
where libpetsc.dll
ldd ..\bin\mf6
pixi run autotest --netcdf -m "%MARKERS%"
