cd "%GITHUB_WORKSPACE%\modflow6"
pixi add pkg-config
pixi run which meson
pixi run which pkg-config
pixi run pkg-config --list-all
