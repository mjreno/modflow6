cd "$GITHUB_WORKSPACE/hdf5/hdf5-1.14.4-3"
./configure --prefix=$(pwd)/.. --with-zlib=/usr/include --enable-hl --enable-shared  CC=icx-cl CXX=icx FC=ifort
#make check
make
make install
