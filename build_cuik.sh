git submodule update --init

# build truct first
git clone https://github.com/RealNeGate/Truct
cd Truct
git submodule update --init
git pull
sh ./build.sh
cd ..

# actually build Cuik
mkdir -p bin
./Truct/bin/truct
