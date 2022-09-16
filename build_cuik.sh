# build truct first
git clone https://github.com/RealNeGate/Truct
cd Truct
git submodule update --init
git pull
./build.sh
cd ..

# actually build Cuik
mkdir -p bin
./Truct/bin/truct
