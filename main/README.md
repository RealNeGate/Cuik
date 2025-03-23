# Main driver

This is the CLI driver for libCuik. It's a mostly CC-like interface with some minor changes along with some behavioral changes. LibCuik is capable of multithreading within one process which means that if you pass multiple source files into Cuik we may compile them on separate threads (unless --threads=1 is specified).
