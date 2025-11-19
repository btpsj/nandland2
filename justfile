# List all commands
default:
    @just --list

# Compile with clash
compile project:
    stack run clash -- {{project}} --verilog

run topEntity pcf:
    yosys -p "read_verilog {{topEntity}}; synth_ice40; write_json design.json"
    nextpnr-ice40 --hx1k --freq 25 --pcf {{pcf}} --json design.json --package vq100 --asc bitstream.txt
    icepack bitstream.txt bitstream.bin
    iceprog bitstream.bin

# Start open-source toolchain docker container
start-docker:
    docker run --privileged --rm -it -v "$(pwd)":/nandland2 -w /nandland2 fpga

build-docker:
    docker build -t fpga .
