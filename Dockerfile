# Use the official Debian stable image as the base
FROM debian:stable

# Set environment variables to non-interactive mode for APT
ENV DEBIAN_FRONTEND=noninteractive

# Update package list and install dependencies
RUN apt-get update && apt-get install -y \
    fpga-icestorm \
    yosys \
    nextpnr-ice40 \
    iverilog \
    just \
    && rm -rf /var/lib/apt/lists/*

# Install Yosys
# RUN git clone https://github.com/YosysHQ/yosys.git /yosys && \
#     cd /yosys && \
#     make && \
#     make install

# Install nextpnr
# RUN git clone https://github.com/YosysHQ/nextpnr.git /nextpnr && \
#     cd /nextpnr && \
#     mkdir build && \
#     cd build && \
#     cmake .. && \
#     make && \
#     make install

# Install icestorm
# RUN git clone https://github.com/YosysHQ/icestorm.git /icestorm && \
#     cd /icestorm && \
#     make && \
#     make install

# Set the entrypoint as a shell, so the user can interact with the container
ENTRYPOINT ["/bin/bash"]
