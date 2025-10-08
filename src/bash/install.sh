#!/bin/bash

# Updates the lists.
sudo apt update

# Installs essential system stuff and compilers.
sudo apt install -y build-essential
sudo apt install -y clamav
sudo apt install -y git
sudo apt install -y tree
sudo apt install -y hexedit

# Install other dev tools
sudo apt install -y chicken-bin
sudo apt install -y verilator
sudo apt install -y iverilog

# Installs essential system stuff and compilers.
#sudo apt install -y audacity
#sudo apt install -y lmms

#sudo apt install -y blender
#sudo apt install -y openscad

#sudo apt install -y gimp
#sudo apt install -y inkscape
