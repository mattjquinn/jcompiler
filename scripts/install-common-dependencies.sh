#!/bin/bash

set -ex

# Install jsoftware interpreter
wget http://www.jsoftware.com/download/j901/install/j901_amd64.deb
sudo dpkg -i j901_amd64.deb
