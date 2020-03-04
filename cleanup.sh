#!/bin/bash

mkdir -p sc3900/SC3900DA
cp enterprise_disasm/sc3900/SC3900DA/vspa.gen.cpp sc3900/SC3900DA/
mkdir vspa
cp enterprise_disasm/vspa/Makefile vspa/
cp -r enterprise_disasm/vspa/src/ vspa/
mkdir -p disasm/disasm_iss
cp enterprise_disasm/disasm/disasm_iss/disasm12_iss.vcxproj disasm/disasm_iss/
cp enterprise_disasm/disasm/disasm_iss/disasm12_iss.vcxproj.filters disasm/disasm_iss/
rm -rf enterprise_disasm/
