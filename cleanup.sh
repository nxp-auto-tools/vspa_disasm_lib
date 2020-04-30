#!/bin/sh

cd enterprise_disasm

rm -rf .git
rm -rf adl
cd disasm
rm -rf arch disasm.dsw disasm.vcxproj disasm12.sln disasm15.vcxproj elflib_lnk kw_vspa-ds_ownership.sow sc100 xrhsimos
rm -rf autobuild disasm.opt disasm.vcxproj.filters disasm12.vcxproj disasmlib kw_sc_disasm_build_spec.out lineinfolib src_config xrhsimos_lib
rm -rf common disasm.sln disasm12.vcxproj.filters disasmlib_sc3900 kw_sc_disasm_ownership.sow newasm src_mangle
rm -rf disasm.dsp disasm.vcproj disasm11.sln disasm15.sln elflib kw_vspa-ds_build_spec.out release_notes tests
rm -rf .cvsignore .gitignore
cd disasm_iss
rm -rf disasm_iss.vcproj disasm_iss.vcxproj disasm_iss.vcxproj.filters disasm15_iss.vcxproj
rm -rf .cvsignore
cd ..
cd ..
cd sc3900
rm -rf .gitignore .cvsignore doc SC3900DA.sln vsbuild.bat vsclean.bat
cd SC3900DA
rm -rf disassembler_api.h sc3900da.cpp sc3900da.gen.cpp sc3900da.rc sc3900da.vcxproj sc3900da_internal.cpp sc3900da_special.cpp sc3900da15.vcxproj
rm -rf Makefile sc3900da.def sc3900da.h SC3900DA.vcproj sc3900da.vcxproj.filters sc3900da_internal.h sc3900da_version.h sc3900da2.cpp
rm -rf .cvsignore
cd ..
cd ..
cd vspa
rm -rf .gitignore .cvsignore disasm.sln  disasm.vcxproj  disasm.vcxproj.filters  disasm15.sln  disasm15.vcxproj
cd ..
cd ..

mv enterprise_disasm/* .
rm -rf enterprise_disasm