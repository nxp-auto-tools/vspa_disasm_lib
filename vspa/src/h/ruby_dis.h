/*
* Copyright 2018-2022 NXP
*
* SPDX-License-Identifier: BSD-3-clause
* Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
* 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _RUBY_DIS_H_
#define _RUBY_DIS_H_

#if defined(__cplusplus)
extern "C"
{
#endif

#ifdef _MSC_VER
# define DLL_EXPORT __declspec(dllexport)
#else
# define DLL_EXPORT
#endif

typedef unsigned long long int bfd_vma;
typedef signed long long int bfd_signed_vma;

typedef enum ruby_symbol_class
{
	pmem_sym = 0,
	dmem_sym = 1
} ruby_sym_class;

typedef enum ruby_insn_decode_type
{
	DECODE_TYPE_MACRO = 0,
	DECODE_TYPE_LOWER_OPS = 1,
	DECODE_TYPE_UPPER_OPS = 2,
	DECODE_TYPE_NO_FLAG_PRINT = 4
} ruby_decode_type;

enum cof_type
{
	COF_NONE,
	COF_RTS,
	COF_JMP_IMM,
	COF_JMP_REG,
	COF_JSR_IMM,
	COF_JSR_REG,
};

static const unsigned char FAMILY_OPA = 0x01;
static const unsigned char FAMILY_OPB = 0x02;
static const unsigned char FAMILY_OPC = 0x04;
static const unsigned char FAMILY_OPD = 0x08;
static const unsigned char FAMILY_OPS = 0x10;
static const unsigned char FAMILY_OPV = 0x20;
static const unsigned char FAMILY_OPX = 0x40;
static const unsigned char FAMILY_OPZ = 0x80;

struct SYMTABLE
{
	char symbol[256];
	unsigned int address;
};

enum DAsmCore
{
	VCPU,
	VCPU2,
#ifdef _VSPA3_
	VCPU3,
#endif
	IPPU,
	IPPU2,
#ifdef _VSPA3_
	IPPU3,
#endif
};

#ifndef EXTENDED_DISASSEMBLER_API

DLL_EXPORT int disassemble_instruction_vcpu(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number);

DLL_EXPORT int disassemble_instruction_vcpu2(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number);

DLL_EXPORT int disassemble_instruction_ippu(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol);

DLL_EXPORT int disassemble_instruction_ippu2(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol);

#ifdef _VSPA3_
DLL_EXPORT int disassemble_instruction_vcpu3(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number);


DLL_EXPORT int disassemble_instruction_ippu3(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol);
#endif

#else

DLL_EXPORT void check_dmem_access(bfd_vma ruby_insn,
	char *compacted,
	char *flag,
	enum DAsmCore core_type);

DLL_EXPORT int check_cof(bfd_vma ruby_insn,
	enum cof_type *type,
	long *immediate,
	enum DAsmCore core_type);

DLL_EXPORT int check_mvip(bfd_vma ruby_insn);
#endif

DLL_EXPORT void ruby_disassemble_init();
DLL_EXPORT void ruby_disassemble_finish();

#if defined(__cplusplus)
};
#endif

#endif
