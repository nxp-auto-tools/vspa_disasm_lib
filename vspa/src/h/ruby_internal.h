/*Copyright 2013-2018 NXP

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef _RUBY_INTERNAL_H_
#define _RUBY_INTERNAL_H_

#include <stdint.h>
#include <string>
#include <vector>
#include <map>

#include "ruby_dis.h"

#if defined(_MSC_VER) && ( _MSC_VER >= 1200 ) 
#define snprintf _snprintf
#define vsnprintf _vsnprintf
#endif

#define DA_MAX_FIELD_NUM_IN_INSTR 128
#define MAX_BITS_NUM_IN_FIELD 128
#define MAX_ENRIES_IN_TABLE 128
#define DA_MAX_FIELD_LEN 128

#define RUBY_SUCCESS 0
#define RUBY_FAIL 1

#define NUM_FAMILY 8

#define MACRO_INSN_BITS 54
#define HW_INSN_POS 0x34
#define HW_INSN_POS2 0x3f
#define HW_DUAL 0x3
#define LSB_2 0x3
#define LSB_1 0x1

#define ADL_LITTLE_ENDIAN 0
#define ADL_BIG_ENDIAN 1

#define NAMESPACE_LIB_DISASSEMBLER VSPADisasmLib

namespace NAMESPACE_LIB_DISASSEMBLER
{

enum DAsmExecBlock {
	opA,
	opB,
	opC,
	opD,
	opS,
	opVs0,
	opVs1,
	opVs2,
	opVau,
	opVrot, /* VSPA1 only */
	opVr,   /* VSPA1 only */
	opVd,   /* VSPA1 only */
	opVror, /* VSPA2 only */
	opVrol, /* VSPA2 only */
	opVld,  /* VSPA2 only */
	opVwr,  /* VSPA2 only */
	opVsau, /* VSPA2 only */
	opX,
	opZ,
	DONE,
	FNOP,
	UNKNOWN,
};

#ifdef _VSPA3_
#define is_vcpu3_core(core) ((core) == VCPU3)
#else
#define is_vcpu3_core(core) (0)
#endif

// Change of flow instruction
#define DA_INSN_PROP_COF 0x00000004
// Uses or modifies SP
#define DA_INSN_PROP_USES_SP 0x00000010
// Null instruction.
#define DA_INSN_PROP_NULL 0x00000080
// Load instruction.
#define DA_INSN_PROP_LOAD 0x00000100
// Store instruction.
#define DA_INSN_PROP_STORE 0x00000200
// MVIP instruction
#define DA_INSN_PROP_IP_CONTROL 0x00000400

// No format options.
#define DA_FMT_NONE 0x00000000
// Dump instructions in lowercase.
#define DA_FMT_LOWERCASE 0x00000001
// Use decimal immediate values.
#define DA_FMT_DECIMALIMM 0x00000002

#define DA_DMA_XFER_CRTL_REG 0x2f

#define MAX_INST_SIZE 1024

/*
* The structure contains information describing an instruction
*/
typedef struct DAsmInstruction
{
	/* instruction internal name */
	const char *name;
	/* instruction syntax */
	const char *syntax;
	/* witdh in bits */
	uint32_t width;
	/* instruction family */
	DAsmExecBlock block;
	/* instruction properties */
	uint32_t properties;
	/* architecture ID */
	uint32_t archID;
	/* functional group */
	const char *group;
	/* group base */
	const char *base;
	/* first flags group */
	const char *flags1;
	/* second flags group */
	const char *flags2;
	const char *fields[DA_MAX_FIELD_NUM_IN_INSTR]; 
	/* predicate name for the instruction */
	const char *predicate;
	/* predicate "else clear" field name */
	const char *predicate_ec;
	/* instruction encoding bits */
	uint64_t opcode;
	/* instruction encoding bits mask */
	uint64_t opcodeMask;
} DAsmInstruction;

struct FieldEntry
{
public:
	typedef enum { ENUM, IMM, TABLE } FieldType;

	FieldType kind;
	const char *longName;
	const char *fieldName;
	uint32_t shift;
	uint32_t bitsCount;
	uint32_t bits[MAX_BITS_NUM_IN_FIELD];
	uint32_t entriesCount;
	struct {
			uint64_t index;
			const char *value;
	} entries[MAX_ENRIES_IN_TABLE];
	const char *relocName;
	uint32_t relocValue;
	uint32_t relocWidth;
	bool signed_value;
};

class Field
{
public:
	virtual ~Field() {};
	virtual std::string extract(uint64_t opcode,
		uint32_t ext,
		uint32_t width,
		uint32_t format_options = 0 ) = 0;
	Field *addBitMask(uint32_t position);

	uint64_t extract2value(uint64_t opcode, uint32_t ext, uint32_t width);
protected:

public:
	const FieldEntry *fld_desc;

protected:
	std::vector< uint32_t > bits;
};

class EnumeratedField : public Field
{
public:
	EnumeratedField(std::string longName_p, std::string fieldName_p);
	virtual std::string extract(uint64_t opcode, uint32_t ext, uint32_t width,
		uint32_t format_options);

private:
	std::string longName;
	std::string fieldName;
};


class ImmediateField : public Field
{
public:
	ImmediateField(std::string longName_p, uint32_t shift);
	virtual std::string extract(uint64_t opcode, uint32_t ext, uint32_t width,
		uint32_t format_options);

private:
	std::string longName;
	uint32_t shift;
};


class TabledField : public Field
{
public:
	TabledField() {};
	TabledField(std::string longName_p, uint32_t shift_p);
	virtual std::string extract(uint64_t opcode, uint32_t ext, uint32_t width,
		uint32_t format_options);
	TabledField *addEntry( uint64_t value, std::string entry);

protected:
	std::string longName;
	std::map<uint64_t, std::string> table;
	uint32_t shift;
};

class InstrDscDB
{
public:
	InstrDscDB();
	~InstrDscDB();

	void set_core(enum DAsmCore new_core);

	bool findInstr(uint64_t opcode, uint32_t width,
		DAsmInstruction** instrDsc);
	bool findInstr(uint64_t opcode, uint32_t width, DAsmExecBlock execBlock,
		DAsmInstruction **instrDsc);
	bool findInstr(const char *name, uint32_t width, DAsmExecBlock execBlock,
		DAsmInstruction **instrDsc);
	bool findInstr(const char *name, DAsmInstruction **instrDsc);

	Field *findField(std::string name);

	static uint32_t endianness;
	static DAsmInstruction instructions[];
	static size_t numof_instructions;
	static FieldEntry fields[];
	static size_t numof_fields;

	static DAsmInstruction vcpu2_instructions[];
	static size_t vcpu2_numof_instructions;
	static FieldEntry vcpu2_fields[];
	static size_t vcpu2_numof_fields;

	static DAsmInstruction ippu_instructions[];
	static size_t ippu_numof_instructions;
	static FieldEntry ippu_fields[];
	static size_t ippu_numof_fields;

	static DAsmInstruction ippu2_instructions[];
	static size_t ippu2_numof_instructions;
	static FieldEntry ippu2_fields[];
	static size_t ippu2_numof_fields;

#ifdef _VSPA3_
	static DAsmInstruction vcpu3_instructions[];
	static size_t vcpu3_numof_instructions;
	static FieldEntry vcpu3_fields[];
	static size_t vcpu3_numof_fields;

	static DAsmInstruction ippu3_instructions[];
	static size_t ippu3_numof_instructions;
	static FieldEntry ippu3_fields[];
	static size_t ippu3_numof_fields;
#endif

protected:
	std::map<std::string, Field *> cls_fields;
	std::map<std::string, Field *> vcpu2_cls_fields;
	std::map<std::string, Field *> ippu_cls_fields;
	std::map<std::string, Field *> ippu2_cls_fields;
#ifdef _VSPA3_
	std::map<std::string, Field *> vcpu3_cls_fields;
	std::map<std::string, Field *> ippu3_cls_fields;
#endif

	DAsmInstruction *instrs;
	size_t num_instrs;
	std::map<std::string, Field *> *flds;

	enum DAsmCore core;

	void init_fields(std::map<std::string, Field *> *flds,
		FieldEntry *field_list, size_t num_fields);
	void destroy_fields(std::map<std::string, Field *> fields);
};


typedef struct DAsmField
{
	/* field name as it appear in instruction's description */
	const char *name;
	/* internal, architecture defined name for this field */
	const char *full_name;
	/* field's representation after disassembly */
	char value[DA_MAX_FIELD_LEN];
	/* relocation name for this field */
	const char *reloc_name;
	/* relocation index */
	uint32_t reloc_value;
	/* field's width for relocation */
	uint32_t reloc_width;
	/* indication whether the field value is signed */
	uint32_t signed_value;
} DAsmField;

typedef struct Microinstruction
{
	// Instruction description.
	DAsmInstruction *instr;
	// The instruction's fields.
	std::vector<DAsmField> fields;

	// Get the type of change of flow and eventually the value of the target
	void get_cof_type(enum cof_type *type, long *immediate);
} Microinstruction;

class Macroinstruction
{
private:
	// Opcode.
	bfd_vma opcode_;
	// Format options.
	uint32_t format_options_;
	// The macroinstruction is full or is lower/upper short.
	int flag_;
	// VSPA core.
	enum DAsmCore core_;
	// Microinstructions.
	std::vector<Microinstruction> instrs;

	DAsmExecBlock get_block(const char *macro_field);

	void disasm_micro(uint64_t opcode, uint32_t width,
			enum DAsmExecBlock block);
	void disasm_short_macro(DAsmInstruction *instr_dsc);
	bool disasm_loop_instruction(const char *name);
	bool disasm_loop_instructions();
	void disasm(DAsmInstruction *instr_dsc);

	bool special_syntax(Microinstruction micro,
		std::string &syntax, uint32_t num_au);

	int64_t get_str_val(std::string);
	std::string get_val_str(int64_t value, bool sign = false);

public:
	Macroinstruction(bfd_vma opcode, int flag, enum DAsmCore core);

	void disasm();

	void imm_to_sym(struct SYMTABLE sym_table[], unsigned num_symbol);

	unsigned char get_family_map();

	bool contains_null();
	bool contains_dmem_access();
	bool contains_mvip();
	bool contains_cof(Microinstruction *&instr);

	std::string to_str(uint32_t num_au = 64);
};

}
#endif
