/*Copyright 2013-2018 NXP

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <sstream>
#include <algorithm>
#include <cstring>
#include <cstdio>
#include <assert.h>

#include "ruby_dis.h"
#include "ruby_internal.h"

using namespace NAMESPACE_LIB_DISASSEMBLER;

InstrDscDB instr_dsc_db;

static DAsmInstruction invalid_instruction;


/*---------------------------------------------------------------------------*/

void InstrDscDB::init_fields(std::map<std::string, Field *> *flds,
	FieldEntry *field_list, size_t num_fields)
{
	for (size_t i = 0; i < num_fields; ++i )
	{
		if (flds->find(field_list[i].longName) != flds->end())
		{
			continue;
		}

		Field *pf = 0;
		switch (field_list[i].kind)
		{
		case FieldEntry::ENUM:
			pf = new EnumeratedField(field_list[i].longName,
				field_list[i].fieldName);
			break;
		case FieldEntry::IMM:
			pf = new ImmediateField(field_list[i].longName,
				field_list[i].shift );
			break;
		case FieldEntry::TABLE:
			pf = new TabledField(field_list[i].longName, field_list[i].shift);
			for (uint32_t j = 0; j < field_list[i].entriesCount; ++j)
			{
				((TabledField*)pf)->addEntry( field_list[i].entries[j].index,
				field_list[i].entries[j].value );
			}
			break;
		default:
				continue;
		}
		pf->fld_desc = &field_list[i];

		for (uint32_t j = 0; j < field_list[i].bitsCount; ++j)
		{
			pf->addBitMask(field_list[i].bits[j]);
		}

		(*flds)[field_list[i].longName] = pf;
	}
}

InstrDscDB::InstrDscDB()
{
	init_fields(&cls_fields, fields, numof_fields);
	init_fields(&vcpu2_cls_fields, vcpu2_fields, vcpu2_numof_fields);
	init_fields(&ippu_cls_fields, ippu_fields, ippu_numof_fields);
	init_fields(&ippu2_cls_fields, ippu2_fields, ippu2_numof_fields);
#ifdef _VSPA3_
	init_fields(&vcpu3_cls_fields, vcpu3_fields, vcpu3_numof_fields);
	init_fields(&ippu3_cls_fields, ippu3_fields, ippu3_numof_fields);
#endif

	/* default core is VCPU */
	core = VCPU;
	instrs = instructions;
	num_instrs = numof_instructions;
	flds = &cls_fields;
};

void InstrDscDB::set_core(enum DAsmCore new_core)
{
	if (core == new_core)
	{
		return;
	}

	core = new_core;
	switch (core)
	{
	case VCPU:
		instrs = instructions;
		num_instrs = numof_instructions;
		flds = &cls_fields;
		return;
	case VCPU2:
		instrs = vcpu2_instructions;
		num_instrs = vcpu2_numof_instructions;
		flds = &vcpu2_cls_fields;
		return;
#ifdef _VSPA3_
	case VCPU3:
		instrs = vcpu3_instructions;
		num_instrs = vcpu3_numof_instructions;
		flds = &vcpu3_cls_fields;
		return;
#endif
	case IPPU:
		instrs = ippu_instructions;
		num_instrs = ippu_numof_instructions;
		flds = &ippu_cls_fields;
		return;
	case IPPU2:
		instrs = ippu2_instructions;
		num_instrs = ippu2_numof_instructions;
		flds = &ippu2_cls_fields;
		return;
#ifdef _VSPA3_
	case IPPU3:
		instrs = ippu3_instructions;
		num_instrs = ippu3_numof_instructions;
		flds = &ippu3_cls_fields;
		return;
#endif
	default:
		return;
	}
};

void InstrDscDB::destroy_fields(std::map<std::string, Field *> fields)
{
	for (std::map<std::string, Field*>::iterator it = fields.begin();
		it != fields.end(); ++it)
	{
		delete it->second;
	}
	fields.clear();
};

InstrDscDB::~InstrDscDB()
{
	destroy_fields(cls_fields);
	destroy_fields(vcpu2_cls_fields);
	destroy_fields(ippu_cls_fields);
	destroy_fields(ippu2_cls_fields);
#ifdef _VSPA3_
	destroy_fields(vcpu3_cls_fields);
	destroy_fields(ippu3_cls_fields);
#endif
}

Field *InstrDscDB::findField(std::string name)
{
	Field *ret = 0;

	if (flds->find(name) != flds->end())
	{
		ret = (*flds)[name];
	}
	return ret;
}


/*---------------------------------------------------------------------------*/
Field *Field::addBitMask(uint32_t position)
{
	bits.push_back(position);
	return this;
}

uint64_t Field::extract2value(uint64_t opcode, uint32_t ext, uint32_t width)
{
	uint64_t value = 0;
	for (uint32_t i = 0; i < bits.size(); i++)
	{
		value <<= 1;
		if (bits[i] >= width)
		{
			if (((static_cast<uint32_t>(1) << (bits[i] - width)) & ext) != 0)
			{
				value |= 1;
			}
		}
		else if (((static_cast<uint64_t>(1) << bits[i]) & opcode) != 0)
		{
			value |= 1;
		}
	}

	return value;
}

/*---------------------------------------------------------------------------*/
EnumeratedField::EnumeratedField(std::string longName_p,
	std::string fieldName_p) : longName(longName_p), fieldName(fieldName_p)
{
}

std::string EnumeratedField::extract(uint64_t opcode, uint32_t ext,
	uint32_t width, uint32_t format_options)
{
	uint64_t value = extract2value(opcode, ext, width);

	std::ostringstream oss;
	oss << fieldName << value;

	return oss.str();
}


/*---------------------------------------------------------------------------*/
ImmediateField::ImmediateField(std::string longName_p, uint32_t shift_p) :
	longName(longName_p), shift(shift_p)
{
}


/*---------------------------------------------------------------------------*/
std::string ImmediateField::extract(uint64_t opcode, uint32_t ext,
	uint32_t width, uint32_t format_options)
{
	std::ostringstream oss;

	uint64_t value = extract2value(opcode, ext, width);
	if (fld_desc->signed_value)
	{
		if (value & (static_cast<uint64_t>(1) << (fld_desc->bitsCount - 1)))
		{
			oss << "-";
			value = (static_cast<uint64_t>(1) << fld_desc->bitsCount) - value;
		}
	}

	value <<= shift;
	if ((format_options & DA_FMT_DECIMALIMM) == DA_FMT_DECIMALIMM)
	{
		if (fld_desc->signed_value)
		{
			oss << value;
		}
		// For unsigned immediate fields, both hexadecimal and decimal values
		// are printed.
		else
		{
			oss << "0x" << std::hex << value
				<< "(" << std::dec << value << ")";
		}
	}
	else
	{
		oss << "0x" << std::hex << value;
	}

	return oss.str();
}


/*---------------------------------------------------------------------------*/
TabledField::TabledField(std::string longName_p, uint32_t shift_p) :
	longName(longName_p), shift(shift_p)
{
}


/*---------------------------------------------------------------------------*/
std::string TabledField::extract(uint64_t opcode, uint32_t ext, uint32_t width,
	uint32_t format_options)
{
	uint64_t value = extract2value(opcode, ext, width);
	std::string res;

	if (table.find(value) != table.end())
	{
		res = table[value];
	}
	else
	{
		res = "Unknown";
	}

	if (shift != 0)
	{
		uint64_t value = 0;
		std::stringstream ss(res);

		ss >> value;
		value <<= shift;
		
		std::ostringstream oss;
		if ((format_options & DA_FMT_DECIMALIMM) == DA_FMT_DECIMALIMM)
		{
			oss << value;
		}
		else 
		{
			oss << "0x" << std::hex << value;
		}
		
		res = oss.str();
	}

	return res;
}


/*---------------------------------------------------------------------------*/
TabledField *TabledField::addEntry(uint64_t value, std::string entry)
{
	if (table.find(value) == table.end())
	{
		table[value] = entry;
	}

	return this;
}

/*---------------------------------------------------------------------------*/
static size_t getMaskBits(uint64_t mask)
{
	size_t count = 0;

	for (size_t i = 0; i < 64; i++) {
		if (mask & 0x1) {
			count++;
		}
		mask >>= 1;
	}

	return count;
}

bool InstrDscDB::findInstr(uint64_t opcode,
	uint32_t width,
	DAsmInstruction **instrDsc)
{
	bool ret = false;
	size_t maxMaskBits = 0;

	for (size_t i = 0; i < num_instrs; ++i)
	{
		if ((instrs[i].width == width)
			&& ((opcode & instrs[i].opcodeMask) == instrs[i].opcode))
		{
			size_t maskBits = getMaskBits(instrs[i].opcodeMask);
			if (maskBits > maxMaskBits)
			{
				maxMaskBits = maskBits;
				*instrDsc = &instrs[i];
				ret = true;
			}
		}
	}

	return ret;
}

bool InstrDscDB::findInstr(uint64_t opcode, uint32_t width,
		DAsmExecBlock execBlock, DAsmInstruction **instrDsc)
{
	bool ret = false;
	size_t maxMaskBits = 0;

	for (size_t i = 0; i < num_instrs; ++i)
	{
		if ((instrs[i].block == execBlock)
					&& (instrs[i].width == width)
					&& ((opcode & instrs[i].opcodeMask) == instrs[i].opcode))
		{
			size_t maskBits = getMaskBits(instrs[i].opcodeMask);
			if (maskBits > maxMaskBits)
			{
				maxMaskBits = maskBits;
				*instrDsc = &instrs[i];
				ret = true;
			}
		}
	}

	return ret;
}

bool InstrDscDB::findInstr(const char *name, uint32_t width,
		DAsmExecBlock execBlock, DAsmInstruction **instrDsc)
{
	bool ret = false;

	for (size_t i = 0; i < num_instrs; ++i)
	{
		if ((instrs[i].block == execBlock)
			&& (instrs[i].width == width)
			&& (strcmp(instrs[i].name, name) == 0))
		{
			*instrDsc = &instrs[i];
			ret = true;
		}
	}

	return ret;
}

bool InstrDscDB::findInstr(const char *name, DAsmInstruction **instrDsc)
{
	bool ret = false;

	for (size_t i = 0; i < num_instrs; ++i)
	{
		if (!strcmp(instrs[i].name, name))
		{
			*instrDsc = &instrs[i];
			ret = true;
		}
	}

	return ret;
}

Macroinstruction::Macroinstruction(bfd_vma opcode, int flag, enum DAsmCore core) :
	opcode_(opcode),
	format_options_(DA_FMT_DECIMALIMM),
	flag_(flag),
	core_(core)
{
}

void Macroinstruction::disasm_micro(uint64_t opcode, uint32_t width, DAsmExecBlock block)
{
	DAsmInstruction *instr_dsc;
	Microinstruction micro;

	if (!instr_dsc_db.findInstr(opcode, width, block, &instr_dsc))
	{
		micro.instr = &invalid_instruction;
		instrs.push_back(micro);
		return;
	}

	micro.instr = instr_dsc;

	for (uint32_t i = 0; instr_dsc->fields[i] != 0; i += 2)
	{
		Field *pfield = instr_dsc_db.findField(instr_dsc->fields[i + 1]);
		if (pfield == 0)
		{
			continue;
		}

		DAsmField field;
		// Extract the field from opcode.
		std::string tmp = pfield->extract(opcode, 0, width, format_options_);
		// Convert to lower case.
		if (format_options_ & DA_FMT_LOWERCASE)
		{
			std::transform(tmp.begin(), tmp.end(), tmp.begin(), ::tolower);
		}
		// Store the value.
		std::strcpy(field.value, tmp.c_str());

		field.name = instr_dsc->fields[i];
		field.full_name = instr_dsc->fields[i + 1];
		field.reloc_name = pfield->fld_desc->relocName;
		field.reloc_value = pfield->fld_desc->relocValue;
		field.reloc_width = pfield->fld_desc->relocWidth;
		field.signed_value = pfield->fld_desc->signed_value;

		micro.fields.push_back(field);
	}

	instrs.push_back(micro);
}

/*---------------------------------------------------------------------------*/
DAsmExecBlock Macroinstruction::get_block(const char *macro_field)
{
	if (!strcmp(macro_field, "opA"))
	{
		return opA;
	}
	else if (!strcmp(macro_field, "opB"))
	{
		return opB;
	}
	else if (!strcmp(macro_field, "opVs0"))
	{
		return opVs0;
	}
	else if (!strcmp(macro_field, "opVs1"))
	{
		return opVs1;
	}
	else if (!strcmp(macro_field, "opVs2"))
	{
		return opVs2;
	}
	else if (!strcmp(macro_field, "opVau"))
	{
		return opVau;
	}
	else if (!strcmp(macro_field, "opVrot"))
	{
		return opVrot;
	}
	else if (!strcmp(macro_field, "opVr"))
	{
		return opVr;
	}
	else if (!strcmp(macro_field, "opVd"))
	{
		return opVd;
	}
	else if (!strcmp(macro_field, "opVror"))
	{
		return opVror;
	}
	else if (!strcmp(macro_field, "opVrol"))
	{
		return opVrol;
	}
	else if (!strcmp(macro_field, "opVld"))
	{
		return opVld;
	}
	else if (!strcmp(macro_field, "opVwr"))
	{
		return opVwr;
	}
	else if (!strcmp(macro_field, "opVsau"))
	{
		return opVsau;
	}
	else if (!strcmp(macro_field, "opC"))
	{
		return opC;
	}
	else if (!strcmp(macro_field, "opD"))
	{
		return opD;
	}
	else if (!strcmp(macro_field, "opS")
		|| !strcmp(macro_field, "opS_HI")
		|| !strcmp(macro_field, "opS_LO"))
	{
		return opS;
	}
	else if (!strcmp(macro_field, "opX")
		|| !strcmp(macro_field, "opX_HI")
		|| !strcmp(macro_field, "opX_LO"))
	{
		return opX;
	}
	else if (!strcmp(macro_field, "opZ"))
	{
		return opZ;
	}

	return UNKNOWN;
}

void Macroinstruction::disasm_short_macro(DAsmInstruction *instr_dsc)
{
	uint64_t opX_opcode, opX_HI_opcode, opX_LO_opcode, opS_HI_opcode,
		opS_LO_opcode, opZ_opcode;
	uint32_t opX_width, opX_HI_width, opX_LO_width, opS_HI_width, opS_LO_width,
		opZ_width;
	opX_opcode = opX_HI_opcode = opX_LO_opcode  = opS_HI_opcode =
		opS_LO_opcode = opZ_opcode = 0;
	opX_width = opX_HI_width = opX_LO_width = opS_HI_width =
		opS_LO_width = opZ_width = 0;

	for (uint32_t i = 0; instr_dsc->fields[i] != 0; i += 2)
	{
		Field *pfield = instr_dsc_db.findField(instr_dsc->fields[i + 1]);
		if (pfield == 0)
		{
			continue;
		}

		if (core_ == VCPU
			&& !strcmp(instr_dsc->fields[i], "opX"))
		{
			opX_opcode = pfield->extract2value(opcode_, 0, 64);
			opX_width = pfield->fld_desc->bitsCount;
		}
		else if ((core_ == VCPU2
				|| is_vcpu3_core(core_))
			&& !strcmp(instr_dsc->fields[i], "opX_HI"))
		{
			opX_HI_opcode = pfield->extract2value(opcode_, 0, 64);
			opX_HI_width = pfield->fld_desc->bitsCount;
		}
		else if ((core_ == VCPU2
				|| is_vcpu3_core(core_))
			&& !strcmp(instr_dsc->fields[i], "opX_LO"))
		{
			opX_LO_opcode = pfield->extract2value(opcode_, 0, 64);
			opX_LO_width = pfield->fld_desc->bitsCount;
		}
		else if (!strcmp(instr_dsc->fields[i], "opS_LO"))
		{
			opS_LO_opcode = pfield->extract2value(opcode_, 0, 64);
			opS_LO_width = pfield->fld_desc->bitsCount;
		}
		else if (!strcmp(instr_dsc->fields[i], "opS_HI"))
		{
			opS_HI_opcode = pfield->extract2value(opcode_, 0, 64);
			opS_HI_width = pfield->fld_desc->bitsCount;
		}
		else if (!strcmp(instr_dsc->fields[i], "opZ"))
		{
			opZ_opcode = pfield->extract2value(opcode_, 0, 64);
			opZ_width = pfield->fld_desc->bitsCount;
		}
		else
		{
			continue;
		}
	}

	DAsmInstruction *instr;
	if (flag_ & DECODE_TYPE_LOWER_OPS)
	{
		if (core_ == VCPU
			&& opX_opcode
			&& instr_dsc_db.findInstr(opX_opcode, opX_width, opX, &instr))
		{
			if (!strcmp(instr->name, "swbreak"))
			{
				disasm_micro(opX_opcode, opX_width, opX);
			}
			else if (!strcmp(instr->name, "swbreak_lower")
				&& instr_dsc_db.findInstr("swbreak", opX_width, opX, &instr))
			{
				disasm_micro(instr->opcode, opX_width, opX);
			}
		}
		else if ((core_ == VCPU2
				|| is_vcpu3_core(core_))
			&& opX_LO_opcode
			&& instr_dsc_db.findInstr("swbreak", opX_width, opX, &instr))
		{
			disasm_micro(instr->opcode, opX_width, opX);
		}

		if (opS_LO_opcode)
		{
			disasm_micro(opS_LO_opcode, opS_LO_width, opS);
		}

		if (opZ_opcode)
		{
			disasm_micro(opZ_opcode, opZ_width, opZ);
		}
	}
	else if (flag_ & DECODE_TYPE_UPPER_OPS)
	{
		if (core_ == VCPU
			&& opX_opcode
			&& instr_dsc_db.findInstr(opX_opcode, opX_width, opX, &instr))
		{
			if (!strcmp(instr->name, "swbreak"))
			{
				disasm_micro(opX_opcode, opX_width, opX);
			}
			else if (!strcmp(instr->name, "swbreak_upper")
				&& instr_dsc_db.findInstr("swbreak", opX_width, opX, &instr))
			{
				disasm_micro(instr->opcode, opX_width, opX);
			}
		}
		else if ((core_ == VCPU2
				|| is_vcpu3_core(core_))
			&& opX_HI_opcode
			&& instr_dsc_db.findInstr("swbreak", opX_width, opX, &instr))
		{
			disasm_micro(instr->opcode, opX_width, opX);
		}

		if (opS_HI_opcode)
		{
			disasm_micro(opS_HI_opcode, opS_HI_width, opS);
		}
	}

}

void Macroinstruction::disasm(DAsmInstruction *instr_dsc)
{
	if (((core_ == VCPU)
			&& !strcmp(instr_dsc->name, "opXSSZ"))
		|| ((core_ == VCPU2)
			&& !strcmp(instr_dsc->name, "opXXSSZ"))
		|| (is_vcpu3_core(core_)
			&& !strcmp(instr_dsc->name, "opXXSSZ")))
	{
		disasm_short_macro(instr_dsc);
		return;
	}

	/* the macroinstruction will be added to the VLES */
	bool opCD = (instr_dsc->block == opC)
		|| (instr_dsc->block == opD)
		|| (instr_dsc->block == DONE)
		|| (instr_dsc->block == FNOP);
	bool micro_added = false;
	if (opCD)
	{
		Microinstruction micro;
		micro.instr = instr_dsc;
		instrs.push_back(micro);
	}

	for (uint32_t i = 0; instr_dsc->fields[i] != 0; i += 2)
	{
		Field *pfield = instr_dsc_db.findField(instr_dsc->fields[i + 1]);
		if (pfield == 0)
		{
			continue;
		}

		/* macroinstruction */
		if (!strcmp(instr_dsc->fields[i], "opA")
			|| !strcmp(instr_dsc->fields[i], "opB")
			|| !strcmp(instr_dsc->fields[i], "opS_HI")
			|| !strcmp(instr_dsc->fields[i], "opVs0")
			|| !strcmp(instr_dsc->fields[i], "opVs1")
			|| !strcmp(instr_dsc->fields[i], "opVs2")
			|| !strcmp(instr_dsc->fields[i], "opVau")
			|| !strcmp(instr_dsc->fields[i], "opZ")
			|| ((core_ == VCPU)
				&& (!strcmp(instr_dsc->fields[i], "opX")
					|| !strcmp(instr_dsc->fields[i], "opVrot")
					|| !strcmp(instr_dsc->fields[i], "opVr")
					|| !strcmp(instr_dsc->fields[i], "opVd")))
			|| ((core_ == VCPU2)
				&& (!strcmp(instr_dsc->fields[i], "opX_LO")
					|| !strcmp(instr_dsc->fields[i], "opVror")
					|| !strcmp(instr_dsc->fields[i], "opVrol")
					|| !strcmp(instr_dsc->fields[i], "opVld")
					|| !strcmp(instr_dsc->fields[i], "opVwr")
					|| !strcmp(instr_dsc->fields[i], "opVsau")))
			|| (is_vcpu3_core(core_)
				&& (!strcmp(instr_dsc->fields[i], "opX_LO")
					|| !strcmp(instr_dsc->fields[i], "opVror")
					|| !strcmp(instr_dsc->fields[i], "opVrol")
					|| !strcmp(instr_dsc->fields[i], "opVld")
					|| !strcmp(instr_dsc->fields[i], "opVwr")
					|| !strcmp(instr_dsc->fields[i], "opVsau")
					|| !strcmp(instr_dsc->fields[i], "opC")
					|| !strcmp(instr_dsc->fields[i], "opD"))))
		{
			uint64_t micro_opcode = pfield->extract2value(opcode_, 0, 64);
			if(!micro_opcode)
			{
				continue;
			}
			disasm_micro(micro_opcode, pfield->fld_desc->bitsCount,
					get_block(instr_dsc->fields[i]));
		}
		else
		/* field of an "opC" or "opD" microinstructions */
		{
			if (!opCD)
			{
				continue;
			}

			DAsmField field;
			std::string tmp;
			/* extract the field from opcode */
			tmp = pfield->extract(opcode_, 0, 64, format_options_);

			/* convert to lowercase */
			if (format_options_ & DA_FMT_LOWERCASE)
			{
				std::transform(tmp.begin(), tmp.end(), tmp.begin(), ::tolower);
			}

			/* store the value */
			std::strcpy(field.value, tmp.c_str());

			field.name = instr_dsc->fields[i];
			field.full_name = instr_dsc->fields[i + 1];
			field.reloc_name = pfield->fld_desc->relocName;
			field.reloc_value = pfield->fld_desc->relocValue;
			field.reloc_width = pfield->fld_desc->relocWidth;
			field.signed_value = pfield->fld_desc->signed_value;

			instrs[0].fields.push_back(field);
		}
	}
}

bool Macroinstruction::disasm_loop_instruction(const char *name)
{
	DAsmInstruction *loop_instr;

	if (instr_dsc_db.findInstr(name, &loop_instr))
	{
		uint64_t opcode = opcode_ & loop_instr->opcodeMask;
		uint64_t loop_index = (loop_instr->opcode & 0x6) >> 2;
		uint64_t opcode_mask = loop_instr->opcode &0x3;
		uint64_t opcode_index = (opcode & 0x6) >> 2;

		if ((loop_index == opcode_index) && (opcode & opcode_mask))
		{
			Microinstruction micro;
			micro.instr = loop_instr;
			instrs.push_back(micro);
			return true;
		}
	}

	return false;
}

bool Macroinstruction::disasm_loop_instructions()
{
	bool ret = false;

	if (disasm_loop_instruction("loop_begin_0"))
	{
		ret = true;
	}
	if (disasm_loop_instruction("loop_begin_1"))
	{
		ret = true;
	}
	if (disasm_loop_instruction("loop_end_0"))
	{
		ret = true;
	}
	if (disasm_loop_instruction("loop_end_1"))
	{
		ret = true;
	}

	return ret;
}

void Macroinstruction::disasm()
{
	DAsmInstruction *instr_dsc;

	if (core_ == VCPU 
		|| core_ == VCPU2
		|| is_vcpu3_core(core_))
	{
		if (instr_dsc_db.findInstr(opcode_, 64, &instr_dsc))
		{
			disasm(instr_dsc);
		}
		else
		{
			Microinstruction micro;
			micro.instr = &invalid_instruction;
			instrs.push_back(micro);
		}
	}
	else
	{
		DAsmInstruction *loop_instr;
		instr_dsc_db.findInstr("loop_begin_0", &loop_instr);
		if (loop_instr && (opcode_ & loop_instr->opcodeMask)
			&& ((opcode_ & (~loop_instr->opcodeMask)) == 0))
		{
			if (!disasm_loop_instructions())
			{
				disasm_micro(opcode_, 32, UNKNOWN);
			}
		}
		else
		{
			disasm_micro(opcode_, 32, UNKNOWN);
			if (loop_instr && (opcode_ & loop_instr->opcodeMask))
			{
				if (!disasm_loop_instructions())
				{
					disasm_micro(opcode_ & loop_instr->opcodeMask, 32, UNKNOWN);
				}
			}
		}
	}
}

int64_t Macroinstruction::get_str_val(std::string str)
{
	int64_t value;

	if (format_options_ & DA_FMT_DECIMALIMM)
	{
		std::string::size_type lp = str.find_first_of('(');
		std::string::size_type rp = str.find_first_of(')');
		if (lp != std::string::npos && rp != std::string::npos)
		{
			std::stringstream ss(str.substr(lp + 1, rp - lp - 1));
			ss >> value;
		}
		else
		{
			std::stringstream ss(str);
			ss >> value;
		}
	}
	else
	{
		std::stringstream ss(str);
		ss >> std::hex >> value;
	}

	return value;
}

std::string Macroinstruction::get_val_str(int64_t value, bool sign)
{
	std::stringstream ss("");

	if (format_options_ & DA_FMT_DECIMALIMM)
	{
		if (sign)
		{
			ss << value;
		}
		else
		{
			// For unsigned immediate fields, both hexadecimal and decimal
			// values are printed.
			ss << "0x" << std::hex << value
				<< std::dec << "(" << value << ")";
		}
	}
	else
	{
		ss << "0x" << std::hex << value;
	}

	return ss.str();
}

bool Macroinstruction::special_syntax(Microinstruction micro,
	std::string &syntax, uint32_t num_au)
{
	if (((core_ == VCPU)
			&& strcmp(micro.instr->name, "set_loop_I")
			&& strcmp(micro.instr->name, "set_loop_I_I")
			&& strcmp(micro.instr->name, "set_loop_asX_I")
			&& strcmp(micro.instr->name, "set_nco")
			&& strcmp(micro.instr->name, "set_xtrm")
			&& strncmp(micro.instr->name, "set_range1_r", 12)
			&& strncmp(micro.instr->name, "set_range2_r", 12))
		|| ((core_ == IPPU 
			|| core_ == IPPU2
#ifdef _VSPA3_
			|| core_ == IPPU3
#endif
      )
			&& strcmp(micro.instr->name, "set_loop")
			&& strcmp(micro.instr->name, "loop_begin_0")
			&& strcmp(micro.instr->name, "loop_begin_1")
			&& strcmp(micro.instr->name, "loop_end_0")
			&& strcmp(micro.instr->name, "loop_end_1")
			&& ((core_ == IPPU2
#ifdef _VSPA3_
				|| core_ == IPPU3
#endif
        )
				|| strcmp(micro.instr->name, "set_range_aY_asA_Iu17")))
		|| ((core_ == VCPU2
				|| is_vcpu3_core(core_))
			&& strcmp(micro.instr->name, "set_loop_aX_INSTR")
			&& strcmp(micro.instr->name, "setC_loop_aX_INSTR")
			&& strcmp(micro.instr->name, "set_loop_ITER_INSTR")
			&& strcmp(micro.instr->name, "setB_loop_Iu11")
			&& strcmp(micro.instr->name, "setC_loop_ITER")
			&& strcmp(micro.instr->name, "set_xtrm")
			&& strncmp(micro.instr->name, "stm", 3)
			&& strncmp(micro.instr->name, "ldm", 3)))
	{
		return false;
	}

	std::map<std::string, std::string> field_map;
	std::vector<DAsmField>::iterator field_it;
	std::vector<DAsmField> &fields = micro.fields;
	for (field_it = fields.begin(); field_it != fields.end(); ++field_it)
	{
		DAsmField &field = *field_it;
		field_map[field.name] = field.value;
	}

	if (!strcmp(micro.instr->name, "set_loop_I"))
	{
		std::map<std::string, std::string>::iterator iter_cnt =
			field_map.find("ITER_CNT");
		if (iter_cnt == field_map.end())
		{
			return false;
		}

		int64_t iter_cnt_val = get_str_val(iter_cnt->second);

		syntax.append("set.loop ");
		syntax.append(get_val_str(iter_cnt_val + 1));
	}
	else if (!strcmp(micro.instr->name, "set_loop_asX_I"))
	{
		std::map<std::string, std::string>::iterator as =
			field_map.find("AS");
		std::map<std::string, std::string>::iterator loop_size =
			field_map.find("LOOP_SIZE_R10");
		if (as == field_map.end()
			|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("set.loop ");
		syntax.append(as->second);
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strcmp(micro.instr->name, "set_loop_I_I"))
	{
		std::map<std::string, std::string>::iterator iter_cnt =
			field_map.find("ITER_CNT");
		std::map<std::string, std::string>::iterator loop_size =
			field_map.find("LOOP_SIZE_R10");
		if (iter_cnt == field_map.end()
			|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t iter_cnt_val = get_str_val(iter_cnt->second);
		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("set.loop ");
		syntax.append(get_val_str(iter_cnt_val + 1));
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strcmp(micro.instr->name, "set_nco"))
	{
		std::map<std::string, std::string>::iterator nco_mode =
			field_map.find("NCO_MODE");
		std::map<std::string, std::string>::iterator nco_k =
			field_map.find("NCO_K");
		std::map<std::string, std::string>::iterator nco_freq =
			field_map.find("NCO_FREQ");
		std::map<std::string, std::string>::iterator nco_neg_pos =
			field_map.find("NCO_NEG_POS");
		if (nco_mode == field_map.end()
			|| nco_k == field_map.end()
			|| nco_freq == field_map.end()
			|| nco_neg_pos == field_map.end())
		{
			return false;
		}

		int64_t nco_freq_val = get_str_val(nco_freq->second);
		int64_t nco_neg_pos_val = get_str_val(nco_neg_pos->second);

		syntax.append("set.nco ");
		syntax.append(nco_mode->second);
		syntax.append(", ");
		syntax.append(nco_k->second);
		syntax.append(", ");
		syntax.append(get_val_str(nco_freq_val == 0
			? (1 - nco_neg_pos_val) << 30
			: (2 * nco_neg_pos_val - 1) * nco_freq_val,
			true));
	}
	else if (!strcmp(micro.instr->name, "set_xtrm"))
	{
		std::map<std::string, std::string>::iterator uns1b;
		std::map<std::string, std::string>::iterator min1b;
		std::map<std::string, std::string>::iterator ev1b;
		std::map<std::string, std::string>::iterator im6;
		std::map<std::string, std::string>::iterator value_index;
		std::map<std::string, std::string>::iterator line1b;
		if (core_ == VCPU)
		{
			uns1b = field_map.find("UNS1b");
			min1b = field_map.find("MIN1b");
			ev1b = field_map.find("EV1b");
			im6 = field_map.find("IM4");
			line1b = field_map.find("LINE1b");
		}
		else
		{
			uns1b = field_map.find("Bs_signed");
			min1b = field_map.find("Bs_min");
			ev1b = field_map.find("Bs_even");
			im6 = field_map.find("Iu6");
			line1b = field_map.find("B_line");
			value_index = field_map.find("XTRM_VALUE_INDEX");
		}
		if (uns1b == field_map.end()
			|| min1b == field_map.end()
			|| ev1b == field_map.end()
			|| line1b == field_map.end()
			|| im6 == field_map.end()
			|| ((core_ == VCPU2
					|| is_vcpu3_core(core_))
				&& value_index == field_map.end()))
		{
			return false;
		}

		int64_t line1b_val = get_str_val(line1b->second);
		int64_t im6_val = get_str_val(im6->second);

		syntax.append("set.xtrm ");
		syntax.append(uns1b->second);
		syntax.append(", ");
		syntax.append(min1b->second);
		syntax.append(", ");
		syntax.append(ev1b->second);
		syntax.append(", ");
		if (core_ == VCPU2
			|| is_vcpu3_core(core_))
		{
			syntax.append(value_index->second);
			syntax.append(", ");
		}
		syntax.append(get_val_str(line1b_val * 2 * num_au * (1 + im6_val)
			+ ((1 - line1b_val) << im6_val)));
	}
	else if (!strncmp(micro.instr->name, "set_range", 9) && core_ == VCPU)
	{
		std::map<std::string, std::string>::iterator im22 =
			field_map.find("IM22");
		if (im22 == field_map.end())
		{
			return false;
		}

		std::string old_syntax(micro.instr->syntax);
		size_t pos = old_syntax.find_first_of(",");
		if (pos == std::string::npos)
		{
			return false;
		}
		old_syntax.resize(pos);

		uint64_t imm22 = get_str_val(im22->second);
		uint32_t imm11, imm11_2;
		if (num_au < 32)
		{
			imm11 = (imm22 >> 13) & 0x1FF ;
			imm11_2 = (imm22 >> 3) & 0x1FF ;
		}
		else
		{
			imm11 = (imm22 >> 11) & 0x7FF ;
			imm11_2 = imm22 & 0x7FF ;
		}

		syntax.append(old_syntax);
		syntax.append(", ");
		syntax.append(get_val_str(imm11));
		syntax.append(", ");
		syntax.append(get_val_str(imm11_2));
	}
	// VSPA2.
	else if (!strcmp(micro.instr->name, "set_loop_aX_INSTR"))
	{
		std::map<std::string, std::string>::iterator a = field_map.find("AXG");
		std::map<std::string, std::string>::iterator loop_size =
			field_map.find("C_INSTR_CNT");
		if (a == field_map.end()
			|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("set.loop ");
		syntax.append(a->second);
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strcmp(micro.instr->name, "setC_loop_aX_INSTR"))
	{
		std::map<std::string, std::string>::iterator a = field_map.find("AXG");
		std::map<std::string, std::string>::iterator loop_size =
			field_map.find("C_INSTR_CNT");
		if (a == field_map.end()
			|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("setC.loop ");
		syntax.append(a->second);
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strcmp(micro.instr->name, "set_loop_ITER_INSTR"))
	{
		std::map<std::string, std::string>::iterator iter_cnt =
			field_map.find("C_ITER_CNT");
		std::map<std::string, std::string>::iterator loop_size =
			field_map.find("C_INSTR_CNT");
		if (iter_cnt == field_map.end()
				|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t iter_cnt_val = get_str_val(iter_cnt->second);
		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("set.loop ");
		syntax.append(get_val_str(iter_cnt_val + 1));
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strcmp(micro.instr->name, "setC_loop_ITER"))
	{
		std::map<std::string, std::string>::iterator iter_cnt =
			field_map.find("C_ITER_CNT");
		if (iter_cnt == field_map.end())
		{
			return false;
		}

		int64_t iter_cnt_val = get_str_val(iter_cnt->second);

		syntax.append("setC.loop ");
		syntax.append(get_val_str(iter_cnt_val + 1));
	}
	else if (!strcmp(micro.instr->name, "setB_loop_Iu11"))
	{
		std::map<std::string, std::string>::iterator iter_cnt =
			field_map.find("C_ITER_CNT_SHORT");
		if (iter_cnt == field_map.end())
		{
			return false;
		}

		int64_t iter_cnt_val = get_str_val(iter_cnt->second);

		syntax.append("setB.loop ");
		syntax.append(get_val_str(iter_cnt_val + 1));
	}
	else if (!strncmp(micro.instr->name, "stm", 3))
	{
		std::map<std::string, std::string>::iterator is16 =
			field_map.find("Is16");
		std::map<std::string, std::string>::iterator mask_32 =
			field_map.find("MASK_32");

		if (is16 == field_map.end() || mask_32 == field_map.end())
		{
			return false;
		}

		int64_t is16_val = get_str_val(is16->second);
		uint32_t mask_32_val = static_cast<uint32_t>(
			get_str_val(mask_32->second));

		syntax.append("stm [sp");
		if (is16_val > 0)
		{
			syntax.append("+");
		}
		if (is16_val != 0)
		{
			syntax.append(get_val_str(is16_val, true));
		}
		syntax.append("]");

		uint32_t mask = 1u << 12;
		for (int i = 0; i < 20; i++, mask <<= 1)
		{
			if ((mask_32_val & mask) != 0)
			{
				syntax.append(", a");
				syntax.append(get_val_str(i, true));
			}
		}

		mask = 1u;
		for (int i = 0; i < 12; i++, mask <<= 1)
		{
			if ((mask_32_val & mask) != 0)
			{
				syntax.append(", g");
				syntax.append(get_val_str(i, true));
			}
		}
	}
	else if (!strncmp(micro.instr->name, "ldm", 3))
	{
		std::map<std::string, std::string>::iterator is16 =
			field_map.find("Is16");
		std::map<std::string, std::string>::iterator mask_32 =
			field_map.find("MASK_32");

		if (is16 == field_map.end() || mask_32 == field_map.end())
		{
			return false;
		}

		int64_t is16_val = get_str_val(is16->second);
		uint32_t mask_32_val = static_cast<uint32_t>(
			get_str_val(mask_32->second));

		syntax.append("ldm ");

		uint32_t mask = 1u << 12;
		for (int i = 0; i < 20; i++, mask <<= 1)
		{
			if ((mask_32_val & mask) != 0)
			{
				syntax.append("a");
				syntax.append(get_val_str(i, true));
				syntax.append(", ");
			}
		}

		mask = 1u;
		for (int i = 0; i < 12; i++, mask <<= 1)
		{
			if ((mask_32_val & mask) != 0)
			{
				syntax.append("g");
				syntax.append(get_val_str(i, true));
				syntax.append(", ");
			}
		}

		syntax.append("[sp");
		if (is16_val > 0)
		{
			syntax.append("+");
		}
		if (is16_val != 0)
		{
			syntax.append(get_val_str(is16_val, true));
		}
		syntax.append("]");
	}
	// IPPU and IPPU2.
	else if (!strcmp(micro.instr->name, "set_loop"))
	{
		std::map<std::string, std::string>::iterator set_loop_index
			= field_map.find("SET_LOOP_INDEX");
		std::map<std::string, std::string>::iterator loop_size
			= field_map.find("IM8");
		if (set_loop_index == field_map.end()
			|| loop_size == field_map.end())
		{
			return false;
		}

		int64_t loop_size_val = get_str_val(loop_size->second);

		syntax.append("set.loop ");
		syntax.append(set_loop_index->second);
		syntax.append(", ");
		syntax.append(get_val_str(loop_size_val + 1));
	}
	else if (!strncmp(micro.instr->name, "loop_", 5))
	{
		std::string micro_syntax(micro.instr->syntax);
		std::string::size_type pos = micro_syntax.find_last_of(" ");
		int64_t val = get_str_val(micro_syntax.substr(pos + 1));
		syntax.append(micro_syntax.substr(0, pos));
		syntax.append(" ");
		syntax.append(get_val_str(val));
	}
	// IPPU
	else if (!strcmp(micro.instr->name, "set_range_aY_asA_Iu17"))
	{
		std::map<std::string, std::string>::iterator aa = field_map.find("AA");
		std::map<std::string, std::string>::iterator as = field_map.find("AS");
		std::map<std::string, std::string>::iterator im17
			= field_map.find("IM17");
		if (aa == field_map.end()
			|| as == field_map.end()
			|| im17 == field_map.end())
		{
			return false;
		}

		int64_t im17_val = get_str_val(im17->second);

		syntax.append("set.range ");
		syntax.append(aa->second);
		syntax.append(", ");
		syntax.append(as->second);
		syntax.append(", ");
		syntax.append(get_val_str(im17_val + 1));
	}

	return true;
}

bool Macroinstruction::contains_null()
{
	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
			micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		if (micro.instr->properties & DA_INSN_PROP_NULL)
		{
			return true;
		}
	}

	return false;
}

bool Macroinstruction::contains_dmem_access()
{
	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
			micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		if (micro.instr->properties & DA_INSN_PROP_STORE
			|| (micro.instr->properties & DA_INSN_PROP_LOAD
				&& !(micro.instr->block == opVr)))
		{
			return true;
		}
	}

	return false;
}

bool Macroinstruction::contains_mvip()
{
	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
		micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		if (micro.instr->properties & DA_INSN_PROP_IP_CONTROL)
		{
			DAsmField f = micro.fields[0];
			if (strncmp(f.name, "IP_REG", 6) == 0)
			{
				long val;
				//try parsing as hex value
				sscanf(f.value, "0x%lx", &val);
				if (val != DA_DMA_XFER_CRTL_REG)
				{
					//try parsing as dec
					sscanf(f.value, "%d", &val);
				}
				if (val == DA_DMA_XFER_CRTL_REG)
					return true;
			}
		}
	}

	return false;
}


bool Macroinstruction::contains_cof(Microinstruction *&instr)
{
	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
			micro_it != instrs.end(); ++micro_it)
	{
		if (micro_it->instr->properties & DA_INSN_PROP_COF)
		{
			instr = &(*micro_it);
			return true;
		}
	}

	return false;
}

std::string Macroinstruction::to_str(uint32_t num_au)
{
	std::string macro_str("");
	bool first_micro = true;

	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
			micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		std::string micro_str("");
		if (micro.instr == &invalid_instruction)
		{
			micro_str.append("Invalid instruction");
		}
		else
		{
			std::string syntax;
			if (special_syntax(micro, syntax, num_au))
			{
				micro_str.append(syntax);
			}
			else
			{
				micro_str.append(micro.instr->syntax);
			}

			std::vector<DAsmField> &fields = micro.fields;
			for (std::vector<DAsmField>::iterator field_it = fields.begin();
					field_it != fields.end(); ++field_it)
			{
				DAsmField &field = *field_it;

				std::string::size_type pos = micro_str.find(field.name);
				while (pos != std::string::npos)
				{
					micro_str.replace(pos, std::strlen(field.name), field.value);
						pos = micro_str.find(field.name);
				}
			}

			/* convert to lower case until the first blank in the syntax */
			if (format_options_ & DA_FMT_LOWERCASE)
			{
				std::string::size_type pos = micro_str.find(" ");
				if (pos == std::string::npos)
				{
					pos = micro_str.size();
				}
				std::transform(micro_str.begin(), micro_str.begin() + pos,
						micro_str.begin(), ::tolower);
			}
		}


		if (first_micro)
		{
			first_micro = false;
		}
		else
		{
			macro_str.append(" ");
		}
		macro_str.append(micro_str);
		macro_str.append(";");
	}

	/* substitute "+-" with "-" */
	size_t pos = macro_str.find("+-");
	while (pos != std::string::npos)
	{
		macro_str = macro_str.replace(pos, 2, "-");
		pos = macro_str.find("+-");
	}

	/* substitute ",;" with ";" */
	pos = macro_str.find(",;");
	while (pos != std::string::npos)
	{
		macro_str = macro_str.replace(pos, 2, ";");
		pos = macro_str.find(",;");
	}

	/* substitute ",," with "," */
	pos = macro_str.find(",,");
	while (pos != std::string::npos)
	{
		macro_str = macro_str.replace(pos, 2, ",");
		pos = macro_str.find(",,");
	}

	return macro_str;
}

char *find_pmem_sym(unsigned int addr,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol)

{
	for (unsigned i = 0; i < num_symbol; i++)
	{
		if (sym_table[i].address == addr)
		{
			return 1 + sym_table[i].symbol;
		}
	}

	return NULL;
}

bool find_symbol_name(bfd_signed_vma value,
	ruby_sym_class s_type,
	std::string str,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol)
{
	if (s_type == pmem_sym)
	{
		if (num_symbol > 0)
		{
			char *symbol = find_pmem_sym(static_cast<unsigned int>(value),
				sym_table, num_symbol);
			if (symbol)
			{
				str.append(symbol);
				return true;
			}
		}
	}

	return false;
}

void Macroinstruction::imm_to_sym(struct SYMTABLE sym_table[],
		unsigned num_symbol)
{
	std::vector<Microinstruction>::iterator micro_it;
	for (micro_it = instrs.begin(); micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		if (micro.instr == &invalid_instruction)
		{
			continue;
		}

		std::vector<DAsmField>::iterator field_it;
		std::vector<DAsmField> &fields = micro.fields;
		for (field_it = fields.begin(); field_it != fields.end(); ++field_it)
		{
			DAsmField &field = *field_it;
			if (!field.reloc_value)
			{
				continue;
			}

			int64_t value;
			std::stringstream ss(field.value);
			if ((format_options_ & DA_FMT_DECIMALIMM) == DA_FMT_DECIMALIMM)
			{
				ss >> value;
			}
			else
			{
				ss >> std::hex >> value;
			}
			ruby_sym_class sym_class;
			if (field.reloc_width == 17)
			{
				sym_class = pmem_sym;
			}
			else
			{
				sym_class = dmem_sym;
			}

			std::string symbol;
			if (find_symbol_name(value, sym_class, symbol, sym_table,
				num_symbol))
			{
				strncpy(field.value, symbol.c_str(), DA_MAX_FIELD_LEN - 1);
				field.value[DA_MAX_FIELD_LEN - 1] = 0;
			}
		}
	}
}

unsigned char Macroinstruction::get_family_map()
{
	unsigned char op_flag = 0;

	for (std::vector<Microinstruction>::iterator micro_it = instrs.begin();
			micro_it != instrs.end(); ++micro_it)
	{
		Microinstruction &micro = *micro_it;
		switch (micro.instr->block)
		{
		case opA:
			op_flag |= FAMILY_OPA;
			break;
		case opB:
			op_flag |= FAMILY_OPB;
			break;
		case opC:
			op_flag |= FAMILY_OPC;
			break;
		case opD:
		case FNOP:
		case DONE:
			op_flag |= FAMILY_OPD;
			break;
		case opS:
			op_flag |= FAMILY_OPS;
			break;
		case opVs0:
		case opVs1:
		case opVs2:
		case opVau:
		case opVrot:
		case opVd:
		case opVr:
		case opVror:
		case opVrol:
		case opVld:
		case opVwr:
		case opVsau:
			op_flag |= FAMILY_OPV;
			break;
		case opX:
			op_flag |= FAMILY_OPX;
			break;
		case opZ:
			op_flag |= FAMILY_OPZ;
			break;
		}
	}

	return op_flag;
}

void Microinstruction::get_cof_type(enum cof_type *type, long *immediate)
{
	assert(instr->properties & DA_INSN_PROP_COF);

	// Check for a rts.
	if (instr->block == opZ)
	{
		*type = COF_RTS;
		*immediate = 0;
		return;
	}

	// A jmp/jsr is always an opC.
	assert(instr->block == opC);
	DAsmField &param = fields[fields.size() - 1];
	if (param.reloc_value)
	{
		*type = (instr->properties & DA_INSN_PROP_USES_SP)
			? COF_JSR_IMM
			: COF_JMP_IMM;
		*immediate = strtoul(param.value, NULL, 0);
	}
	else
	{
		*type = (instr->properties & DA_INSN_PROP_USES_SP)
			? COF_JSR_REG
			: COF_JMP_REG;
		*immediate = 0;
	}
	
}

int disasm_instruction_vcpuX(DAsmCore core,
	bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number)
{
  /* set the current core */
  instr_dsc_db.set_core(core);

  Macroinstruction macro_inst(ruby_insn, flag, core);
  macro_inst.disasm();
  macro_inst.imm_to_sym(sym_table, num_symbol);

  std::string str = macro_inst.to_str(*au_version_number);
  int used = snprintf(out_buf, out_buf_size - 1, str.c_str());
  if (used == out_buf_size - 1)
  {
    out_buf[used] = '\0';
  }

  unsigned char op_flag = macro_inst.get_family_map();
  if (!(flag & DECODE_TYPE_NO_FLAG_PRINT))
  {
    used = snprintf(out_buf + strlen(out_buf), out_buf_size - strlen(out_buf) - 1, " // op_flag = 0x%x", op_flag);
    if (used == out_buf_size - strlen(out_buf) - 1)
    {
      out_buf[used] = '\0';
    }
  }
  *family = op_flag;

  return RUBY_SUCCESS;
}

#ifndef EXTENDED_DISASSEMBLER_API

DLL_EXPORT int disassemble_instruction_vcpu(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number)
{
	bfd_vma macro_insn = ruby_insn;
	unsigned short hw_insn = 0;


	macro_insn = macro_insn & (((bfd_vma) 1 << MACRO_INSN_BITS) - 1);
	hw_insn = (macro_insn >> HW_INSN_POS) & LSB_2;

	if ((flag & DECODE_TYPE_LOWER_OPS) || (flag & DECODE_TYPE_UPPER_OPS))
	{
		if (!hw_insn || hw_insn != HW_DUAL)
		{
			fprintf(stderr, "error: decode type not matching the instruction type\n");
			return RUBY_FAIL;
		}
	}

  return disasm_instruction_vcpuX(VCPU, ruby_insn, out_buf, out_buf_size, flag, sym_table, num_symbol, family, au_version_number);
}

DLL_EXPORT int disassemble_instruction_vcpu2(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	int flag,
	struct SYMTABLE sym_table[],
	unsigned num_symbol,
	unsigned char *family,
	const unsigned int *au_version_number)
{
	bfd_vma macro_insn = ruby_insn;
	unsigned short hw_insn = 0;

	hw_insn = (macro_insn >> HW_INSN_POS2) & LSB_1;

	if (flag & DECODE_TYPE_LOWER_OPS || flag & DECODE_TYPE_UPPER_OPS)
	{
		if (!hw_insn)
		{
			fprintf(stderr, "error: decode type not matching the instruction type\n");
			return RUBY_FAIL;
		}
	}

	return disasm_instruction_vcpuX(VCPU2, ruby_insn, out_buf, out_buf_size, flag, sym_table, num_symbol, family, au_version_number);
}

#ifdef _VSPA3_
DLL_EXPORT int disassemble_instruction_vcpu3(bfd_vma ruby_insn,
  char *out_buf,
  int out_buf_size,
  int flag,
  struct SYMTABLE sym_table[],
  unsigned num_symbol,
  unsigned char *family,
  const unsigned int *au_version_number)
{
  bfd_vma macro_insn = ruby_insn;
  unsigned short hw_insn = 0;

  hw_insn = (macro_insn >> HW_INSN_POS2) & LSB_1;

  if (flag & DECODE_TYPE_LOWER_OPS || flag & DECODE_TYPE_UPPER_OPS)
  {
    if (!hw_insn)
    {
      fprintf(stderr, "error: decode type not matching the instruction type\n");
      return RUBY_FAIL;
    }
  }

  return disasm_instruction_vcpuX(VCPU3, ruby_insn, out_buf, out_buf_size, flag, sym_table, num_symbol, family, au_version_number);
}
#endif //_VSPA3_

int disassemble_instruction_ippuX(DAsmCore core,
  bfd_vma ruby_insn,
  char *out_buf,
  int out_buf_size,
  struct SYMTABLE sym_table[],
  unsigned int num_symbol)
{
  // Set the current core.
  instr_dsc_db.set_core(core);

  Macroinstruction macro_inst(ruby_insn, DECODE_TYPE_MACRO, core);
  macro_inst.disasm();
  macro_inst.imm_to_sym(sym_table, num_symbol);

  std::string str = macro_inst.to_str();
  int used = snprintf(out_buf, out_buf_size - 1, str.c_str());
  if (used == out_buf_size - 1)
  {
    out_buf[used] = '\0';
  }

  return RUBY_SUCCESS;
}

DLL_EXPORT int disassemble_instruction_ippu(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol)
{
  return disassemble_instruction_ippuX(IPPU, ruby_insn, out_buf, out_buf_size, sym_table, num_symbol);
}

DLL_EXPORT int disassemble_instruction_ippu2(bfd_vma ruby_insn,
	char *out_buf,
	int out_buf_size,
	struct SYMTABLE sym_table[],
	unsigned int num_symbol)
{
	return disassemble_instruction_ippuX(IPPU2, ruby_insn, out_buf, out_buf_size, sym_table, num_symbol);;
}

#ifdef _VSPA3_
DLL_EXPORT int disassemble_instruction_ippu3(bfd_vma ruby_insn,
  char *out_buf,
  int out_buf_size,
  struct SYMTABLE sym_table[],
  unsigned int num_symbol)
{
  return disassemble_instruction_ippuX(IPPU3, ruby_insn, out_buf, out_buf_size, sym_table, num_symbol);;
}
#endif //_VSPA3_

#else // EXTENDED_DISASSEMBLER_API

extern "C" DLL_EXPORT void check_dmem_access(bfd_vma ruby_insn,
	char *compacted,
	char *flag,
	enum DAsmCore core_type)
{
	bfd_vma macro_insn = ruby_insn;
	unsigned short hw_insn = 0;

	/* set the current core */
	instr_dsc_db.set_core(core_type);

	macro_insn = macro_insn & (((bfd_vma) 1 << MACRO_INSN_BITS) - 1);
	hw_insn = (macro_insn >> HW_INSN_POS) & LSB_2;

	*compacted = 0;
	*flag = 0;
	if (hw_insn == HW_DUAL)
	{
		/* hardware frame */
		Macroinstruction upper_macro(ruby_insn, DECODE_TYPE_UPPER_OPS, core_type);
		Macroinstruction lower_macro(ruby_insn, DECODE_TYPE_LOWER_OPS, core_type);

		upper_macro.disasm();
		lower_macro.disasm();

		if (upper_macro.contains_null())
		{
			if (lower_macro.contains_dmem_access())
			{
				*flag |= 0x3;
			}
		}
		else
		{
			*compacted = 1;

			if (upper_macro.contains_dmem_access())
			{
				*flag |= 0x2;
			}
		
			if (lower_macro.contains_dmem_access())
			{
				*flag |= 0x1;
			}
		}
	}
	else
	{
		/* full-word macroinstruction */
		Macroinstruction macro(ruby_insn, DECODE_TYPE_MACRO, core_type);

		macro.disasm();

		if (macro.contains_dmem_access())
		{
			*flag = 0x3;
		}
	}
}

extern "C" DLL_EXPORT int check_cof(bfd_vma ruby_insn,
	enum cof_type *type,
	long *immediate,
	enum DAsmCore core_type)
{
	bfd_vma macro_insn = ruby_insn;
	unsigned short hw_insn = 0;

	// Set the current core.
	instr_dsc_db.set_core(core_type);

	macro_insn = macro_insn & (((bfd_vma) 1 << MACRO_INSN_BITS) - 1);
	hw_insn = (macro_insn >> HW_INSN_POS) & LSB_2;
	if (hw_insn == HW_DUAL)
	{
		// We have a hardware frame.
		Macroinstruction upper_macro(ruby_insn, DECODE_TYPE_UPPER_OPS, core_type);
		Macroinstruction lower_macro(ruby_insn, DECODE_TYPE_LOWER_OPS, core_type);

		upper_macro.disasm();

		// Compacted macros don't include change of flow instructions.
		if (!upper_macro.contains_null())
		{
			return 0;
		}

		lower_macro.disasm();

		// Check for a rts in a null-patched packet.
		Microinstruction *cof = NULL;
		if (!lower_macro.contains_cof(cof))
		{
			return 0;
		}

		cof->get_cof_type(type, immediate);

		return 1;
	}

	Macroinstruction macro(ruby_insn, DECODE_TYPE_MACRO, core_type);
	Microinstruction *cof = NULL;

	macro.disasm();
	if (!macro.contains_cof(cof))
	{
		return 0;
	}

	cof->get_cof_type(type, immediate);

	return 1;
}

extern "C" DLL_EXPORT int check_mvip(bfd_vma ruby_insn)
{
	bfd_vma macro_insn = ruby_insn;
	unsigned short hw_insn = 0;

	// Set the current core.
	instr_dsc_db.set_core(VCPU);

	macro_insn = macro_insn & (((bfd_vma)1 << MACRO_INSN_BITS) - 1);
	hw_insn = (macro_insn >> HW_INSN_POS) & LSB_2;
	if (hw_insn == HW_DUAL)
	{
		// We have a hardware frame.
		Macroinstruction upper_macro(ruby_insn, DECODE_TYPE_UPPER_OPS, VCPU);
		Macroinstruction lower_macro(ruby_insn, DECODE_TYPE_LOWER_OPS, VCPU);

		upper_macro.disasm();

		if (upper_macro.contains_null())
		{
			if (lower_macro.contains_mvip())
			{
				return 1;
			}
		}
		else
		{
			if (upper_macro.contains_mvip())
			{
				return 1;
			}

			if (lower_macro.contains_mvip())
			{
				return 1;
			}
		}
	}
	else
	{
		/* full-word macroinstruction */
		Macroinstruction macro(ruby_insn, DECODE_TYPE_MACRO, VCPU);

		macro.disasm();

		if (macro.contains_mvip())
		{
			return 1;
		}
	}
	return 0;
}

#endif // EXTENDED_DISASSEMBLER_API

void ruby_disassemble_init()
{
}

void ruby_disassemble_finish()
{
}
