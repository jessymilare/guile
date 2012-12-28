/*
 * Copyright (C) 2012  Free Software Foundation, Inc.
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#if PROTO
typedef union {
    struct {	jit_uint32_t _:26;	jit_uint32_t b :  6; } hc;
    struct {	jit_uint32_t _:21;	jit_uint32_t b :  5; } rs;
    struct {	jit_uint32_t _:21;	jit_uint32_t b :  5; } fm;
    struct {	jit_uint32_t _:16;	jit_uint32_t b :  5; } rt;
    struct {	jit_uint32_t _:16;	jit_uint32_t b :  5; } ft;
    struct {	jit_uint32_t _:11;	jit_uint32_t b :  5; } rd;
    struct {	jit_uint32_t _:11;	jit_uint32_t b :  5; } fs;
    struct {	jit_uint32_t _: 6;	jit_uint32_t b :  5; } ic;
    struct {	jit_uint32_t _: 6;	jit_uint32_t b :  5; } fd;
    struct {	jit_uint32_t _: 6;	jit_uint32_t b : 10; } tr;
    struct {	jit_uint32_t _: 6;	jit_uint32_t b : 20; } br;
    struct {				jit_uint32_t b :  6; } tc;
    struct {				jit_uint32_t b : 11; } cc;
    struct {				jit_uint32_t b : 16; } is;
    struct {				jit_uint32_t b : 26; } ii;
    int						  op;
} jit_instr_t;

/* FIXME */
#  define jit_mips2_p()			0

#  define _ZERO_REGNO			0
#  define _T0_REGNO			0x08
#  define _T1_REGNO			0x09
#  define _T2_REGNO			0x0a
#  define _T3_REGNO			0x0b
#  define _T4_REGNO			0x0c
#  define _T5_REGNO			0x0d
#  define _T6_REGNO			0x0e
#  define _T7_REGNO			0x0f
#  define _S0_REGNO			0x10
#  define _S1_REGNO			0x11
#  define _S2_REGNO			0x12
#  define _S3_REGNO			0x13
#  define _S4_REGNO			0x14
#  define _S5_REGNO			0x15
#  define _S6_REGNO			0x16
#  define _S7_REGNO			0x17
#  define _T8_REGNO			0x18
#  define _T9_REGNO			0x19
#  define _SP_REGNO			0x1d
#  define _BP_REGNO			0x1e
#  define _RA_REGNO			0x1f
#  define _F16_REGNO			16
#  define _F18_REGNO			18
#  define _F20_REGNO			20
#  define _F22_REGNO			22
#  define _F24_REGNO			24
#  define _F26_REGNO			26
#  define _F28_REGNO			28
#  define _F30_REGNO			30

#  if __WORDSIZE == 32
#    define stack_framesize		104
#    define ldi(u, v)			ldi_i(u, v)
#    define ldxi(u, v, w)		ldxi_i(u, v, w)
#    define sti(u, v)			sti_i(u, v)
#    define stxi(u, v, w)		stxi_i(u, v, w)
#  else
#    define stack_framesize		144
#    define ldi(u, v)			ldi_l(u, v)
#    define ldxi(u, v, w)		ldxi_l(u, v, w)
#    define sti(u, v)			sti_l(u, v)
#    define stxi(u, v, w)		stxi_l(u, v, w)
#  endif

#  define can_sign_extend_short_p(im)	((im) >= -32678 && (im) <= 32767)
#  define can_zero_extend_short_p(im)	((im) >= 0 && (im) <= 65535)
#  if __WORDSIZE == 32
#    define can_sign_extend_int_p(im)	1
#    define can_zero_extend_int_p(im)	1
#  else
#    define can_sign_extend_int_p(im)					\
	(((im) >= 0 && (im) <=  0x7fffffffL) ||				\
	 ((im) <  0 && (im) >= -0x80000000L))
#    define can_zero_extend_int_p(im)	((im) >= 0 && (im) <= 0xffffffff)
#  endif

#  define MIPS_SPECIAL			0x00
#  define MIPS_REGIMM			0x01
#  define MIPS_J			0x02
#  define MIPS_SRL			0x02
#  define MIPS_JAL			0x03
#  define MIPS_SRA			0x03
#  define MIPS_BEQ			0x04
#  define MIPS_BNE			0x05
#  define MIPS_BLEZ			0x06
#  define MIPS_BGTZ			0x07
#  define MIPS_ADDI			0x08
#  define MIPS_ADDIU			0x09
#  define MIPS_SLTI			0x0a
#  define MIPS_SLTIU			0x0b
#  define MIPS_ANDI			0x0c
#  define MIPS_ORI			0x0d
#  define MIPS_XORI			0x0e
#  define MIPS_LUI			0x0f
#  define MIPS_COP0			0x10
#  define MIPS_COP1			0x11
#  define MIPS_COP2			0x12
#  define MIPS_COP1X			0x13
#  define MIPS_BEQL			0x14
#  define MIPS_BNEL			0x15
#  define MIPS_BLEZL			0x16
#  define MIPS_BGTZL			0x17
#  define MIPS_DADDI			0x18
#  define MIPS_DADDIU			0x19
#  define MIPS_LDL			0x1a
#  define MIPS_LDR			0x1b
#  define MIPS_SPECIAL2			0x1c
#  define MIPS_JALX			0x1d
#  define MIPS_SPECIAL3			0x1f
#  define MIPS_LB 			0x20
#  define MIPS_LH 			0x21
#  define MIPS_LWL			0x22
#  define MIPS_LW 			0x23
#  define MIPS_LBU			0x24
#  define MIPS_LHU			0x25
#  define MIPS_LWR			0x26
#  define MIPS_LWU			0x27
#  define MIPS_SB 			0x28
#  define MIPS_SH 			0x29
#  define MIPS_SWL			0x2a
#  define MIPS_SW 			0x2b
#  define MIPS_SWR			0x2e
#  define MIPS_CACHE			0x2f
#  define MIPS_LL 			0x30
#  define MIPS_LWC1			0x31
#  define MIPS_LWC2			0x32
#  define MIPS_PREF			0x33
#  define MIPS_LLD			0x34
#  define MIPS_LDC1			0x35
#  define MIPS_LDC2			0x36
#  define MIPS_LD 			0x37
#  define MIPS_SC 			0x38
#  define MIPS_SCD			0x3c
#  define MIPS_SDC1			0x3d
#  define MIPS_SDC2			0x3e
#  define MIPS_SWC1			0x39
#  define MIPS_SWC2			0x3a
#  define MIPS_SD 			0x3f

#  define MIPS_MF			0x00
#  define MIPS_DMF			0x01
#  define MIPS_CF			0x02
#  define MIPS_MFH			0x03
#  define MIPS_MT			0x04
#  define MIPS_DMT			0x05
#  define MIPS_CT			0x06
#  define MIPS_MTH			0x07
#  define MIPS_BC			0x08
#  define MIPS_WRPGPR			0x0e
#  define MIPS_BGZAL			0x11
#  define MIPS_MFMC0			0x11

#  define MIPS_BCF			0x00
#  define MIPS_BLTZ			0x00
#  define MIPS_BCT			0x01
#  define MIPS_BGEZ			0x01
#  define MIPS_BCFL			0x02
#  define MIPS_BLTZL			0x02
#  define MIPS_BCTL			0x03
#  define MIPS_BGEZL			0x03
#  define MIPS_TGEI			0x08
#  define MIPS_TGEIU			0x09
#  define MIPS_TLTI			0x0a
#  define MIPS_TLTIU			0x0b
#  define MIPS_TEQI			0x0c
#  define MIPS_TNEI			0x0e
#  define MIPS_BLTZAL			0x10
#  define MIPS_BGEZAL			0x11
#  define MIPS_BLTZALL			0x12
#  define MIPS_BGEZALL			0x13
#  define MIPS_SYNCI			0x1f

#  define MIPS_WSBH			0x02
#  define MIPS_DBSH			0x02
#  define MIPS_DSHD			0x05
#  define MIPS_SEB			0x10
#  define MIPS_SEH			0x18

#  define MIPS_MADD			0x00
#  define MIPS_SLL			0x00
#  define MIPS_EXT			0x00
#  define MIPS_DEXTM			0x01
#  define MIPS_MADDU			0x01
#  define MIPS_MOVFT			0x01
#  define MIPS_TLBR			0x01
#  define MIPS_MUL			0x02
#  define MIPS_DEXTU			0x02
#  define MIPS_TLBWI			0x02
#  define MIPS_DEXT			0x03
#  define MIPS_SLLV			0x04
#  define MIPS_INS			0x04
#  define MIPS_MSUB			0x04
#  define MIPS_DINSM			0x05
#  define MIPS_MSUBU			0x05
#  define MIPS_SRLV			0x06
#  define MIPS_DINSU			0x06
#  define MIPS_TLBWR			0x06
#  define MIPS_SRAV			0x07
#  define MIPS_DINS			0x07
#  define MIPS_JR			0x08
#  define MIPS_TLBP			0x08
#  define MIPS_JALR			0x09
#  define MIPS_MOVZ			0x0a
#  define MIPS_MOVN			0x0b
#  define MIPS_SYSCALL			0x0c
#  define MIPS_BREAK			0x0d
#  define MIPS_PREFX			0x0f
#  define MIPS_SYNC			0x0f
#  define MIPS_MFHI			0x10
#  define MIPS_MTHI			0x11
#  define MIPS_MFLO			0x12
#  define MIPS_MTLO			0x13
#  define MIPS_DSLLV			0x14
#  define MIPS_DSRLV			0x16
#  define MIPS_DSRAV			0x17
#  define MIPS_MULT			0x18
#  define MIPS_ERET			0x18
#  define MIPS_MULTU			0x19
#  define MIPS_DIV			0x1a
#  define MIPS_DIVU			0x1b
#  define MIPS_DMULT			0x1c
#  define MIPS_DMULTU			0x1d
#  define MIPS_DDIV			0x1e
#  define MIPS_DDIVU			0x1f
#  define MIPS_DERET			0x1f
#  define MIPS_ADD			0x20
#  define MIPS_CLZ			0x20
#  define MIPS_BSHFL			0x20
#  define MIPS_ADDU			0x21
#  define MIPS_CLO			0x21
#  define MIPS_SUB			0x22
#  define MIPS_SUBU			0x23
#  define MIPS_AND			0x24
#  define MIPS_DCLZ			0x24
#  define MIPS_DBSHFL			0x24
#  define MIPS_OR			0x25
#  define MIPS_DCLO			0x25
#  define MIPS_XOR			0x26
#  define MIPS_NOR			0x27
#  define MIPS_SLT			0x2a
#  define MIPS_SLTU			0x2b
#  define MIPS_DADD			0x2c
#  define MIPS_DADDU			0x2d
#  define MIPS_DSUB			0x2e
#  define MIPS_DSUBU			0x2f
#  define MIPS_TGE			0x30
#  define MIPS_TGEU			0x31
#  define MIPS_TLT			0x32
#  define MIPS_TLTU			0x33
#  define MIPS_TEQ			0x34
#  define MIPS_TNE			0x36
#  define MIPS_DSLL			0x38
#  define MIPS_DSRL			0x3a
#  define MIPS_DSRA			0x3b
#  define MIPS_DSLL32			0x3c
#  define MIPS_DSRL32			0x3e
#  define MIPS_DSRA32			0x3f
#  define MIPS_SDBPP			0x3f
#  define ii(i)				*_jit->pc.ui++ = i

static void
_hrrrit(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t,jit_int32_t,
	jit_int32_t, jit_int32_t);
#  define hrrrit(hc,rs,rt,rd,im,tc)	_hrrrit(_jit,hc,rs,rt,rd,im,tc)
#  define hrrr_t(hc,rs,rt,rd,tc)	hrrrit(hc,rs,rt,rd,0,tc)
#  define rrr_t(rs,rt,rd,tc)		hrrr_t(0,rs,rt,rd,tc)
#  define hrri(hc,rs,rt,im)		_hrri(_jit,hc,rs,rt,im)
static void _hrri(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t,jit_int32_t);
#  define hi(hc,im)			_hi(_jit,hc,im)
static void _hi(jit_state_t*,jit_int32_t,jit_int32_t);
#  define NOP(i0)			_nop(_jit, i0)
static void _nop(jit_state_t*,jit_int32_t);
#  define h_ri(hc,rt,im)		_hrri(_jit,hc,0,rt,im)
#  define rrit(rt,rd,im,tc)		_hrrrit(_jit,0,0,rt,rd,im,tc)
#  define LUI(rt,im)			h_ri(MIPS_LUI,rt,im)
#  if __WORDSIZE == 32
#    define ADDU(rd,rs,rt)		rrr_t(rs,rt,rd,MIPS_ADDU)
#    define ADDIU(rt,rs,im)		hrri(MIPS_ADDIU,rs,rt,im)
#    define SUBU(rd,rs,rt)		rrr_t(rs,rt,rd,MIPS_SUBU)
#    define MULT(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_MULT)
#    define MULTU(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_MULTU)
#    define DIV(rs, rt)			rrr_t(rs,rt,_ZERO_REGNO,MIPS_DIV)
#    define DIVU(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_DIVU)
#    define SLLV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_SLLV)
#    define SLL(rd,rt,sa)		rrit(rt,rd,sa,MIPS_SLL)
#    define SRAV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_SRAV)
#    define SRA(rd,rt,sa)		rrit(rt,rd,sa,MIPS_SRA)
#    define SRLV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_SRLV)
#    define SRL(rd,rt,sa)		rrit(rt,rd,sa,MIPS_SRL)
#    define ROTR(rd,rt,sa)		hrrrit(MIPS_SPECIAL,1,rt,rd,sa,MIPS_DSRL)
#    define INS(rt,rs,pos,size)		hrriit(MIPS_SPECIAL3,rs,rt,pos,pos+size-1,MIPS_DINS)
#  else
#    define ADDU(rd,rs,rt)		rrr_t(rs,rt,rd,MIPS_DADDU)
#    define ADDIU(rt,rs,im)		hrri(MIPS_DADDIU,rs,rt,im)
#    define SUBU(rd,rs,rt)		rrr_t(rs,rt,rd,MIPS_DSUBU)
#    define MULT(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_DMULT)
#    define MULTU(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_DMULTU)
#    define DIV(rs, rt)			rrr_t(rs,rt,_ZERO_REGNO,MIPS_DDIV)
#    define DIVU(rs, rt)		rrr_t(rs,rt,_ZERO_REGNO,MIPS_DDIVU)
#    define SLLV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_DSLLV)
#    define SLL(rd,rt,sa)		rrit(rt,rd,sa,MIPS_DSLL)
#    define SRAV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_DSRAV)
#    define SRA(rd,rt,sa)		rrit(rt,rd,sa,MIPS_DSRA)
#    define SRLV(rd,rt,rs)		rrr_t(rs,rt,rd,MIPS_DSRLV)
#    define SRL(rd,rt,sa)		rrit(rt,rd,sa,MIPS_DSRL)
#    define ROTR(rd,rt,sa)		hrrrit(MIPS_SPECIAL,1,rt,rd,sa,MIPS_SRL)
#    define INS(rt,rs,pos,size)		hrriit(MIPS_SPECIAL3,rs,rt,pos,pos+size-1,MIPS_INS)
#  endif
#  define MFHI(rd)			rrr_t(_ZERO_REGNO,_ZERO_REGNO,rd,MIPS_MFHI)
#  define MFLO(rd)			rrr_t(_ZERO_REGNO,_ZERO_REGNO,rd,MIPS_MFLO)
#  define MTHI(rs)			rrr_t(rs,_ZERO_REGNO,_ZERO_REGNO,MIPS_MTHI)
#  define MTLO(rs)			rrr_t(rs,_ZERO_REGNO,_ZERO_REGNO,MIPS_MTLO)
#  define AND(rd,rs,rt)			rrr_t(rs,rt,rd,MIPS_AND)
#  define ANDI(rt,rs,im)		hrri(MIPS_ANDI,rs,rt,im)
#  define OR(rd,rs,rt)			rrr_t(rs,rt,rd,MIPS_OR)
#  define ORI(rt,rs,im)			hrri(MIPS_ORI,rs,rt,im)
#  define XOR(rd,rs,rt)			rrr_t(rs,rt,rd,MIPS_XOR)
#  define XORI(rt,rs,im)		hrri(MIPS_XORI,rs,rt,im)
#  define LB(rt,of,rb)			hrri(MIPS_LB,rb,rt,of)
#  define LBU(rt,of,rb)			hrri(MIPS_LBU,rb,rt,of)
#  define LH(rt,of,rb)			hrri(MIPS_LH,rb,rt,of)
#  define LHU(rt,of,rb)			hrri(MIPS_LHU,rb,rt,of)
#  define LW(rt,of,rb)			hrri(MIPS_LW,rb,rt,of)
#  define LWU(rt,of,rb)			hrri(MIPS_LWU,rb,rt,of)
#  define LD(rt,of,rb)			hrri(MIPS_LD,rb,rt,of)
#  define SB(rt,of,rb)			hrri(MIPS_SB,rb,rt,of)
#  define SW(rt,of,rb)			hrri(MIPS_SW,rb,rt,of)
#  define SD(rt,of,rb)			hrri(MIPS_SD,rb,rt,of)
#  define WSBH(rd,rt)			hrrrit(MIPS_SPECIAL3,0,rt,rd,MIPS_WSBH,MIPS_BSHFL)
#  define SEB(rd,rt)			hrrrit(MIPS_SPECIAL3,0,rt,rd,MIPS_SEB,MIPS_BSHFL)
#  define SEH(rd,rt)			hrrrit(MIPS_SPECIAL3,0,rt,rd,MIPS_SEH,MIPS_BSHFL)
#  define SLT(rd,rs,rt)			rrr_t(rs,rt,rd,MIPS_SLT)
#  define SLTU(rd,rs,rt)		rrr_t(rs,rt,rd,MIPS_SLTU)
#  define SLTI(rt,rs,im)		hrri(MIPS_SLTI,rs,rt,im)
#  define SLTIU(rt,rs,im)		hrri(MIPS_SLTIU,rs,rt,im)
#  define BLTZ(rs,im)			hrri(MIPS_REGIMM,rs,MIPS_BLTZ,im)
#  define BLEZ(rs,im)			hrri(MIPS_BLEZ,rs,_ZERO_REGNO,im)
#  define BEQ(rs,rt,im)			hrri(MIPS_BEQ,rs,rt,im)
#  define BGEZ(rs,im)			hrri(MIPS_REGIMM,rs,MIPS_BGEZ,im)
#  define BGTZ(rs,im)			hrri(MIPS_BGTZ,rs,_ZERO_REGNO,im)
#  define BNE(rs,rt,im)			hrri(MIPS_BNE,rs,rt,im)
#  define JALR(r0)			hrrrit(MIPS_SPECIAL,r0,0,_RA_REGNO,0,MIPS_JALR)
#  define JR(r0)			hrrrit(MIPS_SPECIAL,r0,0,0,0,MIPS_JR)
#  define J(i0)				hi(MIPS_J, i0)
#  define MOVZ(rd,rs,rt)		hrrrit(0,rs,rt,rd,0,MIPS_MOVZ)
#  define comr(r0,r1)			xori(r0, r1, -1)
#  define negr(r0,r1)			SUBU(r0,_ZERO_REGNO,r1)
#  define addr(rd,rs,rt)		ADDU(rd,rs,rt)
#  define addi(r0,r1,i0)		_addi(_jit,r0,r1,i0)
static void _addi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define addcr(r0, r1, r2)		_addcr(_jit, r0, r1, r2)
static void _addcr(jit_state_t*, jit_int32_t, jit_int32_t, jit_int32_t);
#define addci(r0, r1, i0)		_addci(_jit, r0, r1, i0)
static void _addci(jit_state_t*, jit_int32_t, jit_int32_t, jit_word_t);
#  define addxr(r0, r1, r2)		_addxr(_jit, r0, r1, r2)
static void _addxr(jit_state_t*, jit_int32_t, jit_int32_t, jit_int32_t);
#  define addxi(r0, r1, i0)		_addxi(_jit, r0, r1, i0)
static void _addxi(jit_state_t*, jit_int32_t, jit_int32_t, jit_word_t);
#  define subr(rd,rs,rt)		SUBU(rd,rs,rt)
#  define subi(r0,r1,i0)		_subi(_jit,r0,r1,i0)
static void _subi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define subcr(r0, r1, r2)		_subcr(_jit, r0, r1, r2)
static void _subcr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define subci(r0, r1, i0)		_subci(_jit, r0, r1, i0)
static void _subci(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define subxr(r0, r1, r2)		_subxr(_jit, r0, r1, r2)
static void _subxr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define subxi(r0, r1, i0)		_subxi(_jit, r0, r1, i0)
static void _subxi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define mulr(r0,r1,r2)		_mulr(_jit,r0,r1,r2)
static void _mulr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define muli(r0,r1,i0)		_muli(_jit,r0,r1,i0)
static void _muli(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define divr(r0,r1,r2)		_divr(_jit,r0,r1,r2)
static void _divr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define divi(r0,r1,i0)		_divi(_jit,r0,r1,i0)
static void _divi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define divr_u(r0,r1,r2)		_divr_u(_jit,r0,r1,r2)
static void _divr_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define divi_u(r0,r1,i0)		_divi_u(_jit,r0,r1,i0)
static void _divi_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define remr(r0,r1,r2)		_remr(_jit,r0,r1,r2)
static void _remr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define remi(r0,r1,i0)		_remi(_jit,r0,r1,i0)
static void _remi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define remr_u(r0,r1,r2)		_remr_u(_jit,r0,r1,r2)
static void _remr_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define remi_u(r0,r1,i0)		_remi_u(_jit,r0,r1,i0)
static void _remi_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define lshr(r0, r1, r2)		SLLV(r0, r1, r2)
#  define lshi(r0, r1, i0)		SLL(r0, r1, i0)
#  define rshr(r0, r1, r2)		SRAV(r0, r1, r2)
#  define rshi(r0, r1, i0)		SRA(r0, r1, i0)
#  define rshr_u(r0, r1, r2)		SRLV(r0, r1, r2)
#  define rshi_u(r0, r1, i0)		SRL(r0, r1, i0)
#  define andr(r0, r1, r2)		AND(r0, r1, r2)
#  define andi(r0, r1, i0)		_andi(_jit, r0, r1, i0)
static void _andi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define orr(r0, r1, r2)		OR(r0, r1, r2)
#  define ori(r0, r1, i0)		_ori(_jit, r0, r1, i0)
static void _ori(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define xorr(r0, r1, r2)		XOR(r0, r1, r2)
#  define xori(r0, r1, i0)		_xori(_jit, r0, r1, i0)
static void _xori(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define movr(r0, r1)			orr(r0, r1, _ZERO_REGNO)
#  define movi(r0, i0)			_movi(_jit, r0, i0)
static void _movi(jit_state_t*,jit_int32_t,jit_word_t);
#  define movi_p(r0, i0)		_movi_p(_jit, r0, i0)
static jit_word_t _movi_p(jit_state_t*,jit_int32_t,jit_word_t);
#  define ldr_c(r0, r1)			LB(r0, 0, r1)
#  define ldi_c(r0, i0)			_ldi_c(_jit, r0, i0)
static void _ldi_c(jit_state_t*,jit_int32_t,jit_word_t);
#  define ldr_uc(r0, r1)		LBU(r0, 0, r1)
#  define ldi_uc(r0, i0)		_ldi_uc(_jit, r0, i0)
static void _ldi_uc(jit_state_t*,jit_int32_t,jit_word_t);
#  define ldr_s(r0, r1)			LH(r0, 0, r1)
#  define ldi_s(r0, i0)			_ldi_s(_jit, r0, i0)
static void _ldi_s(jit_state_t*,jit_int32_t,jit_word_t);
#  define ldr_us(r0, r1)		LHU(r0, 0, r1)
#  define ldi_us(r0, i0)		_ldi_us(_jit, r0, i0)
static void _ldi_us(jit_state_t*,jit_int32_t,jit_word_t);
#  define ldr_i(r0, r1)			LW(r0, 0, r1)
#  define ldi_i(r0, i0)			_ldi_i(_jit, r0, i0)
static void _ldi_i(jit_state_t*,jit_int32_t,jit_word_t);
#  if __WORDSIZE == 64
#    define ldr_ui(r0, r1)		LWU(r0, 0, r1)
#    define ldi_ui(r0, i0)		_ldi_ui(_jit, r0, i0)
static void _ldi_ui(jit_state_t*,jit_int32_t,jit_word_t);
#    define ldr_l(r0, r1)		LD(r0, 0, r1)
#    define ldi_l(r0, i0)		_ldi_l(_jit, r0, i0)
static void _ldi_l(jit_state_t*,jit_int32_t,jit_word_t);
#  endif
#  define ldxr_c(r0, r1, r2)		_ldxr_c(_jit, r0, r1, r2)
static void _ldxr_c(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define ldxi_c(r0, r1, i0)		_ldxi_c(_jit, r0, r1, i0)
static void _ldxi_c(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define ldxr_uc(r0, r1, r2)		_ldxr_uc(_jit, r0, r1, r2)
static void _ldxr_uc(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define ldxi_uc(r0, r1, i0)		_ldxi_uc(_jit, r0, r1, i0)
static void _ldxi_uc(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define ldxr_s(r0, r1, r2)		_ldxr_s(_jit, r0, r1, r2)
static void _ldxr_s(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define ldxi_s(r0, r1, i0)		_ldxi_s(_jit, r0, r1, i0)
static void _ldxi_s(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define ldxr_us(r0, r1, r2)		_ldxr_us(_jit, r0, r1, r2)
static void _ldxr_us(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define ldxi_us(r0, r1, i0)		_ldxi_us(_jit, r0, r1, i0)
static void _ldxi_us(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define ldxr_i(r0, r1, r2)		_ldxr_i(_jit, r0, r1, r2)
static void _ldxr_i(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define ldxi_i(r0, r1, i0)		_ldxi_i(_jit, r0, r1, i0)
static void _ldxi_i(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  if __WORDSIZE == 64
#    define ldxr_ui(r0, r1, r2)		_ldxr_ui(_jit, r0, r1, r2)
static void _ldxr_ui(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#    define ldxi_ui(r0, r1, i0)		_ldxi_ui(_jit, r0, r1, i0)
static void _ldxi_ui(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#    define ldxr_l(r0, r1, r2)		_ldxr_l(_jit, r0, r1, r2)
static void _ldxr_l(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#    define ldxi_l(r0, r1, i0)		_ldxi_l(_jit, r0, r1, i0)
static void _ldxi_l(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  endif
#  define str_c(r0, r1)			SB(r1, 0, r0)
#  define sti_c(i0, r0)			_sti_c(_jit, i0, r0)
static void _sti_c(jit_state_t*,jit_word_t,jit_int32_t);
#  define SH(rt,of,rb)			hrri(MIPS_SH,rb,rt,of)
#  define str_s(r0, r1)			SH(r1, 0, r0)
#  define sti_s(i0, r0)			_sti_s(_jit, i0, r0)
static void _sti_s(jit_state_t*,jit_word_t,jit_int32_t);
#  define str_i(r0, r1)			SW(r1, 0, r0)
#  define sti_i(i0, r0)			_sti_i(_jit, i0, r0)
static void _sti_i(jit_state_t*,jit_word_t,jit_int32_t);
#  if __WORDSIZE == 64
#    define str_l(r0, r1)		SD(r0, 0, r1)
#    define sti_l(i0, r0)		_sti_l(_jit, i0, r0)
static void _sti_l(jit_state_t*,jit_int32_t,jit_word_t);
#  endif
#  define stxr_c(r0, r1, r2)		_stxr_c(_jit, r0, r1, r2)
static void _stxr_c(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define stxi_c(i0, r0, r1)		_stxi_c(_jit, i0, r0, r1)
static void _stxi_c(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define stxr_s(r0, r1, r2)		_stxr_s(_jit, r0, r1, r2)
static void _stxr_s(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define stxi_s(i0, r0, r1)		_stxi_s(_jit, i0, r0, r1)
static void _stxi_s(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define stxr_i(r0, r1, r2)		_stxr_i(_jit, r0, r1, r2)
static void _stxr_i(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#  define stxi_i(i0, r0, r1)		_stxi_i(_jit, i0, r0, r1)
static void _stxi_i(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  if __WORDSIZE == 64
#    define stxr_l(r0, r1, r2)		_stxr_l(_jit, r0, r1, r2)
static void _stxr_l(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#    define stxi_l(i0, r0, r1)		_stxi_l(_jit, i0, r0, r1)
static void _stxi_l(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  endif
#  if __BYTE_ORDER == __LITTLE_ENDIAN
#  define htonr(r0,r1)			_htonr(_jit,r0,r1)
static void _htonr(jit_state_t*,jit_int32_t,jit_int32_t);
#  else
#    define htonr(r0,r1)		movr(r0,r1)
#  endif
#  define extr_c(r0,r1)			_extr_c(_jit,r0,r1)
static void _extr_c(jit_state_t*,jit_int32_t,jit_int32_t);
#  define extr_uc(r0,r1)		ANDI(r0, r1, 0xff)
#  define extr_s(r0,r1)			_extr_s(_jit,r0,r1)
static void _extr_s(jit_state_t*,jit_int32_t,jit_int32_t);
#  define extr_us(r0,r1)		ANDI(r0, r1, 0xffff)
#  if __WORDSIZE == 64
#  define extr_i(r0,r1)			SLL(r0, r0, 0)
#  define extr_ui(r0,r1)		_extr_ui(_jit,r0,r1)
static void _extr_ui(jit_state_t*,jit_int32_t,jit_int32_t);
#  endif
#  define ltr(r0, r1, r2)		SLT(r0, r1, r2)
#  define lti(r0, r1, i0)		_lti(_jit, r0, r1, i0)
static void _lti(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define ltr_u(r0, r1, r2)		SLTU(r0, r1, r2)
#  define lti_u(r0, r1, i0)		_lti_u(_jit, r0, r1, i0)
static void _lti_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define ler(r0, r1, r2)			_ler(_jit, r0, r1, r2)
static void _ler(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define lei(r0, r1, i0)			_lei(_jit, r0, r1, i0)
static void _lei(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define ler_u(r0, r1, r2)		_ler_u(_jit, r0, r1, r2)
static void _ler_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define lei_u(r0, r1, i0)		_lei_u(_jit, r0, r1, i0)
static void _lei_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define eqr(r0, r1, r2)			_eqr(_jit, r0, r1, r2)
static void _eqr(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define eqi(r0, r1, i0)			_eqi(_jit, r0, r1, i0)
static void _eqi(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define ger(r0, r1, r2)			_ger(_jit, r0, r1, r2)
static void _ger(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define gei(r0, r1, i0)			_gei(_jit, r0, r1, i0)
static void _gei(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define ger_u(r0, r1, i0)		_ger_u(_jit, r0, r1, i0)
static void _ger_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define gei_u(r0, r1, i0)		_gei_u(_jit, r0, r1, i0)
static void _gei_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define gtr(r0, r1, r2)		SLT(r0, r2, r1)
#define gti(r0, r1, i0)			_gti(_jit, r0, r1, i0)
static void _gti(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#  define gtr_u(r0, r1, r2)		SLTU(r0, r2, r1)
#  define gti_u(r0, r1, i0)		_gti_u(_jit, r0, r1, i0)
static void _gti_u(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define ner(r0, r1, r2)			_ner(_jit, r0, r1, r2)
static void _ner(jit_state_t*,jit_int32_t,jit_int32_t,jit_int32_t);
#define nei(r0, r1, i0)			_nei(_jit, r0, r1, i0)
static void _nei(jit_state_t*,jit_int32_t,jit_int32_t,jit_word_t);
#define bltr(i0, r0, r1)		_bltr(_jit, i0, r0, r1)
static jit_word_t _bltr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bltr_u(i0, r0, r1)		_bltr_u(_jit, i0, r0, r1)
static jit_word_t _bltr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define blti(i0, r0, i1)		_blti(_jit, i0, r0, i1)
static jit_word_t _blti(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define blti_u(i0, r0, i1)		_blti_u(_jit, i0, r0, i1)
static jit_word_t _blti_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bler(i0, r0, r1)		_bler(_jit, i0, r0, r1)
static jit_word_t _bler(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bler_u(i0, r0, r1)		_bler_u(_jit, i0, r0, r1)
static jit_word_t _bler_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define blei(i0, r0, i1)		_blei(_jit, i0, r0, i1)
static jit_word_t _blei(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define blei_u(i0, r0, i1)		_blei_u(_jit, i0, r0, i1)
static jit_word_t _blei_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define beqr(i0, r0, r1)		_beqr(_jit, i0, r0, r1)
static jit_word_t _beqr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define beqi(i0, r0, i1)		_beqi(_jit, i0, r0, i1)
static jit_word_t _beqi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bger(i0, r0, r1)		_bger(_jit, i0, r0, r1)
static jit_word_t _bger(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bger_u(i0, r0, r1)		_bger_u(_jit, i0, r0, r1)
static jit_word_t _bger_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bgei(i0, r0, i1)		_bgei(_jit, i0, r0, i1)
static jit_word_t _bgei(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bgei_u(i0, r0, i1)		_bgei_u(_jit, i0, r0, i1)
static jit_word_t _bgei_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bgtr(i0, r0, r1)		_bgtr(_jit, i0, r0, r1)
static jit_word_t _bgtr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bgtr_u(i0, r0, r1)		_bgtr_u(_jit, i0, r0, r1)
static jit_word_t _bgtr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bgti(i0, r0, i1)		_bgti(_jit, i0, r0, i1)
static jit_word_t _bgti(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bgti_u(i0, r0, i1)		_bgti_u(_jit, i0, r0, i1)
static jit_word_t _bgti_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#define bner(i0, r0, r1)		_bner(_jit, i0, r0, r1)
static jit_word_t _bner(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#define bnei(i0, r0, i1)		_bnei(_jit, i0, r0, i1)
static jit_word_t _bnei(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define jmpr(r0)			_jmpr(_jit, r0)
static void _jmpr(jit_state_t*,jit_int32_t);
#  define jmpi(i0)			_jmpi(_jit, i0)
static jit_word_t _jmpi(jit_state_t*,jit_word_t);
#  define boaddr(i0, r0, r1)		_boaddr(_jit, i0, r0, r1)
static jit_word_t _boaddr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define boaddi(i0, r0, i1)		_boaddi(_jit, i0, r0, i1)
static jit_word_t _boaddi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define boaddr_u(i0, r0, r1)		_boaddr_u(_jit, i0, r0, r1)
static jit_word_t _boaddr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define boaddi_u(i0, r0, i1)		_boaddi_u(_jit, i0, r0, i1)
static jit_word_t _boaddi_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bxaddr(i0, r0, r1)		_bxaddr(_jit, i0, r0, r1)
static jit_word_t _bxaddr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bxaddi(i0, r0, i1)		_bxaddi(_jit, i0, r0, i1)
static jit_word_t _bxaddi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bxaddr_u(i0, r0, r1)		_bxaddr_u(_jit, i0, r0, r1)
static jit_word_t _bxaddr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bxaddi_u(i0, r0, i1)		_bxaddi_u(_jit, i0, r0, i1)
static jit_word_t _bxaddi_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bosubr(i0, r0, r1)		_bosubr(_jit, i0, r0, r1)
static jit_word_t _bosubr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bosubi(i0, r0, i1)		_bosubi(_jit, i0, r0, i1)
static jit_word_t _bosubi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bosubr_u(i0, r0, r1)		_bosubr_u(_jit, i0, r0, r1)
static jit_word_t _bosubr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bosubi_u(i0, r0, i1)		_bosubi_u(_jit, i0, r0, i1)
static jit_word_t _bosubi_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bxsubr(i0, r0, r1)		_bxsubr(_jit, i0, r0, r1)
static jit_word_t _bxsubr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bxsubi(i0, r0, i1)		_bxsubi(_jit, i0, r0, i1)
static jit_word_t _bxsubi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bxsubr_u(i0, r0, r1)		_bxsubr_u(_jit, i0, r0, r1)
static jit_word_t _bxsubr_u(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bxsubi_u(i0, r0, i1)		_bxsubi_u(_jit, i0, r0, i1)
static jit_word_t _bxsubi_u(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bmsr(i0, r0, r1)		_bmsr(_jit, i0, r0, r1)
static jit_word_t _bmsr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bmsi(i0, r0, i1)		_bmsi(_jit, i0, r0, i1)
static jit_word_t _bmsi(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define bmcr(i0, r0, r1)		_bmcr(_jit, i0, r0, r1)
static jit_word_t _bmcr(jit_state_t*,jit_word_t,jit_int32_t,jit_int32_t);
#  define bmci(i0, r0, i1)		_bmci(_jit, i0, r0, i1)
static jit_word_t _bmci(jit_state_t*,jit_word_t,jit_int32_t,jit_word_t);
#  define callr(r0)			_callr(_jit, r0)
static void _callr(jit_state_t*,jit_int32_t);
#  define calli(i0)			_calli(_jit, i0)
static void _calli(jit_state_t*,jit_word_t);
#  define calli_p(i0)			_calli_p(_jit, i0)
static jit_word_t _calli_p(jit_state_t*,jit_word_t);
#  define prolog(node)			_prolog(_jit, node)
static void _prolog(jit_state_t*,jit_node_t*);
#  define epilog(node)			_epilog(_jit, node)
static void _epilog(jit_state_t*,jit_node_t*);
#define patch_abs(instr, label)		_patch_abs(_jit, instr, label)
static void _patch_abs(jit_state_t*,jit_word_t,jit_word_t);
#define patch_at(jump, label)		_patch_at(_jit, jump, label)
static void _patch_at(jit_state_t*,jit_word_t,jit_word_t);
#endif

#if CODE
static void
_hrrrit(jit_state_t *_jit,jit_int32_t hc,
	jit_int32_t rs, jit_int32_t rt, jit_int32_t rd,
	jit_int32_t ic, jit_int32_t tc)
{
    jit_instr_t		i;
    i.tc.b = tc;
    i.ic.b = ic;
    i.rd.b = rd;
    i.rt.b = rt;
    i.rs.b = rs;
    i.hc.b = hc;
    ii(i.op);
}

static void
_hrri(jit_state_t *_jit, jit_int32_t hc,
      jit_int32_t rs, jit_int32_t rt, jit_int32_t im)
{
    jit_instr_t		i;
    i.op = 0;
    i.is.b = im;
    i.rt.b = rt;
    i.rs.b = rs;
    i.hc.b = hc;
    ii(i.op);
}

static void
_hi(jit_state_t *_jit, jit_int32_t hc, jit_int32_t im)
{
    jit_instr_t		i;
    i.ii.b = im;
    i.hc.b = hc;
    ii(i.op);
}

static void
_nop(jit_state_t *_jit, jit_int32_t i0)
{
    while (i0--)
	ii(0);
}

static void
_addi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (i0 == 0)
	movr(r0, r1);
    else if (can_sign_extend_short_p(i0))
	ADDIU(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	addr(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_addcr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    jit_int32_t		t0;

    if (jit_carry == _NOREG)
	jit_carry = jit_get_reg(jit_class_gpr);
    if (r0 == r1) {
	t0 = jit_get_reg(jit_class_gpr);
	ADDU(rn(t0), r1, r2);
	SLTU(rn(jit_carry), rn(t0), r1);
	movr(r0, rn(t0));
	jit_unget_reg(t0);
    }
    else {
	ADDU(r0, r1, r2);
	SLTU(rn(jit_carry), r0, r1);
    }
}

static void
_addci(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		t0;

    if (jit_carry == _NOREG)
	jit_carry = jit_get_reg(jit_class_gpr);
    t0 = jit_get_reg(jit_class_gpr);
    if (r0 == r1) {
	if (can_sign_extend_short_p(i0))
	    ADDIU(rn(t0), r1, i0);
	else {
	    movi(rn(t0), i0);
	    addr(rn(t0), r1, rn(t0));
	}
	SLTU(rn(jit_carry), rn(t0), r1);
	movr(r0, rn(t0));
    }
    else {
	if (can_sign_extend_short_p(i0))
	    ADDIU(r0, r1, i0);
	else {
	    movi(rn(t0), i0);
	    addr(r0, r1, rn(t0));
	}
	SLTU(rn(jit_carry), r0, r1);
    }
    jit_unget_reg(t0);
}

static void
_addxr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    jit_int32_t		t0;

    assert(jit_carry != _NOREG);
    t0 = jit_get_reg(jit_class_gpr);
    movr(rn(t0), rn(jit_carry));
    addcr(r0, r1, r2);
    addcr(r0, r0, rn(t0));
    jit_unget_reg(t0);
}

static void
_addxi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		t0;

    assert(jit_carry != _NOREG);
    t0 = jit_get_reg(jit_class_gpr);
    movr(rn(t0), rn(jit_carry));
    addci(r0, r1, i0);
    addcr(r0, r0, rn(t0));
    jit_unget_reg(t0);
}

static void
_subi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (i0 == 0)
	movr(r0, r1);
    else if (can_sign_extend_short_p(i0) && (i0 & 0xffff) != 0x8000)
	ADDIU(r0, r1, -i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	subr(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_subcr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    jit_int32_t		t0;

    if (jit_carry == _NOREG)
	jit_carry = jit_get_reg(jit_class_gpr);
    if (r0 == r1) {
	t0 = jit_get_reg(jit_class_gpr);
	SUBU(rn(t0), r1, r2);
	SLTU(rn(jit_carry), r1, rn(t0));
	movr(r0, rn(t0));
	jit_unget_reg(t0);
    }
    else {
	SUBU(r0, r1, r2);
	SLTU(rn(jit_carry), r1, r0);
    }
}

static void
_subci(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		t0;

    if (jit_carry == _NOREG)
	jit_carry = jit_get_reg(jit_class_gpr);
    t0 = jit_get_reg(jit_class_gpr);
    if (r0 == r1) {
	if (can_sign_extend_short_p(i0) && (i0 & 0xffff) != 0x8000)
	    ADDIU(rn(t0), r1, -i0);
	else {
	    movi(rn(t0), i0);
	    subr(rn(t0), r1, rn(t0));
	}
	SLTU(rn(jit_carry), r1, rn(t0));
	movr(r0, rn(t0));
    }
    else {
	if (can_sign_extend_short_p(i0) && (i0 & 0xffff) != 0x8000)
	    ADDIU(r0, r1, -i0);
	else {
	    movi(rn(t0), i0);
	    subr(r0, r1, rn(t0));
	}
	SLTU(rn(jit_carry), r1, r0);
    }
    jit_unget_reg(t0);
}

static void
_subxr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    jit_int32_t		t0;

    assert(jit_carry != _NOREG);
    t0 = jit_get_reg(jit_class_gpr);
    movr(rn(t0), rn(jit_carry));
    subcr(r0, r1, r2);
    subcr(r0, r0, rn(t0));
    jit_unget_reg(t0);
}

static void
_subxi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		t0;

    assert(jit_carry != _NOREG);
    t0 = jit_get_reg(jit_class_gpr);
    movr(rn(t0), rn(jit_carry));
    subci(r0, r1, i0);
    subcr(r0, r0, rn(t0));
    jit_unget_reg(t0);
}

static void
_mulr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    MULTU(r1, r2);
    MFLO(r0);
}

static void
_muli(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    mulr(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_divr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    DIV(r1, r2);
    MFLO(r0);
}

static void
_divi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    divr(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_divr_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    DIVU(r1, r2);
    MFLO(r0);
}

static void
_divi_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    divr_u(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_remr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    DIV(r1, r2);
    MFHI(r0);
}

static void
_remi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    remr(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_remr_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    DIVU(r1, r2);
    MFHI(r0);
}

static void
_remi_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    remr_u(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_andi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_zero_extend_short_p(i0))
	ANDI(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	AND(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ori(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_zero_extend_short_p(i0))
	ORI(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	OR(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_xori(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_zero_extend_short_p(i0))
	XORI(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	XOR(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_movi(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    if (i0 == 0)
	OR(r0, _ZERO_REGNO, _ZERO_REGNO);
    else if (can_sign_extend_short_p(i0))
	ADDIU(r0, _ZERO_REGNO, i0);
    else if (can_zero_extend_short_p(i0))
	ORI(r0, _ZERO_REGNO, i0);
    else {
	if (can_sign_extend_int_p(i0))
	    LUI(r0, i0 >> 16);
	else if (can_zero_extend_int_p(i0)) {
	    if (i0 & 0xffff0000) {
		ORI(r0, r0, i0 >> 16);
		SLL(r0, r0, 16);
	    }
	}
#  if __WORDSIZE == 64
	else {
	    movi(r0, (jit_uword_t)i0 >> 32);
	    if (i0 & 0xffff0000) {
		SLL(r0, r0, 16);
		ORI(r0, r0, i0 >> 16);
		SLL(r0, r0, 16);
	    }
	    else
		SLL(r0, r0, 32);
	}
#  endif
	if (i0 & 0xffff)
	    ORI(r0, r0, i0);
    }
}

static jit_word_t
_movi_p(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_word_t		w;

    w = _jit->pc.w;
#  if __WORDSIZE == 32
    LUI(r0, i0 >> 16);
    ORI(r0, r0, i0);
#  else
    LUI(r0, i0 >> 48);
    ORI(r0, r0, i0 >> 32);
    SLL(r0, r0, 16);
    ORI(r0, r0, i0 >> 16);
    SLL(r0, r0, 16);
    ORI(r0, r0, i0);
#  endif

    return (w);
}

static void
_ldi_c(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LB(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_c(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldi_uc(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LBU(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_uc(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldi_s(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LH(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_s(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldi_us(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LHU(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_us(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldi_i(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LW(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_i(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

#if __WORDSIZE == 64
static void
_ldi_ui(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LWU(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_ui(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldi_l(jit_state_t *_jit, jit_int32_t r0, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LD(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ldr_l(r0, rn(reg));
	jit_unget_reg(reg);
    }
}
#endif

static void
_ldxr_c(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_c(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_c(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LB(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_c(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldxr_uc(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_uc(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_uc(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LBU(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_uc(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldxr_s(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_s(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_s(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LH(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_s(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldxr_us(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_us(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_us(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LHU(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_us(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldxr_i(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_i(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_i(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LW(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_i(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

#if __WORDSIZE == 64
static void
_ldxr_ui(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_ui(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_ui(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LWU(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_ui(r0, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ldxr_l(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    ldr_l(r0, rn(reg));
    jit_unget_reg(reg);
}

static void
_ldxi_l(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	LD(r0, i0, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r1, i0);
	ldr_l(r0, rn(reg));
	jit_unget_reg(reg);
    }
}
#endif

static void
_sti_c(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SB(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	str_c(rn(reg), r0);
	jit_unget_reg(reg);
    }
}

static void
_sti_s(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SH(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	str_s(rn(reg), r0);
	jit_unget_reg(reg);
    }
}

static void
_sti_i(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SW(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	str_i(rn(reg), r0);
	jit_unget_reg(reg);
    }
}

#if __WORDSIZE == 64
static void
_sti_l(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SD(r0, i0, _ZERO_REGNO);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	str_l(rn(reg), r0);
	jit_unget_reg(reg);
    }
}
#endif

static void
_stxr_c(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r0, r1);
    str_c(rn(reg), r2);
    jit_unget_reg(reg);
}

static void
_stxi_c(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SB(r1, i0, r0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r0, i0);
	str_c(rn(reg), r1);
	jit_unget_reg(reg);
    }
}

static void
_stxr_s(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r0, r1);
    str_s(rn(reg), r2);
    jit_unget_reg(reg);
}

static void
_stxi_s(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SH(r1, i0, r0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r0, i0);
	str_s(rn(reg), r1);
	jit_unget_reg(reg);
    }
}

static void
_stxr_i(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r0, r1);
    str_i(rn(reg), r2);
    jit_unget_reg(reg);
}

static void
_stxi_i(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SW(r1, i0, r0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r0, i0);
	str_i(rn(reg), r1);
	jit_unget_reg(reg);
    }
}

#if __WORDSIZE == 64
static void
_stxr_l(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1 ,jit_int32_t r2)
{
    jit_int32_t		reg;
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r0, r1);
    str_l(rn(reg), r2);
    jit_unget_reg(reg);
}

static void
_stxi_l(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_int32_t		reg;
    if (can_sign_extend_short_p(i0))
	SD(r1, i0, r0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	addi(rn(reg), r0, i0);
	str_l(rn(reg), r1);
	jit_unget_reg(reg);
    }
}
#endif

#  if __BYTE_ORDER == __LITTLE_ENDIAN
static void
_htonr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1)
{
    jit_int32_t		rg0;
    jit_int32_t		rg1;
    if (jit_mips2_p()) {
	WSBH(r0, r1);
	ROTR(r0, r0, 16);
    }
    else {
	/* FIXME rewrite in a more sane way, but unlikely to be used
	 * in near time... */
	rg0 = jit_get_reg(jit_class_gpr);
	rg1 = jit_get_reg(jit_class_gpr);
	LUI(rn(rg0), 0xff00);
	ORI(rn(rg0), rn(rg0), 0xff00);
	AND(rn(rg1), r1, rn(rg0));
	SRL(rn(rg0), rn(rg0), 8);
	AND(rn(rg0), r1, rn(rg0));
	SRL(rn(rg1), rn(rg1), 8);
	SLL(rn(rg0), rn(rg0), 8);
	OR(r0, rn(rg0), rn(rg1));
	ANDI(rn(rg1), r0, 0xffff);
	LUI(rn(rg0), 0xffff);
	AND(rn(rg0), r0, rn(rg0));
	SLL(rn(rg1), rn(rg1), 16);
	SRL(rn(rg0), rn(rg0), 16);
	OR(r0, rn(rg0), rn(rg1));
	jit_unget_reg(rg0);
	jit_unget_reg(rg1);
    }
}
#  endif

static void
_extr_c(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1)
{
    if (jit_mips2_p())
	SEB(r0, r1);
    else {
	SLL(r0, r1, 24);
	SRA(r0, r0, 24);
    }
}

static void
_extr_s(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1)
{
    if (jit_mips2_p())
	SEH(r0, r1);
    else {
	SLL(r0, r1, 16);
	SRA(r0, r0, 16);
    }
}

#  if __WORDSIZE == 64
static void
_extr_ui(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1)
{
    movr(r0, r1);
    INS(r0, _RZERO_REGNO, 32, 32);
}
#  endif

static void
_lti(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (can_sign_extend_short_p(i0))
	SLTI(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ltr(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_lti_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (can_sign_extend_short_p(i0))
	SLTIU(r0, r1, i0);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ltr_u(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ler(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    SLT(r0, r2, r1);
    XORI(r0, r0, 1);
}

static void
_lei(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (i0 == 0) {
	SLT(r0, _ZERO_REGNO, r1);
	XORI(r0, r0, 1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ler(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_ler_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    SLTU(r0, r2, r1);
    XORI(r0, r0, 1);
}

static void
_lei_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (i0 == 0) {
	SLTU(r0, _ZERO_REGNO, r1);
	XORI(r0, r0, 1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	ler_u(r0, r1, rn(reg));
	jit_unget_reg(reg);
    }
}

static void
_eqr(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    subr(r0, r1, r2);
    SLTU(r0, _ZERO_REGNO, r0);
    XORI(r0, r0, 1);
}

static void
_eqi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    if (i0) {
	subi(r0, r1, i0);
	SLTU(r0, _ZERO_REGNO, r0);
    }
    else
	SLTU(r0, _ZERO_REGNO, r1);
    XORI(r0, r0, 1);
}

static void
_ger(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    SLT(r0, r1, r2);
    XORI(r0, r0, 1);
}

static void
_gei(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    ger(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_ger_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    SLTU(r0, r1, r2);
    XORI(r0, r0, 1);
}

static void
_gei_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    ger_u(r0, r1, rn(reg));
    jit_unget_reg(reg);
}

static void
_gti(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (i0 == 0)
	SLT(r0, _ZERO_REGNO, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	SLT(r0, rn(reg), r1);
	jit_unget_reg(reg);
    }
}

static void
_gti_u(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    jit_int32_t		reg;

    if (i0 == 0)
	SLTU(r0, _ZERO_REGNO, r1);
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i0);
	SLTU(r0, rn(reg), r1);
	jit_unget_reg(reg);
    }
}

static void
_ner(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_int32_t r2)
{
    subr(r0, r1, r2);
    SLTU(r0, _ZERO_REGNO, r0);
}

static void
_nei(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    if (i0) {
	subi(r0, r1, i0);
	SLTU(r0, _ZERO_REGNO, r0);
    }
    else
	SLTU(r0, _ZERO_REGNO, r1);
}

static jit_word_t
_bltr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLT(rn(reg), r0, r1);
    w = _jit->pc.w;
    BNE(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bltr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLTU(rn(reg), r0, r1);
    w = _jit->pc.w;
    BNE(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_blti(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_word_t		d;
    jit_int32_t		reg;
    jit_bool_t		zero_p;

    if (!(zero_p = i1 == 0))
	reg = jit_get_reg(jit_class_gpr);
    if (can_sign_extend_short_p(i1)) {
	if (!zero_p)
	    SLTI(rn(reg), r0, i1);
	w = _jit->pc.w;
	d = ((i0 - w) >> 2) - 1;
	if (!zero_p)
	    BNE(rn(reg), _ZERO_REGNO, d);
	else
	    BLTZ(r0, d);
	NOP(1);
    }
    else {
	movi(rn(reg), i1);
	w = bltr(i0, r0, rn(reg));
    }
    if (!zero_p)
	jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_blti_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    if (can_sign_extend_short_p(i1)) {
	SLTIU(rn(reg), r0, i1);
	w = _jit->pc.w;
	BNE(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	movi(rn(reg), i1);
	w = bltr_u(i0, r0, rn(reg));
    }
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bler(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLT(rn(reg), r1, r0);
    w = _jit->pc.w;
    BEQ(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bler_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLTU(rn(reg), r1, r0);
    w = _jit->pc.w;
    BEQ(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_blei(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BLEZ(r0, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = bler(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_blei_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BEQ(r0, _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = bler_u(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_beqr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;

    w = _jit->pc.w;
    BEQ(r0, r1, ((i0 - w) >> 2) - 1);
    NOP(1);

    return (w);
}

static jit_word_t
_beqi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BEQ(r0, _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = beqr(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_bger(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLT(rn(reg), r0, r1);
    w = _jit->pc.w;
    BEQ(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bger_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLTU(rn(reg), r0, r1);
    w = _jit->pc.w;
    BEQ(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bgei(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_word_t		d;
    jit_int32_t		reg;
    jit_bool_t		zero_p;

    if (!(zero_p = i1 == 0))
	reg = jit_get_reg(jit_class_gpr);
    if (can_sign_extend_short_p(i1)) {
	if (!zero_p)
	    SLTI(rn(reg), r0, i1);
	w = _jit->pc.w;
	d = ((i0 - w) >> 2) - 1;
	if (!zero_p)
	    BEQ(rn(reg), _ZERO_REGNO, d);
	else
	    BGEZ(r0, d);
	NOP(1);
    }
    else {
	movi(rn(reg), i1);
	w = bger(i0, r0, rn(reg));
    }
    if (!zero_p)
	jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bgei_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    if (can_sign_extend_short_p(i1)) {
	SLTIU(rn(reg), r0, i1);
	w = _jit->pc.w;
	BEQ(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	movi(rn(reg), i1);
	w = bger_u(i0, r0, rn(reg));
    }
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bgtr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLT(rn(reg), r1, r0);
    w = _jit->pc.w;
    BNE(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bgtr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    reg = jit_get_reg(jit_class_gpr);
    SLTU(rn(reg), r1, r0);
    w = _jit->pc.w;
    BNE(rn(reg), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(reg);

    return (w);
}

static jit_word_t
_bgti(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BGTZ(r0, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = bgtr(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_bgti_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BNE(r0, _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = bgtr_u(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_bner(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;

    w = _jit->pc.w;
    BNE(r0, r1, ((i0 - w) >> 2) - 1);
    NOP(1);

    return (w);
}

static jit_word_t
_bnei(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		reg;

    if (i1 == 0) {
	w = _jit->pc.w;
	BNE(r0, _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi(rn(reg), i1);
	w = bner(i0, r0, rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static void
_jmpr(jit_state_t *_jit, jit_int32_t r0)
{
    JR(r0);
    NOP(1);
}

static jit_word_t
_jmpi(jit_state_t *_jit, jit_word_t i0)
{
    jit_word_t		w;
    jit_int32_t		reg;

    w = _jit->pc.w;
    if (((w + sizeof(jit_int32_t)) & 0xf0000000) == (i0 & 0xf0000000)) {
	J((i0 & ~0xf0000000) >> 2);
	NOP(1);
    }
    else {
	reg = jit_get_reg(jit_class_gpr);
	movi_p(rn(reg), i0);
	jmpr(rn(reg));
	jit_unget_reg(reg);
    }

    return (w);
}

static jit_word_t
_boaddr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    /* t1 = r0 + r1;	overflow = r1 < 0 ? r0 < t1 : t1 < r0 */
    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    t2 = jit_get_reg(jit_class_gpr);
    SLT(rn(t0), r1, _ZERO_REGNO);	/* t0 = r1 < 0 */
    ADDU(rn(t1), r0, r1);		/* t1 = r0 + r1 */
    SLT(rn(t2), rn(t1), r0);		/* t2 = t1 < r0 */
    SLT(rn(t1), r0, rn(t1));		/* t1 = r0 < t1 */
    MOVZ(rn(t1), rn(t2), rn(t0));	/* if (r0 == 0) t1 = t2 */
    w = _jit->pc.w;
    BNE(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    /* delay slot */
    ADDU(r0, r0, r1);
    jit_unget_reg(t2);
    jit_unget_reg(t1);
    jit_unget_reg(t0);

    return (w);
}

static jit_word_t
_boaddi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    if (can_sign_extend_short_p(i1)) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	t2 = jit_get_reg(jit_class_gpr);
	SLTI(rn(t0), _ZERO_REGNO, i1);
	ADDIU(rn(t1), r0, i1);
	SLT(rn(t2), r0, rn(t1));
	SLT(rn(t1), rn(t1), r0);
	MOVZ(rn(t1), rn(t2), rn(t0));
	w = _jit->pc.w;
	BNE(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	/* delay slot */
	ADDIU(r0, r0, i1);
	jit_unget_reg(t2);
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = boaddr(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_boaddr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    ADDU(rn(t0), r0, r1);
    SLTU(rn(t1), rn(t0), r0);
    w = _jit->pc.w;
    BNE(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
    /* delay slot */
    movr(r0, rn(t0));
    jit_unget_reg(t1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_boaddi_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    if (can_sign_extend_short_p(i0)) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	ADDIU(rn(t0), r0, i1);
	SLTU(rn(t1), rn(t0), r0);
	w = _jit->pc.w;
	BNE(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
	/* delay slot */
	movr(r0, rn(t0));
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = boaddr_u(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bxaddr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    /* t1 = r0 + r1;	overflow = r1 < 0 ? r0 < t1 : t1 < r0 */
    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    t2 = jit_get_reg(jit_class_gpr);
    SLT(rn(t0), r1, _ZERO_REGNO);	/* t0 = r1 < 0 */
    ADDU(rn(t1), r0, r1);		/* t1 = r0 + r1 */
    SLT(rn(t2), rn(t1), r0);		/* t2 = t1 < r0 */
    SLT(rn(t1), r0, rn(t1));		/* t1 = r0 < t1 */
    MOVZ(rn(t1), rn(t2), rn(t0));	/* if (r0 == 0) t1 = t2 */
    w = _jit->pc.w;
    BEQ(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    /* delay slot */
    ADDU(r0, r0, r1);
    jit_unget_reg(t2);
    jit_unget_reg(t1);
    jit_unget_reg(t0);

    return (w);
}

static jit_word_t
_bxaddi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    if (can_sign_extend_short_p(i1)) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	t2 = jit_get_reg(jit_class_gpr);
	SLTI(rn(t0), _ZERO_REGNO, i1);
	ADDIU(rn(t1), r0, i1);
	SLT(rn(t2), r0, rn(t1));
	SLT(rn(t1), rn(t1), r0);
	MOVZ(rn(t1), rn(t2), rn(t0));
	w = _jit->pc.w;
	BEQ(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	/* delay slot */
	ADDIU(r0, r0, i1);
	jit_unget_reg(t2);
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bxaddr(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bxaddr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    ADDU(rn(t0), r0, r1);
    SLTU(rn(t1), rn(t0), r0);
    w = _jit->pc.w;
    BEQ(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
    /* delay slot */
    movr(r0, rn(t0));
    jit_unget_reg(t1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bxaddi_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    if (can_sign_extend_short_p(i0)) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	ADDIU(rn(t0), r0, i1);
	SLTU(rn(t1), rn(t0), r0);
	w = _jit->pc.w;
	BEQ(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
	/* delay slot */
	movr(r0, rn(t0));
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bxaddr_u(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bosubr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    /* t1 = r0 - r1;	overflow = 0 < r1 ? r0 < t1 : t1 < r0 */
    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    t2 = jit_get_reg(jit_class_gpr);
    SLT(rn(t0), _ZERO_REGNO, r1);	/* t0 = 0 < r1 */
    SUBU(rn(t1), r0, r1);		/* t1 = r0 - r1 */
    SLT(rn(t2), rn(t1), r0);		/* t2 = t1 < r0 */
    SLT(rn(t1), r0, rn(t1));		/* t1 = r0 < t1 */
    MOVZ(rn(t1), rn(t2), rn(t0));	/* if (r0 == 0) t1 = t2 */
    w = _jit->pc.w;
    BNE(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    /* delay slot */
    SUBU(r0, r0, r1);
    jit_unget_reg(t2);
    jit_unget_reg(t1);
    jit_unget_reg(t0);

    return (w);
}

static jit_word_t
_bosubi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    if (can_sign_extend_short_p(i1) && (i1 & 0xffff) != 0x8000) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	t2 = jit_get_reg(jit_class_gpr);
	SLTI(rn(t0), _ZERO_REGNO, i1);
	ADDIU(rn(t1), r0, -i1);
	SLT(rn(t2), rn(t1), r0);
	SLT(rn(t1), r0, rn(t1));
	MOVZ(rn(t1), rn(t2), rn(t0));
	w = _jit->pc.w;
	BNE(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	/* delay slot */
	ADDIU(r0, r0, -i1);
	jit_unget_reg(t2);
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bosubr(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bosubr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    SUBU(rn(t0), r0, r1);
    SLTU(rn(t1), r0, rn(t0));
    w = _jit->pc.w;
    BNE(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
    /* delay slot */
    movr(r0, rn(t0));
    jit_unget_reg(t1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bosubi_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    if (can_sign_extend_short_p(i0) && (i0 & 0xffff) != 0x8000) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	ADDIU(rn(t0), r0, -i1);
	SLTU(rn(t1), r0, rn(t0));
	w = _jit->pc.w;
	BNE(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
	/* delay slot */
	movr(r0, rn(t0));
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bosubr_u(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bxsubr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    /* t1 = r0 - r1;	overflow = 0 < r1 ? r0 < t1 : t1 < r0 */
    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    t2 = jit_get_reg(jit_class_gpr);
    SLT(rn(t0), _ZERO_REGNO, r1);	/* t0 = 0 < r1 */
    SUBU(rn(t1), r0, r1);		/* t1 = r0 - r1 */
    SLT(rn(t2), rn(t1), r0);		/* t2 = t1 < r0 */
    SLT(rn(t1), r0, rn(t1));		/* t1 = r0 < t1 */
    MOVZ(rn(t1), rn(t2), rn(t0));	/* if (r0 == 0) t1 = t2 */
    w = _jit->pc.w;
    BEQ(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
    /* delay slot */
    SUBU(r0, r0, r1);
    jit_unget_reg(t2);
    jit_unget_reg(t1);
    jit_unget_reg(t0);

    return (w);
}

static jit_word_t
_bxsubi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;
    jit_int32_t		t2;

    if (can_sign_extend_short_p(i1) && (i1 & 0xffff) != 0x8000) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	t2 = jit_get_reg(jit_class_gpr);
	SLTI(rn(t0), _ZERO_REGNO, i1);
	ADDIU(rn(t1), r0, -i1);
	SLT(rn(t2), rn(t1), r0);
	SLT(rn(t1), r0, rn(t1));
	MOVZ(rn(t1), rn(t2), rn(t0));
	w = _jit->pc.w;
	BEQ(rn(t1), _ZERO_REGNO, ((i0 - w) >> 2) - 1);
	/* delay slot */
	ADDIU(r0, r0, -i1);
	jit_unget_reg(t2);
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bxsubr(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bxsubr_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    t0 = jit_get_reg(jit_class_gpr);
    t1 = jit_get_reg(jit_class_gpr);
    SUBU(rn(t0), r0, r1);
    SLTU(rn(t1), r0, rn(t0));
    w = _jit->pc.w;
    BEQ(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
    /* delay slot */
    movr(r0, rn(t0));
    jit_unget_reg(t1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bxsubi_u(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    jit_int32_t		t1;

    if (can_sign_extend_short_p(i0) && (i0 & 0xffff) != 0x8000) {
	t0 = jit_get_reg(jit_class_gpr);
	t1 = jit_get_reg(jit_class_gpr);
	ADDIU(rn(t0), r0, -i1);
	SLTU(rn(t1), r0, rn(t0));
	w = _jit->pc.w;
	BEQ(_ZERO_REGNO, rn(t1), ((i0 - w) >> 2) - 1);
	/* delay slot */
	movr(r0, rn(t0));
	jit_unget_reg(t1);
	jit_unget_reg(t0);
    }
    else {
	t0 = jit_get_reg(jit_class_gpr);
	movi(rn(t0), i1);
	w = bxsubr_u(i0, r0, rn(t0));
	jit_unget_reg(t0);
    }
    return (w);
}

static jit_word_t
_bmsr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    t0 = jit_get_reg(jit_class_gpr);
    AND(rn(r0), r0, r1);
    w = _jit->pc.w;
    BNE(_ZERO_REGNO, rn(t0), ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bmsi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    t0 = jit_get_reg(jit_class_gpr);
    if (can_zero_extend_short_p(i1)) {
	ANDI(rn(t0), r0, i1);
	w = _jit->pc.w;
	BNE(_ZERO_REGNO, rn(t0), ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	movi(rn(t0), i1);
	w = bmsr(i0, r0, rn(t0));
    }
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bmcr(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    t0 = jit_get_reg(jit_class_gpr);
    AND(rn(t0), r0, r1);
    w = _jit->pc.w;
    BEQ(_ZERO_REGNO, rn(t0), ((i0 - w) >> 2) - 1);
    NOP(1);
    jit_unget_reg(t0);
    return (w);
}

static jit_word_t
_bmci(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_word_t i1)
{
    jit_word_t		w;
    jit_int32_t		t0;
    t0 = jit_get_reg(jit_class_gpr);
    if (can_zero_extend_short_p(i1)) {
	ANDI(rn(t0), r0, i1);
	w = _jit->pc.w;
	BEQ(_ZERO_REGNO, rn(t0), ((i0 - w) >> 2) - 1);
	NOP(1);
    }
    else {
	movi(rn(t0), i1);
	w = bmcr(i0, r0, rn(t0));
    }
    jit_unget_reg(t0);
    return (w);
}

static void
_callr(jit_state_t *_jit, jit_int32_t r0)
{
    if (r0 != _T9_REGNO)
	movr(_T9_REGNO, r0);
    JALR(r0);
    NOP(1);
}

static void
_calli(jit_state_t *_jit, jit_word_t i0)
{
    movi(_T9_REGNO, i0);
    JALR(_T9_REGNO);
    NOP(1);
}

static jit_word_t
_calli_p(jit_state_t *_jit, jit_word_t i0)
{
    jit_word_t		word;

    word = _jit->pc.w;
    movi_p(_T9_REGNO, i0);
    JALR(_T9_REGNO);
    NOP(1);

    return (word);
}

static void
_prolog(jit_state_t *_jit, jit_node_t *node)
{
    _jit->function->stack = ((/* first 16 bytes must be allocated */
			      (_jit->function->self.alen > 16 ?
			       _jit->function->self.alen : 16) -
			      /* align stack at 8 bytes */
			      _jit->function->self.aoff) + 7) & -8;
    /* callee save registers */
    subi(_SP_REGNO, _SP_REGNO, stack_framesize);
#if __WORDSIZE == 32
    if (jit_regset_tstbit(_jit->function->regset, _F30))
	stxi_d(96, _SP_REGNO, _F30_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F28))
	stxi_d(88, _SP_REGNO, _F28_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F26))
	stxi_d(80, _SP_REGNO, _F26_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F24))
	stxi_d(72, _SP_REGNO, _F24_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F22))
	stxi_d(64, _SP_REGNO, _F22_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F20))
	stxi_d(56, _SP_REGNO, _F20_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F18))
	stxi_d(48, _SP_REGNO, _F18_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F16))
	stxi_d(40, _SP_REGNO, _F16_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S7))
	stxi(36, _SP_REGNO, _S7_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S6))
	stxi(32, _SP_REGNO, _S6_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S5))
	stxi(28, _SP_REGNO, _S5_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S4))
	stxi(24, _SP_REGNO, _S4_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S3))
	stxi(20, _SP_REGNO, _S3_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S2))
	stxi(16, _SP_REGNO, _S2_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S1))
	stxi(12, _SP_REGNO, _S1_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S0))
	stxi( 8, _SP_REGNO, _S0_REGNO);
    stxi( 4, _SP_REGNO, _RA_REGNO);
#else
    if (jit_regset_tstbit(_jit->function->regset, _F30))
	stxi_d(136, _SP_REGNO, _F30_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F28))
	stxi_d(128, _SP_REGNO, _F28_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F26))
	stxi_d(120, _SP_REGNO, _F26_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F24))
	stxi_d(112, _SP_REGNO, _F24_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F22))
	stxi_d(104, _SP_REGNO, _F22_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F20))
	stxi_d(96, _SP_REGNO, _F20_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F18))
	stxi_d(88, _SP_REGNO, _F18_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _F16))
	stxi_d(80, _SP_REGNO, _F16_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S7))
	stxi(72, _SP_REGNO, _S7_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S6))
	stxi(64, _SP_REGNO, _S6_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S5))
	stxi(56, _SP_REGNO, _S5_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S4))
	stxi(48, _SP_REGNO, _S4_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S3))
	stxi(40, _SP_REGNO, _S3_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S2))
	stxi(32, _SP_REGNO, _S2_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S1))
	stxi(24, _SP_REGNO, _S1_REGNO);
    if (jit_regset_tstbit(_jit->function->regset, _S0))
	stxi(16, _SP_REGNO, _S0_REGNO);
    stxi( 8, _SP_REGNO, _RA_REGNO);
#endif
    stxi(0, _SP_REGNO, _BP_REGNO);
    movr(_BP_REGNO, _SP_REGNO);

    /* alloca */
    subi(_SP_REGNO, _SP_REGNO, _jit->function->stack);
}

static void
_epilog(jit_state_t *_jit, jit_node_t *node)
{
    /* callee save registers */
    movr(_SP_REGNO, _BP_REGNO);
#if __WORDSIZE == 32
    if (jit_regset_tstbit(_jit->function->regset, _F30))
	ldxi_d(_F30_REGNO, _SP_REGNO, 96);
    if (jit_regset_tstbit(_jit->function->regset, _F28))
	ldxi_d(_F28_REGNO, _SP_REGNO, 88);
    if (jit_regset_tstbit(_jit->function->regset, _F26))
	ldxi_d(_F26_REGNO, _SP_REGNO, 80);
    if (jit_regset_tstbit(_jit->function->regset, _F24))
	ldxi_d(_F24_REGNO, _SP_REGNO, 72);
    if (jit_regset_tstbit(_jit->function->regset, _F22))
	ldxi_d(_F22_REGNO, _SP_REGNO, 64);
    if (jit_regset_tstbit(_jit->function->regset, _F20))
	ldxi_d(_F20_REGNO, _SP_REGNO, 56);
    if (jit_regset_tstbit(_jit->function->regset, _F18))
	ldxi_d(_F18_REGNO, _SP_REGNO, 48);
    if (jit_regset_tstbit(_jit->function->regset, _F16))
	ldxi_d(_F16_REGNO, _SP_REGNO, 40);
    if (jit_regset_tstbit(_jit->function->regset, _S7))
	ldxi(_S7_REGNO, _SP_REGNO, 36);
    if (jit_regset_tstbit(_jit->function->regset, _S6))
	ldxi(_S6_REGNO, _SP_REGNO, 32);
    if (jit_regset_tstbit(_jit->function->regset, _S5))
	ldxi(_S5_REGNO, _SP_REGNO, 28);
    if (jit_regset_tstbit(_jit->function->regset, _S4))
	ldxi(_S4_REGNO, _SP_REGNO, 24);
    if (jit_regset_tstbit(_jit->function->regset, _S3))
	ldxi(_S3_REGNO, _SP_REGNO, 20);
    if (jit_regset_tstbit(_jit->function->regset, _S2))
	ldxi(_S2_REGNO, _SP_REGNO, 16);
    if (jit_regset_tstbit(_jit->function->regset, _S1))
	ldxi(_S1_REGNO, _SP_REGNO, 12);
    if (jit_regset_tstbit(_jit->function->regset, _S0))
	ldxi(_S0_REGNO, _SP_REGNO, 8);
    ldxi(_RA_REGNO, _SP_REGNO, 4);
#else
    if (jit_regset_tstbit(_jit->function->regset, _F30))
	ldxi_d(_F30_REGNO, _SP_REGNO, 136);
    if (jit_regset_tstbit(_jit->function->regset, _F28))
	ldxi_d(_F28_REGNO, _SP_REGNO, 128);
    if (jit_regset_tstbit(_jit->function->regset, _F26))
	ldxi_d(_F26_REGNO, _SP_REGNO, 120);
    if (jit_regset_tstbit(_jit->function->regset, _F24))
	ldxi_d(_F24_REGNO, _SP_REGNO, 112);
    if (jit_regset_tstbit(_jit->function->regset, _F22))
	ldxi_d(_F22_REGNO, _SP_REGNO, 104);
    if (jit_regset_tstbit(_jit->function->regset, _F20))
	ldxi_d(_F20_REGNO, _SP_REGNO, 96);
    if (jit_regset_tstbit(_jit->function->regset, _F18))
	ldxi_d(_F18_REGNO, _SP_REGNO, 88);
    if (jit_regset_tstbit(_jit->function->regset, _F16))
	ldxi_d(_F16_REGNO, _SP_REGNO, 80);
    if (jit_regset_tstbit(_jit->function->regset, _S7))
	ldxi(_S7_REGNO, _SP_REGNO, 72);
    if (jit_regset_tstbit(_jit->function->regset, _S6))
	ldxi(_S6_REGNO, _SP_REGNO, 64);
    if (jit_regset_tstbit(_jit->function->regset, _S5))
	ldxi(_S5_REGNO, _SP_REGNO, 56);
    if (jit_regset_tstbit(_jit->function->regset, _S4))
	ldxi(_S4_REGNO, _SP_REGNO, 48);
    if (jit_regset_tstbit(_jit->function->regset, _S3))
	ldxi(_S3_REGNO, _SP_REGNO, 40);
    if (jit_regset_tstbit(_jit->function->regset, _S2))
	ldxi(_S2_REGNO, _SP_REGNO, 32);
    if (jit_regset_tstbit(_jit->function->regset, _S1))
	ldxi(_S1_REGNO, _SP_REGNO, 24);
    if (jit_regset_tstbit(_jit->function->regset, _S0))
	ldxi(_S0_REGNO, _SP_REGNO, 16);
    ldxi(_RA_REGNO, _SP_REGNO, 8);
#endif
    ldxi(_BP_REGNO, _SP_REGNO, 0);
    JR(_RA_REGNO);
    /* delay slot */
    addi(_SP_REGNO, _SP_REGNO, stack_framesize);
}

static void
_patch_abs(jit_state_t *_jit, jit_word_t instr, jit_word_t label)
{
    jit_instr_t		i;
    union {
	jit_int32_t	*i;
	jit_word_t	 w;
    } u;

    u.w = instr;
    i.op = u.i[0];
    assert(i.hc.b == MIPS_LUI);
    i.is.b = label >> 16;
    u.i[0] = i.op;
    i.op = u.i[1];
    assert(i.hc.b == MIPS_ORI);
    i.is.b = label;
    u.i[1] = i.op;
}

static void
_patch_at(jit_state_t *_jit, jit_word_t instr, jit_word_t label)
{
    jit_instr_t		i;
    union {
	jit_int32_t	*i;
	jit_word_t	 w;
    } u;

    u.w = instr;
    i.op = u.i[0];
    switch (i.hc.b) {
	/* 16 bit immediate opcodes */
	case MIPS_REGIMM:
	    switch (i.rt.b) {
		case MIPS_BLTZ:		case MIPS_BLTZL:
		case MIPS_BLTZAL:	case MIPS_BLTZALL:
		case MIPS_BGEZ:		case MIPS_BGEZAL:
		case MIPS_BGEZALL:	case MIPS_BGEZL:
		case MIPS_TEQI:		case MIPS_TGEI:
		case MIPS_TGEIU:	case MIPS_TLTI:
		case MIPS_TLTIU:	case MIPS_TNEI:
		    i.is.b = ((label - instr) >> 2) - 1;
		    u.i[0] = i.op;
		    break;
		default:
		    assert(!"unhandled branch opcode");
		    break;
	    }
	    break;

	case MIPS_COP1:			case MIPS_COP2:
	    assert(i.rs.b == MIPS_BC);
	    switch (i.rt.b) {
		case MIPS_BCF:		case MIPS_BCFL:
		case MIPS_BCT:		case MIPS_BCTL:
		    i.is.b = ((label - instr) >> 2) - 1;
		    u.i[0] = i.op;
		    break;
		default:
		    assert(!"unhandled branch opcode");
		    break;
	    }
	    break;

	case MIPS_BLEZ:			case MIPS_BLEZL:
	case MIPS_BEQ:			case MIPS_BEQL:
	case MIPS_BGTZ:			case MIPS_BGTZL:
	case MIPS_BNE:			case MIPS_BNEL:
	    i.is.b = ((label - instr) >> 2) - 1;
	    u.i[0] = i.op;
	    break;

	case MIPS_LUI:
	    patch_abs(instr, label);
	    break;

	case MIPS_J:			case MIPS_JAL:
	case MIPS_JALX:
	    assert(((instr + sizeof(jit_int32_t)) & 0xf0000000) ==
		   (label & 0xf0000000));
	    i.ii.b = (label & ~0xf0000000) >> 2;
	    u.i[0] = i.op;
	    break;

	default:
	    assert(!"unhandled branch opcode");
	    break;
    }
}
#endif
