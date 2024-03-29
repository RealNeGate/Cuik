// general integer binops (GPR ins, GPR outs)
X(add) X(or)  X(and) X(sub)
X(xor) X(cmp) X(mov) X(test)
// general integer binops with immediate
X(addimm) X(orimm)  X(andimm) X(subimm)
X(xorimm) X(cmpimm) X(movimm) X(testimm)
// shift ops with immediates
X(shlimm) X(shrimm) X(sarimm) X(rolimm) X(rorimm)
// misc
X(lea) X(setcc) X(cmovcc) X(imulimm)
// casts
X(movsx8)
X(movzx8)
X(movsx16)
X(movzx16)
X(movsx32)
// float/vector ops
X(vmov) X(vadd) X(vmul) X(vsub) X(vdiv)
X(vmin) X(vmax) X(vxor)
// scalar only float ops
X(ucomi)
// simple-high level ops
X(static_call)
X(call)
#undef X
