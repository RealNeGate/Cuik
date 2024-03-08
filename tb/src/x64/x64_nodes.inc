// general integer binops (GPR ins, GPR outs)
X(add) X(or)  X(and) X(sub)
X(xor) X(cmp) X(mov) X(test)
// general integer binops with immediate
X(addimm) X(orimm)  X(andimm) X(subimm)
X(xorimm) X(cmpimm) X(movimm) X(testimm)
// shift ops with immediates
X(shlimm) X(shrimm) X(sarimm) X(rolimm) X(rorimm)
// misc
X(lea) X(cmovcc) X(imulimm)
// casts
X(movsx) X(movzx)
// simple-high level ops
X(static_call)
X(call)
#undef X
