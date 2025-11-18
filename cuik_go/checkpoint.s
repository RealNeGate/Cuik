default rel

global checkpoint_handler, lvb_handler
extern c_checkpoint, c_lvb, gc_mid_reloc

section .text
lvb_handler:
  ; if we're mid_reloc, that means not all threads
  ; have acknowledged the phase shift which means
  ; we can't start moving values just yet.
  cmp qword [gc_mid_reloc], 0
  jne .early_out
  ; save GPRs
  push rdx
  push rbx
  push rbp
  push rsi
  push rdi
  push r8
  push r9
  push r10
  push r11
  push r12
  push r13
  push r14
  push r15
  ; fxsave needs to be aligned to 16bytes
  mov rbx,rsp
  and rsp,~0xF
  fxsave  [rsp - 512]
  mov rsp,rbx
  sub rsp,512 + 16
  call c_lvb
  ; fxrstor also needs to be aligned to 16bytes
  add rsp, 512 + 16
  mov rbx,rsp
  and rbx,~0xF
  fxrstor [rbx - 512]
  ; restore GPRs
  pop r15
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  pop rdx
  ret
.early_out:
  mov rax, [rcx]
  ret

checkpoint_handler:
  ; we need at least one reg
  push rcx
  mov rcx, rsp
  and rcx, -0x4000
  add rcx, 0x40
  ; manually save all the regs
  mov qword [rcx],       rax
  mov qword [rcx + 8],   rcx
  mov qword [rcx + 16],  rdx
  mov qword [rcx + 24],  rbx
  mov qword [rcx + 32],  rsp
  mov qword [rcx + 40],  rbp
  mov qword [rcx + 48],  rsi
  mov qword [rcx + 56],  rdi
  mov qword [rcx + 64],  r8
  mov qword [rcx + 72],  r9
  mov qword [rcx + 80],  r10
  mov qword [rcx + 88],  r11
  mov qword [rcx + 96],  r12
  mov qword [rcx + 104], r13
  mov qword [rcx + 112], r14
  mov qword [rcx + 120], r15
  fxsave [rcx + 128]
  ; call into real C
  sub rcx, 0x40
  call c_checkpoint
  ; restore regs
  lea rcx, [rax + 0x40]
  mov rax, [rcx]
  mov rdx, [rcx + 16]
  mov rbx, [rcx + 24]
  mov rsp, [rcx + 32]
  mov rbp, [rcx + 40]
  mov rsi, [rcx + 48]
  mov rdi, [rcx + 56]
  mov r8,  [rcx + 64]
  mov r9,  [rcx + 72]
  mov r10, [rcx + 80]
  mov r11, [rcx + 88]
  mov r12, [rcx + 96]
  mov r13, [rcx + 104]
  mov r14, [rcx + 112]
  mov r15, [rcx + 120]
  fxrstor  [rcx + 128]
  mov rcx, [rcx + 8]
  add rsp, 8
  ret
