.class public good15
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
ldc 1
istore 1
iload 1
invokestatic Runtime/printInt(I)V
ldc 1
ifne label0
iload 1
iinc 1 1
ldc 45
if_icmpne label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifne label0
ldc 0
goto label1
label0:
ldc 1
label1:
pop
iload 1
invokestatic Runtime/printInt(I)V
ldc 0
ifne label4
iload 1
iinc 1 1
ldc 0
if_icmpge label6
ldc 0
goto label7
label6:
ldc 1
label7:
ifne label4
ldc 0
goto label5
label4:
ldc 1
label5:
pop
iload 1
invokestatic Runtime/printInt(I)V
ldc 1
ifeq label8
iload 1
iinc 1 1
ldc 0
if_icmplt label10
ldc 0
goto label11
label10:
ldc 1
label11:
ifeq label8
ldc 1
goto label9
label8:
ldc 0
label9:
pop
iload 1
invokestatic Runtime/printInt(I)V
ldc 0
ifeq label12
iload 1
iinc 1 1
ldc 0
if_icmpgt label14
ldc 0
goto label15
label14:
ldc 1
label15:
ifeq label12
ldc 1
goto label13
label12:
ldc 0
label13:
pop
iload 1
invokestatic Runtime/printInt(I)V
ldc 0
istore 2
ldc 34
ldc 6
if_icmplt label20
ldc 0
goto label21
label20:
ldc 1
label21:
ifeq label18
iload 2
ldc 0
if_icmplt label22
ldc 0
goto label23
label22:
ldc 1
label23:
ifeq label18
ldc 1
goto label19
label18:
ldc 0
label19:
ifeq label16
iload 1
invokestatic Runtime/printInt(I)V
goto label17
label16:
ldc 42
invokestatic Runtime/printInt(I)V
label17:
return
.end method

