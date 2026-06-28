.class public good13
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
invokestatic Runtime/readInt()I
istore 1
iload 1
pop
ldc 2
istore 2
iload 2
pop
label0:
iload 2
iload 1
if_icmple label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
ldc 1
istore 3
ldc 2
istore 4
label4:
iload 4
iload 4
imul
iload 2
if_icmple label8
ldc 0
goto label9
label8:
ldc 1
label9:
ifeq label6
iload 3
ifeq label6
ldc 1
goto label7
label6:
ldc 0
label7:
ifeq label5
iload 2
iload 4
idiv
iload 4
imul
iload 2
if_icmpeq label12
ldc 0
goto label13
label12:
ldc 1
label13:
ifeq label10
ldc 0
istore 3
iload 3
pop
goto label11
label10:
label11:
iload 4
iinc 4 1
pop
goto label4
label5:
iload 3
ifeq label16
iload 1
iload 2
idiv
iload 2
imul
iload 1
if_icmpeq label18
ldc 0
goto label19
label18:
ldc 1
label19:
ifeq label16
ldc 1
goto label17
label16:
ldc 0
label17:
ifeq label14
iload 2
invokestatic Runtime/printInt(I)V
iload 1
iload 2
idiv
istore 1
iload 1
pop
goto label15
label14:
iload 2
iinc 2 1
pop
label15:
goto label0
label1:
return
return
.end method

