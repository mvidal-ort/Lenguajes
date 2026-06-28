.class public good07
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
ldc 2
idiv
istore 2
label0:
iload 2
ldc 1
if_icmpgt label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
iload 2
iload 1
iload 2
idiv
imul
iload 1
if_icmpeq label6
ldc 0
goto label7
label6:
ldc 1
label7:
ifeq label4
iload 2
invokestatic Runtime/printInt(I)V
goto label5
label4:
label5:
iload 2
iinc 2 -1
pop
goto label0
label1:
return
.end method

