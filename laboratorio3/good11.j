.class public good11
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
ldc 0
istore 1
ldc 0
istore 2
label0:
invokestatic Runtime/readInt()I
istore 3
iload 3
ldc 0
if_icmpne label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
iload 1
iload 3
iadd
istore 1
iload 1
pop
iload 2
iinc 2 1
pop
goto label0
label1:
iload 1
iload 2
idiv
invokestatic Runtime/printInt(I)V
return
.end method

