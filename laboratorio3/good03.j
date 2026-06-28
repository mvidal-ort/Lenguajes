.class public good03
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
ldc 1
istore 2
ldc 1
istore 3
label0:
iload 3
iload 1
ldc 1
iadd
if_icmplt label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
iload 3
iload 2
imul
istore 2
iload 2
pop
iinc 3 1
iload 3
pop
goto label0
label1:
iload 2
invokestatic Runtime/printInt(I)V
return
.end method

