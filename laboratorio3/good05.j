.class public good05
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
pop
iload 1
istore 2
iload 2
pop
invokestatic Runtime/readInt()I
istore 3
iload 3
pop
iload 1
invokestatic Runtime/printInt(I)V
label0:
iload 2
iload 3
if_icmplt label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
iload 2
invokestatic Runtime/printInt(I)V
iload 1
iload 2
iadd
istore 2
iload 2
pop
iload 2
iload 1
isub
istore 1
iload 1
pop
goto label0
label1:
return
.end method

