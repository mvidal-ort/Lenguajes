.class public good17
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
ldc 6
istore 1
iload 1
pop
iload 1
ldc 7
iadd
istore 2
iload 2
pop
iload 2
invokestatic Runtime/printInt(I)V
ldc 4
istore 3
iload 3
pop
iload 3
invokestatic Runtime/printInt(I)V
iload 3
istore 1
iload 1
pop
iload 1
invokestatic Runtime/printInt(I)V
iload 1
invokestatic Runtime/printInt(I)V
iload 2
invokestatic Runtime/printInt(I)V
return
.end method

