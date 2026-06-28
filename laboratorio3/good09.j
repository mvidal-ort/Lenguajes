.class public good09
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
invokestatic Runtime/printInt(I)V
iload 1
iinc 1 1
invokestatic Runtime/printInt(I)V
iload 1
invokestatic Runtime/printInt(I)V
iinc 1 1
iload 1
invokestatic Runtime/printInt(I)V
iload 1
invokestatic Runtime/printInt(I)V
return
.end method

