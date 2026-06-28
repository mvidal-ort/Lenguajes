.class public multiple01
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
invokestatic multiple01/fun()I
istore 1
iload 1
pop
iload 1
invokestatic Runtime/printInt(I)V
invokestatic multiple01/fun()I
invokestatic Runtime/printInt(I)V
return
.end method

.method public static fun()I
.limit locals 1000
.limit stack  1000
ldc 1
ireturn
return
.end method

