.class public multiple03
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
invokestatic Runtime/readInt()I
istore 2
iload 2
pop
ldc "El maximo entre "
iload 1
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
ldc " y "
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
iload 2
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
ldc " es "
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
iload 1
iload 2
invokestatic multiple03/max(II)I
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
return
.end method

.method public static max(II)I
.limit locals 1000
.limit stack  1000
iload 1
ireturn
return
.end method

