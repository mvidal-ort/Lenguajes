.class public mix
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
ldc 2
istore 2
iload 2
pop
ldc2_w 4.5
dstore 3
dload 3
pop2
ldc2_w 0.5
dstore 5
dload 5
pop2
ldc "a = "
iload 1
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
ldc "b = "
iload 2
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
ldc "a + b = "
iload 1
iload 2
iadd
invokestatic Runtime/i2Str(I)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
ldc "c = "
dload 3
invokestatic Runtime/d2Str(D)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
ldc "d = "
dload 5
invokestatic Runtime/d2Str(D)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
ldc "c + d = "
dload 3
dload 5
dadd
invokestatic Runtime/d2Str(D)Ljava/lang/String;
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
return
.end method

