.class public double01
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
invokestatic Runtime/readDouble()D
dstore 1
dload 1
pop2
dload 1
invokestatic Runtime/printDouble(D)V
label0:
dload 1
ldc2_w 26.0
dcmpg
iflt label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
dload 1
dload 1
ldc2_w 1.0
dadd
dstore 1
pop2
dload 1
invokestatic Runtime/printDouble(D)V
goto label0
label1:
return
.end method

