.class public double02
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
invokestatic Runtime/readDouble()D
dstore 3
dload 1
dload 3
dcmpg
ifge label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label0
dload 1
invokestatic Runtime/printDouble(D)V
goto label1
label0:
dload 3
invokestatic Runtime/printDouble(D)V
label1:
dload 1
dload 3
dsub
invokestatic Runtime/printDouble(D)V
dload 1
dload 3
dadd
invokestatic Runtime/printDouble(D)V
dload 3
dload 1
dsub
invokestatic Runtime/printDouble(D)V
dload 1
dload 1
dmul
invokestatic Runtime/printDouble(D)V
dload 3
dload 1
ddiv
invokestatic Runtime/printDouble(D)V
dload 1
dload 3
ldc2_w 1.0
dadd
dstore 3
dload 3
dmul
invokestatic Runtime/printDouble(D)V
dload 3
invokestatic Runtime/printDouble(D)V
dload 1
dload 1
dload 1
ldc2_w 1.0
dadd
dstore 1
dcmpg
ifeq label6
ldc 0
goto label7
label6:
ldc 1
label7:
ifeq label4
dload 1
dload 1
ldc2_w 1.0
dsub
dstore 1
invokestatic Runtime/printDouble(D)V
goto label5
label4:
ldc2_w 99.9
invokestatic Runtime/printDouble(D)V
label5:
dload 1
invokestatic Runtime/printDouble(D)V
return
.end method

