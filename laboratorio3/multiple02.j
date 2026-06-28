.class public multiple02
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
label0:
iload 1
ldc 6
if_icmplt label2
ldc 0
goto label3
label2:
ldc 1
label3:
ifeq label1
iload 1
invokestatic multiple02/fib(I)I
invokestatic Runtime/printInt(I)V
iload 1
iinc 1 1
pop
goto label0
label1:
return
.end method

.method public static fib(I)I
.limit locals 1000
.limit stack  1000
iload 0
ldc 2
if_icmplt label6
ldc 0
goto label7
label6:
ldc 1
label7:
ifeq label4
iload 0
ireturn
goto label5
label4:
iload 0
ldc 1
isub
invokestatic multiple02/fib(I)I
iload 0
ldc 2
isub
invokestatic multiple02/fib(I)I
iadd
ireturn
label5:
return
.end method

