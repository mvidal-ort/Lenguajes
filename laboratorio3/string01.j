.class public string01
.super java/lang/Object

.method public <init>()V
  aload_0
  invokenonvirtual java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1000
.limit stack  1000
invokestatic Runtime/readString()Ljava/lang/String;
astore 1
aload 1
pop
aload 1
invokestatic Runtime/printString(Ljava/lang/String;)V
invokestatic Runtime/readString()Ljava/lang/String;
astore 2
aload 2
invokestatic Runtime/printString(Ljava/lang/String;)V
aload 1
ldc " "
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
aload 2
invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
invokestatic Runtime/printString(Ljava/lang/String;)V
return
.end method

