; ModuleID = 'prelude.c'
source_filename = "prelude.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.String = type { ptr, i32 }
%struct.FatPtr = type { ptr, i32 }

@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.1 = private unnamed_addr constant [5 x i8] c"%.*s\00", align 1
@.str.2 = private unnamed_addr constant [6 x i8] c"%.*s\0A\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@stdin = external global ptr, align 8

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { ptr, i32 } @to_string(ptr noundef %0) #0 {
  %2 = alloca %struct.String, align 8
  %3 = alloca ptr, align 8
  %4 = alloca [16 x i8], align 16
  %5 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  %6 = getelementptr inbounds [16 x i8], ptr %4, i64 0, i64 0
  %7 = load ptr, ptr %3, align 8
  %8 = load i32, ptr %7, align 4
  %9 = call i32 (ptr, ptr, ...) @sprintf(ptr noundef %6, ptr noundef @.str, i32 noundef %8) #5
  %10 = sub nsw i32 %9, 1
  store i32 %10, ptr %5, align 4
  %11 = getelementptr inbounds %struct.String, ptr %2, i32 0, i32 0
  %12 = getelementptr inbounds [16 x i8], ptr %4, i64 0, i64 0
  store ptr %12, ptr %11, align 8
  %13 = getelementptr inbounds %struct.String, ptr %2, i32 0, i32 1
  %14 = load i32, ptr %5, align 4
  store i32 %14, ptr %13, align 8
  %15 = load { ptr, i32 }, ptr %2, align 8
  ret { ptr, i32 } %15
}

; Function Attrs: nounwind
declare i32 @sprintf(ptr noundef, ptr noundef, ...) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { ptr, i32 } @string_plus(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca %struct.String, align 8
  %4 = alloca ptr, align 8
  %5 = alloca ptr, align 8
  %6 = alloca i32, align 4
  %7 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8
  store ptr %1, ptr %5, align 8
  %8 = load ptr, ptr %4, align 8
  %9 = getelementptr inbounds %struct.String, ptr %8, i32 0, i32 1
  %10 = load i32, ptr %9, align 8
  %11 = zext i32 %10 to i64
  %12 = load ptr, ptr %5, align 8
  %13 = call i64 @strlen(ptr noundef %12) #6
  %14 = add i64 %11, %13
  %15 = trunc i64 %14 to i32
  store i32 %15, ptr %6, align 4
  %16 = load i32, ptr %6, align 4
  %17 = add i32 %16, 1
  %18 = zext i32 %17 to i64
  %19 = call noalias ptr @malloc(i64 noundef %18) #7
  store ptr %19, ptr %7, align 8
  %20 = load ptr, ptr %7, align 8
  %21 = load ptr, ptr %4, align 8
  %22 = getelementptr inbounds %struct.String, ptr %21, i32 0, i32 0
  %23 = load ptr, ptr %22, align 8
  %24 = call ptr @strcpy(ptr noundef %20, ptr noundef %23) #5
  %25 = load ptr, ptr %7, align 8
  %26 = load ptr, ptr %4, align 8
  %27 = getelementptr inbounds %struct.String, ptr %26, i32 0, i32 1
  %28 = load i32, ptr %27, align 8
  %29 = zext i32 %28 to i64
  %30 = getelementptr inbounds i8, ptr %25, i64 %29
  %31 = load ptr, ptr %5, align 8
  %32 = call ptr @strcpy(ptr noundef %30, ptr noundef %31) #5
  %33 = getelementptr inbounds %struct.String, ptr %3, i32 0, i32 0
  %34 = load ptr, ptr %7, align 8
  store ptr %34, ptr %33, align 8
  %35 = getelementptr inbounds %struct.String, ptr %3, i32 0, i32 1
  %36 = load i32, ptr %6, align 4
  store i32 %36, ptr %35, align 8
  %37 = load { ptr, i32 }, ptr %3, align 8
  ret { ptr, i32 } %37
}

; Function Attrs: nounwind willreturn memory(read)
declare i64 @strlen(ptr noundef) #2

; Function Attrs: nounwind allocsize(0)
declare noalias ptr @malloc(i64 noundef) #3

; Function Attrs: nounwind
declare ptr @strcpy(ptr noundef, ptr noundef) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @string_len(ptr noundef %0) #0 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8
  %3 = load ptr, ptr %2, align 8
  %4 = getelementptr inbounds %struct.String, ptr %3, i32 0, i32 1
  %5 = load i32, ptr %4, align 8
  ret i32 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @string_as_str(ptr noundef %0, ptr noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca ptr, align 8
  store ptr %0, ptr %3, align 8
  store ptr %1, ptr %4, align 8
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr inbounds %struct.String, ptr %5, i32 0, i32 0
  %7 = load ptr, ptr %6, align 8
  %8 = load ptr, ptr %3, align 8
  %9 = getelementptr inbounds %struct.FatPtr, ptr %8, i32 0, i32 0
  store ptr %7, ptr %9, align 8
  %10 = load ptr, ptr %4, align 8
  %11 = getelementptr inbounds %struct.String, ptr %10, i32 0, i32 1
  %12 = load i32, ptr %11, align 8
  %13 = load ptr, ptr %3, align 8
  %14 = getelementptr inbounds %struct.FatPtr, ptr %13, i32 0, i32 1
  store i32 %12, ptr %14, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print(ptr noundef %0, i32 noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, i32 noundef %5, ptr noundef %6)
  ret void
}

declare i32 @printf(ptr noundef, ...) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @println(ptr noundef %0, i32 noundef %1) #0 {
  %3 = alloca ptr, align 8
  %4 = alloca i32, align 4
  store ptr %0, ptr %3, align 8
  store i32 %1, ptr %4, align 4
  %5 = load i32, ptr %4, align 4
  %6 = load ptr, ptr %3, align 8
  %7 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, i32 noundef %5, ptr noundef %6)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @printInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @printlnInt(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  %3 = load i32, ptr %2, align 4
  %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.3, i32 noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local { ptr, i32 } @getString() #0 {
  %1 = alloca %struct.String, align 8
  %2 = alloca ptr, align 8
  %3 = alloca i32, align 4
  store ptr null, ptr %2, align 8
  %4 = load ptr, ptr %2, align 8
  %5 = load ptr, ptr @stdin, align 8
  %6 = call i64 @getline(ptr noundef %4, ptr noundef null, ptr noundef %5)
  %7 = trunc i64 %6 to i32
  store i32 %7, ptr %3, align 4
  %8 = load i32, ptr %3, align 4
  %9 = icmp ugt i32 %8, 0
  br i1 %9, label %10, label %29

10:                                               ; preds = %0
  %11 = load ptr, ptr %2, align 8
  %12 = load ptr, ptr %11, align 8
  %13 = load i32, ptr %3, align 4
  %14 = sub i32 %13, 1
  %15 = zext i32 %14 to i64
  %16 = getelementptr inbounds i8, ptr %12, i64 %15
  %17 = load i8, ptr %16, align 1
  %18 = sext i8 %17 to i32
  %19 = icmp eq i32 %18, 10
  br i1 %19, label %20, label %29

20:                                               ; preds = %10
  %21 = load ptr, ptr %2, align 8
  %22 = load ptr, ptr %21, align 8
  %23 = load i32, ptr %3, align 4
  %24 = sub i32 %23, 1
  %25 = zext i32 %24 to i64
  %26 = getelementptr inbounds i8, ptr %22, i64 %25
  store i8 0, ptr %26, align 1
  %27 = load i32, ptr %3, align 4
  %28 = add i32 %27, -1
  store i32 %28, ptr %3, align 4
  br label %29

29:                                               ; preds = %20, %10, %0
  %30 = getelementptr inbounds %struct.String, ptr %1, i32 0, i32 0
  %31 = load ptr, ptr %2, align 8
  %32 = load ptr, ptr %31, align 8
  store ptr %32, ptr %30, align 8
  %33 = getelementptr inbounds %struct.String, ptr %1, i32 0, i32 1
  %34 = load i32, ptr %3, align 4
  store i32 %34, ptr %33, align 8
  %35 = load { ptr, i32 }, ptr %1, align 8
  ret { ptr, i32 } %35
}

declare i64 @getline(ptr noundef, ptr noundef, ptr noundef) #4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @getInt() #0 {
  %1 = alloca i32, align 4
  %2 = call i32 (ptr, ...) @__isoc99_scanf(ptr noundef @.str, ptr noundef %1)
  %3 = load i32, ptr %1, align 4
  ret i32 %3
}

declare i32 @__isoc99_scanf(ptr noundef, ...) #4

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nounwind willreturn memory(read) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { nounwind allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { nounwind }
attributes #6 = { nounwind willreturn memory(read) }
attributes #7 = { nounwind allocsize(0) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"Ubuntu clang version 18.1.3 (1ubuntu1)"}
