; ModuleID = 'wapl_module'
source_filename = "wapl_module"

%Pos = type { double, double }

@str_0 = private unnamed_addr constant [16 x i8] c"hello from test\00", align 1
@println_fmt_1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str_2 = private unnamed_addr constant [18 x i8] c"i = %lld,j = %lld\00", align 1
@println_fmt_3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str_4 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@println_fmt_5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str_6 = private unnamed_addr constant [11 x i8] c"pos:x = %g\00", align 1
@println_fmt_7 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str_8 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@str_9 = private unnamed_addr constant [11 x i8] c"input = %d\00", align 1
@println_fmt_10 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str_11 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@println_fmt_12 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i64 @strtol(i8*, i8**, i32)

declare double @atof(i8*)

declare i32 @printf(i8*, ...)

declare i32 @sprintf(i8*, i8*, ...)

declare i8* @malloc(i64)

declare void @free(i8*)

declare i32 @scanf(i8*, ...)

define void @test() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_1, i32 0, i32 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @str_0, i32 0, i32 0))
  ret void
}

define i64 @tt(i64 %b) {
entry:
  %b1 = alloca i64, align 8
  store i64 %b, i64* %b1, align 4
  %ret_val = alloca i64, align 8
  %i = alloca i64, align 8
  store i64 0, i64* %i, align 4
  br label %start

start:                                            ; preds = %loop2end, %entry
  %i2 = load i64, i64* %i, align 4
  %b3 = load i64, i64* %b1, align 4
  %slt = icmp slt i64 %i2, %b3
  br i1 %slt, label %pending_true, label %pending_false

pending_true:                                     ; preds = %start
  br label %loop1

pending_false:                                    ; preds = %start
  br label %end

loop1:                                            ; preds = %pending_true
  %j = alloca i64, align 8
  store i64 0, i64* %j, align 4
  br label %loop2start

loop2start:                                       ; preds = %loop2, %loop1
  %j4 = load i64, i64* %j, align 4
  %b5 = load i64, i64* %b1, align 4
  %slt6 = icmp slt i64 %j4, %b5
  br i1 %slt6, label %pending_true7, label %pending_false8

pending_true7:                                    ; preds = %loop2start
  br label %loop2

pending_false8:                                   ; preds = %loop2start
  br label %loop2end

loop2:                                            ; preds = %pending_true7
  %i9 = load i64, i64* %i, align 4
  %j10 = load i64, i64* %j, align 4
  %fmt_buf = alloca [128 x i8], align 1
  %fmt_buf_ptr = bitcast [128 x i8]* %fmt_buf to i8*
  %sprintf = call i32 (i8*, i8*, ...) @sprintf(i8* %fmt_buf_ptr, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @str_2, i32 0, i32 0), i64 %i9, i64 %j10)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_3, i32 0, i32 0), i8* %fmt_buf_ptr)
  %j11 = load i64, i64* %j, align 4
  %add = add i64 %j11, 1
  store i64 %add, i64* %j, align 4
  br label %loop2start

loop2end:                                         ; preds = %pending_false8
  %i12 = load i64, i64* %i, align 4
  %add13 = add i64 %i12, 1
  store i64 %add13, i64* %i, align 4
  br label %start

end:                                              ; preds = %pending_false
  ret i64 0
}

define i64 @user_main() {
entry:
  %ret_val = alloca i64, align 8
  %sz = alloca i64, align 8
  store i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64* %sz, align 4
  %sz1 = load i64, i64* %sz, align 4
  %mul = mul i64 %sz1, 10
  %malloc_call = call i8* @malloc(i64 %mul)
  %a = alloca i8*, align 8
  store i8* %malloc_call, i8** %a, align 8
  %get_ptr = load i8*, i8** %a, align 8
  %array_idx = getelementptr i8, i8* %get_ptr, i64 0
  store i8 104, i8* %array_idx, align 1
  %array_idx2 = getelementptr i8, i8* %get_ptr, i64 1
  store i8 101, i8* %array_idx2, align 1
  %array_idx3 = getelementptr i8, i8* %get_ptr, i64 2
  store i8 108, i8* %array_idx3, align 1
  %array_idx4 = getelementptr i8, i8* %get_ptr, i64 3
  store i8 108, i8* %array_idx4, align 1
  %array_idx5 = getelementptr i8, i8* %get_ptr, i64 4
  store i8 111, i8* %array_idx5, align 1
  %a6 = load i8*, i8** %a, align 8
  %ptr_add = getelementptr i8, i8* %a6, i64 3
  %fmt_buf = alloca [128 x i8], align 1
  %fmt_buf_ptr = bitcast [128 x i8]* %fmt_buf to i8*
  %sprintf = call i32 (i8*, i8*, ...) @sprintf(i8* %fmt_buf_ptr, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str_4, i32 0, i32 0), i8* %ptr_add)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_5, i32 0, i32 0), i8* %fmt_buf_ptr)
  %get_ptr7 = load i8*, i8** %a, align 8
  call void @free(i8* %get_ptr7)
  br label %B

A:                                                ; No predecessors!
  %pos = alloca %Pos, align 8
  store %Pos zeroinitializer, %Pos* %pos, align 8
  %pos2 = alloca %Pos*, align 8
  store %Pos* %pos, %Pos** %pos2, align 8
  %pos28 = load %Pos*, %Pos** %pos2, align 8
  %access = getelementptr inbounds %Pos, %Pos* %pos28, i32 0, i32 1
  %b = alloca double*, align 8
  store double* %access, double** %b, align 8
  %b9 = load double*, double** %b, align 8
  store double 1.111000e+01, double* %b9, align 8
  %access10 = getelementptr inbounds %Pos, %Pos* %pos, i32 0, i32 1
  %getmembervalue = load double, double* %access10, align 8
  %fmt_buf11 = alloca [128 x i8], align 1
  %fmt_buf_ptr12 = bitcast [128 x i8]* %fmt_buf11 to i8*
  %sprintf13 = call i32 (i8*, i8*, ...) @sprintf(i8* %fmt_buf_ptr12, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @str_6, i32 0, i32 0), double %getmembervalue)
  %printf14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_7, i32 0, i32 0), i8* %fmt_buf_ptr12)
  ret i64 0

B:                                                ; preds = %then, %entry
  %i = alloca i64, align 8
  store i64 0, i64* %i, align 4
  %i15 = load i64, i64* %i, align 4
  %add = add i64 %i15, 1
  store i64 %add, i64* %i, align 4
  %i16 = load i64, i64* %i, align 4
  %sge = icmp sge i64 %i16, 10
  br i1 %sge, label %pending_true, label %pending_false

pending_true:                                     ; preds = %B
  br label %then

pending_false:                                    ; preds = %B
  br label %else

then:                                             ; preds = %pending_true
  %input = alloca i64, align 8
  store i64 0, i64* %input, align 4
  %scanf = call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str_8, i32 0, i32 0), i64* %input)
  %input17 = load i64, i64* %input, align 4
  %fmt_buf18 = alloca [128 x i8], align 1
  %fmt_buf_ptr19 = bitcast [128 x i8]* %fmt_buf18 to i8*
  %sprintf20 = call i32 (i8*, i8*, ...) @sprintf(i8* %fmt_buf_ptr19, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @str_9, i32 0, i32 0), i64 %input17)
  %printf21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_10, i32 0, i32 0), i8* %fmt_buf_ptr19)
  br label %B

else:                                             ; preds = %pending_false
  %printf22 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @println_fmt_12, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str_11, i32 0, i32 0))
  ret i64 0
}

define void @toplevel_child() {
entry:
  %calltmp = call i64 @user_main()
  ret void
}

define void @toplevel_child.1() {
entry:
  call void @test()
  ret void
}

define void @toplevel_child.2() {
entry:
  %calltmp = call i64 @tt(i64 100)
  ret void
}

define i32 @main(i32 %argc, i8** %argv) {
entry:
  call void @toplevel_child()
  call void @toplevel_child.1()
  call void @toplevel_child.2()
  ret i32 0
}
