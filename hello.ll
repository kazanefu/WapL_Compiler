; ModuleID = 'hello_module'
source_filename = "hello_module"

@hello = private unnamed_addr constant [15 x i8] c"Hello, World!\0A\00", align 1

declare i32 @puts(i8*)

define i32 @main() {
entry:
  %call = call i32 @puts(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @hello, i32 0, i32 0))
  ret i32 0
}
