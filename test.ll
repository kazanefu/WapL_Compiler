; ModuleID = 'wapl_module'
source_filename = "wapl_module"

declare i64 @strtol(i8*, i8**, i32)

declare double @atof(i8*)

declare i32 @printf(i8*, ...)

declare i32 @sprintf(i8*, i8*, ...)

declare i8* @malloc(i64)

declare void @free(i8*)
