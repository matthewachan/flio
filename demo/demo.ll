; ModuleID = 'flio'
source_filename = "flio"

@strptr = private unnamed_addr constant [11 x i8] c"sample.txt\00"
@mode = private unnamed_addr constant [3 x i8] c"r+\00"
@strptr.1 = private unnamed_addr constant [7 x i8] c"lined_\00"
@mode.2 = private unnamed_addr constant [3 x i8] c"r+\00"
@strptr.3 = private unnamed_addr constant [2 x i8] c"[\00"
@strptr.4 = private unnamed_addr constant [3 x i8] c"] \00"
@strptr.5 = private unnamed_addr constant [1 x i8] zeroinitializer

declare i32 @printf(i8*, ...)

declare i8* @concat(i8*, i8*)

declare i8* @intToStr(i32)

declare i32 @strcmp(i8*, i8*)

declare i8* @fopen(i8*, ...)

declare i8* @opendir(i8*)

declare i32 @remove(i8*, ...)

declare i32 @rmdir(i8*)

declare i32 @copy(i8*, i8*)

declare i32 @move(i8*, i8*)

declare i32 @bwrite(i8*, i8*)

declare i32 @appendString(i8*, i8*)

declare i8* @bread(i8*, i32)

declare i8* @readLine(i8*)

define void @addLineNumbers(i8* %filename) {
entry:
  %filename1 = alloca i8*
  store i8* %filename, i8** %filename1
  %f = alloca i8*
  %filename2 = load i8*, i8** %filename1
  %fopen = call i8* (i8*, ...) @fopen(i8* %filename2, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @mode, i32 0, i32 0))
  store i8* %fopen, i8** %f
  %copyName = alloca i8*
  %filename3 = load i8*, i8** %filename1
  %concat = call i8* @concat(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @strptr.1, i32 0, i32 0), i8* %filename3)
  store i8* %concat, i8** %copyName
  %copyName4 = load i8*, i8** %copyName
  %filename5 = load i8*, i8** %filename1
  %copy = call i32 @copy(i8* %filename5, i8* %copyName4)
  %newFile = alloca i8*
  %copyName6 = load i8*, i8** %copyName
  %fopen7 = call i8* (i8*, ...) @fopen(i8* %copyName6, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @mode.2, i32 0, i32 0))
  store i8* %fopen7, i8** %newFile
  %line = alloca i8*
  %f8 = load i8*, i8** %f
  %readLine = call i8* @readLine(i8* %f8)
  store i8* %readLine, i8** %line
  %lineNo = alloca i32
  store i32 0, i32* %lineNo
  %prefix = alloca i8*
  br label %init

init:                                             ; preds = %entry
  br label %for

for:                                              ; preds = %for_body, %init
  %line20 = load i8*, i8** %line
  %strcmp = call i32 @strcmp(i8* %line20, i8* getelementptr inbounds ([1 x i8], [1 x i8]* @strptr.5, i32 0, i32 0))
  %tmp21 = icmp ne i32 %strcmp, 0
  br i1 %tmp21, label %for_body, label %merge

for_body:                                         ; preds = %for
  %lineNo9 = load i32, i32* %lineNo
  %intToStr = call i8* @intToStr(i32 %lineNo9)
  %concat10 = call i8* @concat(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @strptr.3, i32 0, i32 0), i8* %intToStr)
  store i8* %concat10, i8** %prefix
  %prefix11 = load i8*, i8** %prefix
  %concat12 = call i8* @concat(i8* %prefix11, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @strptr.4, i32 0, i32 0))
  store i8* %concat12, i8** %prefix
  %line13 = load i8*, i8** %line
  %prefix14 = load i8*, i8** %prefix
  %concat15 = call i8* @concat(i8* %prefix14, i8* %line13)
  %newFile16 = load i8*, i8** %newFile
  %write = call i32 @bwrite(i8* %newFile16, i8* %concat15)
  %f17 = load i8*, i8** %f
  %readLine18 = call i8* @readLine(i8* %f17)
  store i8* %readLine18, i8** %line
  %lineNo19 = load i32, i32* %lineNo
  %tmp = add i32 %lineNo19, 1
  store i32 %tmp, i32* %lineNo
  br label %for

merge:                                            ; preds = %for
  ret void
}

define i32 @main() {
entry:
  call void @addLineNumbers(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @strptr, i32 0, i32 0))
  ret i32 0
}
