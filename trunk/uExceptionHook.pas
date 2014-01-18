unit uExceptionHook;

interface

uses Classes, SysUtils, Windows;

implementation

const
  MAX_STACK_LENGTH = 16;

type
  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallerFrame: Pointer;
    CallerAddr: Pointer;
  end;

  NT_TIB32 = packed record
    ExceptionList: DWORD;
    StackBase: DWORD;
    StackLimit: DWORD;
    SubSystemTib: DWORD;
    case Integer of
      0 : (
        FiberData: DWORD;
        ArbitraryUserPointer: DWORD;
        Self: DWORD;
      );
      1 : (
        Version: DWORD;
      );
  end;

threadvar
  _Buf: TMemoryBasicInformation;

var
  OldDebugHook: Byte = 0;
  InDebugMode: Boolean = False;

function IsValidCodeAddr(const Addr: Pointer): Boolean;
const
  _PAGE_CODE: Cardinal = (PAGE_EXECUTE Or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE Or PAGE_EXECUTE_WRITECOPY);
Begin
  Result := (VirtualQuery(Addr, _Buf, SizeOf(TMemoryBasicInformation)) <> 0) And ((_Buf.Protect And _PAGE_CODE) <> 0);
end;

function IsValidAddr(const Addr: Pointer): Boolean;
Begin
  Result := (VirtualQuery(Addr, _Buf, SizeOf(TMemoryBasicInformation)) <> 0);
end;

function GetStackTop: Pointer; assembler;
asm
  MOV     EAX, FS:[0].NT_TIB32.StackBase
end;

function GetCallStack(const EIP, EBP: Pointer): TList; overload;
var
  TopOfStack: Pointer;
  BaseOfStack: Pointer;
  StackFrame: PStackFrame;
  Level: Integer;
begin
  Result := TList.Create;
  try
    Level := 0; // пропуск по стеку

    Result.Add(EIP);

    StackFrame := EBP;
    BaseOfStack := Pointer(Cardinal(StackFrame) - 1);

    TopOfStack := GetStackTop;
    while (Level < MAX_STACK_LENGTH) and (
        (Cardinal(BaseOfStack) < Cardinal(StackFrame)) and
        (Cardinal(StackFrame) < Cardinal(TopOfStack)) and
        (StackFrame <> StackFrame^.CallerFrame) and
        IsValidAddr(StackFrame) and
        IsValidCodeAddr(StackFrame^.CallerAddr)
      )
    do begin
      if Level >= 0 then
        Result.Add(Pointer(Cardinal(StackFrame^.CallerAddr) - 1));

      StackFrame := PStackFrame(StackFrame^.CallerFrame);

      Inc(Level);
    end;
  except
    // Skip
  end;
end;

function GetCallStack(Context: PContext): TList; overload;
begin
  Result := GetCallStack(Pointer(Context^.Eip), Pointer(Context^.Ebp));
end;

procedure _CleanUpStackInfoProc(Info: Pointer);
begin
  FreeAndNil(Info);
end;

function _GetStackInfoStringProc(Info: Pointer): String;
var
  StackList: TList;
  I: Integer;
begin
  Result := '';
  if Assigned(Info) then
  begin
    StackList := TList(Info);

    for I := 0 to StackList.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ' ';

      Result := Result + Format('%p', [StackList[I]]);
    end;
  end;
end;

var
  _BaseRaiseExceptionProc: TRaiseExceptionProc = nil;

type
  TParamArray = array[0..14] of Pointer;
  HookException = class(Exception);

const
  cNonDelphiException = $0EEDFAE4;
  cDelphiException    = $0EEDFADE;
  cContinuable        = 0;

procedure _RaiseExceptionProc(ExceptionCode, ExceptionFlags: LongWord; NumberOfArguments: LongWord; Args: Pointer); stdcall;
var
  ContextRecord: PContext;
  ExceptionObj: HookException;
begin
//  if InDebugMode then
//  begin
//    // Дебагер сам отработает все коды эксепшинов
//    _BaseRaiseExceptionProc(ExceptionCode, ExceptionFlags, NumberOfArguments, Args);
//  end
//  else
  begin
    if (ExceptionCode = cNonDelphiException) then
    begin
      ContextRecord := TParamArray(Args^)[0];
      ExceptionObj := TParamArray(Args^)[1];
      ExceptionObj.SetStackInfo(GetCallStack(ContextRecord));
    end
    else
    if (ExceptionCode = cDelphiException) and (ExceptionFlags <> cContinuable) then
    begin
      ExceptionObj := TParamArray(Args^)[1]; // Except object
      ExceptionObj.SetStackInfo(GetCallStack(TParamArray(Args^)[0]{Address}, TParamArray(Args^)[5]{Stack frame}));
    end;

    if ExceptionFlags <> cContinuable then
    begin
      if not InDebugMode then
        DebugHook := OldDebugHook;
      try
        _BaseRaiseExceptionProc(ExceptionCode, ExceptionFlags, NumberOfArguments, Args)
      finally
        if not InDebugMode then
          DebugHook := 1;
      end;
    end;
  end;
end;

procedure _InitExceptionHook;
begin
  // Блокировка всех системынх сообщений про ошибку
  {$IFNDEF DEBUG}
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOOPENFILEERRORBOX);
  {$ENDIF}

  // Приложение запущено по дебагом
  InDebugMode := (DebugHook <> 0);

  OldDebugHook := DebugHook;

  if not InDebugMode then
    DebugHook := 1; // Для вызова RaiseExceptionProc

  //if not InDebugMode then
  begin
    _BaseRaiseExceptionProc := RaiseExceptionProc;
    RaiseExceptionProc := @_RaiseExceptionProc;

    Exception.CleanUpStackInfoProc := @_CleanUpStackInfoProc;
    Exception.GetStackInfoStringProc := @_GetStackInfoStringProc;
  end;
end;

procedure _ReleaseExceptionHook;
begin
  if not InDebugMode then
    DebugHook := OldDebugHook;

  //if not InDebugMode then
  begin
    RaiseExceptionProc := @_BaseRaiseExceptionProc;

    Exception.CleanUpStackInfoProc := nil;
    Exception.GetStackInfoStringProc := nil;
  end;
end;

initialization
  _InitExceptionHook;

finalization
  _ReleaseExceptionHook;

end.
