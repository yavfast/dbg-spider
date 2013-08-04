library DbgHook32;

uses
  Windows, SysUtils, Classes, JclPEImage;

type
  TKernel32_CreateThread = function(SecurityAttributes: Pointer; StackSize: LongWord;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;

var
  ThreadsHooked: Boolean;
  Kernel32_CreateThread: TKernel32_CreateThread = nil;

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;


function _HookedCreateThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer;
  CreationFlags: LongWord; var ThreadId: LongWord): Integer; stdcall;
var
  Name: String;
  ThRec: PThreadRec;
  Th: TObject;
  ParentId: Cardinal;
begin
  Name := '';

  if Assigned(Parameter) then
  begin
    ThRec := PThreadRec(Parameter);
    try
      Th := TObject(ThRec^.Parameter);
      Name := Th.ClassName;
      ParentId := GetCurrentThreadId;
    except

    end;
  end;

  Result := Kernel32_CreateThread(SecurityAttributes, StackSize, ThreadFunc, Parameter, CreationFlags, ThreadId);

  if (Result <> 0) and (Name <> '') then
  begin
    // Thread ClassName
    OutputDebugString(PWideChar(Format('###%d|%d|%s', [1, ThreadId, Name])));
    // Thread ParentID
    OutputDebugString(PWideChar(Format('###%d|%d|%d', [2, ThreadId, ParentId])));
  end;
end;

function _HookThreads(ImageBase: Pointer): Boolean;
var
  ProcAddrCache: Pointer;
begin
  if not ThreadsHooked then
  begin
    ProcAddrCache := GetProcAddress(GetModuleHandle(kernel32), 'CreateThread');
    OutputDebugString(PWideChar(Format('ProcAddrCache: %p', [ProcAddrCache])));
    OutputDebugString(PWideChar(Format('ImageBase: %p', [ImageBase])));

    Result := TJclPeMapImgHooks.ReplaceImport(ImageBase, kernel32, ProcAddrCache, @_HookedCreateThread);

    if Result then
    begin
      @Kernel32_CreateThread := ProcAddrCache;
    end;

    ThreadsHooked := Result;
  end
  else
    Result := True;
end;


procedure InitHook(ImageBase: Pointer); stdcall;
begin
  OutputDebugString('Init debug hooks...');

  if _HookThreads(ImageBase) then
    OutputDebugString('Init thread hook - ok')
  else
    OutputDebugString('Init thread hook - fail')
end;

exports InitHook;

begin
end.
