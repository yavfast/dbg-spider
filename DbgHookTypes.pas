unit DbgHookTypes;

interface

const
  DBG_EXCEPTION = $0EEDFFF0;

  _EHOOK_GetCallStack = 1;
  _EHOOK_GetObjClassType = 2;

type
  TDbgInfoType = (dstUnknown = 0, dstThreadInfo, dstMemInfo, dstPerfomance, dstPerfomanceAndMemInfo, dstMemHookStatus);

  TDbgMemInfoType = (miGetMem = 0, miFreeMem);

  TDbgMemInfoStack = array[0..31] of Pointer;
  TObjClassTypeName = array[0..SizeOf(TDbgMemInfoStack) - 1] of AnsiChar;

  PDbgMemInfo = ^TDbgMemInfo;
  TDbgMemInfo = packed record
    Ptr: Pointer;
    ThreadId: Cardinal;
    case MemInfoType: TDbgMemInfoType of
      miGetMem: (
        Size: Cardinal;
        Stack: TDbgMemInfoStack;
      );
      miFreeMem: (
        ObjClassType: TObjClassTypeName;
      );
  end;

const
  _DbgMemListLength = ($FFFFF div SizeOf(TDbgMemInfo));

type
  PDbgMemInfoList = ^TDbgMemInfoList;
  TDbgMemInfoList = array[0.._DbgMemListLength - 1] of TDbgMemInfo;

implementation

end.
