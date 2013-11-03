unit DbgHookTypes;

interface

uses
  Windows;

const
  DBG_EXCEPTION = $0EEDFFF0;

  _EHOOK_GetCallStack = 1;
  _EHOOK_GetObjClassType = 2;

type
  TDbgInfoType = (dstUnknown = 0, dstThreadInfo, dstMemInfo, dstPerfomance, dstPerfomanceAndInfo, dstMemHookStatus, dstSyncObjsInfo);

  // Memory Info

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

  // SyncObjs

  TDbgSyncObjsType = (soSleep = 0, soWaitForSingleObject, soWaitForMultipleObjects, soEnterCriticalSection, soLeaveCriticalSection);
  TDbgSyncObjsStateType = (sosEnter = 0, sosLeave = 1);

  PDbgSyncObjsInfo = ^TDbgSyncObjsInfo;
  TDbgSyncObjsInfo = packed record
    ThreadId: Cardinal;
    SyncObjsType: TDbgSyncObjsType;
    SyncObjsStateType: TDbgSyncObjsStateType;
    Id: NativeUInt;
    Data: NativeUInt;
  end;

const
  _DbgMemListLength = ($FFFFF div SizeOf(TDbgMemInfo));
  _DbgSyncObjsListLength = ($FFFF div SizeOf(TDbgSyncObjsInfo));

type
  PDbgMemInfoList = ^TDbgMemInfoList;
  TDbgMemInfoList = array[0.._DbgMemListLength - 1] of TDbgMemInfo;

  PDbgSyncObjsInfoList = ^TDbgSyncObjsInfoList;
  TDbgSyncObjsInfoList = array[0.._DbgSyncObjsListLength - 1] of TDbgSyncObjsInfo;

implementation

end.
