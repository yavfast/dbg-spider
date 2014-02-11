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

  TDbgHookInfoStack = array[0..31] of Pointer;
  TObjClassTypeName = array[0..SizeOf(TDbgHookInfoStack) - 1] of AnsiChar;

  PDbgMemInfo = ^TDbgMemInfo;
  TDbgMemInfo = packed record
    Ptr: Pointer;
    ThreadId: Cardinal;
    case MemInfoType: TDbgMemInfoType of
      miGetMem: (
        Size: Cardinal;
        Stack: TDbgHookInfoStack;
      );
      miFreeMem: (
        ObjClassType: TObjClassTypeName;
      );
  end;

  // SyncObjs

  TDbgSyncObjsType = (
    soUnknown = 0,
    soSleep,
    soWaitForSingleObject, soWaitForMultipleObjects,
    soEnterCriticalSection, soLeaveCriticalSection, soInCriticalSection
  );

  TDbgSyncObjsStateType = (sosUnknown = 0, sosEnter, sosLeave);

  PDbgSyncObjsInfo = ^TDbgSyncObjsInfo;
  TDbgSyncObjsInfo = packed record
    Id: NativeUInt;
    ThreadId: Cardinal;
    CurTime: Int64;
    Stack: TDbgHookInfoStack;
    SyncObjsStateType: TDbgSyncObjsStateType;
    case SyncObjsType: TDbgSyncObjsType of
      soUnknown: (
        //Data: NativeUInt;
        //AdvData: NativeUInt;
      );
      soSleep: (
        MSec: NativeUInt;
      );
      soWaitForSingleObject: (
        Handle: THandle;
      );
      soWaitForMultipleObjects: (
        Handles: PWOHandleArray;
      );
      soEnterCriticalSection,
      soLeaveCriticalSection,
      soInCriticalSection:
      (
        CS: PRTLCriticalSection;
        OwningThreadId: Cardinal;
      );
  end;

const
  _DbgMemListLength = ($FFFFF div SizeOf(TDbgMemInfo));
  _DbgSyncObjsListLength = ($FFFFF div SizeOf(TDbgSyncObjsInfo));

type
  PDbgMemInfoList = ^TDbgMemInfoList;
  TDbgMemInfoList = array[0.._DbgMemListLength - 1] of TDbgMemInfo;

  PDbgSyncObjsInfoList = ^TDbgSyncObjsInfoList;
  TDbgSyncObjsInfoList = array[0.._DbgSyncObjsListLength - 1] of TDbgSyncObjsInfo;

implementation

end.
