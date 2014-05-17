unit DbgHookTypes;

interface

uses
  Windows;

const
  DBG_EXCEPTION = $0EEDFFF0;

  _EHOOK_GetCallStack = 1;
  _EHOOK_GetObjClassType = 2;

  DBG_STACK_LENGTH = 32;

type
  TDbgInfoType = (
    dstUnknown = 0,
    dstThreadInfo,
    dstMemInfo,
    dstPerfomance,
    dstPerfomanceAndInfo,
    dstMemHookStatus,
    dstSyncObjsInfo,
    dstSampling
  );

  // Memory Info

  TDbgMemInfoType = (miGetMem = 0, miFreeMem);

  PDbgHookInfoStack = ^TDbgHookInfoStack;
  TDbgHookInfoStack = array[0..(DBG_STACK_LENGTH - 1)] of Pointer;

  TObjClassTypeName = array[0..(SizeOf(TDbgHookInfoStack) - 1)] of AnsiChar;

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
    soEnterCriticalSection, soLeaveCriticalSection, soInCriticalSection,
    soSendMessage
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
      soUnknown:
      ();
      soSleep:
      (
        MSec: NativeUInt;
      );
      soWaitForSingleObject:
      (
        Handle: THandle;
      );
      soWaitForMultipleObjects:
      (
        Handles: PWOHandleArray;
      );
      soEnterCriticalSection,
      soLeaveCriticalSection,
      soInCriticalSection:
      (
        CS: PRTLCriticalSection;
        OwningThreadId: Cardinal;
      );
      soSendMessage:
      (
        Msg: UINT;
      );
  end;

const
  _DbgMemListLength = ($FFFFF div SizeOf(TDbgMemInfo));
  _DbgSyncObjsListLength = ($FFFFF div SizeOf(TDbgSyncObjsInfo));

type
  PDbgMemInfoList = ^TDbgMemInfoList;
  TDbgMemInfoList = array[0.._DbgMemListLength - 1] of TDbgMemInfo;

  PDbgMemInfoListBuf = ^TDbgMemInfoListBuf;
  TDbgMemInfoListBuf = record
    Count: Integer;
    DbgMemInfoList: PDbgMemInfoList;
    DbgPointIdx: Cardinal;
  end;

  PDbgSyncObjsInfoList = ^TDbgSyncObjsInfoList;
  TDbgSyncObjsInfoList = array[0.._DbgSyncObjsListLength - 1] of TDbgSyncObjsInfo;

  PDbgSyncObjsInfoListBuf = ^TDbgSyncObjsInfoListBuf;
  TDbgSyncObjsInfoListBuf = record
    Count: Integer;
    DbgSyncObjsInfoList: PDbgSyncObjsInfoList;
    DbgPointIdx: Cardinal;
  end;

implementation

end.
