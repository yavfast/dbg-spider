unit DbgHookTypes;

interface

const
  DBG_EXCEPTION = $0EEDFFF0;

type
  TDbgInfoType = (dstUnknown = 0, dstThreadInfo, dstGetMem, dstFreeMem, dstMemInfo, dstPerfomance);

implementation

end.
