{ TCollectList<T> }

function TCollectList<T>.Add: PData;
var
  Idx: Cardinal;
  Seg, Offset: Integer;
begin
  Idx := Count;
  IndexToSegment(Idx, Seg, Offset);
  CheckSeg(Seg);
  FCount := Idx + 1;

  Result := @FSegList[Seg][Offset];

  FillChar(Result^, SizeOf(T), 0);
end;

procedure TCollectList<T>.CheckSeg(const Seg: Integer);
begin
  if Length(FSegList) <= Seg then
  begin
    SetLength(FSegList, Seg + 1);
    SetLength(FSegList[Seg], FSegSize);
  end;
end;

procedure TCollectList<T>.Clear;
begin
  FCount := 0;
  SetLength(FSegList, 0);
end;

constructor TCollectList<T>.Create;
begin
  inherited;

  FCount := 0;
  FLock := TCriticalSection.Create;
  FSegSize := _SEGMENT_SIZE div SizeOf(T);
  SetLength(FSegList, 0);
end;

destructor TCollectList<T>.Destroy;
begin
  Clear;

  FreeAndNil(FLock);

  inherited;
end;

function TCollectList<T>.GetItem(const Index: Cardinal): PData;
var
  Seg, Offset: Integer;
begin
  if IndexToSegment(Index, Seg, Offset) then
    Result := @FSegList[Seg][Offset]
  else
    RaiseError(@EIndexError, [Index]);
end;

function TCollectList<T>.IndexToSegment(const Index: Cardinal; var Seg, Offset: Integer): Boolean;
begin
  Result := Index < Count;

  Seg := Index div FSegSize;
  Offset := Index mod FSegSize;
end;

procedure TCollectList<T>.RaiseError(Msg: PString; const Args: Array of const);
begin
  raise TCollectListError.CreateFmt(Msg^, Args);
end;

procedure TCollectList<T>.Lock;
begin
  FLock.Enter;
end;

procedure TCollectList<T>.UnLock;
begin
  FLock.Leave;
end;
