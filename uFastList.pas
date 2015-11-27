unit uFastList;

interface

uses
  Classes, Sysutils;

type
  TListSorted = class(TList)
  private
    //FValues: TList;

    // Allow duplicate objects in the
    // list of objects based on
    // compare(item1,item2) = 0
    // Default to dupIgnore (dupes ok)
    Duplicates : TDuplicates;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    constructor Create;

    // an abstract compare function
    // this should be overridden by an inheriting class
    // it should return   -1 if item1 < item2
    //                    0 if item1 = item2
    //                    1 if item1 > item2
    function Compare(Item1, Item2: Pointer; aFindMode: Boolean): Integer; virtual; abstract;

    //function AddKeyValue(Key, Value: Pointer): Integer;
    function Add(Item: Pointer): Integer;

    // returns the index of Item using the compare method to find
    // the object
    // note: if more than one object matches using the compare method,
    //       this does not look for the same memory address by
    //       matching the pointers, it looks for the same value
    //       ie compare method returns 0
    // then any one of those matching could be returned
    // The index returned, ranges from 0 to Count-1
    // A value of -1 indicates that no Item was found
    function FindObject(Item : Pointer) : Integer;
  end;

implementation

procedure TListSorted.AfterConstruction;
begin
  inherited;
  //FValues := TList.Create;
end;

constructor TListSorted.Create;
begin
   Duplicates := dupIgnore;
   inherited Create;
end;

destructor TListSorted.Destroy;
begin
  //FValues.Free;
  inherited;
end;

//function TListSorted.AddKeyValue(Key, Value: Pointer): Integer;
function TListSorted.Add(Item: Pointer): Integer;
var
   nCount  : Integer;
   bFound  : Boolean;
   nResult : Integer;
begin
   nCount := 0;
   bFound := False;
   // search the list of objects until we find the
   // correct position for the new object we are adding
   while (not bFound) and (nCount < Count) do
   begin
      if (Compare(Items[nCount], Item, False) >= 0) then
         bFound := True
      else
         inc(nCount);
   end;
   if (bFound) then
   begin
      if (Duplicates = dupIgnore) or (Compare(Items[nCount], Item, False) <> 0) then
      begin
         Insert(nCount,Item);
         nResult := nCount;
      end
      else
         nResult := -1;
   end
   else
      nResult := inherited Add(Item);
   Add := nResult;
end;

function TListSorted.FindObject(Item : Pointer) : Integer;
// Find the object using the compare method and
// a binary chop search
var
   nResult   : Integer;
   nLow      : Integer;
   nHigh     : Integer;
   nCompare  : Integer;
   nCheckPos : Integer;
begin
   nLow := 0;
   nHigh := Count-1;
   nResult := -1;
   Result := -1;
   // keep searching until found or
   // no more items to search
   while (nResult = -1) and (nLow <= nHigh) do
   begin
      nCheckPos := (nLow + nHigh) div 2;
      nCompare := Compare(Item, Items[nCheckPos], True);
      if (nCompare <= -1) then                // less than
        nHigh := nCheckPos - 1
      else if (nCompare >= 1) then            // greater than
      begin
        FindObject := nCheckPos; //best c.q. last c.q. lowest result
        nLow := nCheckPos + 1
      end
      else                                  // equal to
      begin
        nResult := nCheckPos;
        Exit;
      end;
   end;
   //FindObject := nCheckPos; //nResult;   best c.q. last result
end;

end.
