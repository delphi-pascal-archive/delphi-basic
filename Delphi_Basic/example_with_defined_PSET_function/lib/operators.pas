unit operators;

interface
    uses Classes, Namespace, OutStream;

    Type

    PAbstractOperator = ^TAbstractOperator;
    TAbstractOperator = object
        OutStream : TOutStream;
        IDs       : PIDHolder;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream);
        Procedure   Run;  virtual; abstract;
        Destructor  Done; virtual; abstract;
    end;

    PEvaluateOperator = ^TEvaluateOperator;
    TEvaluateOperator = object(TAbstractOperator)
        IDName, IDValue : String;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream; _IDName, _IDValue : String);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

    TOpArr = array[0..65536] of PAbstractOperator;

    PBlockOperator = ^TBlockOperator;
    TBlockOperator = object(TAbstractOperator)
        Size        : LongInt;
        Count       : LongInt;
        operators   : ^TOpArr;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream);
        Procedure   AddOperator(_Operator : PAbstractOperator);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

implementation
    
    constructor TAbstractOperator.Init(_IDs : PIDHolder; _OutStream : TOutStream);
    begin
        IDs := _IDs;
        OutStream := _OutStream;
    end;

{------------------------------------------------------}

    constructor TEvaluateOperator.Init(_IDs : PIDHolder; _OutStream : TOutStream; _IDName, _IDValue : String);
    begin
        inherited Init(_IDs, _OutStream);
        IDName  := _IDName;
        IDValue := _IDValue;
    end;

    procedure  TEvaluateOperator.Run;
    begin
        IDs^.AddVar(IDName, IDValue);
    end;
    
    destructor TEvaluateOperator.Done;
    begin
    end;

{------------------------------------------------------}
    constructor TBlockOperator.Init(_IDs: PIDHolder; _OutStream : TOutStream);
    begin
        inherited Init(_IDs, _OutStream);
        Count := 0;
        Size := 1;
        GetMem(Operators, Size * sizeof(pointer));
    end;

    procedure   TBlockOperator.AddOperator(_Operator : PAbstractOperator);
    var i : LongInt;
    begin
            if Count >= Size then
            begin
                Size := Size * 2;
                ReallocMem(Operators, Size * sizeof(pointer));
            end;

            inc(Count);
            Operators^[Count-1] := _Operator;
    end;
    
    destructor TBlockOperator.Done;
    var i : LongInt;
    begin
        for i := 0 to Count-1 do
        Dispose(Operators^[i], Done);

        FreeMem(Operators, Size);
    end;

    procedure TBlockOperator.Run;
    var i : LongInt;
    begin
        for i := 0 to Count-1 do
        Operators^[i]^.Run;
    end;

end.