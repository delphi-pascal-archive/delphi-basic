Unit _WhileSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace;

    Type 
    PWhileOperator = ^TWhileOperator;
    TWhileOperator = object(TAbstractOperator)
        Condition: PIdentifier;
        Body     : PAbstractOperator;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream; _Condition : PIdentifier; _Body : PAbstractOperator);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

    PWhileSemantic = ^TWhileSemantic;
    TWhileSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses qbsem, _DataSem;

    Constructor TWhileOperator.Init(_IDs : PIDHolder; _OutStream : TOutStream; _Condition : PIdentifier; _Body : PAbstractOperator);
    Begin
        // ѕространоство имен у него свое-собственное.
//        New(IDs,InitFrom(_IDs));
        Inherited Init(_IDs, _OutStream);
        Condition := _Condition;
        Body := _Body;
    End;

    Procedure TWhileOperator.Run;
    Begin
        While Condition^.BValue = true do Body^.Run;
    End;

    Destructor TWhileOperator.Done;
    Begin                       
//        Dispose(IDs, Done);
    End;

{------------------------------------------------------}

    constructor TWhileSemantic.Init;
    begin
        inherited Init(_MyParser, _OutStream, _IDs);
    end;


    function    TWhileSemantic.Process : PAbstractOperator;
    var 
        ConditionSem: PDataSemantic;
        Condition   : PIdentifier;

        Body       : PAbstractOperator;
        BodySem    : PQBSemantic;


    begin
        Result := nil;
        
        New(ConditionSem, Init(MyParser, OutStream, IDs));
        Condition := PIdentifier(ConditionSem^.Process);
        if Condition <> nil then
        begin
            New(BodySem, Init(MyParser, OutStream, IDs, 'ENDW'));
            Body := BodySem^.Process;
            if Body <> nil then
            Result := New(PWhileOperator, Init(IDs, OutStream, Condition, Body)) else
            ErrorString := 'Body expected! ' + BodySem^.GetError;
            
            Dispose(BodySem, Done);
        end else
            ErrorString := 'Condition expected! '+ConditionSem^.GetError;

        Dispose(ConditionSem, Done);
    end;

end.