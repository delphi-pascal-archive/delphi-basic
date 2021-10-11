Unit _IfSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace;

    Type 
    PIfOperator = ^TIfOperator;
    TIfOperator = object(TAbstractOperator)
        Condition: PIdentifier;
        Body     : PAbstractOperator;
        ElseBody : PAbstractOperator;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream; _Condition : PIdentifier; _Body, _ElseBody : PAbstractOperator);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

    PIfSemantic = ^TIfSemantic;
    TIfSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses qbsem, _DataSem;

    Constructor TIfOperator.Init(_IDs : PIDHolder; _OutStream : TOutStream; _Condition : PIdentifier; _Body, _ElseBody : PAbstractOperator);
    Begin
        Inherited Init(_IDs, _OutStream);
        Condition := _Condition;
        Body := _Body;
        ElseBody := _ElseBody;
    End;

    Procedure TIfOperator.Run;
    Begin
        If Condition^.BValue = true then 
            if Body <> nil then Body^.Run else 
        else
            if ElseBody <> nil then ElseBody^.Run;
    End;

    Destructor TIfOperator.Done;
    Begin                       
        Dispose(Condition, Done);
        Dispose(Body, Done);
    End;

{------------------------------------------------------}

    constructor TIfSemantic.Init;
    begin
        inherited Init(_MyParser, _OutStream, _IDs);
    end;


    function    TIfSemantic.Process : PAbstractOperator;
    var 
        ConditionSem: PDataSemantic;
        Condition   : PIdentifier;

        Body       : PAbstractOperator;
        ElseBody   : PAbstractOperator;
        BodySem    : PQBSemantic;

    begin
        Result := nil;
        
        New(ConditionSem, Init(MyParser, OutStream, IDs));
        Condition := PIdentifier(ConditionSem^.Process);
        if Condition <> nil then 
        begin
            New(BodySem, Init(MyParser, OutStream, IDs, 'ENDIF'));
            Body := BodySem^.Process;
            if Body <> nil then
            begin
                if MyParser^.NextIs('ELSEIF') <> '' then
                begin
                    ElseBody := BodySem^.Process;
                    if ElseBody = nil then 
                       ErrorString := 'Alternate body expected! '+BodySem^.GetError
                    else
                       Result := New(PIfOperator, Init(IDs, OutStream, Condition, Body, ELseBody));
                end else 
                Result := New(PIfOperator, Init(IDs, OutStream, Condition, Body, nil));
            end else
            ErrorString := 'Body expected! '+BodySem^.GetError;
            
            Dispose(BodySem, Done);
        end else
            ErrorString := 'Condition expected! '+ConditionSem^.GetError;

        Dispose(ConditionSem, Done);
    end;

end.