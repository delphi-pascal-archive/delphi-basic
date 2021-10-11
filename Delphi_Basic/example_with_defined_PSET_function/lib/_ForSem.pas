Unit _ForSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace;

    Type 
    PForOperator = ^TForOperator;
    TForOperator = object(TAbstractOperator)
        Value,
        FromCond,
        ToCond,
        StepCond : PIdentifier;
        Increment: Boolean;
        Body     : PAbstractOperator;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream; _Value, _FromCond, _ToCond, _StepCond : PIdentifier; _Increment : Boolean; _Body : PAbstractOperator);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

    PForSemantic = ^TForSemantic;
    TForSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses qbsem, _DataSem, SysUtils;

    Constructor TForOperator.Init(_IDs : PIDHolder; _OutStream : TOutStream; _Value, _FromCond, _ToCond, _StepCond : PIdentifier; _Increment : Boolean; _Body : PAbstractOperator);
    Begin
        Inherited Init(_IDs, _OutStream);
        Value := _Value;
        FromCond := _FromCond;
        ToCond := _ToCond;
        StepCond := _StepCond;
        Increment := _Increment;
        Body := _Body;
    End;

    Procedure TForOperator.Run;
    Begin
        Value^.Value := IntToStr(FromCond^.IValue);
        
        if Increment then
        while Value^.IValue <= ToCond^.IValue do
        begin
            Body^.Run;
            Value^.Value := IntToStr(Value^.IValue + StepCond^.IValue);
        end else
        while Value^.IValue >= ToCond^.IValue do
        begin
            Body^.Run;
            Value^.Value := IntToStr(Value^.IValue - StepCond^.IValue);
        end;
    End;

    Destructor TForOperator.Done;
    Begin                       
//        Dispose(IDs, Done);
    End;

{------------------------------------------------------}

    constructor TForSemantic.Init;
    begin
        inherited Init(_MyParser, _OutStream, _IDs);
    end;


    function    TForSemantic.Process : PAbstractOperator;
    var
        ValueSem: PDataSemantic;
        Value, FromCond, ToCond, StepCond : PIdentifier;

        ValueName   : String;

        Body        : PAbstractOperator;
        BodySem     : PQBSemantic;
        Increment   : Boolean;
    begin
        Result := nil;

        Value := nil;
        ValueName := MyParser^.NextIs(DEF_ID);

        if ValueName = '' then ErrorString := 'Variable expected but: ' +MyParser^.NextIs(DEF_WORD) +' found' else
        begin
            Value := IDs^.GetVar(ValueName);
            If Value = nil then Value := IDs^.AddVar(ValueName, '');
            Value^.MyType := TYPE_NUMERIC;
        end;

        New(ValueSem, Init(MyParser, OutStream, IDs));
        if Value <> nil then
            if MyParser^.NextIs('=') <> '' then
            begin
                FromCond := PIdentifier(ValueSem^.Process);
                if FromCond <> nil then
                begin
                    if MyParser^.NextIs('TO') <> '' then Increment := True else
                    if MyParser^.NextIs('DOWNTO') <> '' then Increment := False else
                    ErrorString := '"TO" or "DOWNTO" expected';

                    if ErrorString <> '"TO" or "DOWNTO" expected' then
                    Begin
                        ToCond := PIdentifier(ValueSem^.Process);
                        if ToCond <> nil then
                        begin
                            if Increment then 
                                StepCond := New(PIdentifier, Init('', '1', TYPE_NUMERIC)) else
                                StepCond := New(PIdentifier, Init('', '-1', TYPE_NUMERIC));
                            
                            New(BodySem, Init(MyParser, OutStream, IDs, 'endf'));
                            Body := BodySem^.Process;
                            if Body <> nil then
                                Result := New(PForOperator, Init(IDs, OutStream, Value, FromCond, ToCond, StepCond, Increment, Body))
                            else ErrorString := 'Body expected! ' +BodySem^.GetError;
                            Dispose(BodySem, Done);
                        end
                        else ErrorString := 'Target value expected! ' +ValueSem^.GetError;
                    End;
                end
                else ErrorString := 'Source value expected! ' +ValueSem^.GetError;
            end 
            else ErrorString := '= expected. '
        else ErrorString := 'Value expected! '+ValueSem^.GetError;

(*
            
            New(BodySem, Init(MyParser, OutStream, IDs, 'ENDW'));
            Body := BodySem^.Process;
            if Body <> nil then
            Result := New(PWhileOperator, Init(IDs, OutStream, Condition, Body)) else
            ErrorString := 'Body expected! ' + BodySem^.GetError;
            
            Dispose(BodySem, Done);
        end else*)

        Dispose(ValueSem, Done);
    end;

end.