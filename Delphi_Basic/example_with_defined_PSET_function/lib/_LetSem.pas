Unit _LetSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace, SysUtils;

    Type 
    PLetOperator = ^TLetOperator;
    TLetOperator = object(TAbstractOperator)
        Variable : PIdentifier;
        Value    : PIdentifier;
        Constructor Init(_IDs : PIDHolder; _OutStream : TOutStream; _Variable, _Value : PIdentifier);
        Procedure   Run;  virtual;
        Destructor  Done; virtual;
    end;

{------------------------------------------------------}

    PLetSemantic = ^TLetSemantic;
    TLetSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDS : PIDHolder);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses _DataSem;

    Constructor TLetOperator.Init;
    Begin
        Inherited Init(_IDs, _OutStream);
        Variable:= _Variable;
        Value   := _Value;
    End;
    
    Procedure   TLetOperator.Run;
    Begin
        Variable^.Value := Value^.SValue;
    End;


    Destructor  TLetOperator.Done;
    Begin
    End;

{------------------------------------------------------}
    
    Constructor TLetSemantic.Init(_MyParser : PParser; _OutStream : TOutStream; _IDS : PIDHolder);
    Begin
        inherited Init(_MyParser, _OutStream, _IDs);
    End;
    
    Function    TLetSemantic.Process : PAbstractOperator;
    var Variable, Value: PIdentifier;
        VariableName : String;
        Descendant : PDataSemantic;
    Begin
        Result := nil;
        MyParser^.FixSpaces;

        VariableName := MyParser^.NextIs(DEF_ID);

        if VariableName = '' then ErrorString := 'Variable expected but: ' +MyParser^.NextIs(DEF_WORD) +' found' else
        begin
            MyParser^.FixSpaces;
            if MyParser^.NextIs('=') = '' then ErrorString := '= expected' else
            begin
                MyParser^.FixSpaces;

                New(Descendant, Init(MyParser, OutStream, IDs));
                Value := PIdentifier(Descendant^.Process);
                
                if Value <> nil then
                    begin
                        Variable := IDs^.GetVar(VariableName);
                        If Variable = nil then Variable := IDs^.AddVar(VariableName, '');
                        Variable^.MyType := Value^.MyType;
                        Result := New(PLetOperator, Init(IDs, OutStream, Variable, Value));
                    end
                else
                    begin
                        Result := nil;
                        ErrorString := Descendant^.GetError;
                    end;
                Dispose(Descendant, Done);
            end;
        end;
    End;

End.