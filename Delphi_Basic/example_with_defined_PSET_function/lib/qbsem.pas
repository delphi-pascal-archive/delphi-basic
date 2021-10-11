unit QBSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace;

    Type
    PQBSemantic = ^TQBSemantic;
    TQBSemantic = object (TAbstractSemantic)
        StopWord : String; // слово, по нахождению которого происходит выход из семантики.
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder; _StopWord : String = '');
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses _Printsem, _LetSem, _WhileSem, _IfSem, _ForSem;

    constructor TQBSemantic.Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder; _StopWord : String = '');
    begin
        inherited Init(_MyParser, _OutStream, _IDs);
        StopWord := _StopWord;
    end;

    function    TQBSemantic.Process : PAbstractOperator;
    var Descendant : PAbstractSemantic;
        NewOp      : PAbstractOperator;
    begin
        MyParser^.FixSpaces;

        Result := nil;

        while ((StopWord = '') or (MyParser^.NextIs(StopWord) = '')) and (GetError = '') do
        begin
            MyParser^.FixSpaces;

            Descendant := nil;

            if MyParser^.NextIs('Print') <> '' then 
                Descendant := New(PPrintSemantic, Init(MyParser, OutStream, IDS))
            else if MyParser^.NextIs('Let') <> '' then 
                Descendant := New(PLetSemantic, Init(MyParser, OutStream, IDS))
            else if MyParser^.NextIs('While') <> '' then
                Descendant := New(PWhileSemantic, Init(MyParser, OutStream, IDS))
            else if MyParser^.NextIs('If') <> '' then 
                Descendant := New(PIfSemantic, Init(MyParser, OutStream, IDS))
            else if MyParser^.NextIs('For') <> '' then 
                Descendant := New(PForSemantic, Init(MyParser, OutStream, IDS))
            ;

            if Descendant = nil then ErrorString := 'Unknown BBasic operator: '+MyParser^.NextIs(DEF_WORD) else
            Begin
                NewOp := Descendant^.Process;
                if NewOp <> nil then 
                begin
                    if Result = nil then Result := New(PBlockOperator, Init(IDs, OutStream));
                    PBlockOperator(Result)^.AddOperator(NewOp);
                end else
                begin
                    ErrorString := Descendant^.GetError;
                    if Result <> nil then Dispose(Result, Done);
                    Result := nil;
                end;
            End;

            MyParser^.FixSpaces;
        end;
    end;

end.