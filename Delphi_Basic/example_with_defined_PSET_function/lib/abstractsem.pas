Unit AbstractSem;

interface
    uses Parser, OutStream, Operators, Namespace;

    Type 
    PAbstractSemantic =  ^TAbstractSemantic;
    TAbstractSemantic = object
        MyParser  : PParser;
        OutStream : TOutStream;
        ErrorString : String;
        IDs       : PIDHolder;
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder);
        Function    Process : PAbstractOperator; virtual; abstract;
        Function    GetError: String;  virtual;
        Destructor  Done; virtual;
    end;

implementation

    Constructor TAbstractSemantic.Init;
    Begin
        ErrorString := '';
        MyParser    := _MyParser;
        OutStream   := _OutStream;
        IDs         := _IDs;
    End;

    Function    TAbstractSemantic.GetError: String;
    begin
        Result := ErrorString;
    end;


    destructor  TAbstractSemantic.Done;
    begin
    end;

end.