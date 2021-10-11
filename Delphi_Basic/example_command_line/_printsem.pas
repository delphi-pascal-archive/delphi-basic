unit _PrintSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace;

    Type 
    PPrintOperator = ^TPrintOperator;
    TPrintOperator = object(TAbstractOperator)
        Variable   : PIdentifier;
//        Parameters : Array[1..5] of PIdentifier;
        Constructor InitID(_IDs : PIDHolder; _OutStream : TOutStream; _ID : PIdentifier);
        Procedure   Run;  virtual;
//        Procedure   SetParam(ParamName : String; Param : PIdentifier);
        Destructor  Done; virtual;
    end;

{------------------------------------------------------}

    PPrintSemantic = ^TPrintSemantic;
    TPrintSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDs : PIDHolder);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses _DataSem;

    constructor TPrintOperator.InitID(_IDs : PIDHolder; _OutStream : TOutStream; _ID : PIdentifier);
    begin
        inherited Init(_IDs, _OutStream);
        Variable := _ID;
    end;

(*    procedure   TPrintOperator.SetParam(ParamName : String; Param : PIdentifier);
    begin
        if ParamName = 'R' then
        Parameters[1] := Param;

        if ParamName = 'G' then
        Parameters[2] := Param;

        if ParamName = 'B' then
        Parameters[3] := Param;

        if ParamName = 'FACE' then
        Parameters[4] := Param;

        if ParamName = 'SIZE' then
        Parameters[5] := Param;
    end;
  *)

(*    constructor TPrintOperator.InitText(_IDs : PCommonIDs; _OutStream : TOutStream; _Symbols : String);
    begin
        inherited Init(_IDs, _OutStream);
        Symbols := _Symbols;
        Variable := nil;
    end;*)

    procedure TPrintOperator.Run;
    begin
        OutStream.WriteS(Variable^.SValue);
    end;

    destructor TPrintOperator.Done;
    begin
    end;

{------------------------------------------------------}

    constructor TPrintSemantic.Init;
    begin
        inherited Init(_MyParser, _OutStream, _IDs);
    end;

    function    TPrintSemantic.Process : PAbstractOperator;
    var Descendant : PDataSemantic;
        PrintVal   : PIdentifier;
        NextOp     : PAbstractOperator;

        PrintProp  : PIdentifier;

        FontProperties : Boolean;
        Error          : Boolean;

        Procedure InsertText(Str : String);
        Begin
            PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, New(PIdentifier, Init('',Str)))));
        End;

    begin
        Result := New(PBlockOperator, Init(IDs, OutStream));

        MyParser^.FixSpaces;
        New(Descendant, Init(MyParser, OutStream, IDs));

        Error := false;

        FontProperties := MyParser^.NextIs('[') <> '';
        if FontProperties then
        begin
            InsertText('<font ');
            
            if MyParser^.NextIs('SIZE') <> '' then
            begin
                if MyParser^.NextIs('=') <> '' then
                begin
                    PrintProp := PIdentifier(Descendant^.Process);
                    if PrintProp = nil then 
                        begin
                            ErrorString := 'Size value expected. ' + Descendant^.GetError;
                            Error := True;
                        end 
                    else
                        begin
                            InsertText('size="');
                            PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, PrintProp)));
                            InsertText('" ');
                        end;
                end else 
                begin
                    ErrorString := '"=" expected';
                    Error := true;
                end;
            end;

            if MyParser^.NextIs('FACE') <> '' then
            begin
                if MyParser^.NextIs('=') <> '' then
                begin
                    PrintProp := PIdentifier(Descendant^.Process);
                    if PrintProp = nil then 
                        begin
                            ErrorString := 'Face property expected (ex. "arial"). ' + Descendant^.GetError;
                            Error := True;
                        end 
                    else
                        begin
                            InsertText('face="');
                            PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, PrintProp)));
                            InsertText('" ');
                        end;
                end else 
                begin
                    ErrorString := '"=" expected';
                    Error := true;
                end;
            end;

            if MyParser^.NextIs('Color') <> '' then
            begin
                if MyParser^.NextIs('=') <> '' then
                begin
                    PrintProp := PIdentifier(Descendant^.Process);
                    if PrintProp = nil then
                        begin
                            ErrorString := '1-st color property expected. ' + Descendant^.GetError;
                            Error := True;
                        end
                    else
                        begin
                            InsertText('Color="#');
                            PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, New(PHexIdentifier, Init('', PrintProp)) )));
                            PrintProp := PIdentifier(Descendant^.Process);
                               if PrintProp = nil then
                                   begin
                                       ErrorString := '2-nd color property expected. ' + Descendant^.GetError;
                                       Error := True;
                                   end
                               else
                                   begin
                                        PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, New(PHexIdentifier, Init('', PrintProp)) )));
                                        PrintProp := PIdentifier(Descendant^.Process);
                                        if PrintProp = nil then
                                            begin
                                                ErrorString := '3-rd color property expected. ' + Descendant^.GetError;
                                                Error := True;
                                            end
                                        else
                                            PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, New(PHexIdentifier, Init('', PrintProp)) )));
                                   end;

                            InsertText('"');
                        end;
                end else
                begin
                    ErrorString := '"=" expected';
                    Error := true;
                end;
            end;

            if MyParser^.NextIs(']') = '' then
            begin
                ErrorString := '"]" expected';
                Error := true;
            end;

            InsertText('>');
        end;

        if Error then
        begin
            Dispose(Result, Done);
            Result := nil;
        end else
        begin
            PrintVal := PIdentifier(Descendant^.Process);
            if PrintVal <> nil then
            begin
                PBlockOperator(Result)^.AddOperator(new(PPrintOperator, InitID(IDs, OutStream, PrintVal)));
                If MyParser^.NextIs(',') <> '' then
                begin
                    NextOp := Process;
                    if NextOp <> nil then 
                        PBlockOperator(Result)^.AddOperator(NextOp) else
                    begin
                        Dispose(Result, Done);
                        Result := nil;
                    end;
                end;
            end else
            begin
                Dispose(Result);
                Result := nil;
                ErrorString := Descendant^.ErrorString;        
            end;

            if (Result <> nil) and FontProperties then InsertText('</font>');
        end;

    end;

end.