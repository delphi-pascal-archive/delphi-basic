unit BBSem;
interface
    uses AbstractSem, Parser, OutStream, Operators, QBSem;

    Type 
    PBBSemantic = ^TBBSemantic;
    TBBSemantic = object (TAbstractSemantic)
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream);
        Function    Process : PAbstractOperator; virtual;
    end;

implementation
    uses Namespace;

    Constructor TBBSemantic.Init;
    Begin
        inherited Init(_MyParser, _OutStream, nil);
    End;
    
    function    TBBSemantic.Process : PAbstractOperator;
    var
        QBSemantic : PQBSemantic;
        QBOperator : PAbstractOperator;
    begin
        Result := nil;
        while (not MyParser.EOF) and (GetError = '') do        
            if MyParser^.NextIsAsIs('[b]' )  <> ''   then OutStream.WriteS('<b>') else
            if MyParser^.NextIsAsIs('[/b]')  <> ''   then OutStream.WriteS('</b>') else
            if MyParser^.NextIsAsIs('[i]' )  <> ''   then OutStream.WriteS('<i>') else
            if MyParser^.NextIsAsIs('[/i]')  <> ''   then OutStream.WriteS('</i>') else
            if MyParser^.NextIsAsIs('[u]' )  <> ''   then OutStream.WriteS('<u>') else
            if MyParser^.NextIsAsIs('[/u]')  <> ''   then OutStream.WriteS('</u>') else
            if MyParser^.NextIs(DEF_EOLN)    <> ''   then OutStream.WriteS(#13#10) else
            if MyParser^.NextIsAsIs('[basic]') <> ''then 
            begin
                New(QBSemantic, Init(MyParser, OutStream, New(PIDHolder, Init), '[/basic]'));
                QBOperator := QBSemantic^.Process;
                if QBSemantic^.GetError <> '' then 
                begin
                    OutStream.WriteS(QBSemantic^.GetError);
//                    WriteLn(QBSemantic^.GetError);
                    Result := Pointer(1);
                end else
                if QBOperator <> nil then QBOperator^.Run;
            end else
            OutStream.WriteS(MyParser^.ReadChar);
    end;

end.