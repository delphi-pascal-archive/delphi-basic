Unit _DataSem;

interface
    uses AbstractSem, Parser, OutStream, Operators, Namespace, SysUtils;

    Type 
    PDataSemantic = ^TDataSemantic;
    TDataSemantic = object (TAbstractSemantic)
        AllowEQ : Boolean;
        Constructor Init(_MyParser : PParser; _OutStream : TOutStream; _IDS : PIDHolder; _AllowEQ : Boolean = true);
        Function    Process : PAbstractOperator; virtual;
        Function    ProcessWithRank(Rank : byte) : PIdentifier; virtual;
    end;

implementation

    Constructor TDataSemantic.Init(_MyParser : PParser; _OutStream : TOutStream; _IDS : PIDHolder; _AllowEQ : Boolean = true);
    Begin
        AllowEQ := _AllowEQ;
        inherited Init(_MyParser, _OutStream, _IDs);
    End;

    Function TDataSemantic.Process : PAbstractOperator;
    begin
        Result := PAbstractOperator(ProcessWithRank(0));
    end;

    Function TDataSemantic.ProcessWithRank(Rank : Byte) : PIdentifier;
    var NumValue, StrValue,IDName   : String;
        ID1, ID2                    : PIdentifier;
    begin
        Result := nil;

        case Rank of
        0:  Begin // =, <, >, <>, and, or, xor
                ID1 := ProcessWithRank(Rank+1);
                if ID1 <> nil then
                    if MyParser^.NextIs('=') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PEqualIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else
                    
                    if MyParser^.NextIs('>') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PMoreIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    if MyParser^.NextIs('<>') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PNotEqualIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    if MyParser^.NextIs('<') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PLessIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    if MyParser^.NextIs('and') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PAndIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    if MyParser^.NextIs('or') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(POrIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    if MyParser^.NextIs('xor') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PXorIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 

                    Result := ID1
                else Result := nil;
            End;

        1:  Begin // 
                ID1 := ProcessWithRank(Rank+1);
(*                if ID1 <> nil then

                    if MyParser^.NextIs('and') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PAndIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 
                                 *)
                    Result := ID1
(*                else Result := nil;*)
            End;

        2:  Begin // +, -
                ID1 := ProcessWithRank(Rank+1);
                if ID1 <> nil then

                    if MyParser^.NextIs('+') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PPlusIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 
                    
                    if MyParser^.NextIs('-') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PMinusIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 
                    
                    Result := ID1
                else Result := nil;
            End;

        3:  Begin // *, /
                ID1 := ProcessWithRank(Rank+1);
                if ID1 <> nil then

                    if MyParser^.NextIs('*') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PMulIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 
                    
                    if MyParser^.NextIs('/') <> '' then
                    Begin
                        ID2 := ProcessWithRank(Rank);
                        if ID2 <> nil then
                        Result := New(PDivIdentifier, Init('', ID1, ID2)) else
                        Begin
                            Result := nil;
                            Dispose(ID1, Done);
                        End;
                    End else 
                    
                    Result := ID1
                else Result := nil;
            End;

        4:  Begin // ()
                if MyParser^.NextIs('(') <> '' then
                begin
                    Result := ProcessWithRank(0);
                    If Result <> nil then
                        if MyParser^.NextIs(')') = '' then 
                        begin
                            Dispose(Result, Done);
                            Result := nil;
                            ErrorString := '")" expected';
                        end;
                end else

                if MyParser^.NextIs('SIN') <> '' then
                begin
                    if MyParser^.NextIs('(') <> '' then
                    Begin
                        ID1 := ProcessWithRank(0);
                        if ID1 <> nil then
                            if MyParser^.NextIs(')') <> '' then
                                Result := New(PSinIdentifier, Init('', ID1)) else
                                ErrorString := '")" expected' else
                            ErrorString := 'Argument extected; '+ErrorString;
                    End else ErrorString := '"(" expected';
                end else
                
                if MyParser^.NextIs('ABS') <> '' then
                begin
                    if MyParser^.NextIs('(') <> '' then
                    Begin
                        ID1 := ProcessWithRank(0);
                        if ID1 <> nil then
                            if MyParser^.NextIs(')') <> '' then
                                Result := New(PABSIdentifier, Init('', ID1)) else
                                ErrorString := '")" expected' else
                            ErrorString := 'Argument extected; '+ErrorString;
                    End else ErrorString := '"(" expected';
                end else
                
                Result := ProcessWithRank(Rank+1);
            End;

        5:  Begin // identifiers
                ID1 := nil;

                NumValue := MyParser^.NextIs(DEF_NUMERIC);
                if NumValue <> '' then ID1 := IDs^.AddUniqueVar(NumValue, TYPE_NUMERIC);

                if ID1 = nil then
                begin
                    StrValue := MyParser^.NextIs(DEF_STRING);
                    if StrValue <> '' then 
                        ID1 := IDs^.AddUniqueVar(StrValue, TYPE_STRING);

                    if ID1 = nil then
                    begin
                        IDName := MyParser^.NextIs(DEF_ID);
                        if IDName <> '' then
                        begin
                            ID1 := IDs^.GetVar(IDName);
                            if ID1 = nil then ErrorString := 'Unknown Identifier: ' + IDName;
                        end else
                            ErrorString := 'Identifier expected.';
                    end;
                end;

                Result := ID1;
            End;
        end;
    end;



end.