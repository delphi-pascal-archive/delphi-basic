Unit OutStream;

interface
    uses Classes;

    type TOutStream = class(TFileStream)
        procedure WriteS(S : String);
    end;


implementation
    
    Procedure TOutStream.WriteS(S : String);
    Begin
        if S <> '' then
        Write(S[1], Length(S));
    End;

end.