unit Namespace;

interface
    uses Classes;

CONST
    TYPE_STRING  = 0;
    TYPE_NUMERIC = 1;
    TYPE_BOOLEAN = 2;

type

    PIdentifier = ^TIdentifier;
    TIdentifier = object
        MyType : byte;
        Value  : String;
        Name   : String;
        Constructor Init(_Name, _Value : String; _Type : byte = TYPE_STRING);
        Function    Compare(id : PIdentifier) : integer; virtual;
        Function    BValue : Boolean; virtual;
        Function    SValue : String;  virtual;
        Function    IValue : LongInt; virtual;
        Function    FValue : Double; virtual;
        Function    Verify : Boolean; virtual;
        Destructor  Done; Virtual;
    end;

    PPlusIdentifier = ^TPlusIdentifier;
    TPlusIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PMinusIdentifier = ^TMinusIdentifier;
    TMinusIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PMulIdentifier = ^TMulIdentifier;
    TMulIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PDivIdentifier = ^TDivIdentifier;
    TDivIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PMoreIdentifier = ^TMoreIdentifier;
    TMoreIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PLessIdentifier = ^TLessIdentifier;
    TLessIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PEqualIdentifier = ^TEqualIdentifier;
    TEqualIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PNotEqualIdentifier = ^TNotEqualIdentifier;
    TNotEqualIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PAndIdentifier = ^TAndIdentifier;
    TAndIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    POrIdentifier = ^TOrIdentifier;
    TOrIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PXorIdentifier = ^TXorIdentifier;
    TXorIdentifier = object(TIdentifier)
        Val1, Val2  : PIdentifier;
        Constructor Init(_Name : String; _Val1, _Val2 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;


    PSinIdentifier = ^TSinIdentifier;
    TSinIdentifier = object(TIdentifier)
        Val1: PIdentifier;
        Constructor Init(_Name : String; _Val1 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PPsetIdentifier = ^TPsetIdentifier;
    TPsetIdentifier = object(TIdentifier)
        x,y,r,g,b : PIdentifier;
        Constructor Init(_Name : String; _x,_y,_r,_g,_b : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PABSIdentifier = ^TABSIdentifier;
    TABSIdentifier = object(TIdentifier)
        Val1: PIdentifier;
        Constructor Init(_Name : String; _Val1 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    PHexIdentifier = ^THexIdentifier;
    THexIdentifier = object(TIdentifier)
        Val1: PIdentifier;
        Constructor Init(_Name : String; _Val1 : PIdentifier);
        Function    Verify : Boolean; Virtual;
        Destructor  Done; Virtual;
    end;

    TIDRecordStruct = 
    record
        Identifier : PIdentifier;
        Owner : Boolean;
    end;

    TIDArr = array[0..65536] of TIDRecordStruct;

    PIDHolder = ^TIDHolder;
    TIDHolder = object
        Size        : LongInt;
        Count       : LongInt;
        Variables   : ^TIDArr;
        Constructor Init;
        Constructor InitFrom(IDs : PIDHolder);
        Function    AddVar(Name, Value : String; _Type : byte = TYPE_STRING) : PIdentifier;
        Function    AddUniqueVar(Value : String; _Type : byte = TYPE_STRING) : PIdentifier;
        Function    GetVar(Name: String) : PIdentifier;
        Function    GetUniqueName : String;
        Destructor  Done;
    end;

var
    PsetFunc : procedure(x,y : longint; r,g,b : byte) of object;


implementation
    uses SysUtils, QBConsts, Dialogs;

    Constructor TIdentifier.Init(_Name, _Value : String; _Type : Byte = TYPE_STRING);
    Begin
        Name  := _Name;
        Value := _Value;
        MyType:= _Type;
    End;

    Function TIdentifier.Compare(id : PIdentifier) : integer;
    begin
        if SValue = ID^.SValue then Result := 0 else Result := -1;
    end;

    Function   TIdentifier.SValue : String;
    Begin
        Verify;
        Result := Value;
    End;

    Function TIdentifier.BValue : Boolean;
    begin
        Verify;
        Result :=   (Value <> '') 
                and (Value <> 'FALSE')
                and (Value <> '0')
                ;
    end;                    

    Function TIdentifier.IValue : LongInt;
    Begin
        if MyType = TYPE_NUMERIC then
        Result := Round(StrToFloat(SValue)) else
        Result := 0;
    End;

    Function TIdentifier.FValue : Double;
    Begin
        if MyType = TYPE_NUMERIC then
        Result := StrToFloat(SValue) else Result := 0;
    End;

    Function   TIdentifier.Verify : Boolean;
    Begin
        Result := True;
    End;

    Destructor TIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TPlusIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TPlusIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                Value := FloatToStr(StrToFloat(Val1^.SValue) + StrToFloat(Val2^.SValue));
           end else
                Value := Val1^.SValue + Val2^.SValue;

        Result := True;
    end;                    

    Destructor TPlusIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TMinusIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TMinusIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                Value := FloatToStr(StrToFloat(Val1^.SValue) - StrToFloat(Val2^.SValue));
           end else
                Value := ''; //exception

        Result := True;
    end;                    

    Destructor TMinusIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TMulIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TMulIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                Value := FloatToStr(StrToFloat(Val1^.SValue) * StrToFloat(Val2^.SValue));
           end else
                Value := ''; //exception

        Result := True;
    end;                    

    Destructor TMulIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TDivIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TDivIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                Value := FloatToStr(StrToFloat(Val1^.SValue) / StrToFloat(Val2^.SValue));
           end else
                Value := ''; //exception

        Result := True;
    end;                    

    Destructor TDivIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TMoreIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TMoreIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                if StrToFloat(Val1^.SValue) > StrToFloat(Val2^.SValue) then 
                     Value := 'TRUE' 
                else Value := 'FALSE';
           end else
                Value := ''; //exception

        Result := True;
    end;                    

    Destructor TMoreIdentifier.Done;
    Begin
    End;

{---------------------------------}
{---------------------------------}

    Constructor TLessIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
            MyType := TYPE_NUMERIC else
            MyType := TYPE_STRING;
    End;

    Function TLessIdentifier.Verify : Boolean;
    begin
        if (Val1^.MyType = TYPE_NUMERIC) and
           (Val2^.MyType = TYPE_NUMERIC) then
           begin
                if StrToFloat(Val1^.SValue) < StrToFloat(Val2^.SValue) then 
                     Value := 'TRUE' 
                else Value := 'FALSE';
           end else
                Value := ''; //exception

        Result := True;
    end;                    

    Destructor TLessIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TEqualIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        MyType := TYPE_BOOLEAN;
    End;

    Function TEqualIdentifier.Verify : Boolean;
    begin
        if (Val1^.Compare(Val2) = 0) then Value := 'TRUE' else Value := 'FALSE';
        Result := True;
    end;                    

    Destructor TEqualIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TNotEqualIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        MyType := TYPE_BOOLEAN;
    End;

    Function TNotEqualIdentifier.Verify : Boolean;
    begin
        if (Val1^.Compare(Val2) = 0) then Value := 'FALSE' else Value := 'TRUE';
        Result := True;
    end;                    

    Destructor TNotEqualIdentifier.Done;
    Begin
    End;


{---------------------------------}

    Constructor TAndIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        MyType := TYPE_BOOLEAN;
    End;

    Function TAndIdentifier.Verify : Boolean;
    begin
        if Val1^.BValue and Val2^.BValue then Value := 'TRUE' else Value := 'FALSE';
        Result := true;
    end;                    

    Destructor TAndIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TOrIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        MyType := TYPE_BOOLEAN;
    End;

    Function TOrIdentifier.Verify : Boolean;
    begin
        if Val1^.BValue or Val2^.BValue then Value := 'TRUE' else Value := 'FALSE';
        Result := true;
    end;                    

    Destructor TOrIdentifier.Done;
    Begin
    End;


{---------------------------------}

    Constructor TXorIdentifier.Init(_Name : String; _Val1, _Val2 : PIdentifier);
    Begin
        Inherited Init(_Name, '');
        Val1 := _Val1;
        Val2 := _Val2;
        MyType := TYPE_BOOLEAN;
    End;

    Function TXorIdentifier.Verify : Boolean;
    begin
        if Val1^.BValue xor Val2^.BValue then Value := 'TRUE' else Value := 'FALSE';
        Result := true;
    end;                    

    Destructor TXorIdentifier.Done;
    Begin
    End;

{---------------------------------}

    Constructor TSinIdentifier.Init(_Name : String; _Val1 : PIdentifier);
    begin
        Inherited Init(_Name, '', TYPE_NUMERIC);
        Val1 := _Val1;
    end;

    Function    TSinIdentifier.Verify : Boolean;
    begin
        if Val1^.MyType = TYPE_NUMERIC then
        Value := FloatToStr(Sin(Val1^.FValue)) else Value := '0';
    end;

    Destructor  TSinIdentifier.Done; 
    begin
    end;

{---------------------------------}

    Constructor TAbsIdentifier.Init(_Name : String; _Val1 : PIdentifier);
    begin
        Inherited Init(_Name, '', TYPE_NUMERIC);
        Val1 := _Val1;
    end;

    Function    TAbsIdentifier.Verify : Boolean;
    begin
        if Val1^.MyType = TYPE_NUMERIC then
        Value := FloatToStr(Abs(Val1^.FValue)) else Value := '0';
    end;

    Destructor  TAbsIdentifier.Done; 
    begin
    end;

{---------------------------------}

    Constructor THexIdentifier.Init(_Name : String; _Val1 : PIdentifier);
    begin
        Inherited Init(_Name, '', TYPE_NUMERIC);
        Val1 := _Val1;
    end;

    Function    THexIdentifier.Verify : Boolean;
    Var Val : LongInt;
    begin
        
        Val := Val1^.IValue;
        if Val < 0 then Val := 0;
        if Val > 255 then Val := 255;
        
        case Val mod 16 of
            0..9: Value := IntToStr(Val mod 16);
            10: Value   := 'A';
            11: Value   := 'B';
            12: Value   := 'C';
            13: Value   := 'D';
            14: Value   := 'E';
            15: Value   := 'F';
            else Value  := '0';
        end;

        case Val div 16 of
            0..9: Value := IntToStr(Val div 16) + Value;
            10: Value   := 'A' + Value;
            11: Value   := 'B' + Value;
            12: Value   := 'C' + Value;
            13: Value   := 'D' + Value;
            14: Value   := 'E' + Value;
            15: Value   := 'F' + Value;
            else Value  := '0' + Value; 
        end;
        
        Result := true;
    end;

    Destructor  THexIdentifier.Done; 
    begin
    end;

{---------------------------------}

    
    constructor TIDHolder.Init;
    begin
        Count := 0;
        Size := 1;
        GetMem(variables, Size * sizeof(TIdRecordStruct));
        QBConsts.AddConsts(@Self);
    end;

    constructor TIDHolder.InitFrom(IDs : PIDHolder);
    var i : longint;
    begin
        for i := 0 to IDs^.Count-1 do
        begin
            AddVar('','');
            Dispose(Variables^[Count-1].Identifier, Done);
            Variables^[Count-1].Identifier := IDs^.Variables[i].Identifier;
            Variables^[Count-1].Owner:= false;
        end;
    end;

    function    TIDHolder.AddVar(Name, Value : String; _Type : byte = TYPE_STRING) : PIdentifier;
    var i : LongInt;
        Res : boolean;
    begin
        Res := false;
        for i := 0 to Count-1 do
        if Variables^[i].Identifier^.Name = Name then
        begin
            Variables^[i].Identifier^.Value  := Value;
            Variables^[i].Identifier^.MyType := _Type;
            Res := true;
            Result := Variables^[i].Identifier;
        end;

        if not res then
        begin
            if Count >= Size then
            begin
                Size := Size * 2;
                ReallocMem(Variables, Size * sizeof(TIdRecordStruct));
            end;

            inc(Count);
            Variables^[Count-1].Identifier := New(PIdentifier, Init(Name, Value));
            Variables^[Count-1].Owner := True;
            Variables^[Count-1].Identifier^.MyType := _Type;
            
            Result := Variables^[Count-1].Identifier;
        end;
    end;
    
    function    TIDHolder.GetVar(Name: String) : PIdentifier;
    var i : LongInt;
        Res : boolean;
    begin
        Result := nil;

        for i := 0 to Count-1 do
        if Variables^[i].Identifier^.Name = Name then
        Result := Variables^[i].Identifier;
    end;

    destructor TIDHolder.Done;
    var i : LongInt;
    begin
        for i := 0 to Count-1 do
        if Variables^[i].Owner then
        Dispose(Variables^[i].Identifier, Done);

        FreeMem(Variables, Size * sizeof(TIdRecordStruct));
    end;

    function    TIDHolder.AddUniqueVar(Value : String; _Type : byte = TYPE_STRING) : PIdentifier;
    Var UniqueName : String;
    Begin
        UniqueName := GetUniqueName;
        AddVar(UniqueName, Value, _Type);
        Result := GetVar(UniqueName);
    End;

    function    TIDHolder.GetUniqueName : String;
    var i : Word;
    begin
        i := 0;
        While GetVar('UNIQUE'+IntToStr(i)) <> nil do inc(i);
        Result := 'UNIQUE'+IntToStr(i);
    end;

{ TPsetIdentifier }

destructor TPsetIdentifier.Done;
begin

end;

constructor TPsetIdentifier.Init(_Name : String; _x,_y,_r,_g,_b : PIdentifier);
begin
        Inherited Init(_Name, '', TYPE_NUMERIC);
        x := _x;
        y := _y;
        r := _r;
        g := _g;
        b := _b;
end;

function TPsetIdentifier.Verify: Boolean;
var _r,_g,_b : byte;
begin

  _r := byte(round(strtofloat(r.Value)));
  _g := byte(round(strtofloat(g.Value)));
  _b := byte(round(strtofloat(b.Value)));
  PsetFunc(StrToInt(x.Value), StrToInt(y.Value), _r,_g,_b);

//  PsetFunc(StrToInt(x.Value), StrToInt(y.Value), StrToInt(r.Value), StrToInt(g.Value), StrToInt(b.Value));
end;

end.