Unit Parser;
interface
    uses Classes, SysUtils;

Const 
    DEF_ID      = 0; // Слово, состоящее из [0..9+A..Z+_] и начинающееся с [A..Z+_]
    DEF_SPACE   = 1; // Ничего нет (пробел, табуляция, перенос строки или конец файла).
    DEF_WORD    = 2; // Получить слово до пробела
    DEF_NUMERIC = 3; // Число
    DEF_EOLN    = 4; // перенос строки
    DEF_STRING  = 5; // NextIs c этим параметром возвращает строку от позиции до '"'
    DEF_DELIMITER = 6; // Далее следует пробел, знаки + - * / и т.д. Следующий символ не поедается.

    ID_BEGINNING = ['A'..'Z','_'];
    ID_BODY      = ['0'..'9','A'..'Z','_'];

    ID_DELIMITERS= ['+' , '-' , '*' , '/' , '<' , '>' , ',', '(', ')', '=', '[', ']' ];
                    {|}   {|}   {|}   {|}   {|}   {|}   {|}  {|}  {|}  {|}  {|}  {|}


type 

    PParser = ^TParser;
    TParser = object
        Stream  : TFileStream;
        Error   : String;
        constructor Init(FName : string);
        function    GetError : string;
        function    NextIs(Lexem : string; DoFixSpaces : Boolean = true) : String; overload;
        function    NextIs(Kind : byte) : String; overload;
        function    NextIsAsIs(Lexem : String) : String;
        function    ReadChar : Char;
        Function    EOF : Boolean;
        procedure   FixSpaces;
        destructor  Done;
    end;

implementation
    
    constructor TParser.Init(FName : String);
    begin
        Error := '';
        Stream := TFileStream.Create(FName, fmOpenRead);
        Stream.Seek(0,0);
    end;

    procedure   TParser.FixSpaces;
    var OldPosition : int64;
        buf     : byte;

        Fixed   : boolean;
        
    begin
        Fixed := false;

        OldPosition := Stream.Position;
        Stream.Read(buf, 1);
        case buf of
            $20:    Fixed := true; // space
            $09:    Fixed := true; // tab
            $0D:    begin          // end of line
                        Stream.Read(buf, 1);
                        Fixed := (buf = $0A);
                    end;
        end;

        if Fixed then FixSpaces else Stream.Seek(OldPosition, 0);
    end;

    function    TParser.GetError : string;
    begin
        result := '';
    end;
    
    function    TParser.NextIs(Lexem : string; DoFixSpaces : Boolean = true) : String;
    var OldPosition : int64;
        buf         : char;
        i           : LongInt;
        Success     : Boolean;

    begin
        if DoFixSpaces then FixSpaces;
        Result := '';
        Success := True;

        OldPosition := Stream.Position;

        for i := 1 to Length(Lexem) do
        begin
            Stream.Read(buf, 1);
            Success := Success and (UpCase(buf) = UpCase(Lexem[i]));
            Result := Result + UpCase(Lexem[i]);
        end;

        if not Success then 
        begin
            Stream.Seek(OldPosition, 0);
            Result := '';
        end;
    end;

    function    TParser.NextIsAsIs(Lexem : string) : String;
    var OldPosition : int64;
        buf         : char;
        i           : LongInt;
        Success     : Boolean;
    begin
        Result := '';
        Success := True;

        OldPosition := Stream.Position;
        for i := 1 to Length(Lexem) do
        begin
            Stream.Read(buf, 1);
            Success := Success and (UpCase(buf) = UpCase(Lexem[i]));
            Result := Result + UpCase(Lexem[i]);
        end;

        if not Success then 
        begin
            Stream.Seek(OldPosition, 0);
            Result := '';
        end;
    end;


    function    TParser.NextIs(Kind : byte) : String;
    var OldPosition : int64;
        buf         : byte;
        i           : LongInt;
        Success     : Boolean;

    begin
        if (Kind <> DEF_SPACE) and (Kind <> DEF_EOLN) and (Kind <> DEF_DELIMITER) then FixSpaces;
        
        if Stream.Position >= Stream.Size then Result := '' else
        case Kind of  
            DEF_SPACE:  begin
                            OldPosition := Stream.Position;
                            Stream.Read(buf, 1);
                            case buf of
                                $20:    Result := ' '; // space
                                $09:    Result := ' '; // tab
                                $0D:    begin          // end of line
                                            Stream.Read(buf, 1);
                                            if (buf = $0A) then 
                                            Result := ' ' else Result := '';
                                        end;
                                else Result := '';
                            end;
                            if Result = '' then Stream.Seek(OldPosition, 0) else FixSpaces;
                        end;

            DEF_ID:     begin
                            OldPosition := Stream.Position;
                            Stream.Read(buf, 1);
                            Result := '';
                            if not (UpCase(chr(buf)) in ID_BEGINNING) then Success := False else
                            begin
                                Success := True;
                                Result := UpCase(chr(buf));

                                while (NextIs(DEF_DELIMITER) = '') and (Success) do
                                begin
                                    Stream.Read(buf, 1);
                                    Result  := Result + UpCase(chr(buf));
                                    Success := Success and (UpCase(chr(buf)) in ID_Body);
                                end;
                            end;
                                
                            if not Success then 
                            begin
                                Result := '';
                                Stream.Seek(OldPosition, 0);
                            end
                        end;

            DEF_NUMERIC:begin
                            OldPosition := Stream.Position;
                            Stream.Read(buf, 1);
                            Result := '';
                            if not (chr(buf) in ['0'..'9']) then Success := False else
                            begin
                                Success := True;
                                Result := chr(buf);

                                while (NextIs(DEF_DELIMITER) = '') and (Success) do
                                begin
                                    Stream.Read(buf, 1);
                                    Result  := Result + chr(buf);
                                    Success := Success and (chr(buf) in ['0'..'9']);
                                end;
                            end;
                                
                            if not Success then 
                            begin
                                Result := '';
                                Stream.Seek(OldPosition, 0);
                            end
                        end;     

            DEF_STRING:begin
                            OldPosition := Stream.Position;
                            Stream.Read(buf, 1);
                            Result := '';
                            if chr(buf) <> '"' then Success := false else
                            begin
                                Success := True;
                                Stream.Read(buf, 1);

                                while (chr(buf) <> '"') and (not EOF) do
                                begin
                                    Result  := Result + chr(buf);
                                    Stream.Read(buf, 1);
                                end;
                            end;

                            if EOF then Success := false;
                                
                            if not Success then 
                            begin
                                Result := '';
                                Stream.Seek(OldPosition, 0);
                            end
                        end;     

            DEF_DELIMITER:begin
                            If NextIs(DEF_Space) <> '' then Result := ' ' else
                            If EOF Then Result := ' ' else
                            begin
                                OldPosition := Stream.Position;
                                Stream.Read(buf, 1);
                                if chr(buf) in ID_DELIMITERS then Result := chr(buf) else Result := '';
                                Stream.Seek(OldPosition, 0);
                            end;
                        end;     

            DEF_WORD:   begin
                            Result := '';
                            While NextIs(DEF_SPACE) = '' do
                            begin
                                Stream.Read(buf, 1);
                                Result := Result + UpCase(chr(buf));
                            end;
                        end;
            
            DEF_EOLN:   begin
                            OldPosition := Stream.Position;
                            Stream.Read(buf, 1);
                            if buf = $0D then
                            begin
                                Stream.Read(buf, 1);
                                if (buf = $0A) then Result := ' ' else Result := '';
                            end 
                            else Result := '';

                            if Result = '' then Stream.Seek(OldPosition, 0);
                        end;
        end; //case
    end;

    Function   TParser.EOF : boolean;
    begin
        result := Stream.Position >= Stream.Size;
    end;

    function   TParser.ReadChar : char;
    begin
        Stream.Read(Result, 1);
    end;

    destructor TParser.Done;
    begin
        Stream.Free;
    end;

end.