unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Run: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Memo2: TMemo;
    Image1: TImage;
    Panel3: TPanel;
    Memo1: TMemo;
    procedure RunClick(Sender: TObject);
    procedure Pset(x,y : longint; r,g,b : byte);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
  uses    Operators,
          Parser,
          BBSem,
          OutStream,
          Namespace;

{$R *.dfm}

procedure TForm1.Pset(x, y: Integer; r, g, b: byte);
var i,j : longint;
begin
  Image1.Canvas.Pixels[x,y] := TColor(b shl 16 + g shl 8 + r);;
end;

procedure TForm1.RunClick(Sender: TObject);
var MyParser        : PParser;
    MySemantic      : PBBSemantic;
    SourceStream : TOutStream;
    OutStream : TOutStream;
begin
  SourceStream := TOutStream.Create;
  SourceStream.WriteS(Memo2.Lines.Text);
  SourceStream.Seek(0,0);
  Namespace.PsetFunc := Self.Pset;

  New(MyParser, Init(SourceStream));
  OutStream := TOutStream.Create;
  New(MySemantic, Init(MyParser, OutStream));
  MySemantic^.Process;
  Dispose(MySemantic, Done);
  Dispose(MyParser, Done);
  OutStream.Seek(0,0);
  Memo1.Lines.LoadFromStream(OutStream);
  OutStream.Free;


end;

end.
