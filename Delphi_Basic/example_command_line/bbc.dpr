{$APPTYPE CONSOLE}
uses    Operators,
        Parser, 
        BBSem,
        OutStream, 
        Classes;

var MyParser        : PParser;
    MySemantic      : PBBSemantic;
    MyOutStream     : TOutStream;

begin
    New(MyParser, Init('input.txt'));

    MyOutStream := TOutStream.Create('output.html', fmCreate);

    MyOutStream.WriteS('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN"> '#13#10);
    MyOutStream.WriteS('<!--  (c) Korsun Viktor, 2008.   BBCode translator. --> '#13#10);
    MyOutStream.WriteS(' <HTML>                                                 '#13#10);
    MyOutStream.WriteS('     <HEAD>                                             '#13#10);
    MyOutStream.WriteS('         <TITLE> BBSem output stream </TITLE>           '#13#10);
    MyOutStream.WriteS('         <META charset="windows-1251">                  '#13#10);
    MyOutStream.WriteS('     </HEAD>                                            '#13#10);
    MyOutStream.WriteS('     <BODY BGCOLOR="#E1E1E0">                           '#13#10);
    
    New(MySemantic, Init(MyParser, MyOutStream));
    MySemantic^.Process;
    Dispose(MySemantic, Done);
    Dispose(MyParser, Done);

    MyOutStream.WriteS('     </BODY>                                            '#13#10);
    MyOutStream.WriteS('</HTML>                                                 '#13#10);

    MyOutStream.Free;
end.