Unit QBConsts;

Interface
    Uses Namespace;

    Procedure AddConsts(IDS: PIDHolder);

Implementation
    
    Procedure AddConsts(IDS: PIDHolder);
    begin
        IDS^.AddVar('EOLN','<br>'#13#10);
        IDS^.AddVar('TRUE','TRUE');
        IDS^.AddVar('FALSE','FALSE');
        IDS^.AddVar('TABLE_START','<TABLE cellspacing="10">');
        IDS^.AddVar('TABLE_END','</TABLE>');
        IDS^.AddVar('TABLE_ROW_START','<TR>');
        IDS^.AddVar('TABLE_ROW_END','</TR>');
        IDS^.AddVar('TABLE_CELL_START','<TD>');
        IDS^.AddVar('TABLE_CELL_END','</TD>');
    end;

end.