UNIT analysisDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls,logicGates,baseGate;

TYPE

  { TanalysisForm }

  TanalysisForm = class(TForm)
    UpdateTableButton: TButton;
    PageControl1: TPageControl;
    StringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    PROCEDURE UpdateTableButtonClick(Sender: TObject);
  private
    clonedGate:P_abstractGate;
    PROCEDURE setupTable;
  public
    PROCEDURE showForGate (CONST gate:P_abstractGate);
    PROCEDURE showForBoard(CONST board:P_circuitBoard);
  end;

VAR
  analysisForm: TanalysisForm;

IMPLEMENTATION

{$R *.lfm}

{ TanalysisForm }

PROCEDURE TanalysisForm.UpdateTableButtonClick(Sender: TObject);
  VAR input:array of boolean;
  FUNCTION nextInput:boolean;
    VAR k:longint=0;
    begin
      //Counting up in binary ;-)
      repeat
        input[k]:=not(input[k]);
        if input[k] then exit(true)
                    else inc(k);
      until k>=length(input);
      result:=false;
    end;

  VAR i:longint;
      r,c:longint;
  begin
    setLength(input,clonedGate^.numberOfInputs);
    StringGrid.rowCount:=1;
    for i:=0 to length(input)-1 do input[i]:=false;
    repeat
      clonedGate^.reset;
      for i:=0 to length(input)-1 do if input[i]
      then clonedGate^.setInput(i,tsv_true )
      else clonedGate^.setInput(i,tsv_false);

      i:=0;
      while (i<=1000) and clonedGate^.simulateStep do inc(i);

      r:=StringGrid.rowCount;
      StringGrid.rowCount:=r+1;
      c:=0;

      for i:=0 to length(input)-1 do begin
        if input[i] then StringGrid.Cells[c,r]:='1'
                    else StringGrid.Cells[c,r]:='0';
        inc(c);
      end;
      if i<=1000 then StringGrid.Cells[c,r]:=intToStr(i)
                 else StringGrid.Cells[c,r]:='>1000';
      inc(c);
      for i:=0 to clonedGate^.numberOfOutputs-1 do begin
        case clonedGate^.getOutput(i) of
          tsv_true        : StringGrid.Cells[c,r]:='1';
          tsv_false       : StringGrid.Cells[c,r]:='0';
          tsv_undetermined: StringGrid.Cells[c,r]:='?';
        end;
        inc(c);
      end;
      Application.ProcessMessages;
    until not(nextInput);
  end;

PROCEDURE TanalysisForm.setupTable;
  VAR colIndex:longint=0;
      i:longint;
  begin
    StringGrid.colCount:=clonedGate^.numberOfInputs+clonedGate^.numberOfOutputs+1;
    StringGrid.rowCount:=1;

    if (clonedGate^.gateType=gt_compound) then with P_customGate(clonedGate)^ do begin
      for i:=0 to numberOfInputs-1 do begin
        StringGrid.Cells[colIndex,0]:=inputConnections[i].caption;
        inc(colIndex);
      end;
      StringGrid.Cells[colIndex,0]:='Schritte bis stabil';
      inc(colIndex);
      for i:=0 to numberOfOutputs-1 do begin
        StringGrid.Cells[colIndex,0]:=outputConnections[i].caption;
        inc(colIndex);
      end;
    end else begin
      for i:=0 to clonedGate^.numberOfInputs-1 do begin
        StringGrid.Cells[colIndex,0]:='in '+intToStr(i);
        inc(colIndex);
      end;
      StringGrid.Cells[colIndex,0]:='Schritte bis stabil';
      inc(colIndex);
      for i:=0 to clonedGate^.numberOfOutputs-1 do begin
        StringGrid.Cells[colIndex,0]:='out '+intToStr(i);
        inc(colIndex);
      end;
    end;

  end;

PROCEDURE TanalysisForm.showForGate(CONST gate: P_abstractGate);
  begin
    clonedGate:=gate^.clone;
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
  end;

PROCEDURE TanalysisForm.showForBoard(CONST board: P_circuitBoard);
  begin
    new(P_customGate(clonedGate),create(board));
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
  end;

end.

