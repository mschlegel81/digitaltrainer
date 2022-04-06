UNIT analysisDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls,logicGates,baseGate;

TYPE

  { TanalysisForm }

  TanalysisForm = class(TForm)
    GroupBox1: TGroupBox;
    rbBinary: TRadioButton;
    rbPositive: TRadioButton;
    rb2Complement: TRadioButton;
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
      inputsGenerated:longint=0;
  FUNCTION nextInput:boolean;
    VAR k:longint=0;
    begin
      inc(inputsGenerated);
      if inputsGenerated>1000 then exit;
      if k>=length(input) then exit;

      if length(input)<9 then begin
        //Counting up in binary ;-)
        repeat
          input[k]:=not(input[k]);
          if input[k] then exit(true)
                      else inc(k);
        until k>=length(input);
      end else begin
        for k:=0 to length(input)-1 do input[k]:=(random>0.5);
      end;
      result:=false;
    end;

  FUNCTION getIoString(CONST wire:T_wireValue):string;
    VAR i:longint;
        k:int64=0;
        maxVal:int64;
    begin
      if rbBinary.checked then begin
        result:='';
        for i:=wire.width-1 downto 0 do
        case wire.bit[i] of
          tsv_true        : result+='1';
          tsv_false       : result+='0';
          tsv_undetermined: result+='?';
        end;
      end else begin
        for i:=wire.width-1 downto 0 do begin
          case wire.bit[i] of
            tsv_true        : inc(k);
            tsv_false       : begin end;
            tsv_undetermined: exit('?');
          end;
          k:=k shl 1;
        end;
        if rb2Complement.checked and (wire.width>1) then begin
          maxVal:=1 shl (wire.width-1);
          if k>maxVal then k:=maxVal-k;
        end;
        result:=intToStr(k);
      end;
    end;

  VAR i:longint;
      r,c:longint;
      w:T_wireValue;
      wIn:array of record
        v:T_wireValue;
        firstIndex:longint;
      end;
  begin
    StringGrid.rowCount:=1;
    c:=0;
    setLength(wIn,clonedGate^.numberOfInputs);
    for i:=0 to clonedGate^.numberOfInputs-1 do begin
      wIn[i].firstIndex:=c;
      wIn[i].v.width:=clonedGate^.inputWidth(i);
      c+=clonedGate^.inputWidth(i);
    end;
    setLength(input,c);
    for i:=0 to length(input)-1 do input[i]:=false;

    repeat
      clonedGate^.reset;
      for c:=0 to length(wIn)-1 do
        for i:=0 to wIn[c].v.width-1 do
          if input[wIn[c].firstIndex+i]
          then wIn[c].v.bit[i]:=tsv_true
          else wIn[c].v.bit[i]:=tsv_false;

      i:=0;
      while (i<=1000) and clonedGate^.simulateStep do inc(i);

      r:=StringGrid.rowCount;
      StringGrid.rowCount:=r+1;
      c:=0;
      for i:=0 to length(wIn)-1 do begin
        StringGrid.Cells[c,r]:=getIoString(wIn[i].v);
        inc(c);
      end;
      if i<=1000 then StringGrid.Cells[c,r]:=intToStr(i)
                 else StringGrid.Cells[c,r]:='>1000';
      inc(c);
      for i:=0 to clonedGate^.numberOfOutputs-1 do begin
        StringGrid.Cells[c,r]:=getIoString(clonedGate^.getOutput(i));
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
      for i:=0 to length(inputConnections)-1 do begin
        StringGrid.Cells[colIndex,0]:=inputConnections[i].caption;
        inc(colIndex);
      end;
      StringGrid.Cells[colIndex,0]:='Schritte bis stabil';
      inc(colIndex);
      for i:=0 to length(outputConnections)-1 do begin
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

