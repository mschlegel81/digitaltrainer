UNIT analysisDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls,logicGates,baseGate;

TYPE

  { TanalysisForm }

  TanalysisForm = class(TForm)
    ResetCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbSimOrdered: TRadioButton;
    rbBitFlip: TRadioButton;
    rbSimRandom: TRadioButton;
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
      if (inputsGenerated>1000) or (length(input)=0) then exit(false);
      if rbSimOrdered.checked then begin
        //Counting up in binary ;-)
        repeat
          input[k]:=not(input[k]);
          if input[k] then exit(true)
                      else inc(k);
        until k>=length(input);
        result:=false;
      end else if rbBitFlip.checked then begin
        k:=random(length(input));
        input[k]:=not(input[k]);
        result:=true;
      end else begin
        for k:=0 to length(input)-1 do input[k]:=(random>0.5);
        result:=true;
      end;
    end;

  FUNCTION getIoString(CONST wire:T_wireValue):string;
    begin
      if rbBinary.checked        then result:=getBinaryString     (wire)
      else if rbPositive.checked then result:=getDecimalString    (wire)
      else                            result:=get2ComplementString(wire);
    end;

  VAR i,stepCount:longint;
      r,c:longint;
      wIn:array of record
        v:T_wireValue;
        firstIndex:longint;
      end;
      startTicks: qword;
  begin
    startTicks:=GetTickCount64;
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
    clonedGate^.reset;
    repeat
      if ResetCheckBox.checked then clonedGate^.reset;
      for c:=0 to length(wIn)-1 do begin
        for i:=0 to wIn[c].v.width-1 do
          if input[wIn[c].firstIndex+i]
          then wIn[c].v.bit[i]:=tsv_true
          else wIn[c].v.bit[i]:=tsv_false;
        clonedGate^.setInput(c,wIn[c].v);
      end;
      stepCount:=0;
      while (stepCount<=1000) and clonedGate^.simulateStep do inc(stepCount);

      r:=StringGrid.rowCount;
      StringGrid.rowCount:=r+1;
      c:=0;
      for i:=0 to length(wIn)-1 do begin
        StringGrid.Cells[c,r]:=getIoString(wIn[i].v);
        inc(c);
      end;

      if stepCount<=1000
      then StringGrid.Cells[c,r]:=intToStr(stepCount)
      else StringGrid.Cells[c,r]:='>1000';

      inc(c);
      for i:=0 to clonedGate^.numberOfOutputs-1 do begin
        StringGrid.Cells[c,r]:=getIoString(clonedGate^.getOutput(i));
        inc(c);
      end;
      if GetTickCount64>startTicks+500 then begin
        Application.ProcessMessages;
        startTicks:=GetTickCount64;
      end;
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

