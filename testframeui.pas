UNIT testFrameUI;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, ExtCtrls, Grids, StdCtrls, challenges;

TYPE

  { TTestCreationFrame }

  TTestCreationFrame = class(TFrame)
    generateTestCasesLabel: TLabel;
    generateTestCasesShape: TShape;
    Label6: TLabel;
    Label7: TLabel;
    TestCaseCountEdit: TEdit;
    StepCountEdit: TEdit;
    TestCasesStringGrid: TStringGrid;
    TestInputsPanel: TPanel;
    Timer1: TTimer;
    PROCEDURE generateTestCasesShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
    PROCEDURE StepCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCaseCountEditEditingDone(Sender: TObject);
    PROCEDURE TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
    PROCEDURE TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
    PROCEDURE Timer1Timer(Sender: TObject);
  private
    testGenerator:P_testCreator;
    lastUpdatedRow:longint;
    PROCEDURE updateTableRow(CONST j:longint);
  public
    PROCEDURE fillTable;
    PROCEDURE setTestGenerator(CONST generator:P_testCreator);
    PROCEDURE detachTestGenerator;
  end;

IMPLEMENTATION
USES compoundGates,logicalGates, Graphics;

{$R *.lfm}

{ TTestCreationFrame }

PROCEDURE TTestCreationFrame.TestCasesStringGridValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR
    newStepCount: longint;
    wireValue: T_wireValue;
    i:longint;
  begin
    //Editing output...
    if aCol>length(testGenerator^.Interfaces.inputs) then begin
      newValue:=oldValue;
      exit;
    end;
    //Editing number of steps...
    if aCol=length(testGenerator^.Interfaces.inputs) then begin
      newStepCount:=strToIntDef(newValue,-1);
      if (newStepCount<1) then begin
        newValue:=oldValue;
      end else begin
        testGenerator^.tests[aRow-1].maxTotalSteps:=newStepCount;
        lastUpdatedRow:=-1;
        //TODO: Restart of test generator is only necessary if editing an editor before (or equal to) the current test step
        testGenerator^.updateTestCaseResults;
      end;
      exit;
    end;
    //Editing input...
    wireValue:=parseWire(newValue,testGenerator^.Interfaces.inputs[aCol].wireWidth,testGenerator^.Interfaces.inputs[aCol].representation);
    for i:=0 to wireValue.width-1 do if wireValue.bit[i]=tsv_undetermined then begin
      newValue:=oldValue;
      exit;
    end;
    newValue:=getWireString(wireValue,testGenerator^.Interfaces.inputs[aCol].representation);
    testGenerator^.tests[aRow-1].inputs[aCol]:=wireValue;
    lastUpdatedRow:=-1;
    testGenerator^.updateTestCaseResults;
  end;

PROCEDURE TTestCreationFrame.Timer1Timer(Sender: TObject);
  VAR i, newUpdatedRow: longint;
  begin
    if testGenerator=nil then exit;
    if TestCasesStringGrid.editor.showing then exit;

    newUpdatedRow:=testGenerator^.lastTestCasePrepared;
    if (newUpdatedRow<>lastUpdatedRow) then begin
      if (lastUpdatedRow<0) or (newUpdatedRow<lastUpdatedRow)
      then fillTable
      else for i:=lastUpdatedRow+1 to newUpdatedRow do updateTableRow(i);
      lastUpdatedRow:=newUpdatedRow;
    end;
  end;

PROCEDURE TTestCreationFrame.updateTableRow(CONST j: longint);
  VAR i,k:longint;
      gateInterface: T_gateInterface;
  begin
    if (j<0) then begin
      fillTable;
      exit;
    end;

    i:=0;
    for gateInterface in testGenerator^.Interfaces.inputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].inputs[i],gateInterface.representation);
      inc(i);
    end;
    TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].maxTotalSteps)+' ('+intToStr(testGenerator^.tests[j].actuallyActive)+')';
    inc(i);
    k:=0;
    for gateInterface in testGenerator^.Interfaces.outputs do begin
      TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].outputs[k],gateInterface.representation);
      inc(i); inc(k);
    end;
    if (j=length(testGenerator^.tests)-1) then TestCasesStringGrid.AutoSizeColumns;
    Application.ProcessMessages;
  end;

PROCEDURE TTestCreationFrame.fillTable;
  VAR i,j,k:longint;
      gateInterface: T_gateInterface;
  begin
    TestCasesStringGrid.rowCount:=1+length(testGenerator^.tests);
    TestCasesStringGrid.colCount:=length(testGenerator^.Interfaces.inputs)+length(testGenerator^.Interfaces.outputs)+1;

    //Header
    i:=0;
    for gateInterface in testGenerator^.Interfaces.inputs do begin
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Cells[i,0]:=gateInterface.name
      else TestCasesStringGrid.Cells[i,0]:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;
    TestCasesStringGrid.Cells[i,0]:='Steps';
    inc(i);
    for gateInterface in testGenerator^.Interfaces.outputs do begin
      if gateInterface.wireWidth<=1
      then TestCasesStringGrid.Cells[i,0]:=gateInterface.name
      else TestCasesStringGrid.Cells[i,0]:=gateInterface.name+' ('+ C_multibitWireRepresentationName[gateInterface.representation]+')';
      inc(i);
    end;

    //Body
    for j:=0 to length(testGenerator^.tests)-1 do begin
      i:=0;
      for gateInterface in testGenerator^.Interfaces.inputs do begin
        TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].inputs[i],gateInterface.representation);
        inc(i);
      end;
      TestCasesStringGrid.Cells[i,j+1]:=intToStr(testGenerator^.tests[j].maxTotalSteps)+' ('+intToStr(testGenerator^.tests[j].actuallyActive)+')';
      inc(i);
      k:=0;
      for gateInterface in testGenerator^.Interfaces.outputs do begin
        if k<length(testGenerator^.tests[j].outputs)
        then TestCasesStringGrid.Cells[i,j+1]:=getWireString(testGenerator^.tests[j].outputs[k],gateInterface.representation)
        else TestCasesStringGrid.Cells[i,j+1]:='?';
        inc(i); inc(k);
      end;
    end;
    TestCasesStringGrid.AutoSizeColumns;

  end;

PROCEDURE TTestCreationFrame.setTestGenerator(CONST generator: P_testCreator);
  begin
    testGenerator:=generator;
    TestCasesStringGrid.editor.Font.color:=clWhite;
    TestCasesStringGrid.editor.color:=clBlack;
    TestCaseCountEdit.text:=intToStr(length(testGenerator^.tests));
    fillTable;
    lastUpdatedRow:=-1;
    Timer1.enabled:=true;
  end;

PROCEDURE TTestCreationFrame.detachTestGenerator;
  begin
    Timer1.enabled:=false;
  end;

PROCEDURE TTestCreationFrame.TestCaseCountEditEditingDone(Sender: TObject);
  VAR newCount: longint;
  begin
    newCount:=strToIntDef(TestCaseCountEdit.text,-1);
    if (newCount>256) or (newCount<=0) or (newCount=length(testGenerator^.tests)) then begin
      TestCaseCountEdit.text:=intToStr(length(testGenerator^.tests));
      exit;
    end;
    testGenerator^.setNumberOfTestCases(newCount);
    lastUpdatedRow:=-1;
  end;

PROCEDURE TTestCreationFrame.generateTestCasesShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    lastUpdatedRow:=-1;
    testGenerator^.generateTestCases;
  end;

PROCEDURE TTestCreationFrame.StepCountEditEditingDone(Sender: TObject);
  VAR newCount: int64;
      i:longint;
  begin
    newCount:=StrToInt64Def(StepCountEdit.text,-1);
    if newCount>maxLongint
    then begin
      newCount:=maxLongint;
      StepCountEdit.text:=intToStr(maxLongint);
    end else if (newCount<=0) then begin
      StepCountEdit.text:='';
      exit;
    end;
    for i:=0 to length(testGenerator^.tests)-1 do testGenerator^.tests[i].maxTotalSteps:=newCount;
    testGenerator^.reInitStepCounts;
    testGenerator^.updateTestCaseResults;
    lastUpdatedRow:=-1;
  end;

PROCEDURE TTestCreationFrame.TestCasesStringGridHeaderClick(Sender: TObject; IsColumn: boolean; index: integer);
  CONST nextMode:array[T_multibitWireRepresentation] of T_multibitWireRepresentation=(wr_decimal,wr_2complement,wr_binary);
  begin
    if not(IsColumn) then exit;
    if index<length(testGenerator^.Interfaces.inputs) then begin
      testGenerator^.Interfaces.inputs[index].representation:=nextMode[testGenerator^.Interfaces.inputs[index].representation];
      fillTable;
      exit;
    end;
    dec(index,1+length(testGenerator^.Interfaces.inputs));
    if index<0 then exit;
    if index<length(testGenerator^.Interfaces.outputs) then begin
      testGenerator^.Interfaces.outputs[index].representation:=nextMode[testGenerator^.Interfaces.outputs[index].representation];
      fillTable;
    end;
  end;

end.

