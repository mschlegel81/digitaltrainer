UNIT analysisDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, ComCtrls, Grids,
  StdCtrls, ExtCtrls, logicGates,baseGate;

TYPE
  T_scaleType=(st_binary,st_unsigned,st_signed);

  T_ranges=array of record
    y0:longint;
    dy:double;
  end;

  T_graphMetaDataEntry=object
    caption:string;
    bitWidth:longint;
    plotY0:longint;
    ranges:T_ranges;

    FUNCTION transformY(CONST v:T_wireValue; CONST scaleType:T_scaleType; OUT determinedValue:boolean; CONST bitIndex:longint=0):longint;
  end;

  { T_graphMetaData }

  T_graphMetaData=object
    input,output:array of T_graphMetaDataEntry;

    PROCEDURE initialize(CONST gate:P_abstractGate);
    PROCEDURE update(CONST scaleType:T_scaleType; CONST imageHeight:longint);
    FUNCTION ioMeta(CONST index:longint):T_graphMetaDataEntry;
  end;

  T_paintable=object
    rows:array of array of record x:longint; v:T_wireValue; end;
    simSplits:array of record x,simIndex:longint; end;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addSimStart(CONST stepIndex,simIndex:longint);
    PROCEDURE addValue(CONST rowIndex,stepIndex:longint; CONST value:T_wireValue);
    PROCEDURE paint(CONST startIndex,zoom:longint; CONST scaleType:T_scaleType; CONST meta:T_graphMetaData; CONST Canvas:TCanvas);
  end;

  T_simulationOutput=object
    startAtStep,endAtStep:longint;
    inputs:array of T_wireValue;
    outputHistory:array of array of record
      stepIndex:longint;
      value:T_wireValue;
    end;

    CONSTRUCTOR create(CONST firstStepIndex:longint);
    DESTRUCTOR destroy;
    PROCEDURE addInput (CONST v:T_wireValue);
    PROCEDURE addOutput(CONST stepIndex,outputIndex:longint; CONST v:T_wireValue);
    PROCEDURE finishRun;

    PROCEDURE updateTable(CONST scaleType:T_scaleType; CONST rowIndex:longint; CONST table:TStringGrid);
    PROCEDURE collectPaintable(CONST startIndex,endIndex,simIndex:longint; VAR paintable:T_paintable);
    FUNCTION stepsTotal:longint;
  end;

  { TanalysisForm }

  TanalysisForm = class(TForm)
    cancelSimButton: TButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    maxResponseTimeLabel: TLabel;
    minResponseTimeLabel: TLabel;
    maximum: TLabel;
    Minimum: TLabel;
    ProgressBar1: TProgressBar;
    ResetCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbSimOrdered: TRadioButton;
    rbBitFlip: TRadioButton;
    rbSimRandom: TRadioButton;
    rbBinary: TRadioButton;
    rbPositive: TRadioButton;
    rb2Complement: TRadioButton;
    SizesStringGrid: TStringGrid;
    TabSheet3: TTabSheet;
    TimeScrollBar: TScrollBar;
    TabSheet2: TTabSheet;
    zoomTrackBar: TTrackBar;
    UpdateTableButton: TButton;
    PageControl1: TPageControl;
    StringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    PROCEDURE cancelSimButtonClick(Sender: TObject);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE rbBinaryChange(Sender: TObject);
    PROCEDURE TimeScrollBarChange(Sender: TObject);
    PROCEDURE UpdateTableButtonClick(Sender: TObject);
    PROCEDURE ZoomTrackBarChange(Sender: TObject);
  private
    clonedGate:P_abstractGate;
    graphMetaData:T_graphMetaData;
    simulationOutputs:array of T_simulationOutput;
    cancelled:boolean;
    PROCEDURE setupTable;
    PROCEDURE repaintTable;
    PROCEDURE repaintGraph;
  public
    PROCEDURE showForGate (CONST gate:P_abstractGate);
    PROCEDURE showForBoard(CONST board:P_circuitBoard);
  end;

VAR
  analysisForm: TanalysisForm;
CONST MAX_TOTAL_SIM_STEPS=1000000;

IMPLEMENTATION
FUNCTION screenXToTimestep(CONST zoom,timestepOffset,x:longint):longint;
  begin
    result:=round((x-20)/zoom+timestepOffset);
  end;

FUNCTION timestepToScreenX(CONST zoom,timestepOffset,timeStep:longint):longint;
  begin
    result:=(timeStep-timestepOffset)*zoom+20;
  end;

{$R *.lfm}

{ T_graphMetaDataEntry }

FUNCTION T_graphMetaDataEntry.transformY(CONST v: T_wireValue; CONST scaleType: T_scaleType; OUT determinedValue:boolean; CONST bitIndex: longint): longint;
  begin
    case scaleType of
      st_binary: begin
        case v.bit[bitIndex] of
          tsv_false       : begin result:=      ranges[bitIndex].y0;                          determinedValue:=true ; end;
          tsv_undetermined: begin result:=round(ranges[bitIndex].y0+0.5*ranges[bitIndex].dy); determinedValue:=false; end;
          tsv_true        : begin result:=round(ranges[bitIndex].y0+    ranges[bitIndex].dy); determinedValue:=true ; end;
        end;
      end;
      st_unsigned: result:=round(ranges[0].y0+ranges[0].dy*getDecimalValue    (v,determinedValue));
      st_signed  : result:=round(ranges[0].y0+ranges[0].dy*get2ComplementValue(v,determinedValue));
    end;
  end;

{ T_paintable }

CONSTRUCTOR T_paintable.create;
  begin
    setLength(rows,0);
    setLength(simSplits,0);
  end;

DESTRUCTOR T_paintable.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(rows)-1 do setLength(rows[i],0);
    setLength(rows,0);
    setLength(simSplits,0);
  end;

PROCEDURE T_paintable.addSimStart(CONST stepIndex, simIndex: longint);
  VAR k:longint;
  begin
    k:=length(simSplits);
    setLength(simSplits,k+1);
    simSplits[k].x       :=stepIndex;
    simSplits[k].simIndex:=simIndex;
  end;

PROCEDURE T_paintable.addValue(CONST rowIndex, stepIndex: longint; CONST value: T_wireValue);
  VAR k:longint;
  begin
    if rowIndex>=length(rows) then setLength(rows,rowIndex+1);
    k:=length(rows[rowIndex]);
    setLength(rows[rowIndex],k+1);
    rows[rowIndex,k].x:=stepIndex;
    rows[rowIndex,k].v:=value;
  end;

PROCEDURE T_paintable.paint(CONST startIndex, zoom: longint; CONST scaleType:T_scaleType; CONST meta: T_graphMetaData; CONST Canvas: TCanvas);
  FUNCTION attenuatedColor(CONST foreground:longint):TColor;
    begin
      result:=ColorToRGB(clBtnFace);
      result:=round(( result         and 255)*0.7 + ( foreground         and 255)*0.3)
          or (round(((result shr  8) and 255)*0.7 + ((foreground shr  8) and 255)*0.3) shl  8)
          or (round(((result shr 16) and 255)*0.7 + ((foreground shr 16) and 255)*0.3) shl 16);
    end;

  VAR colorTab:array[false..true,false..true] of TColor;

      i,bitIdx,k:longint;
      x,y:longint;
      red:boolean=true;
      rowMeta: T_graphMetaDataEntry;
      determinedValue: boolean;

  begin
    colorTab[false,false]:=attenuatedColor(clBlue);
    colorTab[false,true ]:=                clBlue ;
    colorTab[true ,false]:=attenuatedColor(clRed);
    colorTab[true ,true ]:=                clRed ;

    //Grid
    Canvas.Font.Orientation:=0;
    Canvas.Font.color:=clBlack;
    Canvas.Pen.style:=psSolid;
    Canvas.Pen.color:=attenuatedColor(clBlack);
    for i:=0 to length(simSplits)-1 do begin
      x:=timestepToScreenX(zoom,startIndex,simSplits[i].x);
      Canvas.line(x,Canvas.height-1,x,20);
      Canvas.textOut(x,5,'#'+intToStr(simSplits[i].simIndex));
    end;
    if length(meta.output)>0 then begin
      i:=meta.output[0].plotY0+5;
      Canvas.line(20,i,Canvas.width-1,i);
    end;

    //Data rows
    for i:=0 to length(rows)-1 do begin
      rowMeta:=meta.ioMeta(i);
      if scaleType=st_binary then begin
        for bitIdx:=0 to rowMeta.bitWidth-1 do begin
          x:=timestepToScreenX(zoom,startIndex,rows[i,0].x);
          y:=rowMeta.transformY(rows[i,0].v,scaleType,determinedValue,bitIdx);
          Canvas.MoveTo(x,y);
          Canvas.Pen.color:=colorTab[red,determinedValue];
          for k:=1 to length(rows[i])-1 do begin
            x:=timestepToScreenX(zoom,startIndex,rows[i,k].x);
            Canvas.LineTo(x,y);
            y:=rowMeta.transformY(rows[i,k].v,scaleType,determinedValue,bitIdx);
            Canvas.Pen.color:=colorTab[red,determinedValue];
            Canvas.LineTo(x,y);
          end;
        end;
      end else begin
        x:=timestepToScreenX(zoom,startIndex,rows[i,0].x);
        y:=rowMeta.transformY(rows[i,0].v,scaleType,determinedValue);
        Canvas.MoveTo(x,y);
        Canvas.Pen.color:=colorTab[red,determinedValue];
        for k:=1 to length(rows[i])-1 do begin
          x:=timestepToScreenX(zoom,startIndex,rows[i,k].x);
          Canvas.LineTo(x,y);
          y:=rowMeta.transformY(rows[i,k].v,scaleType,determinedValue);
          Canvas.Pen.color:=colorTab[red,determinedValue];
          Canvas.LineTo(x,y);
        end;
      end;
      red:=not(red);
    end;

  end;

{ T_graphMetaData }

PROCEDURE T_graphMetaData.initialize(CONST gate: P_abstractGate);
  VAR i:longint;
  begin
    if (gate^.gateType=gt_compound) then with P_customGate(gate)^ do begin
      setLength(input,length(inputConnections));
      for i:=0 to length(inputConnections)-1 do begin
        input[i].caption :=inputConnections[i].caption;
        input[i].bitWidth:=inputConnections[i].width;
      end;
      setLength(output,length(outputConnections));
      for i:=0 to length(outputConnections)-1 do begin
        output[i].caption :=outputConnections[i].caption;
        output[i].bitWidth:=outputConnections[i].width;
      end;
    end else begin
      setLength(input,gate^.numberOfInputs);
      for i:=0 to gate^.numberOfInputs-1 do begin
        input[i].caption:='in '+intToStr(i);
        input[i].bitWidth:=gate^.inputWidth(i);
      end;
      setLength(output,gate^.numberOfOutputs);
      for i:=0 to gate^.numberOfOutputs-1 do begin
        output[i].caption:='out '+intToStr(i);
        output[i].bitWidth:=gate^.outputWidth(i);
      end;
    end;
  end;

PROCEDURE T_graphMetaData.update(CONST scaleType: T_scaleType; CONST imageHeight: longint);
  PROCEDURE updateEntry(VAR entry:T_graphMetaDataEntry; CONST zoomFactor:double; VAR yTally:longint);
    VAR yOffset:double=0;
        dyFactor:double;
        dynamicRange:longint=1;
        k:longint;
    begin
      entry.plotY0:=yTally;
      case scaleType of
        st_binary: begin
          setLength(entry.ranges,entry.bitWidth);
          dyFactor:=1;
        end;
        st_unsigned: begin
          setLength(entry.ranges,1);
          case entry.bitWidth of
           16: begin dyFactor:=16/65536; dynamicRange:=65536; end;
            8: begin dyFactor:= 8/  256; dynamicRange:=256; end;
            4: begin dyFactor:= 4/   16; dynamicRange:=16;  end;
          else dyFactor:=1;
          end;
        end;
        st_signed: begin
          setLength(entry.ranges,1);
          case entry.bitWidth of
            16: begin
                 dyFactor:=16/65536;
                 yOffset :=dyFactor*32768;
                 dynamicRange:=65536;
               end;
            8: begin
                 dyFactor:=8/256;
                 yOffset :=dyFactor*128;
                 dynamicRange:=256;
               end;
            4: begin
                 dyFactor:=4/16;
                 yOffset :=dyFactor*8;
                 dynamicRange:=16;
               end
          else begin
                 dyFactor:=1;
                 yOffset:=0;
               end;
          end;
        end;
      end;
      for k:=0 to length(entry.ranges)-1 do begin
        entry.ranges[k].y0:=round(yTally-yOffset*zoomFactor);
        entry.ranges[k].dy:=-dyFactor*zoomFactor;
        yTally+=round(0.5+1.1*entry.ranges[k].dy*dynamicRange);
      end;
    end;

  VAR yTally:longint;
      verticalZoom:longint;
      i:longint;
  begin
    if (length(input)=0) and (length(output)=0) then exit;

    //20 pixels space at the top, distribute the rest evenly, negate y-axis ("high" values are represented as "low" pixel coordinates)

    //1. increase zoom until representation is too high
    verticalZoom:=1;
    repeat
      yTally:=imageHeight-1;
      for i:=0 to length(input )-1 do updateEntry(input[i] ,verticalZoom,yTally);
      for i:=0 to length(output)-1 do updateEntry(output[i],verticalZoom,yTally);
      inc(verticalZoom);
    until yTally<20;
    //2. decrease soom, so that representation still fits
    if verticalZoom>1 then begin
      dec(verticalZoom);
      yTally:=imageHeight-1;
      for i:=0 to length(input )-1 do updateEntry(input[i] ,verticalZoom,yTally);
      for i:=0 to length(output)-1 do updateEntry(output[i],verticalZoom,yTally);
    end;
    //3. shift in order to center vertically

    // top gap (is)    =                   yTally@post
    // bottom gap (is) = 0 = imageHeight-1-yTally@pre

    //TODO: RECENTER!!!

  end;

FUNCTION T_graphMetaData.ioMeta(CONST index: longint): T_graphMetaDataEntry;
  begin
    if index<length(input)
    then result:=input[index]
    else result:=output[index-length(input)];
  end;

{ T_simulationOutput }

CONSTRUCTOR T_simulationOutput.create(CONST firstStepIndex: longint);
  begin
    setLength(inputs,0);
    setLength(outputHistory,0);
    startAtStep:=firstStepIndex;
    endAtStep:=firstStepIndex;
  end;

DESTRUCTOR T_simulationOutput.destroy;
  VAR i:longint;
  begin
    setLength(inputs,0);
    for i:=0 to length(outputHistory)-1 do setLength(outputHistory[i],0);
    setLength(outputHistory,0);
  end;

PROCEDURE T_simulationOutput.addInput(CONST v: T_wireValue);
  VAR k:longint;
  begin
    k:=length(inputs);
    setLength(inputs,k+1);
    inputs[k]:=v;
  end;

PROCEDURE T_simulationOutput.addOutput(CONST stepIndex, outputIndex: longint; CONST v: T_wireValue);
  VAR k:longint;
  begin
    if outputIndex<0 then exit;
    if outputIndex>=length(outputHistory) then setLength(outputHistory,outputIndex+1);

    k:=length(outputHistory[outputIndex]);
    if (k=0) or (outputHistory[outputIndex,k-1].value<>v) then begin
      setLength(outputHistory[outputIndex],k+1);
      outputHistory[outputIndex,k].value    :=v;
      outputHistory[outputIndex,k].stepIndex:=stepIndex;
    end;
  end;

PROCEDURE T_simulationOutput.finishRun;
  VAR outputIndex:longint;
      k:longint;
      maxSteps:longint=0;
  begin
    for outputIndex:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[outputIndex])-1;
      if   maxSteps< outputHistory[outputIndex,k].stepIndex
      then maxSteps:=outputHistory[outputIndex,k].stepIndex;
    end;

    for outputIndex:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[outputIndex]);
      if outputHistory[outputIndex,k-1].stepIndex<maxSteps then begin
        setLength(outputHistory[outputIndex],k+1);
        outputHistory[outputIndex,k].value:=outputHistory[outputIndex,k-1].value;
        outputHistory[outputIndex,k].stepIndex:=maxSteps;
      end;
    end;

    endAtStep:=maxSteps;
  end;

PROCEDURE T_simulationOutput.updateTable(CONST scaleType: T_scaleType; CONST rowIndex: longint; CONST table: TStringGrid);
  FUNCTION getIoString(CONST wire:T_wireValue):string;
    begin
      case scaleType of
        st_binary  : result:=getBinaryString     (wire);
        st_unsigned: result:=getDecimalString    (wire);
        st_signed  : result:=get2ComplementString(wire);
      end;
    end;

  VAR col:longint=0;
      i,k:longint;
  begin
    if table.rowCount<=rowIndex then table.rowCount:=rowIndex+1;

    col:=0;
    for i:=0 to length(inputs)-1 do begin
      table.Cells[col,rowIndex]:=getIoString(inputs[i]);
      inc(col);
    end;

    if stepsTotal<=MAX_TOTAL_SIM_STEPS
    then table.Cells[col,rowIndex]:=intToStr(stepsTotal)
    else table.Cells[col,rowIndex]:='>'+intToStr(MAX_TOTAL_SIM_STEPS);

    inc(col);
    for i:=0 to length(outputHistory)-1 do begin
      k:=length(outputHistory[i])-1;
      table.Cells[col,rowIndex]:=getIoString(outputHistory[i,k].value);
      inc(col);
    end;
  end;

PROCEDURE T_simulationOutput.collectPaintable(CONST startIndex,endIndex,simIndex:longint; VAR paintable:T_paintable);
  VAR r:longint=0;
      i,k:longint;
  begin
    if startIndex>endAtStep   then exit;
    if endIndex  <startAtStep then exit;
    paintable.addSimStart(startAtStep,simIndex);
    for i:=0 to length(inputs)-1 do begin
      paintable.addValue(r,startAtStep,inputs[i]);
      paintable.addValue(r,endAtStep  ,inputs[i]);
      inc(r);
    end;
    for i:=0 to length(outputHistory)-1 do begin
      for k:=0 to length(outputHistory[i])-1 do
      if (outputHistory[i,k].stepIndex>=startIndex) and
         (outputHistory[i,k].stepIndex<=endIndex  ) then paintable.addValue(r,outputHistory[i,k].stepIndex,outputHistory[i,k].value);
      inc(r);
    end;
  end;

FUNCTION T_simulationOutput.stepsTotal: longint;
  begin
    result:=endAtStep-startAtStep;
  end;

{ TanalysisForm }

PROCEDURE TanalysisForm.UpdateTableButtonClick(Sender: TObject);
  CONST ONE_MINUTE=1/(24*60);
  VAR input:array of boolean;
      inputsGenerated:longint=0;
      expectedTotalInputs:longint=0;
      generationDeadline:double;
      simulationStartStep:longint=0;
  FUNCTION nextInput:boolean;
    VAR k:longint=0;
    begin
      inc(inputsGenerated);
      if (inputsGenerated>expectedTotalInputs) or (simulationStartStep>MAX_TOTAL_SIM_STEPS) or (length(input)=0) or (now>generationDeadline) then exit(false);

      k:=0;
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

  VAR startTicks: qword;
  PROCEDURE updateProgress;
    VAR progress, tmpProgress: int64;
    begin

      StringGrid.EndUpdate();
      //progress by time:
      // 100*(now-start)/(generationDeadline-start) ; start=generationDeadline-ONE_MINUTE;
      //=100*(now-generationDeadline+ONE_MINUTE)/ONE_MINUTE;
      progress:=round(ProgressBar1.max*(now-generationDeadline+ONE_MINUTE)/ONE_MINUTE);
      tmpProgress:=round(inputsGenerated/expectedTotalInputs*ProgressBar1.max);
      if tmpProgress>progress then progress:=tmpProgress;
      tmpProgress:=round(simulationStartStep/MAX_TOTAL_SIM_STEPS*ProgressBar1.max);
      if tmpProgress>progress then progress:=tmpProgress;
      if progress<0 then progress:=0 else if progress>ProgressBar1.max then progress:=ProgressBar1.max;
      ProgressBar1.position:=progress;

      Application.ProcessMessages;
      StringGrid.BeginUpdate;
      startTicks:=GetTickCount64;
    end;

  VAR i,stepCount:longint;
      simIndex:longint=0;
      minResponseTime:longint=maxLongint;
      maxResponseTime:longint=0;
      c:longint;
      wIn:array of record
        v:T_wireValue;
        firstIndex:longint;
      end;
      scaleType:T_scaleType;
  begin
    cancelled:=false;
    cancelSimButton.enabled:=true;

    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;

    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);

    generationDeadline:=now+ONE_MINUTE;
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
    if (c<10) and (rbSimOrdered.checked)
    then expectedTotalInputs:=1 shl c
    else expectedTotalInputs:=1000;

    for i:=0 to length(input)-1 do input[i]:=false;
    clonedGate^.reset;
    StringGrid.BeginUpdate;
    repeat
      setLength(simulationOutputs,simIndex+1);
      simulationOutputs[simIndex].create(simulationStartStep);

      if ResetCheckBox.checked then clonedGate^.reset;
      for c:=0 to length(wIn)-1 do begin
        for i:=0 to wIn[c].v.width-1 do
          if input[wIn[c].firstIndex+i]
          then wIn[c].v.bit[i]:=tsv_true
          else wIn[c].v.bit[i]:=tsv_false;
        clonedGate^.setInput(c,wIn[c].v);
        simulationOutputs[simIndex].addInput(wIn[c].v);
      end;
      stepCount:=0;
      while (stepCount+simulationStartStep<=MAX_TOTAL_SIM_STEPS) and not(cancelled) and clonedGate^.simulateStep do begin
        inc(stepCount);
        for i:=0 to clonedGate^.numberOfOutputs-1 do
        simulationOutputs[simIndex].addOutput(stepCount+simulationStartStep,i,clonedGate^.getOutput(i));

        if GetTickCount64>startTicks+200 then updateProgress;
      end;
      inc(stepCount);
      for i:=0 to clonedGate^.numberOfOutputs-1 do
      simulationOutputs[simIndex].addOutput(stepCount+simulationStartStep,i,clonedGate^.getOutput(i));

      simulationOutputs[simIndex].finishRun;

      if simulationOutputs[simIndex].stepsTotal<minResponseTime then begin
        minResponseTime:=simulationOutputs[simIndex].stepsTotal;
        minResponseTimeLabel.caption:=intToStr(minResponseTime);
      end;
      if simulationOutputs[simIndex].stepsTotal>maxResponseTime then begin
        maxResponseTime:=simulationOutputs[simIndex].stepsTotal;
        maxResponseTimeLabel.caption:=intToStr(maxResponseTime);
      end;
      simulationStartStep:=simulationOutputs[simIndex].endAtStep+1;
      simulationOutputs[simIndex].updateTable(scaleType,simIndex+1,StringGrid);

      inc(simIndex);
    until not(nextInput) or cancelled;
    cancelSimButton.enabled:=false;
    StringGrid.EndUpdate();
    ProgressBar1.position:=0;

    if zoomTrackBar.position*simulationStartStep>Image1.width
    then zoomTrackBar.position:=trunc(Image1.width/simulationStartStep);
    TimeScrollBar.position:=0;
    TimeScrollBar.min:=0;
    i:=simulationStartStep-trunc(Image1.width/zoomTrackBar.position);
    if i<=0 then begin
      TimeScrollBar.position:=0;
      TimeScrollBar.enabled:=false;
    end else begin
      TimeScrollBar.max:=i;
      TimeScrollBar.enabled:=true;
    end;
    repaintGraph;
    repaintTable;
  end;

PROCEDURE TanalysisForm.ZoomTrackBarChange(Sender: TObject);
  VAR lastPointInTime:longint;
      newMax:longint;
  begin
    if length(simulationOutputs)=0 then exit;
    lastPointInTime:=simulationOutputs[length(simulationOutputs)-1].endAtStep;

    newMax:=lastPointInTime-trunc(Image1.width/zoomTrackBar.position);
    if newMax<=0 then begin
      TimeScrollBar.position:=0;
      TimeScrollBar.enabled:=false;
    end else begin
      TimeScrollBar.max:=newMax;
      TimeScrollBar.enabled:=true;
    end;

    repaintGraph;
  end;

PROCEDURE TanalysisForm.rbBinaryChange(Sender: TObject);
  begin
    repaintTable;
    repaintGraph;
  end;

PROCEDURE TanalysisForm.FormResize(Sender: TObject);
  begin
    repaintGraph;
  end;

PROCEDURE TanalysisForm.cancelSimButtonClick(Sender: TObject);
  begin
    cancelled:=true;
  end;

PROCEDURE TanalysisForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    if cancelSimButton.enabled then begin
      cancelled:=true;
      CanClose:=false;
    end else CanClose:=true;
  end;

PROCEDURE TanalysisForm.TimeScrollBarChange(Sender: TObject);
  begin
    repaintGraph;
  end;

PROCEDURE TanalysisForm.setupTable;
  VAR colIndex:longint=0;
      rowIndex:longint=1;
      i:longint;

      gt:T_gateType;
      gateCount:T_gateCount;
  begin
    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);

    graphMetaData.initialize(clonedGate);

    StringGrid.colCount:=clonedGate^.numberOfInputs+clonedGate^.numberOfOutputs+1;
    StringGrid.rowCount:=1;
    minResponseTimeLabel.caption:='?';
    maxResponseTimeLabel.caption:='?';

    for i:=0 to length(graphMetaData.input)-1 do begin
      StringGrid.Cells[colIndex,0]:=graphMetaData.input[i].caption;
      inc(colIndex);
    end;
    StringGrid.Cells[colIndex,0]:='Schritte bis stabil';
    inc(colIndex);
    for i:=0 to length(graphMetaData.output)-1 do begin
      StringGrid.Cells[colIndex,0]:=graphMetaData.output[i].caption;
      inc(colIndex);
    end;

    for gt in T_gateType do gateCount[gt]:=0;
    clonedGate^.countGates(gateCount);
    i:=0;
    for gt in T_gateType do if gateCount[gt]>0 then begin;
      inc(i,gateCount[gt]);
      if SizesStringGrid.rowCount<=rowIndex then SizesStringGrid.rowCount:=rowIndex+1;
      SizesStringGrid.Cells[0,rowIndex]:=C_gateTypeName[gt];
      SizesStringGrid.Cells[1,rowIndex]:=intToStr(gateCount[gt]);
      inc(rowIndex);
    end;
    SizesStringGrid.rowCount:=rowIndex+1;
    SizesStringGrid.Cells[0,rowIndex]:='';
    SizesStringGrid.Cells[1,rowIndex]:=intToStr(i);
    SizesStringGrid.AutoSizeColumns;
  end;

PROCEDURE TanalysisForm.repaintTable;
  VAR simIndex:longint;
      scaleType: T_scaleType;
  begin
    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;
    for simIndex:=0 to length(simulationOutputs)-1 do simulationOutputs[simIndex].updateTable(scaleType,simIndex+1,StringGrid);
    StringGrid.AutoSizeColumns;
  end;

PROCEDURE TanalysisForm.repaintGraph;
  VAR simIndex:longint;
      scaleType: T_scaleType;
      i:longint;
      red:boolean=true;
      paintable:T_paintable;
  begin
    if length(simulationOutputs)=0 then exit;
    if rbBinary.checked then scaleType:=st_binary
    else if rbPositive.checked then scaleType:=st_unsigned
    else scaleType:=st_signed;

    Image1.picture.Bitmap.setSize(Image1.width,Image1.height);
    Image1.picture.Bitmap.Canvas.Brush.color:=clBtnFace;
    Image1.picture.Bitmap.Canvas.FillRect(0,0,Image1.width,Image1.height);

    graphMetaData.update(scaleType,Image1.height);

    Image1.picture.Bitmap.Canvas.Font.Orientation:=900;
    for i:=0 to length(graphMetaData.input)-1 do begin
      if red then Image1.picture.Bitmap.Canvas.Font.color:=clRed
             else Image1.picture.Bitmap.Canvas.Font.color:=clBlue;
      red:=not(red);
      Image1.picture.Bitmap.Canvas.textOut(0,graphMetaData.input[i].plotY0,graphMetaData.input[i].caption);
    end;
    for i:=0 to length(graphMetaData.output)-1 do begin
      if red then Image1.picture.Bitmap.Canvas.Font.color:=clRed
             else Image1.picture.Bitmap.Canvas.Font.color:=clBlue;
      red:=not(red);
      Image1.picture.Bitmap.Canvas.textOut(0,graphMetaData.output[i].plotY0,graphMetaData.output[i].caption);
    end;

    paintable.create;
    i:=screenXToTimestep(zoomTrackBar.position,TimeScrollBar.position,Image1.width)+1;
    for simIndex:=0 to length(simulationOutputs)-1 do simulationOutputs[simIndex].collectPaintable(TimeScrollBar.position,i,simIndex,paintable);
    paintable.paint(TimeScrollBar.position,zoomTrackBar.position,scaleType,graphMetaData,Image1.picture.Bitmap.Canvas);
    paintable.destroy;

  end;

PROCEDURE TanalysisForm.showForGate(CONST gate: P_abstractGate);
  VAR i:longint;
  begin
    clonedGate:=gate^.clone(false);
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);
  end;

PROCEDURE TanalysisForm.showForBoard(CONST board: P_circuitBoard);
  VAR i:longint;
  begin
    new(P_customGate(clonedGate),createFromBoard(board));
    setupTable;
    ShowModal;
    dispose(clonedGate,destroy);
    for i:=0 to length(simulationOutputs)-1 do simulationOutputs[i].destroy;
    setLength(simulationOutputs,0);
  end;

end.

