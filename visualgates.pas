UNIT visualGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Graphics, logicalGates, compoundGates, StdCtrls, ExtCtrls,
  serializationUtil, Forms, Controls, wiringUtil, math, sprites, BGRABitmap,
  BGRACanvas;
TYPE
  P_uiAdapter=^T_uiAdapter;
  P_visualGate=^T_visualGate;
  P_visualBoard=^T_visualBoard;
  T_visualGateArray=array of P_visualGate;

  T_hoverInfo=record
    subElement:(noHit,block,inputConnector,outputConnector);
    ioIndex:longint;
  end;

  {$define includeInterface}
  {$i visualWires.inc}
  {$i uiAdapters.inc}
  {$i visualBoards.inc}

  { T_visualGate }

  T_visualGate=object
    private
      gridWidth,gridHeight:longint;
      behavior:P_abstractGate;
      ioLocations: T_ioLocations;
      outputMark:T_ioMark;
      marked:boolean;
      PROCEDURE ioEditEditingDone(Sender: TObject);
      PROCEDURE ioEditKeyPress(Sender: TObject; VAR key: char);
    public
      ioMode:T_multibitWireRepresentation;
      fixedPosition,fixedProperties:boolean;
      canvasPos,
      gridPos:T_point;
      uiAdapter:P_uiAdapter;

      CONSTRUCTOR create(CONST behavior_:P_abstractGate);
      CONSTRUCTOR createForDisabledPaletteItem(CONST behavior_:P_abstractGate);
      DESTRUCTOR destroy;
      PROCEDURE setupVisuals;

      FUNCTION simulateStep:boolean;
      PROCEDURE paintAll(CONST Canvas:TCanvas);
      PROCEDURE paintAll(CONST Canvas:TCanvas; CONST zoom:longint);
      FUNCTION  clone:P_visualGate;
      PROCEDURE propertyEditDone;

      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;

      FUNCTION overlaps(CONST other:P_visualGate):boolean;
      PROPERTY  getBehavior:P_abstractGate read behavior;

      PROPERTY getGridWidth:longint read gridWidth;
      PROPERTY getGridHeight:longint read gridHeight;
      FUNCTION isAtGridPos(CONST p:T_point; OUT info:T_hoverInfo):boolean;
      PROCEDURE flipInputBits;
      FUNCTION isMarkedAsDisabled:boolean;
  end;

{$undef includeInterface}
IMPLEMENTATION
USES visuals;
{$define includeImplementation}
{$i visualWires.inc}
{$i uiAdapters.inc}
{$i visualBoards.inc}
{$undef includeImplementation}
PROCEDURE T_visualGate.ioEditKeyPress(Sender: TObject; VAR key: char);
  VAR AllowedKeys:array[T_multibitWireRepresentation] of set of char=
      (['0','1'     ,#8], //wr_binary,
       ['0'..'9'    ,#8], //wr_decimal,
       ['0'..'9','-',#8]);//wr_2complement
  begin
    if key=#13 then begin
      ioEditEditingDone(Sender);
      uiAdapter^.hideIoEdit;
    end else if key=#32 then begin
      behavior^.setInput(0,parseWire(uiAdapter^.uiElement.ioEdit.text,behavior^.inputWidth(0),ioMode));
      paintAll(uiAdapter^.uiElement.boardImage.Canvas);
      uiAdapter^.uiElement.boardImage.Invalidate;
      uiAdapter^.callback.boardModifiedCallback();
      key:=#0;
    end else if not(key in AllowedKeys[ioMode]) then key:=#0;
  end;

PROCEDURE T_visualGate.ioEditEditingDone(Sender: TObject);
  begin
    behavior^.setInput(0,parseWire(uiAdapter^.uiElement.ioEdit.text,behavior^.inputWidth(0),ioMode));
    uiAdapter^.uiElement.ioEdit.text:=getWireString(behavior^.getInput(0),ioMode);
    paintAll(uiAdapter^.uiElement.boardImage.Canvas);
    uiAdapter^.uiElement.boardImage.Invalidate;
    uiAdapter^.callback.boardModifiedCallback();
  end;

CONSTRUCTOR T_visualGate.create(CONST behavior_: P_abstractGate);
  begin
    fixedPosition:=false;
    fixedProperties:=false;
    uiAdapter :=nil;
    canvasPos:=pointOf(0,0);
    gridPos  :=pointOf(0,0);
    gridWidth :=0;
    gridHeight:=0;
    behavior:=behavior_;
    ioLocations:=behavior^.getIoLocations;
    outputMark:=iom_none;
    setupVisuals;
  end;

CONSTRUCTOR T_visualGate.createForDisabledPaletteItem(CONST behavior_:P_abstractGate);
  begin
    create(behavior_);
    outputMark:=iom_disabled;
  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    setLength(ioLocations.p[gt_input],0);
    setLength(ioLocations.p[gt_output],0);
    dispose(behavior,destroy);
  end;

PROCEDURE T_visualGate.setupVisuals;
  begin
    ioLocations:=behavior^.getIoLocations;
    case behavior^.gateType of
      gt_input,gt_output: begin
        gridWidth :=4;
        gridHeight:=4;
      end;
      gt_7segmentDummy: begin
        gridWidth :=4;
        gridHeight:=6;
      end;
      else begin
        gridHeight:=1;
        if (length(ioLocations.p[gt_input])>0) and (length(ioLocations.p[gt_output])>0)
        then gridWidth:=2
        else gridWidth:=1;
        gridHeight:=2*max(gridHeight,max(ioLocations.numberOfLeftInputs,ioLocations.numberOfRightOutputs ));
        gridWidth :=2*max(gridWidth ,max(ioLocations.numberOfTopInputs ,ioLocations.numberOfBottomOutputs));
      end;
    end;
  end;

FUNCTION T_visualGate.simulateStep: boolean;
  begin
    result:=behavior^.simulateStep;
  end;

PROCEDURE T_visualGate.paintAll(CONST Canvas: TCanvas);
  begin
    canvasPos:=uiAdapter^.gridToCanvas(gridPos);
    paintAll(Canvas,uiAdapter^.zoom);
  end;

PROCEDURE T_visualGate.paintAll(CONST Canvas: TCanvas; CONST zoom: longint);
  FUNCTION myInputIndex:longint;
    begin
      if behavior^.gateType<>gt_input then exit(-1);
      result:=P_inputGate(behavior)^.ioIndex;
    end;

  VAR k: integer;
      p: T_point;
  begin
    case behavior^.gateType of
      gt_input,gt_output: begin
        getIoBlockSprite(behavior^.getCaption,myInputIndex,marked,outputMark)^.renderAt(Canvas,zoom,canvasPos);
        getIoTextSprite(behavior^.getInput(0),ioMode)^.renderAt(Canvas,zoom,canvasPos);
      end;
      gt_7segmentDummy:
        get7SegmentSprite(behavior^.getInput(0),marked)^.renderAt(Canvas,zoom,canvasPos);
      gt_clock,gt_gatedClock:
        getClockSprite(behavior^.gateType=gt_gatedClock,P_clock(behavior)^.tick,P_clock(behavior)^.interval,P_clock(behavior)^.counter,marked)^.renderAt(Canvas,zoom,canvasPos);
      gt_adapter:
        getAdapterSprite(behavior^.inputWidth(0),behavior^.outputWidth(0),marked)^.renderAt(Canvas,zoom,canvasPos);
      else begin
        getBlockSprite  (behavior^.getCaption,gridWidth,gridHeight,marked,outputMark)^.renderAt(Canvas,zoom,canvasPos);
      end;
    end;

    for k:=0 to length(ioLocations.p[gt_input])-1 do with ioLocations.p[gt_input,k] do
      if leftOrRight then begin
        p:=pointOf(canvasPos[0],
                   canvasPos[1]+zoom*((positionIndex*2-(ioLocations.numberOfLeftInputs-1))+gridHeight div 2));
        getIoSprite(io_right,behavior^.getInput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end else begin
        p:=pointOf(canvasPos[0]+zoom*((positionIndex*2-(ioLocations.numberOfTopInputs-1))+gridWidth div 2),
                   canvasPos[1]);
        getIoSprite(io_bottom,behavior^.getInput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end;

    for k:=0 to length(ioLocations.p[gt_output])-1 do with ioLocations.p[gt_output,k] do
      if leftOrRight then begin
        p:=pointOf(canvasPos[0]+gridWidth*zoom,
                   canvasPos[1]+zoom*((positionIndex*2-(ioLocations.numberOfRightOutputs-1))+gridHeight div 2));
        getIoSprite(io_left,behavior^.getOutput(k),ioLabel)^.renderAt(Canvas,zoom,p);
      end else begin
        p:=pointOf(canvasPos[0]+zoom*((positionIndex*2-(ioLocations.numberOfBottomOutputs-1))+gridWidth div 2),
                   canvasPos[1]+zoom*gridHeight);
        getIoSprite(io_top,behavior^.getOutput(k),ioLabel)^.renderAt(Canvas,uiAdapter^.getZoom,p);
      end;
  end;

FUNCTION T_visualGate.clone: P_visualGate;
  begin
    new(result,create(behavior^.clone(true)));
    result^.gridPos        :=gridPos;
    result^.uiAdapter      :=uiAdapter;
    result^.canvasPos      :=canvasPos;
    result^.ioMode         :=ioMode;
    result^.fixedProperties:=fixedProperties;
    result^.fixedPosition  :=false;
  end;

PROCEDURE T_visualGate.propertyEditDone();
  begin
    setupVisuals;
  end;

FUNCTION T_visualGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    if ioLocations.p[gt_input,index].leftOrRight
    then begin
      result[0]:=0;
      result[1]:=(ioLocations.p[gt_input,index].positionIndex*2-(ioLocations.numberOfLeftInputs-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations.p[gt_input,index].positionIndex*2-(ioLocations.numberOfTopInputs-1))+gridWidth div 2;
      result[1]:=0;
    end;
  end;

FUNCTION T_visualGate.getOutputPositionInGridSize(CONST index: longint): T_point;
  begin
    if ioLocations.p[gt_output,index].leftOrRight
    then begin
      result[0]:=gridWidth;
      result[1]:=(ioLocations.p[gt_output,index].positionIndex*2-(ioLocations.numberOfRightOutputs-1))+gridHeight div 2;
    end else begin
      result[0]:=(ioLocations.p[gt_output,index].positionIndex*2-(ioLocations.numberOfBottomOutputs-1))+gridWidth div 2;
      result[1]:=gridHeight;
    end;
  end;

FUNCTION T_visualGate.overlaps(CONST other: P_visualGate): boolean;
  begin
    result:=(max(gridPos[0],other^.gridPos[0])<min(gridPos[0]+gridWidth ,other^.gridPos[0]+other^.gridWidth ))
        and (max(gridPos[1],other^.gridPos[1])<min(gridPos[1]+gridHeight,other^.gridPos[1]+other^.gridHeight));
  end;

FUNCTION T_visualGate.isAtGridPos(CONST p: T_point; OUT info: T_hoverInfo): boolean;
  VAR i:longint;
      d: T_point;
  begin
    if (p[0]<gridPos[0]) or (p[1]<gridPos[1]) or (p[0]>gridPos[0]+gridWidth) or (p[1]>gridPos[1]+gridHeight) then exit(false);

    for i:=0 to length(ioLocations.p[gt_input])-1 do begin
      d:=gridPos+getInputPositionInGridSize(i);
      if (d=p) then begin
        info.subElement:=inputConnector;
        info.ioIndex:=i;
        exit(true);
      end;
    end;

    for i:=0 to length(ioLocations.p[gt_output])-1 do begin
      d:=gridPos+getOutputPositionInGridSize(i);
      if (d=p) then begin
        info.subElement:=outputConnector;
        info.ioIndex:=i;
        exit(true);
      end;
    end;

    if p[1]-gridPos[1]>=gridHeight div 2 then info.ioIndex:=1 else info.ioIndex:=0;
    info.subElement:=block;
    result:=true;
  end;

PROCEDURE T_visualGate.flipInputBits;
  CONST flipped:array[T_triStateValue] of T_triStateValue=//tsv_false,tsv_undetermined,tsv_true
                                                          (tsv_true,tsv_false,tsv_false);
  VAR wireValue: T_wireValue;
      b:byte;
  begin
    wireValue:=behavior^.getInput(0);
    for b:=0 to wireValue.width-1 do wireValue.bit[b]:=flipped[wireValue.bit[b]];
    behavior^.setInput(0,wireValue);
    paintAll(uiAdapter^.uiElement.boardImage.Canvas);
    uiAdapter^.repaintImage;
    uiAdapter^.callback.boardModifiedCallback();
    uiAdapter^.hideIoEdit;
  end;

FUNCTION T_visualGate.isMarkedAsDisabled:boolean;
  begin
    result:=outputMark=iom_disabled;
  end;

end.

