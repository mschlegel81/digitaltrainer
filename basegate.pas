UNIT baseGate;

{$mode objfpc}{$H+}

INTERFACE
USES ExtCtrls, Classes, Controls, StdCtrls, UITypes, wiringUtil,
  serializationUtil, logicGates,Menus;

CONST defaultBoardCaption='unbenannt';

TYPE
  F_simpleCallback=PROCEDURE of object;

  P_circuitBoard=^T_circuitBoard;
  P_workspace=^T_workspace;
  P_visualGate=^T_visualGate;
  T_visualGate=object
    private
      behavior:P_abstractGate;

      origin,size:T_point;
      board:P_circuitBoard;
      //visual
      labels:array of TLabel;
      shapes:array of TShape;
      //mouse interaction
      dragX,dragY:longint;
      dragging:boolean;
      movedDuringDrag:boolean;
      wireDragOutputIndex:longint;
      marked_:boolean;
    protected
      PROCEDURE ensureGuiElements;  virtual;
      PROCEDURE disposeGuiElements; virtual;
      PROCEDURE updateIoVisuals; virtual;

      CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
      DESTRUCTOR destroy; virtual;
    private
      PROCEDURE setMarked(CONST value:boolean);
      PROCEDURE inputClick(Sender: TObject);
      PROCEDURE mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE outputMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      FUNCTION getInputPositionInGridSize (CONST index:longint):T_point;
      FUNCTION getOutputPositionInGridSize(CONST index:longint):T_point;
      PROPERTY marked:boolean read marked_ write setMarked;
      FUNCTION numberOfInputs:longint;
      FUNCTION numberOfOutputs:longint;
    public
      PROCEDURE Repaint; virtual;
      PROPERTY getBehavior:P_abstractGate read behavior;
  end;

  P_visualGateForCustom=^T_visualGateForCustom;
  T_visualGateForCustom=object(T_visualGate)
  protected
    PROCEDURE ensureGuiElements; virtual;
    CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
  public
    PROCEDURE Repaint; virtual;
  end;

  P_visualGateForOutput=^T_visualGateForOutput;
  T_visualGateForOutput=object(T_visualGate)
  protected
    PROCEDURE ensureGuiElements; virtual;
    PROCEDURE updateIoVisuals; virtual;
    CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
  public
    PROCEDURE Repaint; virtual;
  end;

  T_inputMode=(im_bin,im_dec,im_2complement);

  P_visualGateForInput=^T_visualGateForInput;
  T_visualGateForInput=object(T_visualGate)
  protected
    inputMode:T_inputMode;
    edit     :TEdit;
    button   :TButton;
    PROCEDURE ensureGuiElements; virtual;
    PROCEDURE disposeGuiElements; virtual;
    PROCEDURE updateIoVisuals; virtual;
    CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
  public
    PROCEDURE Repaint; virtual;
  private
    //Events...
    PROCEDURE updateText;
    PROCEDURE ButtonClick   (Sender: TObject);
    PROCEDURE inputModeClick(Sender: TObject);
    PROCEDURE inputEditEditingDone(Sender: TObject);
    PROCEDURE inputEditKeyEvent(Sender: TObject; VAR key: char);
  end;

  T_visualGateConnector=object
    gate:P_visualGate;
    index:longint;
    FUNCTION loadFromStream(CONST board:P_circuitBoard; VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(CONST board:P_circuitBoard; VAR stream:T_bufferedOutputStreamWrapper);
  end;

  { T_workspace }

  T_workspace=object(T_serializable)
    paletteEntries:array of P_circuitBoard;
    currentBoard:P_circuitBoard;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
    PROCEDURE addCustomGate(CONST index:longint; CONST x0,y0:longint);

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE addCurrentBoardToPalette;

    PROCEDURE removePaletteEntry(CONST index:longint);
    PROCEDURE editPaletteEntry(CONST index:longint);
  end;

  T_repositionOutput=(ro_positionUnchanged,ro_positionFound,ro_noPositionFound);

  { T_gateConnector }

  { T_customGate }
  P_customGate=^T_customGate;
  T_customGate=object(T_abstractGate)
    prototype  :P_circuitBoard;
    gates      :array of P_abstractGate;
    name       :string;
    connections:array of record source,sink:T_gateConnector end;
    //TODO: Handle adapters separately?
    inputConnections :array of record
                        caption:string;
                        width:byte;
                        value:T_wireValue;
                        goesTo:array of T_gateConnector;
                      end;
    outputConnections:array of record
                        caption:string;
                        width:byte;
                        comesFrom:T_gateConnector;
                      end;

    CONSTRUCTOR create(CONST origin:P_circuitBoard);
    DESTRUCTOR destroy; virtual;
    PROCEDURE reset;                   virtual;
    FUNCTION  caption:string;          virtual;
    FUNCTION  numberOfInputs :longint; virtual;
    FUNCTION  numberOfOutputs:longint; virtual;
    FUNCTION  inputWidth (CONST index:longint):byte; virtual;
    FUNCTION  outputWidth(CONST index:longint):byte; virtual;
    FUNCTION  gateType:T_gateType; virtual;
    FUNCTION  simulateStep:boolean; virtual;
    FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
    FUNCTION setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
    FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
    FUNCTION  clone:P_abstractGate; virtual;
  end;

  { T_circuitBoard }

  T_circuitBoard=object
    public
      name       :string;
      description:string;
      paletteIndex:longint;
    private
      GUI:record
        zoom:longint;
        container:TWinControl;
        wireImage:TImage;
        gateContextMenu:TPopupMenu;
        anyChangeCallback:F_simpleCallback;
        lastClickedGate:P_visualGate;
      end;

      gates      :array of P_visualGate;

      //One source can be associated with many sinks
      logicWires:array of record
        source:   T_visualGateConnector;
        width:byte;
        wires:array of record
          sink:   T_visualGateConnector;
          visual: T_wirePath; //Nonpersistent!
          marked: boolean;    //Nonpersistent!
        end;
      end;

      incompleteWire:record
        dragging:boolean;
        width:byte;
        source:T_visualGateConnector;
        sourcePoint:T_point;
        lastPreviewTarget:T_point;
      end;
      wireGraph:P_wireGraph;
      FUNCTION repositionGate(CONST gateToCheck:P_visualGate; CONST considerWires:boolean):T_repositionOutput;
      FUNCTION positionNewGate(CONST gateToAdd:P_visualGate):boolean;
      FUNCTION isInputConnected(CONST gate:P_visualGate; CONST inputIndex:longint):boolean;
      PROCEDURE initWireGraph(CONST start: T_visualGateConnector; CONST includeWires:boolean=true);
      PROCEDURE drawAllWires;
      FUNCTION findWirePath(CONST start:T_visualGateConnector; CONST endPoint:T_point):T_wirePath;
      PROCEDURE drawTempWire(CONST targetPoint:T_point);
      PROCEDURE finishWireDrag(CONST targetPoint:T_point);
      PROCEDURE rewire;
      PROCEDURE WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      FUNCTION wrapGate(CONST origin:T_point;CONST g:P_abstractGate):P_visualGate;
      FUNCTION clone:P_circuitBoard;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;  virtual;
      PROCEDURE clear;
      PROCEDURE setSelectForAll(CONST doSelect:boolean);
      PROCEDURE attachGUI(CONST zoom:longint; CONST container:TWinControl; CONST wireImage:TImage; CONST gatePopup:TPopupMenu; CONST anyChangeCallback:F_simpleCallback);
      PROCEDURE detachGUI;
      PROCEDURE gateMoved(CONST gate:P_visualGate; CONST doneDragging:boolean);
      PROCEDURE anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE fixWireImageSize;
      PROCEDURE setZoom(CONST zoom:longint);
      PROCEDURE deleteInvalidWires;
      PROCEDURE deleteMarkedElements;
      PROCEDURE Repaint;
      FUNCTION simulateSteps(CONST count:longint):boolean;

      FUNCTION loadFromStream(CONST workspace:P_workspace; VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);

      PROPERTY lastClickedGate:P_visualGate read GUI.lastClickedGate;
      PROCEDURE reset;
  end;

IMPLEMENTATION
USES sysutils,math,Graphics,myGenerics,Dialogs;

OPERATOR =(CONST x,y:T_visualGateConnector):boolean;
  begin
    result:=(x.gate=y.gate) and (x.index=y.index);
  end;

PROCEDURE T_visualGateForInput.disposeGuiElements;
  begin
    inherited;
    if button<>nil then FreeAndNil(button);
    if edit  <>nil then FreeAndNil(edit);
  end;

PROCEDURE T_visualGateForInput.updateIoVisuals;
  begin
    inherited;
    if not(edit.Focused) then updateText;
  end;

CONSTRUCTOR T_visualGateForInput.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin
    inherited; size[0]+=1; size[1]+=1;
    inputMode:=im_bin;
  end;

CONSTRUCTOR T_visualGateForCustom.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin inherited; end;

CONSTRUCTOR T_visualGateForOutput.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin inherited; size[1]+=1; end;

FUNCTION T_visualGateConnector.loadFromStream(CONST board: P_circuitBoard; VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR gateIndex:longint;
  begin
    gateIndex:=stream.readNaturalNumber;
    if gateIndex>length(board^.gates) then exit(false);
    gate:=board^.gates[gateIndex];
    index:=stream.readNaturalNumber;
    result:=stream.allOkay and ((index<gate^.numberOfInputs) or (index<gate^.numberOfOutputs));
  end;

PROCEDURE T_visualGateConnector.saveToStream(CONST board: P_circuitBoard; VAR stream: T_bufferedOutputStreamWrapper);
  VAR gateIndex:longint=0;
  begin
    while (gateIndex<length(board^.gates)) and (gate<>board^.gates[gateIndex]) do inc(gateIndex);
    stream.writeNaturalNumber(gateIndex);
    stream.writeNaturalNumber(index);
  end;

PROCEDURE T_visualGate.ensureGuiElements;
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI.container<>nil) then begin
      setLength(shapes,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
      shapes[0]:=TShape.create(board^.GUI.container);
      shapes[0].parent:=board^.GUI.container;
      shapes[0].Shape :=stRectangle;
      shapes[0].OnMouseDown:=@mainShapeMouseDown;
      shapes[0].OnMouseMove:=@mainShapeMouseMove;
      shapes[0].OnMouseUp  :=@mainShapeMouseUp;
      shapes[0].PopupMenu  :=board^.GUI.gateContextMenu;
      setLength(labels,1);

      labels[0]:=TLabel.create(board^.GUI.container);
      labels[0].caption:=behavior^.caption;
      labels[0].AutoSize:=true;
      labels[0].Font.size:=6;
      labels[0].parent:=board^.GUI.container;
      labels[0].OnMouseDown:=@mainShapeMouseDown;
      labels[0].OnMouseMove:=@mainShapeMouseMove;
      labels[0].OnMouseUp  :=@mainShapeMouseUp;
      labels[0].PopupMenu  :=board^.GUI.gateContextMenu;

      for k:=0 to behavior^.numberOfInputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnClick  :=@inputClick;
        shapes[shapeIndex].parent:=board^.GUI.container;
        inc(shapeIndex);
      end;

      for k:=0 to behavior^.numberOfOutputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnMouseDown:=@outputMouseDown;
        shapes[shapeIndex].OnMouseMove:=@outputMouseMove;
        shapes[shapeIndex].OnMouseUp  :=@outputMouseUp;
        shapes[shapeIndex].parent:=board^.GUI.container;
        inc(shapeIndex);
      end;
    end;
  end;

PROCEDURE T_visualGateForCustom.ensureGuiElements;
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI.container<>nil) then begin
      inherited;
      setLength(labels,length(shapes));
      for k:=0 to behavior^.numberOfInputs-1 do begin
        labels[shapeIndex]:=TLabel.create(board^.GUI.container);
        labels[shapeIndex].Tag:=k;
        labels[shapeIndex].parent:=board^.GUI.container;
        labels[shapeIndex].caption:=P_customGate(behavior)^.inputConnections[k].caption;
        labels[shapeIndex].OnClick:=@inputClick;
        inc(shapeIndex);
      end;
      for k:=0 to behavior^.numberOfOutputs-1 do begin
        labels[shapeIndex]:=TLabel.create(board^.GUI.container);
        labels[shapeIndex].Tag:=k;
        labels[shapeIndex].parent:=board^.GUI.container;
        labels[shapeIndex].caption:=P_customGate(behavior)^.outputConnections[k].caption;
        labels[shapeIndex].OnMouseDown:=@outputMouseDown;
        labels[shapeIndex].OnMouseMove:=@outputMouseMove;
        labels[shapeIndex].OnMouseUp  :=@outputMouseUp;
        inc(shapeIndex);
      end;
    end;
  end;

PROCEDURE T_visualGateForOutput.ensureGuiElements;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI.container<>nil) then begin
      inherited;
      setLength(labels,2);
      labels[1]:=TLabel.create(board^.GUI.container);
      labels[1].parent:=board^.GUI.container;
      labels[1].caption:='bin: ?'+LineEnding+
                         'dec: ?'+LineEnding+
                         'neg: ?';
      labels[1].OnMouseDown:=@mainShapeMouseDown;
      labels[1].OnMouseMove:=@mainShapeMouseMove;
      labels[1].OnMouseUp  :=@mainShapeMouseUp;
    end;
  end;

PROCEDURE T_visualGateForInput.ensureGuiElements;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI.container<>nil) then begin
      inherited;
      button:=TButton.create(board^.GUI.container);
      button.parent:=        board^.GUI.container;
      button.caption:=behavior^.caption;
      button.Font.size:=board^.GUI.zoom;
      button.OnMouseDown:=@mainShapeMouseDown;
      button.OnMouseMove:=@mainShapeMouseMove;
      button.OnMouseUp  :=@mainShapeMouseUp;
      button.Font.name :=labels[0].Font.name;
      button.OnClick:=@ButtonClick;

      edit:=TEdit.create(board^.GUI.container);
      edit.parent:=      board^.GUI.container;
      edit.OnEditingDone:=@inputEditEditingDone;
      edit.OnKeyPress   :=@inputEditKeyEvent;

      setLength(labels,2);
      labels[1]:=TLabel.create(board^.GUI.container);
      labels[1].parent:=board^.GUI.container;
      labels[1].caption:='bin:';
      labels[1].OnClick:=@inputModeClick;
    end;
  end;

PROCEDURE T_visualGateForOutput.updateIoVisuals;
  begin
    inherited;
    labels[1].caption:='bin: '+getBinaryString(behavior^.getInput(0))+LineEnding+
                       'dec: '+getDecimalString(behavior^.getInput(0))+LineEnding+
                       'neg: '+get2ComplementString(behavior^.getInput(0));
  end;

PROCEDURE T_visualGate.disposeGuiElements;
  VAR i:longint;
  begin
    if length(shapes)>0 then begin
      for i:=0 to length(shapes)-1 do shapes[i].free;
      for i:=0 to length(labels)-1 do labels[i].free;
      setLength(labels,0);
      setLength(shapes,0);
    end;
  end;

CONSTRUCTOR T_visualGate.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin
    origin:=origin_;
    behavior:=gateToWrap;
    size:=pointOf(4,max(2,2*max(numberOfInputs,numberOfOutputs)));

    dragging:=false;
    marked  :=false;
    board   :=board_;

    setLength(shapes,0);
  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    dispose(behavior,destroy);
    disposeGuiElements;
  end;

PROCEDURE T_visualGate.setMarked(CONST value: boolean);
  begin
    if marked_=value then exit;
    marked_:=value;
    if length(shapes)=0 then exit;
    if marked
    then shapes[0].Brush.color:=clYellow
    else shapes[0].Brush.color:=clWhite;
  end;

CONST TRI_STATE_NOT:array[T_triStateValue] of T_triStateValue=(tsv_true,tsv_false,tsv_false);

PROCEDURE T_visualGate.inputClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=TControl(Sender).Tag;
    if behavior^.inputWidth(k)=1 then behavior^.setInput(k,TRI_STATE_NOT[behavior^.getInput(k).bit[0]]);
    updateIoVisuals;
    if (board<>nil) and (board^.GUI.anyChangeCallback<>nil) then board^.GUI.anyChangeCallback();
  end;

PROCEDURE T_visualGate.mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=true;
      movedDuringDrag:=false;
      if (ssShift in Shift) or (ssCtrl in Shift) then marked:=not(marked);
      shapes[0].Pen.style:=psDash;
      dragX:=x;
      dragY:=y;
    end else if (button=mbRight) then begin
      board^.GUI.lastClickedGate:=@self;
    end;
  end;

PROCEDURE T_visualGate.mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR newOrigin:T_point;
      dx,dy:longint;
  begin
    if dragging then begin
      dx:=x-dragX;
      dy:=y-dragY;
      newOrigin:=origin+pointOf(round(dx/board^.GUI.zoom),round(dy/board^.GUI.zoom));
      if newOrigin<>origin then begin
        movedDuringDrag:=true;
        origin:=newOrigin;
        board^.gateMoved(@self,false);
      end else Repaint;
    end;
  end;

PROCEDURE T_visualGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      dragging:=false;
      board^.incompleteWire.dragging:=false;
      shapes[0].Pen.style:=psSolid;
      if movedDuringDrag then board^.gateMoved(@self,true);
      Repaint;
    end;
  end;

PROCEDURE T_visualGate.outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
  begin
    if (button=mbLeft) then begin
      wireDragOutputIndex:=TControl(Sender).Tag;
      p:=getOutputPositionInGridSize(wireDragOutputIndex);
      with board^.incompleteWire do begin
        dragging:=true;
        source.gate:=@self;
        source.index:=wireDragOutputIndex;
        width:=behavior^.outputWidth(source.index);
        sourcePoint:=p;
        lastPreviewTarget:=p;
      end;
      dragX:=x+TControl(Sender).Left;
      dragY:=y+TControl(Sender).top;
    end;
  end;

PROCEDURE T_visualGate.outputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if board^.incompleteWire.dragging
    then board^.drawTempWire(pointOf(round((dragX+x)/board^.GUI.zoom),
                                     round((dragY+y)/board^.GUI.zoom)));
  end;

PROCEDURE T_visualGate.outputMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if board=nil then exit;
    board^.finishWireDrag(pointOf(dragX+x,dragY+y));
  end;

FUNCTION T_visualGate.getInputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0];
    result[1]:=origin[1]+(index*2-(numberOfInputs-1))+size[1] div 2;
  end;

FUNCTION T_visualGate.getOutputPositionInGridSize(CONST index: longint): T_point;
  begin
    result[0]:=origin[0]+size[0];
    result[1]:=origin[1]+(index*2-(numberOfOutputs-1))+size[1] div 2;
  end;

PROCEDURE T_visualGate.Repaint;
  VAR k,newFontSize:longint;
      shapeIndex :longint=1;
      p:T_point;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=origin[0]*board^.GUI.zoom;
    shapes[0].top   :=origin[1]*board^.GUI.zoom;
    shapes[0].width :=size  [0]*board^.GUI.zoom;
    shapes[0].height:=size  [1]*board^.GUI.zoom;

    newFontSize:=min(round(labels[0].Font.size*shapes[0].width  *0.75/labels[0].width),
                     round(labels[0].Font.size*shapes[0].height *0.5 /labels[0].height));
    if abs(newFontSize-labels[0].Font.size)>1 then labels[0].Font.size:=newFontSize;
    labels[0].top :=shapes[0].top +(shapes[0].height-labels[0].height) div 2;
    labels[0].Left:=shapes[0].Left+(shapes[0].width -labels[0].width) div 2 ;

    for k:=0 to numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_visualGateForCustom.Repaint;
  VAR shapeIndex:longint=1;
      k:longint;
      ioLabelFontSize:longint;
  begin
    if length(shapes)=0 then exit;
    inherited;
    ioLabelFontSize:=round(board^.GUI.zoom*0.2);
    if ioLabelFontSize<3 then begin
      for k:=1 to length(labels)-1 do labels[k].visible:=false;
      exit;
    end;

    for k:=0 to numberOfInputs-1 do begin
      labels[shapeIndex].visible:=true;
      labels[shapeIndex].Font.size:=ioLabelFontSize;
      labels[shapeIndex].Left:=shapes[shapeIndex].Left+(shapes[shapeIndex].width -labels[shapeIndex].width ) div 2;
      labels[shapeIndex].top :=shapes[shapeIndex].top +(shapes[shapeIndex].height-labels[shapeIndex].height) div 2;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      labels[shapeIndex].visible:=true;
      labels[shapeIndex].Font.size:=ioLabelFontSize;
      labels[shapeIndex].Left:=shapes[shapeIndex].Left+(shapes[shapeIndex].width -labels[shapeIndex].width ) div 2;
      labels[shapeIndex].top :=shapes[shapeIndex].top +(shapes[shapeIndex].height-labels[shapeIndex].height) div 2;
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_visualGateForInput.Repaint;
  VAR caption:shortstring;
      newFontSize:longint;
  begin
    if length(shapes)=0 then exit;
    caption:=behavior^.caption;
    button   .caption:=caption;
    labels[0].caption:=caption;
    labels[0].visible:=true;
    inherited;
    labels[0].visible:=false;

    button.top   :=shapes[0].top;
    button.Left  :=shapes[0].Left;
    button.width :=shapes[0].width;
    button.height:=shapes[0].height div 2;
    edit  .height:=shapes[0].height div 2;

    button.Font.size:=labels[0].Font.size;

    newFontSize:=min(round(labels[1].Font.size*shapes[0].width *0.25/labels[1].width),
                     round(labels[1].Font.size*shapes[0].height*0.25/labels[1].height));
    if abs(newFontSize-labels[1].Font.size)>1 then begin
      labels[1].Font.size:=newFontSize;
      edit     .Font.size:=newFontSize;
    end;

    labels[1].Left:=shapes[0].Left+4;
    labels[1].top :=round(button.top+1.5*button.height-0.5*labels[1].height);
    edit.top      :=round(button.top+1.5*button.height-0.5*edit     .height);
    edit.Left     := labels[1].Left+labels[1].width+4;
    edit.width    :=(shapes[0].Left+shapes[0].width-4)-edit.Left ;
  end;

PROCEDURE T_visualGateForInput.updateText;
 begin
   case inputMode of
     im_bin:         edit.text:=getBinaryString (behavior^.getOutput(0));
     im_dec:         edit.text:=getDecimalString(behavior^.getOutput(0));
     im_2complement: edit.text:=get2ComplementString(behavior^.getOutput(0));
   end;
 end;

PROCEDURE T_visualGateForInput.ButtonClick(Sender: TObject);
  VAR i:longint;
      w:T_wireValue;
  begin
    w:=behavior^.getInput(0);
    for i:=0 to 7 do w.bit[i]:=TRI_STATE_NOT[w.bit[i]];
    behavior^.setInput(0,w);
    updateIoVisuals;
    if (board<>nil) and (board^.GUI.anyChangeCallback<>nil) then board^.GUI.anyChangeCallback();
  end;

PROCEDURE T_visualGateForInput.inputModeClick(Sender: TObject);
  begin
    case inputMode of
      im_bin        : begin inputMode:=im_dec        ; labels[1].caption:='dec:'; end;
      im_dec        : begin inputMode:=im_2complement; labels[1].caption:='neg:'; end;
      im_2complement: begin inputMode:=im_bin        ; labels[1].caption:='bin:'; end;
    end;
    updateText;
  end;

PROCEDURE T_visualGateForInput.inputEditEditingDone(Sender: TObject);
  VAR w:T_wireValue;
  begin
    case inputMode of
      im_bin:         w:=parseWireBin        (edit.text,behavior^.inputWidth(0));
      im_dec:         w:=parseWireDecimal    (edit.text,behavior^.inputWidth(0));
      im_2complement: w:=parseWire2Complement(edit.text,behavior^.inputWidth(0));
    end;
    behavior^.setInput(0,w);
    updateText;
    updateIoVisuals;
    if (board<>nil) and (board^.GUI.anyChangeCallback<>nil) then board^.GUI.anyChangeCallback();
  end;

PROCEDURE T_visualGateForInput.inputEditKeyEvent(Sender: TObject; VAR key: char);
  VAR AllowedKeys:array[T_inputMode] of set of char=
      {im_bin        } ([#8,'0','1'],
      {im_dec        }  [#8,'0'..'9'],
      {im_2complement}  [#8,'-','0'..'9']);
  begin
    if key=#13 then begin
      inputEditEditingDone(Sender);
      key:=#0;
      exit;
    end;
    if not(key in AllowedKeys[inputMode]) then key:=#0;
  end;

PROCEDURE T_visualGateForOutput.Repaint;
  VAR k,newFontSize:longint;
      shapeIndex :longint=1;
      p:T_point;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=origin[0]*board^.GUI.zoom;
    shapes[0].top   :=origin[1]*board^.GUI.zoom;
    shapes[0].width :=size  [0]*board^.GUI.zoom;
    shapes[0].height:=size  [1]*board^.GUI.zoom;

    for k:=0 to numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI.zoom);
      shapes[shapeIndex].width :=board^.GUI.zoom;
      shapes[shapeIndex].height:=board^.GUI.zoom;
      inc(shapeIndex);
    end;

    labels[0].caption:=behavior^.caption;
    newFontSize:=min(round(labels[0].Font.size*shapes[0].width *0.75/labels[0].width),
                     round(labels[0].Font.size*shapes[0].height*0.4 /labels[0].height));
    if abs(newFontSize-labels[0].Font.size)>1 then labels[0].Font.size:=newFontSize;

    newFontSize:=min(round(labels[1].Font.size*shapes[0].width *0.75/labels[1].width),
                     round(labels[1].Font.size*shapes[0].height*0.5 /labels[1].height));
    if abs(newFontSize-labels[1].Font.size)>1 then labels[1].Font.size:=newFontSize;

    labels[0].top:=shapes[0].top;
    labels[1].top:=labels[0].top+labels[0].height;

    labels[0].Left:=shapes[0].Left+(shapes[0].width -labels[0].width) div 2 ;
    labels[1].Left:=shapes[0].Left+round(board^.GUI.zoom*0.7);
  end;

PROCEDURE T_visualGate.updateIoVisuals;
  VAR shapeIndex:longint=1;
      i:longint;
      w:T_wireValue;
  begin
    if (length(shapes)=0) then begin
      ensureGuiElements;
      Repaint;
    end;
    for i:=0 to numberOfInputs-1 do begin
      w:=behavior^.getInput(i);
      if w.width>1 then   shapes[shapeIndex].Brush.color:=clBlue
      else case w.bit[0] of
        tsv_true        : shapes[shapeIndex].Brush.color:=clLime;
        tsv_false       : shapes[shapeIndex].Brush.color:=clGray;
        tsv_undetermined: shapes[shapeIndex].Brush.color:=clBtnFace;
      end;
      inc(shapeIndex);
    end;
    for i:=0 to numberOfOutputs-1 do begin
      w:=behavior^.getOutput(i);
      if w.width>1 then   shapes[shapeIndex].Brush.color:=clBlue;
      case w.bit[0]  of
        tsv_true        : shapes[shapeIndex].Brush.color:=clLime;
        tsv_false       : shapes[shapeIndex].Brush.color:=clGray;
        tsv_undetermined: shapes[shapeIndex].Brush.color:=clBtnFace;
      end;
      inc(shapeIndex);
    end;
  end;

FUNCTION T_visualGate.numberOfInputs: longint;
  begin
    result:=behavior^.numberOfInputs;
  end;

FUNCTION T_visualGate.numberOfOutputs: longint;
  begin
    result:=behavior^.numberOfOutputs;
  end;

{ T_customGate }

CONSTRUCTOR T_customGate.create(CONST origin: P_circuitBoard);
  FUNCTION myCorrespondingGate(CONST original:T_visualGateConnector):T_gateConnector;
    VAR i:longint;
    begin
      for i:=0 to length(origin^.gates)-1 do begin
        if origin^.gates[i]=original.gate
        then begin
          result.gate:=gates[i];
          result.index:=original.index;
          exit(result);
        end;
      end;
      raise Exception.create('Connection cannot be reproduced!');
    end;

  PROCEDURE addConnection(CONST src,tgt:T_gateConnector);
    VAR k,ioIdx,i:longint;
    begin
      if src.gate^.gateType=gt_input then begin
        ioIdx:=P_inputGate(src.gate)^.ioIndex;
        if length(inputConnections)<=ioIdx then begin
          k:=length(inputConnections);
          setLength(inputConnections,ioIdx+1);
          while k<length(inputConnections) do begin
            setLength(inputConnections[k].goesTo,0);
            for i:=0 to 7 do inputConnections[k].value.bit[i]:=tsv_true;
            inc(k);
          end;
        end;
        k:=length(inputConnections[ioIdx].goesTo);
        setLength(inputConnections[ioIdx].goesTo,k+1);
        inputConnections[ioIdx].goesTo[k]:=tgt;
        inputConnections[ioIdx].width  :=P_inputGate(src.gate)^.width;
        inputConnections[ioIdx].caption:=P_inputGate(src.gate)^.caption;
      end else if tgt.gate^.gateType=gt_output then begin
        ioIdx:=P_outputGate(tgt.gate)^.ioIndex;
        if length(outputConnections)<=ioIdx then begin
          k:=length(outputConnections);
          setLength(outputConnections,ioIdx+1);
          while k<length(outputConnections) do begin
            outputConnections[k].comesFrom.gate:=nil;
            inc(k);
          end;
        end;
        outputConnections[ioIdx].comesFrom:=src;
        outputConnections[ioIdx].width    :=P_outputGate(tgt.gate)^.width;
        outputConnections[ioIdx].caption  :=P_outputGate(tgt.gate)^.caption;
      end else begin
        k:=length(connections);
        setLength(connections,k+1);
        connections[k].source:=src;
        connections[k].sink  :=tgt;
      end;
    end;

  FUNCTION anyOutputUndetermined:boolean;
    VAR k,j:longint;
        w:T_wireValue;

    begin
      for k:=0 to numberOfOutputs-1 do begin
        w:=getOutput(k);
        for j:=0 to w.width-1 do
        if w.bit[j]=tsv_undetermined then exit(true);
      end;
      result:=false;
    end;

  VAR i,j:longint;
      src,tgt:T_gateConnector;
  begin
    inherited create;
    prototype:=origin;

    setLength(connections      ,0);
    setLength(inputConnections ,0);
    setLength(outputConnections,0);
    setLength(gates,length(origin^.gates));
    for i:=0 to length(gates)-1 do gates[i]:=origin^.gates[i]^.behavior^.clone;
    name:=origin^.name;

    for i:=0 to length(origin^.logicWires)-1 do begin
      src:=myCorrespondingGate(origin^.logicWires[i].source);
      for j:=0 to length(origin^.logicWires[i].wires)-1 do begin
        tgt:=myCorrespondingGate(origin^.logicWires[i].wires[j].sink);
        addConnection(src,tgt);
      end;
    end;

    j:=0;
    //Actually we do not need input and output gates, so we drop them again...
    for i:=0 to length(gates)-1 do
    if gates[i]^.gateType in [gt_input,gt_output]
    then dispose(gates[i],destroy)
    else begin
      gates[j]:=gates[i];
      inc(j);
    end;
    setLength(gates,j);

    //simulate some steps to arrive at valid state...
   // i:=0;
   // while anyOutputUndetermined and (i<1000) do begin
   //   for j:=0 to numberOfInputs-1 do if random>0.5
   //   then setInput(j,tsv_true)
   //   else setInput(j,tsv_false);
   //   for j:=0 to 3 do simulateStep;
   //   inc(i);
   // end;
  end;

PROCEDURE T_customGate.reset;
  VAR gate:P_abstractGate;
      i:longint;
  begin
    for gate in gates do gate^.reset;
    for i:=0 to length(inputConnections)-1 do inputConnections[i].value:=tsv_true;
  end;

DESTRUCTOR T_customGate.destroy;
  VAR i:longint;
  begin
    inherited;
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy); setLength(gates,0);
    for i:=0 to length(inputConnections)-1 do setLength(inputConnections[i].goesTo,0); setLength(inputConnections,0);
    setLength(outputConnections,0);
    setLength(connections,0);
  end;

FUNCTION T_customGate.caption: string;
  begin
    result:=name;
  end;

FUNCTION T_customGate.numberOfInputs: longint;
  begin
    result:=length(inputConnections);
  end;

FUNCTION T_customGate.numberOfOutputs: longint;
  begin
    result:=length(outputConnections);
  end;

FUNCTION T_customGate.inputWidth (CONST index:longint):byte;
  begin
    result:=inputConnections[index].width;
  end;

FUNCTION T_customGate.outputWidth(CONST index:longint):byte;
  begin
    result:=outputConnections[index].width;
  end;

FUNCTION T_customGate.gateType: T_gateType;
  begin
    result:=gt_compound;
  end;

FUNCTION T_customGate.simulateStep:boolean;
  VAR gate:P_abstractGate;
      i:longint;
      tgt:T_gateConnector;
      v:T_wireValue;
  begin
    result:=false;
    for i:=0 to length(inputConnections)-1 do begin
      v:=inputConnections[i].value;
      if isFullyDefined(v)
      then for tgt in inputConnections[i].goesTo do result:=tgt.setInputValue(v) or result;
    end;
    for gate in gates do result:=gate^.simulateStep or result;
    for i:=0 to length(connections)-1 do with connections[i] do begin
      v:=source.getOutputValue;
      if isFullyDefined(v)
      then result:=sink.setInputValue(v) or result;
    end;
  end;

FUNCTION T_customGate.getOutput(CONST index: longint): T_wireValue;
  begin
    result:=outputConnections[index].comesFrom.getOutputValue;
  end;

FUNCTION T_customGate.setInput(CONST index: longint; CONST value: T_wireValue):boolean;
  begin
    result:=inputConnections[index].value<>value;
    inputConnections[index].value:=value;
  end;

FUNCTION T_customGate.getInput(CONST index: longint): T_wireValue;
  begin
    result:=inputConnections[index].value;
  end;

FUNCTION T_customGate.clone: P_abstractGate;
  begin
    new(P_customGate(result),create(prototype));
  end;

{ T_workspace }

CONSTRUCTOR T_workspace.create;
  begin
    setLength(paletteEntries,0);
    new(currentBoard,create);
  end;

DESTRUCTOR T_workspace.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(paletteEntries)-1 do dispose(paletteEntries[i],destroy);
    dispose(currentBoard,destroy);
  end;

PROCEDURE T_workspace.addBaseGate(CONST gateType:T_gateType; CONST x0,y0:longint);
  FUNCTION numberOf(CONST gateType:T_gateType):longint;
    VAR gate:P_visualGate;
    begin
      result:=0;
      for gate in currentBoard^.gates do
      if gate^.behavior^.gateType=gateType
      then inc(result);
    end;

  VAR gateToAdd:P_abstractGate=nil;
      visual:P_visualGate;
  begin
    gateToAdd:=newBaseGate(gateType);
    if gateToAdd<>nil then begin
      case gateToAdd^.gateType of
        gt_input:  P_inputGate (gateToAdd)^.ioIndex:=numberOf(gt_input);
        gt_output: P_outputGate(gateToAdd)^.ioIndex:=numberOf(gt_output);
      end;

      visual:=currentBoard^.wrapGate(pointOf(x0,y0),gateToAdd);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

PROCEDURE T_workspace.addCustomGate(CONST index: longint; CONST x0, y0: longint);
  VAR visual:P_visualGate;
      gateToAdd:P_customGate;
  begin
    if (index>=0) and (index<length(paletteEntries)) then begin
      new(gateToAdd,create(paletteEntries[index]));
      visual:=currentBoard^.wrapGate(pointOf(x0,y0),gateToAdd);
      if not currentBoard^.positionNewGate(visual)
      then dispose(visual,destroy);
    end;
  end;

FUNCTION T_workspace.getSerialVersion: dword;
  begin
    result:=5;
  end;

FUNCTION T_workspace.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      count: qword;
      board:P_circuitBoard;
  begin
    if not(inherited) then exit(false);
    count:=stream.readNaturalNumber;
    result:=true;
    if (count<maxLongint) then begin
      for i:=0 to longint(count)-1 do begin
        new(board,create);
        if board^.loadFromStream(@self,stream) then begin
          setLength(paletteEntries,i+1);
          paletteEntries[i]:=board;
          board^.paletteIndex:=i;
        end else begin
          dispose(board,destroy);
          exit(false);
        end;
      end;
    end else exit(false);
    result:=currentBoard^.loadFromStream(@self,stream);
    result:=result and stream.allOkay;
  end;

PROCEDURE T_workspace.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i:longint;
  begin
    inherited;
    stream.writeNaturalNumber(length(paletteEntries));
    for i:=0 to length(paletteEntries)-1 do paletteEntries[i]^.saveToStream(stream);
    currentBoard^.saveToStream(stream);
  end;

PROCEDURE T_workspace.addCurrentBoardToPalette;
  VAR i:longint;
      gate:P_visualGate;
      anyOut:boolean=false;
      newPaletteEntry:P_circuitBoard;
      doReplace: TModalResult;
  begin
    if currentBoard^.name=defaultBoardCaption then begin
      ShowMessage('Die aktuelle Schaltung muss erst noch benannt werden.');
      exit;
    end;
    for gate in currentBoard^.gates do begin
      anyOut:=anyOut or (gate^.behavior^.gateType=gt_output);
      for i:=0 to gate^.numberOfInputs-1 do
      if not(currentBoard^.isInputConnected(gate,i))
      then begin
        ShowMessage('Es gibt unbelegte Eingänge in der Schaltung.');
        exit;
      end;
    end;
    if not(anyOut) then begin
      ShowMessage('Die Schaltung hat keine Ausgänge.');
      exit;
    end;

    if currentBoard^.paletteIndex>=0 then begin
      doReplace:=QuestionDlg('Ersetzen?','Soll die Schaltung in der Palette aktualisiert werden?',TMsgDlgType.mtConfirmation,[mrYes, 'Ja', mrNo, 'Nein', 'IsDefault'],'');
    end else doReplace:=mrNo;

    if (doReplace=mrNo) then
    for i:=0 to length(paletteEntries)-1 do if paletteEntries[i]^.name=currentBoard^.name
    then begin
      ShowMessage('Es gibt schon eine Schaltung mit diesem Namen in der Palette.');
      exit;
    end;

    newPaletteEntry:=currentBoard;
    currentBoard   :=nil;
    new(currentBoard,create);
    currentBoard^.attachGUI(
      newPaletteEntry^.GUI.zoom,
      newPaletteEntry^.GUI.container,
      newPaletteEntry^.GUI.wireImage,
      newPaletteEntry^.GUI.gateContextMenu,
      newPaletteEntry^.GUI.anyChangeCallback);
    newPaletteEntry^.detachGUI;

    if doReplace=mrYes then begin
      i:=newPaletteEntry^.paletteIndex;
      dispose(paletteEntries[i],destroy);
      paletteEntries[i]:=newPaletteEntry;
      //TODO: The palette may be used in other boards, which would lead to errors...
    end else begin
      i:=length(paletteEntries);
      setLength(paletteEntries,i+1);
      paletteEntries[i]:=newPaletteEntry;
      newPaletteEntry^.paletteIndex:=i;
    end;
  end;

{ T_circuitBoard }

CONSTRUCTOR T_circuitBoard.create;
  begin
    with GUI do begin
      zoom:=1;
      container:=nil;
      wireImage:=nil;
    end;
    paletteIndex:=-1;
    name:=defaultBoardCaption;
    description:='';
    setLength(gates,0);
    setLength(logicWires,0);
    wireGraph:=nil;
    incompleteWire.dragging:=false;
  end;

DESTRUCTOR T_circuitBoard.destroy;
  begin
    clear;
  end;

PROCEDURE T_circuitBoard.attachGUI(CONST zoom: longint; CONST container: TWinControl; CONST wireImage: TImage; CONST gatePopup:TPopupMenu; CONST anyChangeCallback:F_simpleCallback);
  VAR gate:P_visualGate;
  begin
    GUI.zoom:=zoom;
    GUI.container:=container;
    GUI.wireImage:=wireImage;
    GUI.gateContextMenu:=gatePopup;
    GUI.anyChangeCallback:=anyChangeCallback;
    wireImage.OnMouseDown:=@WireImageMouseDown;
    for gate in gates do gate^.ensureGuiElements;
    rewire;
  end;

PROCEDURE T_circuitBoard.detachGUI;
  VAR gate:P_visualGate;
  begin
    for gate in gates do gate^.disposeGuiElements;
    GUI.zoom:=1;
    GUI.container:=nil;
    GUI.wireImage:=nil;
    GUI.gateContextMenu:=nil;
    GUI.anyChangeCallback:=nil;
  end;

PROCEDURE T_circuitBoard.clear;
  VAR i:longint;
  begin
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    setLength(logicWires,0);
    if wireGraph<>nil then dispose(wireGraph,destroy);
    wireGraph:=nil;
    paletteIndex:=-1;
    description:='';
    name:=defaultBoardCaption;
    Repaint;
  end;

PROCEDURE T_circuitBoard.setSelectForAll(CONST doSelect: boolean);
  VAR gate:P_visualGate;
      i,j:longint;
  begin
    for gate in gates do gate^.setMarked(doSelect);
    for i:=0 to length(logicWires)-1 do
    with logicWires[i] do
    for j:=0 to length(wires)-1 do
    wires[j].marked:=doSelect;
    drawAllWires;
  end;

FUNCTION T_circuitBoard.repositionGate(CONST gateToCheck: P_visualGate; CONST considerWires:boolean): T_repositionOutput;
  FUNCTION isOriginValid(CONST o:T_point):boolean;
    VAR gate:P_visualGate;
        i,j,k,run:longint;
        p:T_point;
        direction:T_wireDirection;
    begin
      if (o[0]<5) or (o[0]+gateToCheck^.size[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5) or
         (o[1]<5) or (o[1]+gateToCheck^.size[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES-5)
      then exit(false);
      result:=true;
      for gate in gates do
      if (gate<>gateToCheck) and
         (o[0]+gateToCheck^.size[0]>gate^.origin[0]) and (o[0]<gate^.origin[0]+gate^.size[0]) and
         (o[1]+gateToCheck^.size[1]>gate^.origin[1]) and (o[1]<gate^.origin[1]+gate^.size[1])
      then exit(false);

      if considerWires then
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do begin
        for k:=0 to length(visual)-2 do begin
          p:=visual[k];
          direction:=directionBetween  (p,visual[k+1]);
          for run:=0 to maxNormDistance(p,visual[k+1]) do begin
            if (p[0]>=o[0]-1) and (p[0]<=o[0]+gateToCheck^.size[0]+1) and
               (p[1]>=o[1]-1) and (p[1]<=o[1]+gateToCheck^.size[1]+1) then exit(false);
            p+=direction;
          end;
        end;
      end;
    end;

  VAR newOrigin:T_point;
  FUNCTION applyOrigin:T_repositionOutput;
    begin
      gateToCheck^.origin:=newOrigin;
      result:=ro_positionFound;
    end;

  VAR range:longint=1;
      i:longint;
  begin
    newOrigin:=gateToCheck^.origin;
    if isOriginValid(newOrigin)
    then exit(ro_positionUnchanged)
    else repeat
      for i:=1 to range do begin newOrigin+=wd_right; if isOriginValid(newOrigin) then exit(applyOrigin); end;
      for i:=1 to range do begin newOrigin+=wd_down;  if isOriginValid(newOrigin) then exit(applyOrigin); end;
      range+=1;
      for i:=1 to range do begin newOrigin+=wd_left;  if isOriginValid(newOrigin) then exit(applyOrigin); end;
      for i:=1 to range do begin newOrigin+=wd_up;    if isOriginValid(newOrigin) then exit(applyOrigin); end;
      range+=1;
    until range>BOARD_MAX_SIZE_IN_GRID_ENTRIES;
    result:=ro_noPositionFound;
  end;

FUNCTION T_circuitBoard.positionNewGate(CONST gateToAdd: P_visualGate): boolean;
  begin
    if repositionGate(gateToAdd,true)<>ro_noPositionFound then begin
      setLength(gates,length(gates)+1);
      gates[length(gates)-1]:=gateToAdd;
      result:=true;
      gateToAdd^.ensureGuiElements;
      gateToAdd^.Repaint;
    end else result:=false;
  end;

PROCEDURE T_circuitBoard.deleteInvalidWires;
  VAR k,j,i:longint;
  begin
    for k:=0 to length(logicWires)-1 do with logicWires[k] do begin
      width:=source.gate^.behavior^.outputWidth(source.index);
      j:=0;
      for i:=0 to length(wires)-1 do
      if wires[i].sink.gate^.behavior^.inputWidth(wires[i].sink.index)=width
      then begin
        wires[j]:=wires[i];
        inc(j);
      end;
      setLength(wires,j);
    end;
    j:=0;
    for i:=0 to length(logicWires)-1 do
    if length(logicWires[i].wires)>0 then begin
      logicWires[j]:=logicWires[i];
      inc(j);
    end;
    Repaint;
  end;

PROCEDURE T_circuitBoard.deleteMarkedElements;
  PROCEDURE removeAssociatedWires(CONST gateToDelete:P_visualGate);
    VAR i:longint;
        j:longint=0;
        i_,j_:longint;
    begin
      for i:=0 to length(logicWires)-1 do begin
        if logicWires[i].source.gate<>gateToDelete then begin
          logicWires[j]:=logicWires[i];
          with logicWires[j] do begin
            j_:=0;
            for i_:=0 to length(wires)-1 do begin
              if wires[i_].sink.gate<>gateToDelete then begin
                wires[j_]:=wires[i_];
                inc(j_);
              end;
            end;
            setLength(wires,j_);
          end;
          inc(j);
        end;
      end;
      setLength(logicWires,j);
    end;

  PROCEDURE enumerateIo;
    VAR gate:P_visualGate;
        inputIndex:longint=0;
        outputIndex:longint=0;
    begin
      for gate in gates do
      case gate^.behavior^.gateType of
        gt_input: begin
          P_inputGate(gate^.behavior)^.ioIndex:=inputIndex;
          inc(inputIndex);
        end;
        gt_output: begin
          P_outputGate(gate^.behavior)^.ioIndex:=outputIndex;
          inc(outputIndex);
        end;
      end;
    end;

  VAR k,i:longint;
      j:longint=0;
      ioDeleted:boolean=false;
  begin
    GUI.lastClickedGate:=nil;
    for i:=0 to length(gates)-1 do begin
      if gates[i]^.marked
      then begin
        ioDeleted:=ioDeleted or (gates[i]^.behavior^.gateType in [gt_input,gt_output]);
        removeAssociatedWires(gates[i]);
        dispose(gates[i],destroy)
      end
      else begin
        gates[j]:=gates[i];
        inc(j);
      end;
    end;
    setLength(gates,j);

    if ioDeleted then enumerateIo;

    for k:=0 to length(logicWires)-1 do with logicWires[k] do begin
      j:=0;
      for i:=0 to length(wires)-1 do
      if not(wires[i]).marked
      then begin
        wires[j]:=wires[i];
        inc(j);
      end;
      setLength(wires,j);
    end;

    j:=0;
    for i:=0 to length(logicWires)-1 do
    if length(logicWires[i].wires)>0 then begin
      logicWires[j]:=logicWires[i];
      inc(j);
    end;

    rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.fixWireImageSize;
  VAR width :longint=0;
      height:longint=0;
      gate:P_visualGate;
      p:T_point;
      i,j:longint;
  begin
    if GUI.wireImage<>nil then begin
      for gate in gates do begin
        p:=gate^.origin+gate^.size;
        if p[0]>width  then width :=p[0];
        if p[1]>height then height:=p[1];
      end;
      for i:=0 to length(logicWires)-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      for p in logicWires[i].wires[j].visual do begin
        if p[0]>width  then width :=p[0];
        if p[1]>height then height:=p[1];
      end;
      width +=1; width *=GUI.zoom; width +=max(1,round(GUI.zoom*0.15));
      height+=1; height*=GUI.zoom; height+=max(1,round(GUI.zoom*0.15));
      GUI.wireImage.SetBounds(0,0,width,height);
      GUI.wireImage.picture.Bitmap.Canvas.Brush.color:=clBtnFace;
      GUI.wireImage.picture.Bitmap.setSize(width,height);
      GUI.wireImage.picture.Bitmap.Canvas.clear;
    end;
  end;

PROCEDURE T_circuitBoard.setZoom(CONST zoom: longint);
  begin
    GUI.zoom:=zoom;
    Repaint;
  end;

FUNCTION T_circuitBoard.simulateSteps(CONST count:longint):boolean;
  VAR gate:P_visualGate;
      i,j,step:longint;
      output:T_wireValue;
  begin
    result:=false;
    for step:=1 to count do begin
      for gate in gates do result:=gate^.behavior^.simulateStep or result;
      for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
        output:=source.gate^.behavior^.getOutput(source.index);
        if isFullyDefined(output) then for j:=0 to length(wires)-1 do
          result:=wires[j].sink.gate^.behavior^.setInput(wires[j].sink.index,output) or result;
      end;
    end;
    for gate in gates do gate^.updateIoVisuals;
  end;

FUNCTION T_circuitBoard.wrapGate(CONST origin:T_point; CONST g:P_abstractGate):P_visualGate;
  begin
    case g^.gateType of
      gt_compound: new(P_visualGateForCustom(result),create(origin,g,@self));
      gt_output  : new(P_visualGateForOutput(result),create(origin,g,@self));
      gt_input   : new(P_visualGateForInput (result),create(origin,g,@self));
      else         new(                      result ,create(origin,g,@self));
    end;
  end;

FUNCTION T_circuitBoard.loadFromStream(CONST workspace: P_workspace; VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i:longint;
      gateType: T_gateType;
      k:longint;
      origin: T_point;
      behavior: P_abstractGate;
  begin
    name:=stream.readShortString;
    description:=stream.readShortString;
    paletteIndex:=stream.readLongint;
    if not(stream.allOkay) then exit(false);
    setLength(gates,stream.readNaturalNumber);
    for i:=0 to length(gates)-1 do begin
      gateType:=T_gateType(stream.readByte([byte(low(T_gateType))..byte(high(T_gateType))]));

      if not(stream.allOkay) then exit(false);

      if gateType=gt_compound then begin
        k:=stream.readNaturalNumber;
        //TODO: This provokes later NPEs!
        if k>=length(workspace^.paletteEntries) then exit(false);
        new(P_customGate(behavior),create(workspace^.paletteEntries[k]));
      end else begin
        behavior:=newBaseGate(gateType);
        case gateType of
          gt_input : begin
            P_inputGate (behavior)^.ioIndex:=stream.readNaturalNumber;
            P_inputGate (behavior)^.ioLabel:=stream.readShortString;
            P_inputGate (behavior)^.width  :=stream.readByte;
          end;
          gt_output: begin
            P_outputGate(behavior)^.ioIndex:=stream.readNaturalNumber;
            P_outputGate(behavior)^.ioLabel:=stream.readShortString;
            P_inputGate (behavior)^.width  :=stream.readByte;
          end;
          gt_clock   : P_clock(behavior)^.interval:=stream.readNaturalNumber;
        end;
      end;
      origin:=readPoint(stream);
      behavior^.reset;
      gates[i]:=wrapGate(origin,behavior);
    end;

    setLength(logicWires,stream.readNaturalNumber);
    for i:=0 to length(logicWires)-1 do begin
      logicWires[i].source.loadFromStream(@self,stream);
      logicWires[i].width:=logicWires[i].source.gate^.behavior^.outputWidth(logicWires[i].source.index);
      setLength(logicWires[i].wires,stream.readNaturalNumber);
      for k:=0 to length(logicWires[i].wires)-1 do begin
        logicWires[i].wires[k].sink.loadFromStream(@self,stream);
        setLength(logicWires[i].wires[k].visual,0);
      end;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_circuitBoard.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,k:longint;
  begin
    stream.writeShortString(name);
    stream.writeShortString(description);
    stream.writeLongint(paletteIndex);
    stream.writeNaturalNumber(length(gates));
    for i:=0 to length(gates)-1 do begin
      stream.writeByte(byte(gates[i]^.behavior^.gateType));
      case gates[i]^.behavior^.gateType of
        gt_input   : begin
          stream.writeNaturalNumber(P_inputGate (gates[i]^.behavior)^.ioIndex);
          stream.writeShortString  (P_inputGate (gates[i]^.behavior)^.ioLabel);
          stream.writeByte         (P_inputGate (gates[i]^.behavior)^.width);
        end;
        gt_output  : begin
          stream.writeNaturalNumber(P_outputGate(gates[i]^.behavior)^.ioIndex);
          stream.writeShortString  (P_outputGate(gates[i]^.behavior)^.ioLabel);
          stream.writeByte         (P_outputGate(gates[i]^.behavior)^.width);
        end;
        gt_clock   : stream.writeNaturalNumber(P_clock     (gates[i]^.behavior)^.interval);
        gt_compound: stream.writeNaturalNumber(P_customGate(gates[i]^.behavior)^.prototype^.paletteIndex);
      end;

      writePointToStream(stream,gates[i]^.origin);
    end;
    stream.writeNaturalNumber(length(logicWires));
    for i:=0 to length(logicWires)-1 do begin
      logicWires[i].source.saveToStream(@self,stream);
      stream.writeNaturalNumber(length(logicWires[i].wires));
      for k:=0 to length(logicWires[i].wires)-1 do begin
        logicWires[i].wires[k].sink.saveToStream(@self,stream);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.reset;
  VAR gate:P_visualGate;
  begin
    for gate in gates do begin
      gate^.behavior^.reset;
      gate^.updateIoVisuals;
    end;
  end;

PROCEDURE T_circuitBoard.Repaint;
  VAR gate:P_visualGate;
  begin
    if (GUI.container=nil) then inherited else begin
      for gate in gates do gate^.Repaint;
      fixWireImageSize;
      drawAllWires;
    end;
  end;

FUNCTION T_circuitBoard.isInputConnected(CONST gate: P_visualGate;
  CONST inputIndex: longint): boolean;
  VAR i,j:longint;
  begin
    result:=false;
    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
        for j:=0 to length(wires)-1 do
          if (wires[j].sink.gate=gate) and
             (wires[j].sink.index=inputIndex)
          then exit(true);
  end;

PROCEDURE T_circuitBoard.anyMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    dispose(wireGraph,destroy);
    incompleteWire.dragging:=false;
    drawAllWires;
  end;

PROCEDURE T_circuitBoard.drawAllWires;
  PROCEDURE drawWires(CONST index:longint; CONST foreground:boolean);
    VAR j,k:longint;
    begin for j:=0 to length(logicWires[index].wires)-1 do with logicWires[index].wires[j] do if length(visual)>1 then begin
      if foreground then begin
        if logicWires[index].wires[j].marked
        then GUI.wireImage.Canvas.Pen.color:=clYellow
        else GUI.wireImage.Canvas.Pen.color:=clBlack;
      end;
      GUI.wireImage.Canvas.MoveTo(visual[0,0]*GUI.zoom,visual[0,1]*GUI.zoom);
      for k:=1 to length(visual)-1 do
      GUI.wireImage.Canvas.LineTo(visual[k,0]*GUI.zoom,visual[k,1]*GUI.zoom);
    end; end;

  VAR i:longint;
      wireWidth,gapWidth:longint;
  begin
    if GUI.wireImage=nil then exit;
    with GUI.wireImage.Canvas do begin
      Brush.color:=clBtnFace;
      clear;
      for i:=0 to length(logicWires)-1 do begin
        case logicWires[i].width of
          0..1: begin
            wireWidth:=max(1,round(GUI.zoom*0.08));
            gapWidth :=max(1,round(GUI.zoom*0.38));
          end;
          4..7: begin
            wireWidth:=max(1,round(GUI.zoom*0.15));
            gapWidth :=max(1,round(GUI.zoom*0.45));
          end;
          else begin
            wireWidth:=max(1,round(GUI.zoom*0.25));
            gapWidth :=max(1,round(GUI.zoom*0.55));
          end;
        end;
        Pen.color:=clBtnFace; Pen.width:=gapWidth;
        drawWires(i,false);

        Pen.width:=wireWidth;
        drawWires(i,true);
      end;
    end;
  end;

PROCEDURE T_circuitBoard.initWireGraph(CONST start: T_visualGateConnector; CONST includeWires: boolean);
  VAR gate:P_visualGate;
      x,y,i,j:longint;
  begin
    new(wireGraph,create);
    for gate in gates do begin
      //Points within the gate cannot be reached
      for x:=gate^.origin[0] to gate^.origin[0]+gate^.size[0] do
      for y:=gate^.origin[1] to gate^.origin[1]+gate^.size[1] do
      wireGraph^.dropNode(pointOf(x,y));
      //Input connections:
      for i:=0 to gate^.numberOfInputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getInputPositionInGridSize(i)+wd_left,wd_right);
      //Output connections:
      for i:=0 to gate^.numberOfOutputs-1
      do wireGraph^.addUnidirectionalEdge(gate^.getOutputPositionInGridSize(i),wd_right);
      //No diagonals right left and right of the gate (to prevent blocking of I/O)
      x:=gate^.origin[0];
      for y:=gate^.origin[1]+1 to gate^.origin[1]+gate^.size[1]-1 do wireGraph^.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
      x:=gate^.origin[1]+1;
      for y:=gate^.origin[1]+1 to gate^.origin[1]+gate^.size[1]-1 do wireGraph^.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
    end;
    if includeWires then
    for i:=0 to length(logicWires)-1 do
    if (logicWires[i].source<>start) then with logicWires[i] do
      for j:=0 to length(wires)-1 do wireGraph^.dropWire(wires[j].visual);
  end;

FUNCTION T_circuitBoard.findWirePath(CONST start: T_visualGateConnector; CONST endPoint: T_point): T_wirePath;
  begin
    if wireGraph=nil then initWireGraph(start,true);
    if wireGraph^.anyEdgeLeadsTo(endPoint)
    then result:=wireGraph^.findPath(start.gate^.getOutputPositionInGridSize(start.index),endPoint)
    else setLength(result,0);
  end;

PROCEDURE T_circuitBoard.drawTempWire(CONST targetPoint: T_point);
  VAR wire:T_wirePath;
      i:longint;
  begin
    if (GUI.wireImage=nil) or (incompleteWire.lastPreviewTarget=targetPoint) then exit;
    incompleteWire.lastPreviewTarget:=targetPoint;
    wire:=findWirePath(incompleteWire.source,targetPoint);
    if length(wire)<=0 then exit;
    drawAllWires;
    with GUI.wireImage.Canvas do begin
      Pen.color:=clRed;
      Pen.width:=max(1,round(GUI.zoom*0.15));
      MoveTo(wire[0,0]*GUI.zoom,wire[0,1]*GUI.zoom);
      for i:=1 to length(wire)-1 do LineTo(wire[i,0]*GUI.zoom,wire[i,1]*GUI.zoom);
    end;
  end;

PROCEDURE T_circuitBoard.gateMoved(CONST gate: P_visualGate; CONST doneDragging: boolean);
  begin
    repositionGate(gate,false);
    if doneDragging then rewire;
    Repaint;
  end;

PROCEDURE T_circuitBoard.finishWireDrag(CONST targetPoint: T_point);
  PROCEDURE cleanup;
    begin
      fixWireImageSize;
      drawAllWires;
      dispose(wireGraph,destroy);
      wireGraph:=nil;
      incompleteWire.dragging:=false;
    end;

  VAR i:longint=0;
      j:longint;
      gate:P_visualGate;
      wireAdded:boolean=false;
      connector:T_visualGateConnector;

      distanceToConnector:longint=maxLongint;
      newDistance:longint;
  begin
    if not(incompleteWire.dragging) then exit;
    connector.gate:=nil;
    for gate in gates do
    for j:=0 to gate^.numberOfInputs-1 do begin
      newDistance:=maxNormDistance(gate^.getInputPositionInGridSize(j)*GUI.zoom,targetPoint);
      if newDistance<distanceToConnector then begin
        connector.gate:=gate;
        connector.index:=j;
        distanceToConnector:=newDistance;
      end;
    end;

    if (connector.gate=nil) or (distanceToConnector>1.5*GUI.zoom) then begin cleanup; exit; end;
    if not(isInputConnected(connector.gate,connector.index)) and
       (connector.gate^.behavior^.inputWidth (connector.index)=incompleteWire.width) then begin
      i:=0;
      while (i<length(logicWires)) and (logicWires[i].source<>incompleteWire.source) do inc(i);
      if i>=length(logicWires) then setLength(logicWires,i+1);
      with logicWires[i] do begin
        source:=incompleteWire.source;
        width :=incompleteWire.width;
        j:=length(wires);
        setLength(wires,j+1);
        wires[j].sink:=connector;
        wireAdded:=true;
      end;
    end;
    if wireAdded then rewire;
    if (GUI.anyChangeCallback<>nil) then GUI.anyChangeCallback();
    cleanup;
    Repaint;
  end;

PROCEDURE T_circuitBoard.rewire;
  VAR connector:T_visualGateConnector;
      i,j:longint;

      needAnyRewire:boolean=false;
      preview:array of record
        startPoint:T_point;
        targetPoints:T_wirePath;
        needRewire:boolean;
      end;

      paths:T_wirePathArray;
  begin
    setLength(preview,length(logicWires));
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      preview[i].needRewire:=false;
      preview[i].startPoint:=source.gate^.getOutputPositionInGridSize(source.index);
      setLength(preview[i].targetPoints,length(wires));
      for j:=0 to length(preview[i].targetPoints)-1 do begin
        preview[i].targetPoints[j]:=wires[j].sink.gate^.getInputPositionInGridSize(wires[j].sink.index);
        preview[i].needRewire:=preview[i].needRewire or
           (length(wires[j].visual)<=0) or //no wire there at all
           (wires[j].visual[0]<>preview[i].startPoint) or //start point off
           (wires[j].visual[length(wires[j].visual)-1]<>preview[i].targetPoints[j]);
      end;
      needAnyRewire:=needAnyRewire or preview[i].needRewire;
    end;

    if not(needAnyRewire) then exit;

    if wireGraph<>nil then dispose(wireGraph,destroy);
    connector.gate:=nil;
    connector.index:=0;
    initWireGraph(connector,false);

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if not(preview[i].needRewire)
      then for j:=0 to length(wires)-1 do wireGraph^.dropWire(wires[j].visual);

    for i:=0 to length(logicWires)-1 do
      with logicWires[i] do
      if preview[i].needRewire then begin
        paths:=wireGraph^.findPaths(preview[i].startPoint,preview[i].targetPoints);
        for j:=0 to length(wires)-1 do begin
          wires[j].visual:=   paths[j];
          wireGraph^.dropWire(paths[j]);
        end;
        setLength(paths,0);
      end;

    fixWireImageSize;
  end;

PROCEDURE T_circuitBoard.WireImageMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  FUNCTION wireHitsPoint(CONST p:T_point; CONST wire:T_wirePath):boolean;
    VAR i,j,len:longint;
        a,b,pointBetween:T_point;
        dir:T_wireDirection;
    begin
      for i:=0 to length(wire)-2 do begin
        a:=wire[i];
        b:=wire[i+1];
        if (a=p) or (b=p) then exit(true);
        len:=maxNormDistance(a,b);
        if len>1 then begin
          dir:=directionBetween(a,b);
          pointBetween:=a;
          for j:=1 to len-1 do begin
            pointBetween+=dir;
            if p=pointBetween then exit(true);
          end;
        end;
      end;
      result:=false;
    end;

  VAR p:T_point;
      i,j:longint;
      anyChange:boolean=false;
  begin
    if (button=mbLeft) and ((ssShift in Shift) or (ssCtrl in Shift)) then begin
      p:=pointOf(round(x / GUI.zoom),round(y / GUI.zoom));
      for i:=0 to length(logicWires         )-1 do
      for j:=0 to length(logicWires[i].wires)-1 do
      with logicWires[i].wires[j] do
      if wireHitsPoint(p,visual)
      then begin
        marked:=not(marked);
        anyChange:=true;
      end;
      if anyChange then drawAllWires;
    end;
  end;

FUNCTION T_circuitBoard.clone: P_circuitBoard;
  FUNCTION convert(CONST connector:T_visualGateConnector; CONST tgt:P_circuitBoard):T_visualGateConnector;
    VAR gateIndex:longint=0;
    begin
      while (gateIndex<length(gates)) and (connector.gate<>gates[gateIndex]) do inc(gateIndex);
      result.gate:=tgt^.gates[gateIndex];
      result.index:=connector.index;
    end;

  VAR i,j:longint;
  begin
    new(result,create);
    result^.name:=name;
    result^.description:=description;
    result^.paletteIndex:=paletteIndex;

    setLength(result^.gates,length(gates));
    for i:=0 to length(gates)-1 do begin
      result^.gates[i]:=result^.wrapGate(gates[i]^.origin,gates[i]^.behavior^.clone);
    end;

    setLength(result^.logicWires,length(logicWires));
    for i:=0 to length(logicWires)-1 do begin
      result^.logicWires[i].source:=
      convert(logicWires[i].source,result);
      setLength(result^.logicWires[i].wires,length(logicWires[i].wires));
      for j:=0 to length(logicWires[i].wires)-1 do begin
        setLength(result^.logicWires[i].wires[j].visual,0);
        result^.logicWires[i].wires[j].sink:=
        convert(logicWires[i].wires[j].sink,result);
      end;
    end;
  end;

PROCEDURE T_workspace.removePaletteEntry(CONST index:longint);
  VAR j:longint;
  begin
    //TODO: Entry may be used!
    if (index<0) or (index>length(paletteEntries)) then exit;
    dispose(paletteEntries[index],destroy);
    for j:=index to length(paletteEntries)-2 do begin
      paletteEntries[j]:=paletteEntries[j+1];
      paletteEntries[j]^.paletteIndex:=j;
    end;
    setLength(paletteEntries,length(paletteEntries)-1);
  end;

PROCEDURE T_workspace.editPaletteEntry(CONST index:longint);
  VAR previous:P_circuitBoard;
  begin
    if (index<0) or (index>length(paletteEntries)) then exit;
    previous:=currentBoard;
    currentBoard:=paletteEntries[index]^.clone;
    currentBoard^.attachGUI(
      previous^.GUI.zoom,
      previous^.GUI.container,
      previous^.GUI.wireImage,
      previous^.GUI.gateContextMenu,
      previous^.GUI.anyChangeCallback);
    currentBoard^.rewire;
    dispose(previous,destroy);
    currentBoard^.Repaint;
  end;

end.

