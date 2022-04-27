{$ifdef includeInterface}
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
    dragging:(not_dragging,drag_self,drag_selection);
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
    FUNCTION isCompletelyInsideRect(CONST low,high:T_point):boolean;
  public
    PROCEDURE repaint; virtual;
    PROPERTY getBehavior:P_abstractGate read behavior;
    FUNCTION nextGateAfter:T_point;
    PROCEDURE forcedFullRepaint; virtual;
    FUNCTION getOuputDescription(CONST index:longint):string; virtual;
end;

P_visualGateForCustom=^T_visualGateForCustom;
T_visualGateForCustom=object(T_visualGate)
protected
  PROCEDURE ensureGuiElements; virtual;
  CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
public
  PROCEDURE repaint; virtual;
  FUNCTION getOuputDescription(CONST index:longint):string; virtual;
end;

P_visualGateForOutput=^T_visualGateForOutput;
T_visualGateForOutput=object(T_visualGate)
protected
  PROCEDURE ensureGuiElements; virtual;
  PROCEDURE updateIoVisuals; virtual;
  CONSTRUCTOR create(CONST origin_:T_point; CONST gateToWrap:P_abstractGate; CONST board_:P_circuitBoard);
public
  PROCEDURE repaint; virtual;
  PROCEDURE forcedFullRepaint; virtual;
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
  PROCEDURE repaint; virtual;
  PROCEDURE forcedFullRepaint; virtual;
private
  //Events...
  PROCEDURE updateText;
  PROCEDURE ButtonClick   (Sender: TObject);
  PROCEDURE inputModeClick(Sender: TObject);
  PROCEDURE inputEditEditingDone(Sender: TObject);
  PROCEDURE inputEditKeyEvent(Sender: TObject; VAR key: char);
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_visualGate.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin
    origin:=origin_;
    behavior:=gateToWrap;
    size:=pointOf(4,max(2,2*max(numberOfInputs,numberOfOutputs)));

    dragging:=not_dragging;
    marked  :=false;
    board   :=board_;

    setLength(shapes,0);
  end;

CONSTRUCTOR T_visualGateForCustom.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin inherited; end;

CONSTRUCTOR T_visualGateForInput.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin
    inherited; size:=pointOf(6,3);
    inputMode:=im_bin;
  end;

CONSTRUCTOR T_visualGateForOutput.create(CONST origin_: T_point; CONST gateToWrap: P_abstractGate; CONST board_: P_circuitBoard);
  begin inherited; size:=pointOf(6,3);  end;

DESTRUCTOR T_visualGate.destroy;
  begin
    dispose(behavior,destroy);
    disposeGuiElements;
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

PROCEDURE T_visualGateForInput.disposeGuiElements;
  begin
    inherited;
    if button<>nil then FreeAndNil(button);
    if edit  <>nil then FreeAndNil(edit);
  end;

PROCEDURE T_visualGate.updateIoVisuals;
  FUNCTION colorOf(CONST w:T_wireValue):TColor;
    begin
      if w.width>1 then begin
        if isFullyDefined(w)
        then result:=MultiBitColor
        else result:=BackgroundColor;
      end else case w.bit[0] of
        tsv_true        : result:=TrueColor;
        tsv_false       : result:=FalseColor;
        tsv_undetermined: result:=BackgroundColor;
      end;
    end;

  VAR shapeIndex:longint=1;
      i:longint;
  begin
    if (length(shapes)=0) then begin
      ensureGuiElements;
      repaint;
    end;
    for i:=0 to numberOfInputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getInput(i));
      inc(shapeIndex);
    end;
    for i:=0 to numberOfOutputs-1 do begin
      shapes[shapeIndex].Brush.color:=colorOf(behavior^.getOutput(i));
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_visualGateForInput.updateIoVisuals;
  begin
    inherited;
    if not(edit.Focused) then updateText;
  end;

PROCEDURE T_visualGateForOutput.updateIoVisuals;
  begin
    inherited;
    labels[1].caption:='bin: '+getBinaryString(behavior^.getInput(0))+LineEnding+
                       'dec: '+getDecimalString(behavior^.getInput(0))+LineEnding+
                       'sig: '+get2ComplementString(behavior^.getInput(0));
  end;

PROCEDURE T_visualGate.ensureGuiElements;
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI<>nil) then begin
      setLength(shapes,1+behavior^.numberOfInputs+behavior^.numberOfOutputs);
      shapes[0]:=TShape.create(board^.GUI^.container);
      shapes[0].parent:=board^.GUI^.container;
      shapes[0].Shape :=stRectangle;
      shapes[0].OnMouseDown:=@mainShapeMouseDown;
      shapes[0].OnMouseMove:=@mainShapeMouseMove;
      shapes[0].OnMouseUp  :=@mainShapeMouseUp;
      shapes[0].PopupMenu  :=board^.GUI^.gateContextMenu;
      setLength(labels,1);

      labels[0]:=TLabel.create(board^.GUI^.container);
      labels[0].caption:=behavior^.caption;
      labels[0].AutoSize:=true;
      labels[0].Font.size:=6;
      labels[0].parent:=board^.GUI^.container;
      labels[0].OnMouseDown:=@mainShapeMouseDown;
      labels[0].OnMouseMove:=@mainShapeMouseMove;
      labels[0].OnMouseUp  :=@mainShapeMouseUp;
      labels[0].PopupMenu  :=board^.GUI^.gateContextMenu;

      for k:=0 to behavior^.numberOfInputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI^.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnClick  :=@inputClick;
        shapes[shapeIndex].parent:=board^.GUI^.container;
        inc(shapeIndex);
      end;

      for k:=0 to behavior^.numberOfOutputs-1 do begin
        shapes[shapeIndex]:=TShape.create(board^.GUI^.container);
        shapes[shapeIndex].Shape:=stCircle;
        shapes[shapeIndex].Tag:=k;
        shapes[shapeIndex].OnMouseDown:=@outputMouseDown;
        shapes[shapeIndex].OnMouseMove:=@outputMouseMove;
        shapes[shapeIndex].OnMouseUp  :=@outputMouseUp;
        shapes[shapeIndex].parent:=board^.GUI^.container;
        inc(shapeIndex);
      end;
    end;
  end;

PROCEDURE T_visualGateForCustom.ensureGuiElements;
  VAR shapeIndex:longint=1;
      k:longint;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI<>nil) then begin
      inherited;
      setLength(labels,length(shapes));
      for k:=0 to behavior^.numberOfInputs-1 do begin
        labels[shapeIndex]:=TLabel.create(board^.GUI^.container);
        labels[shapeIndex].Tag:=k;
        labels[shapeIndex].parent:=board^.GUI^.container;
        labels[shapeIndex].caption:=P_customGate(behavior)^.inputConnections[k].caption;
        labels[shapeIndex].OnClick:=@inputClick;
        labels[shapeIndex].Font.size:=10;
        inc(shapeIndex);
      end;
      for k:=0 to behavior^.numberOfOutputs-1 do begin
        labels[shapeIndex]:=TLabel.create(board^.GUI^.container);
        labels[shapeIndex].Tag:=k;
        labels[shapeIndex].parent:=board^.GUI^.container;
        labels[shapeIndex].caption:=P_customGate(behavior)^.outputConnections[k].caption;
        labels[shapeIndex].OnMouseDown:=@outputMouseDown;
        labels[shapeIndex].OnMouseMove:=@outputMouseMove;
        labels[shapeIndex].OnMouseUp  :=@outputMouseUp;
        labels[shapeIndex].Font.size:=10;
        inc(shapeIndex);
      end;
    end;
  end;

PROCEDURE T_visualGateForOutput.ensureGuiElements;
  begin
    if (length(shapes)=0) and (board<>nil) and (board^.GUI<>nil) then begin
      inherited;
      setLength(labels,2);
      labels[1]:=TLabel.create(board^.GUI^.container);
      labels[1].parent:=board^.GUI^.container;
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
    if (length(shapes)=0) and (board<>nil) and (board^.GUI<>nil) then begin
      inherited;
      button:=TButton.create(board^.GUI^.container);
      button.parent:=        board^.GUI^.container;
      button.caption:=behavior^.caption;
      button.Font.size:=board^.GUI^.zoom;
      button.OnMouseDown:=@mainShapeMouseDown;
      button.OnMouseMove:=@mainShapeMouseMove;
      button.OnMouseUp  :=@mainShapeMouseUp;
      button.Font.name :=labels[0].Font.name;
      button.OnClick:=@ButtonClick;

      edit:=TEdit.create(board^.GUI^.container);
      edit.parent:=      board^.GUI^.container;
      edit.OnEditingDone:=@inputEditEditingDone;
      edit.OnKeyPress   :=@inputEditKeyEvent;
      edit.AutoSize     :=false;

      setLength(labels,2);
      labels[1]:=TLabel.create(board^.GUI^.container);
      labels[1].parent:=board^.GUI^.container;
      labels[1].caption:='bin:';
      labels[1].OnClick:=@inputModeClick;
      labels[1].Font.size:=10;

      updateIoVisuals;
    end;
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

PROCEDURE T_visualGate.inputClick(Sender: TObject);
  VAR k:longint;
  begin
    k:=TControl(Sender).Tag;
    if behavior^.inputWidth(k)=1 then behavior^.setInput(k,TRI_STATE_NOT[behavior^.getInput(k).bit[0]]);
    updateIoVisuals;
    if (board<>nil) and (board^.GUI<>nil) then board^.GUI^.anyChangeCallback();
  end;

PROCEDURE T_visualGate.mainShapeMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      if marked_
      then dragging:=drag_selection
      else dragging:=drag_self;
      movedDuringDrag:=false;
      if (ssCtrl in Shift)
      then marked:=not(marked);
      shapes[0].Pen.style:=psDash;
      dragX:=x;
      dragY:=y;
    end else if (button=mbRight) then begin
      board^.GUI^.lastClickedGate:=@self;
    end;
  end;

PROCEDURE T_visualGate.mainShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  VAR dx,dy:longint;
      dragDelta:T_point;
      gate:P_visualGate;
      allCanBeMoved:boolean=true;
  begin
    dx:=x-dragX;
    dy:=y-dragY;
    dragDelta:=pointOf(round(dx/board^.GUI^.zoom),round(dy/board^.GUI^.zoom));
    if dragDelta=ZERO_POINT then exit;
    if dragging=drag_self then begin
      if board^.canGateBeMovedBy(@self,dragDelta,false,false)
      then begin
        if not(movedDuringDrag) then board^.saveStateToUndoList;
        origin+=dragDelta;
        repaint;
        movedDuringDrag:=true;
      end;
    end else if dragging=drag_selection then begin
      for gate in board^.gates do
      if gate^.marked
      then allCanBeMoved:=allCanBeMoved and board^.canGateBeMovedBy(gate,dragDelta,false,false);

      if allCanBeMoved then begin
        if not(movedDuringDrag) then board^.saveStateToUndoList;
        for gate in board^.gates do if gate^.marked then begin
          gate^.origin+=dragDelta;
          gate^.repaint;
        end;
        movedDuringDrag:=true;
      end;
    end;
  end;

PROCEDURE T_visualGate.mainShapeMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  begin
    if (button=mbLeft) then begin
      board^.GUI^.incompleteWire.dragging:=false;
      shapes[0].Pen.style:=psSolid;
      if movedDuringDrag then board^.rewire();
      dragging:=not_dragging;
      movedDuringDrag:=false;
      board^.repaint;
    end;
  end;

PROCEDURE T_visualGate.outputMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
  VAR p:T_point;
      connect:T_visualGateConnector;
  begin
    if (button=mbLeft) then begin
      wireDragOutputIndex:=TControl(Sender).Tag;
      p:=getOutputPositionInGridSize(wireDragOutputIndex);
      with board^.GUI^.incompleteWire do begin
        dragging:=true;
        source.gate:=@self;
        source.index:=wireDragOutputIndex;
        width:=behavior^.outputWidth(source.index);
        sourcePoint:=p;
      end;
      dragX:=x+TControl(Sender).Left;
      dragY:=y+TControl(Sender).top;
    end;
    if (button=mbRight) and (board^.GUI<>nil) then begin
      connect.gate:=@self;
      connect.index:=TControl(Sender).Tag;
      board^.GUI^.startPeeking(connect);
    end;
  end;

PROCEDURE T_visualGate.outputMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
  begin
    if board=nil then exit;
    if board^.GUI^.incompleteWire.dragging
    then board^.finishWireDrag(pointOf(dragX+x,dragY+y),true);
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

PROCEDURE T_visualGate.repaint;
  VAR k,newFontSize:longint;
      shapeIndex :longint=1;
      p:T_point;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=origin[0]*board^.GUI^.zoom;
    shapes[0].top   :=origin[1]*board^.GUI^.zoom;
    shapes[0].width :=size  [0]*board^.GUI^.zoom;
    shapes[0].height:=size  [1]*board^.GUI^.zoom;

    newFontSize:=min(round(labels[0].Font.size*shapes[0].width  *0.75/labels[0].width),
                     round(labels[0].Font.size*shapes[0].height *0.5 /labels[0].height));
    if abs(newFontSize-labels[0].Font.size)>1 then labels[0].Font.size:=newFontSize;
    labels[0].top :=shapes[0].top +(shapes[0].height-labels[0].height) div 2;
    labels[0].Left:=shapes[0].Left+(shapes[0].width -labels[0].width) div 2 ;

    for k:=0 to numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].width :=                 board^.GUI^.zoom;
      shapes[shapeIndex].height:=                 board^.GUI^.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].width :=                 board^.GUI^.zoom;
      shapes[shapeIndex].height:=                 board^.GUI^.zoom;
      inc(shapeIndex);
    end;
  end;

PROCEDURE T_visualGateForCustom.repaint;
  VAR shapeIndex:longint=1;
      k:longint;
      ioLabelFontSize:longint;
  begin
    if length(shapes)=0 then exit;
    inherited;
    ioLabelFontSize:=round(board^.GUI^.zoom*0.2);
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

FUNCTION T_visualGateForCustom.getOuputDescription(CONST index: longint): string;
  begin
    result:=P_customGate(behavior)^.outputConnections[index].caption+' ['+intToStr(behavior^.outputWidth(index))+'bit]'+LineEnding+
            'bin: '+getBinaryString(behavior^.getOutput(index))+LineEnding+
            'dec: '+getDecimalString(behavior^.getOutput(index))+LineEnding+
            'sig: '+get2ComplementString(behavior^.getOutput(index));
  end;

PROCEDURE T_visualGateForInput.repaint;
  VAR caption:shortstring;
      newFontSize:longint;
  begin
    if length(shapes)=0 then exit;
    caption:=behavior^.caption;
    button   .caption:=caption;
    labels[0].caption:=caption;
    labels[0].visible:=true;
    inherited;
    labels[0].top:=shapes[0].top;
    button.top   :=shapes[0].top;
    button.Left  :=shapes[0].Left;
    button.width :=shapes[0].width;
    button.height:=shapes[0].height div 2;
    edit  .height:=shapes[0].height div 2;

    button.Font.size:=labels[0].Font.size;

    newFontSize:=min(round(labels[1].Font.size*shapes[0].width *0.15/labels[1].width),
                     round(labels[1].Font.size*shapes[0].height*0.5 /labels[1].height));
    if abs(newFontSize-labels[1].Font.size)>0 then begin
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
    for i:=0 to WIRE_MAX_WIDTH-1 do w.bit[i]:=TRI_STATE_NOT[w.bit[i]];
    behavior^.setInput(0,w);
    updateIoVisuals;
    if (board<>nil) and (board^.GUI<>nil) then board^.GUI^.anyChangeCallback();
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
    if (board<>nil) and (board^.GUI<>nil) then board^.GUI^.anyChangeCallback();
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

PROCEDURE T_visualGateForOutput.repaint;
  VAR k,newFontSize:longint;
      shapeIndex :longint=1;
      p:T_point;
  begin
    if length(shapes)=0 then exit;
    shapes[0].Left  :=origin[0]*board^.GUI^.zoom;
    shapes[0].top   :=origin[1]*board^.GUI^.zoom;
    shapes[0].width :=size  [0]*board^.GUI^.zoom;
    shapes[0].height:=size  [1]*board^.GUI^.zoom;

    for k:=0 to numberOfInputs-1 do begin
      p:=getInputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].width :=                 board^.GUI^.zoom;
      shapes[shapeIndex].height:=                 board^.GUI^.zoom;
      inc(shapeIndex);
    end;

    for k:=0 to numberOfOutputs-1 do begin
      p:=getOutputPositionInGridSize(k);
      shapes[shapeIndex].Left  :=round((p[0]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].top   :=round((p[1]-0.5)*board^.GUI^.zoom);
      shapes[shapeIndex].width :=                 board^.GUI^.zoom;
      shapes[shapeIndex].height:=                 board^.GUI^.zoom;
      inc(shapeIndex);
    end;

    labels[0].caption:=behavior^.caption;
    newFontSize:=min(round(labels[0].Font.size*shapes[0].width *0.75/labels[0].width),
                     round(labels[0].Font.size*shapes[0].height*0.3 /labels[0].height));
    if abs(newFontSize-labels[0].Font.size)>1 then labels[0].Font.size:=newFontSize;

    newFontSize:=min(round(labels[1].Font.size*shapes[0].width *0.75/labels[1].width),
                     round(labels[1].Font.size*shapes[0].height*0.7 /labels[1].height));
    if abs(newFontSize-labels[1].Font.size)>1 then labels[1].Font.size:=newFontSize;

    labels[0].top:=shapes[0].top;
    labels[1].top:=labels[0].top+labels[0].height;

    labels[0].Left:=shapes[0].Left+(shapes[0].width -labels[0].width) div 2 ;
    labels[1].Left:=shapes[0].Left+round(board^.GUI^.zoom*0.7);
  end;

FUNCTION T_visualGate.numberOfInputs: longint;
  begin
    result:=behavior^.numberOfInputs;
  end;

FUNCTION T_visualGate.numberOfOutputs: longint;
  begin
    result:=behavior^.numberOfOutputs;
  end;

FUNCTION T_visualGate.isCompletelyInsideRect(CONST low, high: T_point): boolean;
    begin
      result:=(origin[0]>=low[0])
          and (origin[1]>=low[1])
          and (origin[0]+size[0]<=high[0])
          and (origin[1]+size[1]<=high[1]);
    end;

FUNCTION T_visualGate.nextGateAfter: T_point;
  begin
    if origin[0]+size[0]+2<origin[1]+size[1]
    then begin
      result[0]:=origin[0]+size[0]+2;
      result[1]:=origin[1];
    end else begin
      result[0]:=origin[0];
      result[1]:=origin[1]+size[1];
    end;
  end;

PROCEDURE T_visualGate.forcedFullRepaint;
  begin
    disposeGuiElements;
    size:=pointOf(4,max(2,2*max(numberOfInputs,numberOfOutputs)));
    ensureGuiElements;
    repaint;
  end;

FUNCTION T_visualGate.getOuputDescription(CONST index: longint): string;
  begin
    result:='out'+intToStr(index)+' ['+intToStr(behavior^.outputWidth(index))+'bit]'+LineEnding+
            'bin: '+getBinaryString(behavior^.getOutput(index))+LineEnding+
            'dec: '+getDecimalString(behavior^.getOutput(index))+LineEnding+
            'sig: '+get2ComplementString(behavior^.getOutput(index));
  end;

PROCEDURE T_visualGateForOutput.forcedFullRepaint;
  begin
    disposeGuiElements;
    size:=pointOf(6,3);
    ensureGuiElements;
    repaint;
  end;

PROCEDURE T_visualGateForInput.forcedFullRepaint;
  begin
    disposeGuiElements;
    size:=pointOf(6,3);
    ensureGuiElements;
    repaint;
  end;

{$endif}
