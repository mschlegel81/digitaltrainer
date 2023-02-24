{$ifdef includeInterface}
T_visualWire=object
  source:P_visualGate;
  sourceOutputIndex:longint;

  sink:array of record
    gate:P_visualGate;
    gateInputIndex:longint;
    path:T_wirePath;
  end;

  PROCEDURE dropWiresAssociatedWith(CONST gate:P_visualGate);
  FUNCTION simulateStep:boolean;
  PROCEDURE paint(CONST zoom:longint; CONST Canvas:TBGRACanvas);
  FUNCTION isWirePosition(CONST gridX,gridY:longint; OUT horizontalWire:boolean):boolean;
  PROCEDURE dropWiresTouchingPosition(CONST gridX,gridY:longint; CONST horizontalWire:boolean);
end;

{$endif}
{$ifdef includeImplementation}
PROCEDURE paintWire(CONST zoom:longint; CONST Canvas:TBGRACanvas; CONST bitWidth:byte; CONST path:T_wirePathArray; CONST preview:boolean=false);
  VAR w:longint=1;
      run:longint;
      i,j:longint;
      surround:longint;
  begin
    surround:=zoom div 2;
    case bitWidth of
       1..3: w:=(1*zoom) shr 4;
       4..7: w:=(2*zoom) shr 4;
      8..15: w:=(3*zoom) shr 4;
       else  w:=(4*zoom) shr 4;
    end;
    if w<1 then w:=1; w+=surround;
    for run:=0 to 1 do begin
      Canvas.Pen.width:=w;
      w-=surround;
      if run=0         then Canvas.Pen.color:=BOARD_COLOR
      else if preview  then Canvas.Pen.color:=MARK_COLOR
                       else Canvas.Pen.color:=WIRE_COLOR;
      for i:=0 to length(path)-1 do if length(path[i])>1 then begin
                                         Canvas.MoveTo(path[i,0,0]*zoom,path[i,0,1]*zoom);
        for j:=1 to length(path[i])-1 do Canvas.LineTo(path[i,j,0]*zoom,path[i,j,1]*zoom);
      end;
    end;
  end;

PROCEDURE T_visualWire.dropWiresAssociatedWith(CONST gate: P_visualGate);
  VAR i,j:longint;
  begin
    if source=gate then begin
      for i:=0 to length(sink)-1 do setLength(sink[i].path,0);
      setLength(sink,0);
    end else begin
      j:=0;
      for i:=0 to length(sink)-1 do if sink[i].gate=gate
      then setLength(sink[i].path,0)
      else begin
        sink[j]:=sink[i]; inc(j);
      end;
      setLength(sink,j);
    end;
  end;

FUNCTION T_visualWire.simulateStep: boolean;
  VAR value:T_wireValue;
      i:longint;
  begin
    value:=source^.behavior^.getOutput(sourceOutputIndex);
    result:=false;
    for i:=0 to length(sink)-1 do if sink[i].gate^.behavior^.setInput(sink[i].gateInputIndex,value) then result:=true;
  end;

PROCEDURE T_visualWire.paint(CONST zoom:longint; CONST Canvas:TBGRACanvas);
  VAR wrap:T_wirePathArray;
      i:longint;
  begin
    setLength(wrap,length(sink));
    for i:=0 to length(wrap)-1 do wrap[i]:=sink[i].path;
    paintWire(zoom,Canvas,
              source^.behavior^.outputWidth(sourceOutputIndex),
              wrap);
    setLength(wrap,0);
  end;

FUNCTION T_visualWire.isWirePosition(CONST gridX, gridY: longint; OUT horizontalWire: boolean): boolean;
  VAR i:longint;
      orientation: T_wireDirection;
  begin
    for i:=0 to length(sink)-1 do if pathContains(sink[i].path,gridX,gridY,orientation) then begin
      horizontalWire:=orientation in [wd_left,wd_right];
      exit(true);
    end;
    result:=false;
  end;

PROCEDURE  T_visualWire.dropWiresTouchingPosition(CONST gridX, gridY: longint; CONST horizontalWire: boolean);
  VAR i:longint;
      j:longint=0;
      orientation: T_wireDirection;
  begin
    for i:=0 to length(sink)-1 do if pathContains(sink[i].path,gridX,gridY,orientation) and (horizontalWire=(orientation in [wd_left,wd_right])) then begin
      setLength(sink[i].path,0);
    end else begin
      if i<>j then sink[j]:=sink[i];
      inc(j);
    end;
    setLength(sink,j);
  end;
{$endif}

