{$ifdef includeInterface}
T_visualWire=object
  source:P_visualGate;
  sourceOutputIndex:longint;

  sink:array of record
    gate:P_visualGate;
    gateInputIndex:longint;
    path:T_wirePath;
    Selected:boolean;
  end;

  PROCEDURE dropWiresAssociatedWith(CONST gate:P_visualGate);
  FUNCTION simulateStep:boolean;
  PROCEDURE paint(CONST x0,y0:longint; CONST zoom:longint; CONST image:TImage);
  FUNCTION isWirePosition(CONST gridX,gridY:longint; OUT horizontalWire:boolean):boolean;
  PROCEDURE dropWiresTouchingPosition(CONST gridX,gridY:longint; CONST horizontalWire:boolean);
end;

{$endif}
{$ifdef includeImplementation}
PROCEDURE paintWire(CONST image:TImage; CONST bitWidth:byte; CONST path:T_wirePathArray; CONST preview:boolean=false);
  CONST surround=4;
  VAR w:longint=1;
      run:longint;
      i,j:longint;
  begin
    case bitWidth of
       1..3: w:=1+surround;
       4..7: w:=2+surround;
      8..15: w:=3+surround;
       else  w:=4+surround;
    end;
    for run:=0 to 1 do begin
      image.Canvas.Pen.width:=w;
      w-=surround;
      if run=0         then image.Canvas.Pen.color:=image.Canvas.Brush.color
      else if preview  then image.Canvas.Pen.color:=$000080ff
                       else image.Canvas.Pen.color:=$00FFFFFF;
      for i:=0 to length(path)-1 do if length(path[i])>1 then begin
                                         image.Canvas.MoveTo(path[i,0,0],path[i,0,1]);
        for j:=1 to length(path[i])-1 do image.Canvas.LineTo(path[i,j,0],path[i,j,1]);
      end;
    end;
  end;

FUNCTION wireToScreen(CONST wire:T_wirePathArray; CONST x0, y0: longint; CONST zoom: longint):T_wirePathArray;
  VAR i,j:longint;
      p:T_point;
  begin
    setLength(result,length(wire));
    for i:=0 to length(wire)-1 do begin
      setLength(result[i],length(wire[i]));
      for j:=0 to length(wire[i])-1 do begin
        p:=wire[i,j];
        p[0]:=x0+p[0]*zoom;
        p[1]:=y0+p[1]*zoom;
        result[i,j]:=p;
      end;
    end;
  end;

FUNCTION wireToScreen(CONST wire:T_wirePath; CONST x0, y0: longint; CONST zoom: longint):T_wirePathArray;
  VAR wrap:T_wirePathArray;
  begin
    setLength(wrap,1);
    wrap[0]:=wire;
    result:=wireToScreen(wrap,x0,y0,zoom);
    setLength(wrap,0);
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

PROCEDURE T_visualWire.paint(CONST x0, y0: longint; CONST zoom: longint;
  CONST image: TImage);
  VAR wrap:T_wirePathArray;
      i:longint;
  begin
    setLength(wrap,length(sink));
    for i:=0 to length(wrap)-1 do wrap[i]:=sink[i].path;
    paintWire(image,
              source^.behavior^.outputWidth(sourceOutputIndex),
              wireToScreen(wrap,x0,y0,zoom));
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

