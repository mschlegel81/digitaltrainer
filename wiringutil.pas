UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil;
TYPE T_wireDirection=(wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up,wd_leftUp);
     T_wireDirectionSet=bitpacked set of T_wireDirection;
     T_point=array[0..1] of longint;
     T_wirePath=array of T_point;
     T_wirePathArray=array of T_wirePath;

     T_sortedDirections=array[0..7] of T_wireDirection;

CONST ZERO_POINT:T_point=(0,0);
      WIRE_DELTA:array[T_wireDirection] of T_point=(
      {wd_left     } (-1, 0),
      {wd_leftDown } (-1, 1),
      {wd_down     } ( 0, 1),
      {wd_rightDown} ( 1, 1),
      {wd_right    } ( 1, 0),
      {wd_rightUp  } ( 1,-1),
      {wd_up       } ( 0,-1),
      {wd_leftUp   } (-1,-1));
      OppositeDirection:array[T_wireDirection] of T_wireDirection=(wd_right,wd_rightUp,wd_up,wd_leftUp,wd_left,wd_leftDown,wd_down,wd_rightDown);
      AllDirections=[wd_left..wd_leftUp];
      StraightDirections=[wd_left,wd_up,wd_right,wd_down];

      allowedSuccessiveDirection:array[T_wireDirection] of T_wireDirectionSet=
          {wd_left     }([wd_left,wd_leftDown,wd_down             ,                    wd_up,wd_leftUp],
          {wd_leftDown } [wd_left,wd_leftDown,wd_down                                                 ],
          {wd_down     } [wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right                           ],
          {wd_rightDown} [                    wd_down,wd_rightDown,wd_right                           ],
          {wd_right    } [                    wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up          ],
          {wd_rightUp  } [                                         wd_right,wd_rightUp,wd_up          ],
          {wd_up       } [wd_left                                 ,wd_right,wd_rightUp,wd_up,wd_leftUp],
          {wd_leftUp   } [wd_left                                 ,                    wd_up,wd_leftUp]);

TYPE
  { T_wireGraph }
  P_wireGraph=^T_wireGraph;
  T_wireGraph=object
    private
      width,height:longint;
      allowed:array of T_wireDirectionSet;

      PROCEDURE dropWireSection(CONST a,b:T_point);
      FUNCTION findMultiPath(CONST startPoint:T_point; CONST endPoints:T_wirePath; CONST exhaustiveScan:boolean):T_wirePathArray;
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      DESTRUCTOR destroy;
      PROCEDURE copyFrom(CONST other:P_wireGraph);
      PROCEDURE initDirections;
      PROCEDURE dropEdges(CONST i:T_point; CONST dirs:T_wireDirectionSet);
      PROCEDURE dropNode(CONST i:T_point);
      PROCEDURE addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
      PROCEDURE dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath; CONST exhaustiveScan:boolean):T_wirePathArray;
      FUNCTION anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
      FUNCTION isWireAllowed(CONST path:T_wirePath):boolean;
  end;

  F_simpleCallback=PROCEDURE of object;

  P_wiringTask=^T_wiringTask;

  { T_wiringTask }

  T_wiringTask=object
    private
      graph:P_wireGraph;
      onFinish:F_simpleCallback;
      toBeDisposed,
      running,
      cancelled:boolean;
    public
      toFind:array of record
        successfulSearches:longint;
        index:longint;
        startPoint:T_point;
        endPoints:T_wirePath;
        foundPath:T_wirePathArray;
      end;
      execTime:qword;

      CONSTRUCTOR create(CONST freshGraph:P_wireGraph; CONST onFinish_:F_simpleCallback);
      DESTRUCTOR destroy;

      PROCEDURE addStartPoint(CONST index:longint; CONST p:T_point);
      PROCEDURE addEndPoint(CONST p:T_point);
      PROCEDURE execute;
      PROCEDURE executeInBackground;
      PROPERTY isRunning:boolean read running;
      PROCEDURE cancelAndDestroy;
  end;

PROCEDURE disposeTask(VAR wiringTask:P_wiringTask);

FUNCTION pointOf(CONST x,y:longint):T_point;
OPERATOR +(CONST x,y:T_point):T_point;
OPERATOR -(CONST x,y:T_point):T_point;
OPERATOR +(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR -(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR =(CONST x,y:T_point):boolean;
OPERATOR *(CONST x:T_point; CONST y:longint):T_point;
OPERATOR *(CONST x,y:T_point):longint;
FUNCTION allPointsBetween(CONST startP,endP:T_point; OUT dir:T_wireDirection):T_wirePath;
FUNCTION pathContains(CONST path:T_wirePath; CONST x,y:longint; OUT orientation:T_wireDirection):boolean;
PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p:T_point);
FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper):T_point;
FUNCTION maxNormDistance(CONST x,y:T_point):longint;
FUNCTION euklideanDistance(CONST x, y: T_point): double;
FUNCTION pathTotalLength(CONST path:T_wirePath):double;
FUNCTION multipathEffectiveLength(CONST list:T_wirePathArray):double;

FUNCTION linesIntersect(CONST a0,a1,b0,b1:T_point):boolean;
FUNCTION lineCrossesRectangle(CONST a0,a1,rectangleOrigin,rectangleExtend:T_point):boolean;

IMPLEMENTATION
USES math,sysutils,myGenerics;
FUNCTION linesIntersect(CONST a0, a1, b0, b1: T_point): boolean;
  FUNCTION inUnitRange(CONST x:double):boolean; inline;
    begin result:=(x>=0) and (x<=1); end;
  VAR u,v,w:T_point;
      f:double;
  begin
    u:=a1-a0;
    v:=b0-b1;
    w:=b1-a0;
    f:= -u[0]*v[1]+u[1]*v[0];
    result:=(abs(f)>1E-3) and
            inUnitRange((-w[0]*v[1]+w[1]*v[0])/f) and
            inUnitRange(( u[0]*w[1]-u[1]*w[0])/f);
  end;

FUNCTION lineCrossesRectangle(CONST a0, a1, rectangleOrigin,
  rectangleExtend: T_point): boolean;
  begin
    if (a0[0]<=rectangleOrigin[0]                   ) and (a1[0]<=rectangleOrigin[0]                   ) then exit(false);
    if (a0[0]>=rectangleOrigin[0]+rectangleExtend[0]) and (a1[0]>=rectangleOrigin[0]+rectangleExtend[0]) then exit(false);
    if (a0[1]<=rectangleOrigin[1]                   ) and (a1[1]<=rectangleOrigin[1]                   ) then exit(false);
    if (a0[1]>=rectangleOrigin[1]+rectangleExtend[1]) and (a1[1]>=rectangleOrigin[1]+rectangleExtend[1]) then exit(false);

    result:=linesIntersect(a0,a1,rectangleOrigin                ,pointOf(rectangleOrigin[0]+rectangleExtend[0],rectangleOrigin[1]))
         or linesIntersect(a0,a1,rectangleOrigin+rectangleExtend,pointOf(rectangleOrigin[0]+rectangleExtend[0],rectangleOrigin[1]))
         or linesIntersect(a0,a1,rectangleOrigin+rectangleExtend,pointOf(rectangleOrigin[0]                   ,rectangleOrigin[1]+rectangleExtend[1]))
         or linesIntersect(a0,a1,rectangleOrigin                ,pointOf(rectangleOrigin[0]                   ,rectangleOrigin[1]+rectangleExtend[1]));
  end;

PROCEDURE disposeTask(VAR wiringTask: P_wiringTask);
  begin
    if wiringTask=nil then exit;
    if wiringTask^.running
    then wiringTask^.cancelAndDestroy
    else dispose(wiringTask,destroy);
    wiringTask:=nil;
  end;

FUNCTION pointOf(CONST x, y: longint): T_point;
  begin
    result[0]:=x;
    result[1]:=y;
  end;

OPERATOR+(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR-(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

OPERATOR+(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]+WIRE_DELTA[y,0];
    result[1]:=x[1]+WIRE_DELTA[y,1];
  end;

OPERATOR-(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]-WIRE_DELTA[y,0];
    result[1]:=x[1]-WIRE_DELTA[y,1];
  end;

OPERATOR*(CONST x: T_point; CONST y: longint): T_point;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR*(CONST x, y: T_point): longint;
  begin
    result:=x[0]*y[0]+x[1]*y[1];
  end;

OPERATOR=(CONST x, y: T_point): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]);
  end;

PROCEDURE append(VAR x:T_wirePath; CONST y:T_wirePath);
  VAR i0,i:longint;
  begin
    i0:=length(x);
    setLength(x,i0+length(y));
    for i:=0 to length(y)-1 do x[i+i0]:=y[i];
  end;

PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper;
  CONST p: T_point);
  begin
    stream.writeLongint(p[0]);
    stream.writeLongint(p[1]);
  end;

FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper): T_point;
  begin
    result[0]:=stream.readLongint;
    result[1]:=stream.readLongint;
  end;

FUNCTION maxNormDistance(CONST x, y: T_point): longint;
  begin
    result:=max(abs(x[0]-y[0]),abs(x[1]-y[1]));
  end;

FUNCTION absNormDistance(CONST x, y: T_point): longint;
  begin
    result:=abs(x[0]-y[0])+abs(x[1]-y[1]);
  end;

FUNCTION euklideanDistance(CONST x, y: T_point): double;
  begin
    result:=sqrt(sqr(x[0]-y[0])+sqr(x[1]-y[1]));
  end;

FUNCTION directionBetween(CONST x, y: T_point; OUT valid:boolean): T_wireDirection;
  begin
    valid:=true;
    if y[0]=x[0] then begin
      if y[1]>x[1]
      then result:=wd_down
      else result:=wd_up;
    end else if y[1]=x[1] then begin
      if y[0]>x[0]
      then result:=wd_right
      else result:=wd_left;
    end else if y[1]-x[1]=y[0]-x[0] then begin
      if y[0]>x[0]
      then result:=wd_rightDown
      else result:=wd_leftUp;
    end else if y[1]-x[1]=x[0]-y[0] then begin
      if y[0]>x[0]
      then result:=wd_rightUp
      else result:=wd_leftDown;
    end else begin
      valid:=false;
      result:=wd_right;
    end;
  end;

CONSTRUCTOR T_wiringTask.create(CONST freshGraph: P_wireGraph; CONST onFinish_: F_simpleCallback);
  begin
    graph:=freshGraph;
    onFinish:=onFinish_;
    toBeDisposed:=false;
    running:=false;
    cancelled:=false;
    setLength(toFind,0);
  end;

DESTRUCTOR T_wiringTask.destroy;
  VAR i,j:longint;
  begin
    cancelled:=true;
    while running do sleep(1);
    dispose(graph,destroy);
    for i:=0 to length(toFind)-1 do begin
      setLength(toFind[i].endPoints,0);
      for j:=0 to length(toFind[i].foundPath)-1 do setLength(toFind[i].foundPath[j],0);
      setLength(toFind[i].foundPath,0);
    end;
    setLength(toFind,0);
  end;

PROCEDURE T_wiringTask.addStartPoint(CONST index: longint; CONST p: T_point);
  VAR i:longint;
  begin
    i:=length(toFind);
    setLength(toFind,i+1);
    toFind[i].index:=index;
    toFind[i].startPoint:=p;
    setLength(toFind[i].endPoints,0);
    setLength(toFind[i].foundPath,0);
    toFind[i].successfulSearches:=0;
  end;

PROCEDURE T_wiringTask.addEndPoint(CONST p:T_point);
  VAR i:longint;
  begin
    with toFind[length(toFind)-1] do begin
      i:=length(endPoints);
      setLength(endPoints,i+1);
      endPoints[i]:=p;
    end;
  end;

PROCEDURE T_wiringTask.execute;
  FUNCTION isBefore(CONST k0,k1:longint):boolean;
    VAR d0,d1,d:longint;
        i:longint;

    begin
      //wire entries with fewer successful searches first
      if (toFind[k0].successfulSearches<toFind[k1].successfulSearches) then exit(true);
      if (toFind[k0].successfulSearches>toFind[k1].successfulSearches) then exit(false);

      //wire entries with more connections first
      if (length(toFind[k0].endPoints)>length(toFind[k1].endPoints)) then exit(true);
      if (length(toFind[k0].endPoints)<length(toFind[k1].endPoints)) then exit(false);

      //wire entries with the shortest single path first
      d0:=maxLongint; with toFind[k0] do begin for i:=0 to length(endPoints)-1 do d:=absNormDistance(startPoint,endPoints[i]); if d<d0 then d0:=d; end;
      d1:=maxLongint; with toFind[k1] do begin for i:=0 to length(endPoints)-1 do d:=absNormDistance(startPoint,endPoints[i]); if d<d1 then d1:=d; end;
      result:=d0<d1;
    end;

  VAR i,j,tmp:longint;
      path:T_wirePath;
      startTicks: qword;
      wiringOrder:array of longint;
      searchOk:boolean;
      allOk:boolean=false;
      workGraph:P_wireGraph;
      attemptsRemaining:longint=5;
  begin
    running:=true;
    startTicks:=GetTickCount64;
    setLength(wiringOrder,length(toFind));
    new(workGraph,create(0,0));
    while not(allOk) and (attemptsRemaining>0) and not(cancelled) do begin
      workGraph^.copyFrom(graph);
      for i:=0 to length(wiringOrder)-1 do begin
        wiringOrder[i]:=i;
        for j:=0 to i-1 do if isBefore(wiringOrder[i],wiringOrder[j]) then begin
          tmp:=wiringOrder[i]; wiringOrder[i]:=wiringOrder[j]; wiringOrder[j]:=tmp;
        end;
      end;

      allOk:=true;
      for i in wiringOrder do if not(cancelled) then begin
        toFind[i].foundPath:=workGraph^.findPaths(toFind[i].startPoint,toFind[i].endPoints,attemptsRemaining<3);
        searchOk:=true;
        for path in toFind[i].foundPath do
          if length(path)=0
          then searchOk:=false
          else workGraph^.dropWire(path,true);
        if searchOk
        then inc(toFind[i].successfulSearches)
        else allOk:=false;
      end;
      dec(attemptsRemaining);
    end;

    execTime:=GetTickCount64-startTicks;
    dispose(workGraph,destroy);
    if not(cancelled) and (onFinish<>nil) then onFinish();
    running:=false;
    {$ifdef debugMode}
    writeln('Wiring task executing took ',execTime,' ticks');
    {$endif}
  end;

FUNCTION wiringTaskThread(p:pointer):ptrint; Register;
  begin
    try
      P_wiringTask(p)^.execute;
      if P_wiringTask(p)^.toBeDisposed then dispose(P_wiringTask(p),destroy);
    finally
      result:=0;
    end;
  end;

PROCEDURE T_wiringTask.executeInBackground;
  begin
    if running then exit;
    running:=true;
    beginThread(@wiringTaskThread,@self);
  end;

PROCEDURE T_wiringTask.cancelAndDestroy;
  begin
    onFinish    :=nil;
    cancelled   :=true;
    toBeDisposed:=true;
  end;

CONSTRUCTOR T_wireGraph.create(CONST width_, height_: longint);
  begin
    width:=width_;
    height:=height_;
    setLength(allowed,width*height);
    initDirections;
  end;

DESTRUCTOR T_wireGraph.destroy;
  begin

  end;

PROCEDURE T_wireGraph.copyFrom(CONST other: P_wireGraph);
  VAR i:longint;
  begin
    width :=other^.width;
    height:=other^.height;
    setLength(allowed,width*height);
    for i:=0 to length(allowed)-1 do allowed[i]:=other^.allowed[i];
  end;

PROCEDURE T_wireGraph.initDirections;
  VAR x,y,i:longint;
  begin
    i:=0;
    for y:=0 to height-1 do for x:=0 to width-1 do begin
      allowed[i]:=AllDirections;
      if y=height-1 then allowed[i]-=[wd_down ,wd_leftDown ,wd_rightDown];
      if y=0        then allowed[i]-=[wd_up   ,wd_leftUp   ,wd_rightUp  ];
      if x=width-1  then allowed[i]-=[wd_right,wd_rightDown,wd_rightUp  ];
      if x=0        then allowed[i]-=[wd_left ,wd_leftDown ,wd_leftUp   ];
      inc(i);
    end;
  end;

PROCEDURE T_wireGraph.dropEdges(CONST i: T_point; CONST dirs: T_wireDirectionSet);
  VAR j:T_point;
      dir:T_wireDirection;
  begin
    if (i[0]<0) or (i[0]>=width) or (i[1]<0) or (i[1]>=height) then exit;
    allowed[i[0]+i[1]*width]-=dirs;
    for dir in dirs do begin
      j:=i+dir;
      if (j[0]>=0) and (j[0]<width) and
         (j[1]>=0) and (j[1]<height) then
      Exclude(allowed[j[0]+j[1]*width],OppositeDirection[dir]);
    end;
  end;

PROCEDURE T_wireGraph.addUnidirectionalEdge(CONST from: T_point; CONST dir: T_wireDirection);
  begin
    if (from[0]<0) or (from[0]>=width) or
       (from[1]<0) or (from[1]>=height) then exit;
    include(allowed[from[0]+from[1]*width],dir);
  end;

PROCEDURE T_wireGraph.dropNode(CONST i: T_point);
  begin
    dropEdges(i,AllDirections);
  end;

FUNCTION allPointsBetween(CONST startP, endP: T_point; OUT dir: T_wireDirection): T_wirePath;
  VAR p:T_point;
      len:longint;
      i:longint;
      validDirectionFound:boolean;
  begin
    len:=maxNormDistance(startP,endP);
    setLength(result,len+1);
    dir:=directionBetween(startP,endP,validDirectionFound);
    if not(validDirectionFound) then begin
      setLength(result,0);
      dir:=wd_left;
      exit;
    end;
    p:=startP;
    for i:=0 to len do begin
      result[i]:=p;
      p+=dir;
    end;
  end;

PROCEDURE T_wireGraph.dropWireSection(CONST a, b: T_point);
  FUNCTION wireStep(CONST start:T_point; CONST direction:T_wireDirection; CONST steps:longint):T_point;
    begin
      result[0]:=start[0]+WIRE_DELTA[direction,0]*steps;
      result[1]:=start[1]+WIRE_DELTA[direction,1]*steps;
    end;

  CONST DIRECTIONS_TO_DROP:array[T_wireDirection] of T_wireDirectionSet=(
  {wd_left     }[wd_left,wd_right],
  {wd_leftDown }[wd_leftDown,wd_rightUp],
  {wd_down     }[wd_down,wd_up],
  {wd_rightDown}[wd_rightDown,wd_leftUp],
  {wd_right    }[wd_left,wd_right],
  {wd_rightUp  }[wd_leftDown,wd_rightUp],
  {wd_up       }[wd_down,wd_up],
  {wd_leftUp   }[wd_rightDown,wd_leftUp]);

  VAR dir:T_wireDirection;
      len,i:longint;
      validDirectionFound:boolean;
  begin
    len:=maxNormDistance(a,b);
    if len<1 then exit;
    dir:=directionBetween(a,b,validDirectionFound);
    if not validDirectionFound then exit;
    for i:=0 to len do dropEdges(wireStep(a,dir,i),DIRECTIONS_TO_DROP[dir]);

  end;

PROCEDURE T_wireGraph.dropWire(CONST path: T_wirePath; CONST diagonalsOnly: boolean);
  VAR i:longint;
  begin
    for i:=0 to length(path)-2 do dropWireSection(path[i],path[i+1]);
    if diagonalsOnly then exit;
    for i:=0 to length(path)-1 do dropNode(path[i]);
  end;

FUNCTION pathTotalLength(CONST path: T_wirePath): double;
  VAR i:longint;
  begin
    if length(path)<=1 then exit(0);
    result:=0;
    for i:=0 to length(path)-2 do result+=euklideanDistance(path[i],path[i+1]);
  end;

FUNCTION multipathEffectiveLength(CONST list:T_wirePathArray):double;
  VAR stepsCounted:array of record p0,p1:T_point; end;
      k:longint=0;
      pathLength:double=0;
      prevPoint:T_point;
  FUNCTION isStepAlreadyCounted(CONST q0,q1:T_point):boolean;
    VAR i:longint;
    begin
      for i:=0 to k-1 do if (stepsCounted[i].p0=q0) and (stepsCounted[i].p1=q1) then exit(true);
      result:=false;
    end;

  PROCEDURE countPoint(CONST nextPoint:T_point);
    begin
      if not(isStepAlreadyCounted(prevPoint,nextPoint)) then begin
        if k>=length(stepsCounted) then setLength(stepsCounted,length(stepsCounted)*2);
        stepsCounted[k].p0:=prevPoint;
        stepsCounted[k].p1:=nextPoint;
        inc(k);
        pathLength+=euklideanDistance(prevPoint,nextPoint);
      end;
      prevPoint:=nextPoint;
    end;

  PROCEDURE countPath(CONST path:T_wirePath);
    VAR i:longint;
    begin
      if length(path)<2 then exit;
      prevPoint:=path[0];
      for i:=1 to length(path)-1 do countPoint(path[i]);
    end;

  VAR i:longint;
  begin
    setLength(stepsCounted,16);
    for i:=0 to length(list)-1 do countPath(list[i]);
    result:=pathLength;
    setLength(stepsCounted,0);
  end;

FUNCTION simplifyPath(CONST path:T_wirePath):T_wirePath;
  VAR i:longint;
      j:longint=1;
      lastDir,dir:T_wireDirection;
      directionIsValid: boolean;
  begin
    if length(path)<2 then exit(path);
    setLength(result,length(path));
    result[0]:=path[0];
    lastDir:=directionBetween(path[0],path[1],directionIsValid);
    for i:=1 to length(path)-1 do begin
      dir:=directionBetween(path[i-1],path[i],directionIsValid);
      if dir<>lastDir then begin
        inc(j);
        lastDir:=dir;
      end;
      result[j]:=path[i];
    end;
    setLength(result,j+1);
  end;

FUNCTION expandPath(CONST path:T_wirePath):T_wirePath;
  VAR i,j,j0:longint;
      dir:T_wireDirection;
      intermediate:T_wirePath;
  begin
    if length(path)<=1 then exit(path);
    setLength(result,1);
    result[0]:=path[0];
    for i:=1 to length(path)-1 do begin
      intermediate:=allPointsBetween(path[i-1],path[i],dir);
      j0:=length(result)-1;
      setLength(result,j0+length(intermediate));
      for j:=1 to length(intermediate)-1 do result[j+j0]:=intermediate[j];
    end;
  end;

FUNCTION pathContains(CONST path: T_wirePath; CONST x, y: longint; OUT orientation: T_wireDirection): boolean;
  VAR i:longint;
      intermediate: T_wirePath;
      p:T_point;
  begin
    if length(path)<=0 then exit(false);
    for i:=1 to length(path)-1 do begin
      intermediate:=allPointsBetween(path[i-1],path[i],orientation);
      for p in intermediate do if (p[0]=x) and (p[1]=y) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.findMultiPath(CONST startPoint: T_point; CONST endPoints: T_wirePath; CONST exhaustiveScan:boolean): T_wirePathArray;
  CONST NO_COME_FROM:T_point=(-1,-1);
        UNVISITED=maxLongint;
        DirectionCost:array[T_wireDirection] of longint=(2,3,
                                                         2,3,
                                                         2,3,
                                                         2,3);
        ChangeDirectionPenalty=2;

  TYPE T_scan=record
         initial:boolean;
         p:T_point;
         direction:array[0..7] of T_wireDirection;
         directionCount:longint;
       end;
       T_mapEntry=record comeFrom:T_point; score:longint; end;

  VAR pointToExamine:array of record p:T_point; initial:boolean; end;
      pointToExamine1:longint=0;
      pointToExamine0:longint=0;
      found:array of boolean;
      allFound:boolean=false;

  PROCEDURE addPointToExamine(CONST p:T_point; CONST initial:boolean);
    VAR i :longint;
    begin
      if pointToExamine1>=length(pointToExamine) then begin
        if pointToExamine0>0 then begin
          for i:=0 to length(pointToExamine)-pointToExamine0-1 do pointToExamine[i]:=pointToExamine[i+pointToExamine0];
          dec(pointToExamine1,pointToExamine0);
          pointToExamine0:=0;
        end;
        if pointToExamine1*2>length(pointToExamine) then setLength(pointToExamine,pointToExamine1*2);
      end;
      pointToExamine[pointToExamine1].p      :=p;
      pointToExamine[pointToExamine1].initial:=initial;
      inc(pointToExamine1);
    end;

  VAR map:array of T_mapEntry;
  VAR prevStep:T_wireDirection;
      prevStepIsValid:boolean;
  FUNCTION nextPointToExamine:T_scan;
    VAR dirs:T_wireDirectionSet;
        d:T_wireDirection;
    begin
      result.p      :=pointToExamine[pointToExamine0].p;
      result.initial:=pointToExamine[pointToExamine0].initial;
      inc(pointToExamine0);
      dirs:=allowed[result.p[0]+result.p[1]*width];
      prevStep:=directionBetween(map[result.p[0]+result.p[1]*width].comeFrom     ,result.p,prevStepIsValid);
      result.directionCount:=0;
      if prevStepIsValid and (prevStep in dirs) then begin
        result.direction[result.directionCount]:=prevStep;
        inc(result.directionCount);
        Exclude(dirs,prevStep);
      end;
      for d in dirs do begin
        result.direction[result.directionCount]:=d;
        inc(result.directionCount);
      end;
    end;

  PROCEDURE rescore(CONST cf:T_point; CONST duringParetoStep:boolean=false);
    VAR n:T_point;
        oldScore, newScore, cf_score:longint;
        cf_dir,n_dir: T_wireDirection;
        validDirection: boolean;
        dirs:T_wireDirectionSet;
    begin
      cf_score:=map[cf[0]+cf[1]*width].score;
      cf_dir  :=directionBetween(map[cf[0]+cf[1]*width].comeFrom,cf,validDirection);

      dirs:=allowed[cf[0]+cf[1]*width];
      for n_dir in dirs do begin
        n:=cf+n_dir;
        oldScore:=map[n[0]+n[1]*width].score;
        if (oldScore<>UNVISITED) or duringParetoStep then begin
          newScore:=cf_score+DirectionCost[n_dir];
          if n_dir<>cf_dir then newScore+=ChangeDirectionPenalty;
          if (newScore<oldScore) or (newScore=oldScore) and (n_dir=cf_dir) then begin
            map[n[0]+n[1]*width].score:=newScore;
            map[n[0]+n[1]*width].comeFrom:=cf;
            rescore(n,duringParetoStep);
          end;
        end;
      end;
    end;

  FUNCTION reconstructPath(p:T_point):T_wirePath;
    VAR i:longint;
    begin
      setLength(result,maxNormDistance(p,startPoint));
      i:=0;
      while (p<>startPoint) do begin
        if i>=length(result) then setLength(result,i*2);
        result[i]:=p; inc(i);
        p:=map[p[0]+p[1]*width].comeFrom;
        if p=NO_COME_FROM then begin
          setLength(result,0);
          exit;
        end;
      end;
      setLength(result,i+1);
      result[i]:=startPoint;
    end;

  FUNCTION reconstructPath(p:T_point; CONST head:T_wirePath; OUT earlyExit:boolean):T_wirePath;
    VAR i,j:longint;
        prev,
        targetPoint:T_point;
    begin
      earlyExit:=false;
      if length(head)=0 then exit(reconstructPath(p));
      targetPoint:=head[length(head)-1];
      {$ifdef debugMode}
      writeln('Reconstructing path with head: ',p[0],',',p[1],' back to ',targetPoint[0],',',targetPoint[1]);
      {$endif}

      prev:=NO_COME_FROM;
      setLength(result,maxNormDistance(p,targetPoint));
      i:=0;
      while (p<>targetPoint) do begin
        {$ifdef debugMode} write(p[0],',',p[1],'  '); {$endif}
        if i>=length(result) then setLength(result,i*2);
        result[i]:=p; inc(i);
        prev:=p;
        p:=map[p[0]+p[1]*width].comeFrom;
        if p=NO_COME_FROM then begin
          for targetPoint in head do if prev=targetPoint then begin
            earlyExit:=true;
            setLength(result,i);
            {$ifdef debugMode} writeln; {$endif}
            exit;
          end;
          {$ifdef debugMode} writeln; {$endif}
          setLength(result,0);
          exit;
        end;
      end;
      setLength(result,i+length(head));
      for j:=length(head)-1 downto 0 do begin
        result[i]:=head[j];
        {$ifdef debugMode} write('H:',result[i,0],',',result[i,1],'  '); {$endif}
        inc(i);
      end;
      {$ifdef debugMode} writeln; {$endif}
    end;

  PROCEDURE foundPath(CONST index:longint; p:T_point);
    VAR i:longint=0;
    begin
      found[index]:=true;
      allFound:=true;
      for i:=0 to length(found)-1 do allFound:=allFound and found[i];
      result[index]:=reconstructPath(p);
      {$ifdef debugMode}
      write('found path #',index,': ');
      for i:=0 to length(result[index])-1 do write(result[index,i,0],',',result[index,i,1],'  ');
      writeln;
      {$endif}
    end;

  PROCEDURE finalizeResult;
    VAR wiringResult:T_wirePathArray;

    //PROCEDURE printScoreMap(CONST mark:T_point);
    //  VAR i,j:longint;
    //      s:longint;
    //      m:longint;
    //  begin
    //    m:=mark[0]+mark[1]*width;
    //    j:=0;
    //    for i:=0 to length(map)-1 do begin
    //      s:=map[i].score;
    //      if i=m then begin
    //        if s>=100 then write('[XX]') else write('[',s:2,']');
    //      end else begin
    //        if s>=100 then write(' XX ') else write(' ',s:2,' ');
    //      end;
    //      inc(j);
    //      if j=width then begin
    //        writeln;
    //        j:=0;
    //      end;
    //    end;
    //  end;

    PROCEDURE rescoreCommonHead(CONST i0:longint; CONST head:T_wirePath; CONST indexes:T_arrayOfLongint);
      VAR i,k:longint;
          p:T_point;
          step:T_wireDirection;
          consensus:boolean=true;
      FUNCTION paretoStep:boolean;
        VAR endPoint:T_point;
            dp,dps:double;
            k:longint;
            checked:longint=0;
        begin
          if not(step in allowed[p[0]+p[1]*width]) then exit(false);
          for k in indexes do if length(wiringResult[k])>i then begin
            checked+=1;
            endPoint:=wiringResult[k,length(wiringResult[k])-1-i];
            //endPoint:=endPoints[k];
            dp :=euklideanDistance(endPoint,p);
            dps:=euklideanDistance(endPoint,p+step);
            if dps>dp then exit(false);
          end;
          result:=checked>=2;
        end;

      VAR subSetA,subSetB:T_arrayOfLongint;
          newPoints:T_wirePath;
          newHead:T_wirePath;

      PROCEDURE updatePath;
        VAR i:longint;
            p:T_point;
            earlyExit: boolean;
        begin
          append(newHead,newPoints);
          p:=NO_COME_FROM;
          {$ifdef debugMode} write('Consensus rescoring: '); {$endif}
          for i:=0 to length(newPoints)-1 do begin
            p:=newPoints[i];
            {$ifdef debugMode} write(p[0],',',p[1],'  '); {$endif}
            with map[p[0]+p[1]*width] do begin
              score:=0;
              if i>0 then comeFrom:=newPoints[i-1]
              else if length(head)>0 then comeFrom:=head[length(head)-1];
            end;
            rescore(p);
          end;
          {$ifdef debugMode}
          writeln;
          //writeln('Score map: ');
          //printScoreMap(p);
          {$endif}

          for i in indexes do begin
            wiringResult[i]:=reconstructPath(endPoints[i],newHead,earlyExit);
            if earlyExit then begin
              dropValues(subSetA,i);
              dropValues(subSetB,i);
            end;
          end;
        end;

      PROCEDURE findSubSets;
        CONST SPLIT_NORMALS:array[0..9] of T_point=((1,0),(0,1),(-1,1),(1,1),
                                                    (1,2),(2,1),(-1,2),(2,-1),(1,-2),(-2,1)
                                                    );

        VAR splitNormal,bestSplitNormal:T_point;
            bestX:longint=maxLongint;
            x,ja,jb:longint;

        begin
          for splitNormal in SPLIT_NORMALS do begin
            ja:=0; jb:=0;
            for k in indexes do if length(wiringResult[k])>i then begin
              if (wiringResult[k,length(wiringResult[k])-i-1]-p)*splitNormal>0
              then inc(ja)
              else inc(jb);
            end;
            x:=ja*ja+jb*jb;
            if x<bestX then begin
              bestSplitNormal:=splitNormal;
              bestX:=x;
            end;
          end;

          splitNormal:=bestSplitNormal;
          setLength(subSetA,length(indexes)); ja:=0;
          setLength(subSetB,length(indexes)); jb:=0;
          for k in indexes do if length(wiringResult[k])>i then begin
            if (wiringResult[k,length(wiringResult[k])-i-1]-p)*splitNormal>0
            then begin subSetA[ja]:=k; inc(ja); end
            else begin subSetB[jb]:=k; inc(jb); end;
          end;
          setLength(subSetA,ja);
          setLength(subSetB,jb);
        end;

      begin
        {$ifdef debugMode}
        writeln('==============================================================');
        write  ('# HEAD SCAN: ');
        for p in head do write(p[0],',',p[1],'  ');
        writeln;
        for k in indexes do begin
          write('# Path #',k,': ');
          for p in wiringResult[k] do write(p[0],',',p[1],'  ');
          writeln;
        end;
        {$endif}

        setLength(newHead,length(head));
        for i:=0 to length(head)-1 do newHead[i]:=head[i];
        setLength(newPoints,0);
        setLength(subSetA,0);
        setLength(subSetB,0);
        i:=i0;

        repeat
          p:=NO_COME_FROM;
          consensus:=true;
          for k in indexes do if (length(wiringResult[k])>i) then begin
            if p=NO_COME_FROM
            then    p:=wiringResult[k,length(wiringResult[k])-1-i]
            else if p<>wiringResult[k,length(wiringResult[k])-1-i] then consensus:=false;
          end;
          consensus:=consensus and (p<>NO_COME_FROM);
          if consensus then begin
            k:=length(newPoints);
            setLength(newPoints,k+1);
            newPoints[k]:=p;
            {$ifdef debugMode}
            writeln('Consensus point: ',p[0],',',p[1]);
            {$endif}
          end;
          inc(i);
        until not(consensus);

        if (length(newPoints)+length(head)>1) then begin
          if (length(newPoints)=0)
          then begin
            p:=                    head[length(head)-1];
            step:=directionBetween(head[length(head)-2],p,consensus)
          end else begin
            p:=newPoints[length(newPoints)-1];
            if length(newPoints)>1
            then step:=directionBetween(newPoints[length(newPoints)-2],p,consensus)
            else step:=directionBetween(head     [length(head     )-1],p,consensus);
          end;

          while paretoStep do begin
            p+=step;
            k:=length(newPoints);
            setLength(newPoints,k+1);
            newPoints[k]:=p;
            {$ifdef debugMode}
            writeln('   Pareto point: ',p[0],',',p[1]);
            {$endif}
            inc(i);
          end;
          if length(indexes)<=2 then begin
            {$ifdef debugMode}
            writeln('Only two paths are left...');
            {$endif}
            updatePath;
            exit;
          end;

          findSubSets;
          if ((length(subSetB)<=0) or (length(subSetA)<=0)) and (length(subSetA)+length(subSetB)=length(indexes)) then begin
            {$ifdef debugMode}
            writeln('NEED ANOTHER IMPLEMENTATION HERE!');
            for k in indexes do if length(wiringResult[k])>i then begin
              p:=wiringResult[k,length(wiringResult[k])-i-1];
              writeln('  * ',p[0],',',p[1]);
            end;
            {$endif}
            updatePath;
            exit;
          end else begin
            updatePath;
            {$ifdef debugMode}
            writeln('Splitting up into sub sets');
            {$endif}

            if length(subSetA)>1 then rescoreCommonHead(i-1,newHead,subSetA);
            if length(subSetB)>1 then rescoreCommonHead(i-1,newHead,subSetB);
          end;
        end else begin
          {$ifdef debugMode}
          writeln('Fallback: no consensus points found');
          {$endif}
          updatePath;
        end;
      end;

    PROCEDURE deepClone(CONST source:T_wirePathArray; VAR target:T_wirePathArray);
      VAR i,j:longint;
      begin
        setLength(target,length(source));
        for i:=0 to length(source)-1 do begin
          setLength(target[i],length(source[i]));
          for j:=0 to length(source[i])-1 do target[i,j]:=source[i,j];
        end;
      end;

    VAR i,j,k:longint;
        n: T_point;
        allIndexes:T_arrayOfLongint;
        emptyHead:T_wirePath;
        mapCopy:array of T_mapEntry;
    TYPE T_sortOrder=record
           first,last:boolean;
           index:longint;
           L:double;
         end;
    VAR SortOrder:array of T_sortOrder;
        tmp:T_sortOrder;
    begin
      if (length(endPoints)>1) then begin
        //BACKUP
        deepClone(result,wiringResult);
        setLength(mapCopy,length(map));
        for i:=0 to length(map)-1 do mapCopy[i]:=map[i];

        //OPTIMIZATION APPROACH 1: Consensus wiring
        setLength(allIndexes,length(result));
        for i:=0 to length(allIndexes)-1 do allIndexes[i]:=i;
        setLength(emptyHead,0);
        rescoreCommonHead(0,emptyHead,allIndexes);

        if not(exhaustiveScan)
        then result:=wiringResult
        else begin
          //RESTORE
          for i:=0 to length(map)-1 do map[i]:=mapCopy[i];

          //OPTIMIZATION APPROACH 2: Iterative, shortest first
          setLength(SortOrder,length(result));
          for i:=0 to length(result)-1 do begin
            SortOrder[i].first:=false;
            SortOrder[i].last :=false;
            SortOrder[i].index:=i;
            SortOrder[i].L:=map[endPoints[i,0]+endPoints[i,1]*width].score;
            for j:=0 to i-1 do if SortOrder[j].L>SortOrder[i].L then begin
              tmp:=SortOrder[i];
              SortOrder[i]:=SortOrder[j];
              SortOrder[j]:=tmp;
            end;
          end;
          SortOrder[                  0].first:=true;
          SortOrder[length(SortOrder)-1].last:=true;
          for tmp in SortOrder do with tmp do begin
            result[index]:=reconstructPath(endPoints[index]);
            if not(last) then begin
              setLength(emptyHead,length(result[index]));
              i:=0;
              for n in result[index] do begin
                if map[n[0]+n[1]*width].score>0 then begin
                  emptyHead[i]:=n; inc(i);
                end;
              end;
              for j:=i-1 downto 0 do rescore(emptyHead[j]);
            end;
          end;

          //PICK THE ONE WITH THE SHORTEST LENGTH:
          if multipathEffectiveLength(result)<multipathEffectiveLength(wiringResult)
          then deepClone(result,wiringResult);

          //RESTORE
          for i:=0 to length(map)-1 do map[i]:=mapCopy[i];

          //OPTIMIZATION APPROACH 3: Iterative, longest first
          for k:=length(SortOrder)-1 downto 0 do with SortOrder[k] do begin
            result[index]:=reconstructPath(endPoints[index]);
            if k>0 then begin
              setLength(emptyHead,length(result[index]));
              i:=0;
              for n in result[index] do begin
                if map[n[0]+n[1]*width].score>0 then begin
                  emptyHead[i]:=n; inc(i);
                end;
              end;
              for j:=i-1 downto 0 do rescore(emptyHead[j]);
            end;
          end;

          //PICK THE ONE WITH THE SHORTEST LENGTH:
          if multipathEffectiveLength(result)>multipathEffectiveLength(wiringResult)
          then result:=wiringResult;
        end;
      end;
      //Reverse and simplify all...
      for k:=0 to length(result)-1 do if length(result[k])>0 then begin
        for i:=0 to (length(result[k])-1) div 2 do begin
          j:=length(result[k])-1-i;
          n          :=result[k,j];
          result[k,j]:=result[k,i];
          result[k,i]:=n;
        end;
        result[k]:=simplifyPath(result[k]);
      end;
    end;

  VAR i,k, newCost:longint;
      scan: T_scan;
      next: T_point;
      needRescore: boolean;
      dir:T_wireDirection;
  begin
    {$ifdef debugMode}
    write('findMultiPath ',startPoint[0],',',startPoint[1],' -> ');
    for i:=0 to length(endPoints)-1 do write(endPoints[i,0],',',endPoints[i,1],'  ');
    writeln;
    {$endif}

    setLength(pointToExamine,16);
    setLength(map,width*height);
    for i:=0 to length(map)-1 do with map[i] do begin score:=UNVISITED; comeFrom:=NO_COME_FROM; end;

    setLength(result,length(endPoints));
    for i:=0 to length(result)-1 do setLength(result[i],0);

    setLength(found,length(endPoints));
    for i:=0 to length(endPoints)-1 do found[i]:=false;

    addPointToExamine(startPoint,true);
    map[startPoint[0]+startPoint[1]*width].score:=0;
    while (pointToExamine1>pointToExamine0) and ((not allFound) or exhaustiveScan) do begin
      scan:=nextPointToExamine;
      for k:=0 to scan.directionCount-1 do begin
        dir:=scan.direction[k];
        next:=scan.p+dir;
        newCost:=map[scan.p[0]+scan.p[1]*width].score+DirectionCost[dir];
        if prevStepIsValid and (prevStep<>dir) then newCost+=ChangeDirectionPenalty;

        if newCost<map[next[0]+next[1]*width].score then begin
          with map[next[0]+next[1]*width] do begin
            if score=UNVISITED then begin
              needRescore:=false;
              addPointToExamine(next,false);
            end else needRescore:=true;
            score:=newCost;
            comeFrom:=scan.p;
          end;
          for i:=0 to length(endPoints)-1 do if next=endPoints[i] then foundPath(i,next);
          if needRescore then rescore(next);
        end;
      end;
    end;
    finalizeResult;
    setLength(map,0);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point): T_wirePath;
  VAR wrappedEndPoint:T_wirePath;
  begin
    setLength(wrappedEndPoint,1);
    wrappedEndPoint[0]:=endPoint;
    result:=findMultiPath(startPoint,wrappedEndPoint,false)[0];
  end;

FUNCTION T_wireGraph.findPaths(CONST startPoint: T_point; CONST endPoints: T_wirePath; CONST exhaustiveScan: boolean): T_wirePathArray;
  begin
    result:=findMultiPath(startPoint,endPoints,exhaustiveScan);
  end;

FUNCTION T_wireGraph.anyEdgeLeadsTo(CONST endPoint: T_point): boolean;
  VAR d:T_wireDirection;
      p:T_point;
  begin
    for d in AllDirections do begin
      p:=endPoint+d;
      if (p[0]>=0) and (p[0]<width) and
         (p[1]>=0) and (p[1]<height) and
         (OppositeDirection[d] in allowed[p[0]+p[1]*width]) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.isWireAllowed(CONST path: T_wirePath): boolean;
  VAR p:T_point;
      i,k,len:longint;
      dir:T_wireDirection;
      valid: boolean;
  begin
    for p in path do if (p[0]<0)
    or (p[1]<0)
    or (p[0]>=width)
    or (p[1]>=height) then exit(false);

    for i:=0 to length(path)-2 do begin
      p:=path[i];
      dir:=directionBetween(p,path[i+1],valid);
      if valid then begin
        len:=maxNormDistance(path[i],path[i+1]);
        for k:=0 to len-1 do begin
          if not(dir in allowed[p[0]+p[1]*width]) then exit(false);
          p+=dir;
        end;
      end;
    end;
    result:=true;
  end;

end.

