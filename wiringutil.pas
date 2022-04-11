UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil;
CONST BOARD_MAX_SIZE_IN_GRID_ENTRIES=200;
TYPE T_wireDirection=(wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up,wd_leftUp);
     T_wireDirectionSet=bitpacked set of T_wireDirection;
     T_point=array[0..1] of longint;
     T_wirePath=array of T_point;
     T_wirePathArray=array of T_wirePath;

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
      allowedDirectionsPerPoint:bitpacked array[0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1,0..BOARD_MAX_SIZE_IN_GRID_ENTRIES-1] of T_wireDirectionSet;
      FUNCTION findPath(CONST startPoint,endPoint:T_point; CONST pathsToPrimeWith:T_wirePathArray):T_wirePath;
      PROCEDURE dropWireSection(CONST a,b:T_point; CONST diagonalsOnly:boolean=false);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE initDirections;
      PROCEDURE dropEdges(CONST i:T_point; CONST dirs:T_wireDirectionSet);
      PROCEDURE dropNode(CONST i:T_point);
      PROCEDURE addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
      PROCEDURE dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath):T_wirePathArray;
      FUNCTION anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
      FUNCTION isWireAllowed(CONST path:T_wirePath):boolean;
  end;

  { T_ioBlock }

  T_ioBlock=object
    id:longint;
    origin,size:T_point;
    inputPoints,outputPoints:array of T_point;
    PROCEDURE clear(CONST o,s:T_point; CONST id_:longint);
    PROCEDURE addInput(CONST p:T_point);
    PROCEDURE addOutput(CONST p:T_point);
  end;

  P_wireHelper=^T_wireHelper;

  { T_connector }

  T_connector=object
    blockId,inputIndex:longint;
    FUNCTION toInputPoint (CONST wireHelper:P_wireHelper):T_point;
    FUNCTION toOutputPoint(CONST wireHelper:P_wireHelper):T_point;
  end;

  T_Trip=record
    endPoint:T_connector;
    path:T_wirePath;
  end;

  T_logicWire=record
    startPoint:T_connector;
    trips:array of T_Trip;
  end;

  { T_wireHelper }

  T_wireHelper=object
    private
      criticalSection:TRTLCriticalSection;
      graph:T_wireGraph;
      blocks:array of T_ioBlock;
      currentTotalCost:double;
      logicWires:array of T_logicWire;

      threadKillRequrested:boolean;
      threadIsUp          :boolean;
      inputChanged        :boolean;
      outputChanged       :boolean;
      PROCEDURE resetGraph;
      PROCEDURE ensureThread;
      PROCEDURE sortWires;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE updateBlock(CONST block:T_ioBlock);
      PROCEDURE deleteBlock(CONST block:T_ioBlock);
      PROCEDURE addWire   (CONST start,goal:T_connector);
      PROCEDURE deleteWire(CONST start,goal:T_connector);
      PROCEDURE logPathsUnchanged;
      FUNCTION havePathsChanged:boolean;
      FUNCTION getDrawablePathsFrom(CONST start:T_connector):T_wirePathArray;
      FUNCTION getPreviewPath(CONST startPoint,endPoint:T_point):T_wirePath;
      PROCEDURE clear;
  end;

FUNCTION pointOf(CONST x,y:longint):T_point;
OPERATOR +(CONST x,y:T_point):T_point;
OPERATOR -(CONST x,y:T_point):T_point;
OPERATOR +(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR -(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR =(CONST x,y:T_point):boolean;
OPERATOR =(CONST x,y:T_connector):boolean;
OPERATOR *(CONST x:T_point; CONST y:longint):T_point;
FUNCTION allPointsAlongWire(CONST path:T_wirePath):T_wirePath;
FUNCTION allPointsBetween(CONST startP,endP:T_point; OUT dir:T_wireDirection):T_wirePath;
PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p:T_point);
FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper):T_point;
FUNCTION maxNormDistance(CONST x,y:T_point):longint;
FUNCTION directionBetween(CONST x,y:T_point):T_wireDirection;

FUNCTION pathScore(CONST path:T_wirePath):double;
IMPLEMENTATION
USES math,sysutils;
TYPE
  P_aStarNodeInfo=^T_aStarNodeInfo;
  T_aStarNodeInfo=record
    p:T_point;
    cameFrom:T_wireDirection;
    costToGetThere:double;
    estimatedCostToGoal:double;
  end;

  { T_nodeMap }

  T_nodeMap=object
    map:array of array of P_aStarNodeInfo;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION containsKey(CONST p:T_point; OUT value:T_aStarNodeInfo):boolean;
    PROCEDURE put(CONST value:T_aStarNodeInfo);
  end;

  T_priorityQueueElement=record
    path:T_wirePath;
    estimatedCostToGoal:double;
  end;

  { T_priorityQueue }

  T_priorityQueue=object
    unsortedEntries,
    sortedEntries:array of T_priorityQueueElement;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE add(CONST path:T_wirePath; CONST fScore:double);
    FUNCTION isEmpty:boolean;
    FUNCTION ExtractMin(OUT fScore:double):T_wirePath;
    PROCEDURE cleanup;
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

OPERATOR=(CONST x, y: T_point): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]);
  end;

OPERATOR =(CONST x,y:T_connector):boolean;
  begin
    result:=(x.blockId=y.blockId) and (x.inputIndex=y.inputIndex);
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

FUNCTION directionBetween(CONST x, y: T_point): T_wireDirection;
  begin
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
    end else raise Exception.create('No valid direction found');
  end;

FUNCTION wireStep(CONST start:T_point; CONST direction:T_wireDirection; CONST steps:longint):T_point;
  begin
    result[0]:=start[0]+WIRE_DELTA[direction,0]*steps;
    result[1]:=start[1]+WIRE_DELTA[direction,1]*steps;
  end;

{ T_connector }

FUNCTION T_connector.toInputPoint(CONST wireHelper: P_wireHelper): T_point;
  VAR b:T_ioBlock;
  begin
    for b in wireHelper^.blocks do if b.id=blockId then exit(b.inputPoints[inputIndex]);
    result:=ZERO_POINT;
  end;

FUNCTION T_connector.toOutputPoint(CONST wireHelper: P_wireHelper): T_point;
  VAR b:T_ioBlock;
  begin
    for b in wireHelper^.blocks do if b.id=blockId then exit(b.outputPoints[inputIndex]);
    result:=ZERO_POINT;
  end;

{ T_ioBlock }

PROCEDURE T_ioBlock.clear(CONST o, s: T_point; CONST id_: longint);
  begin
    id:=id_;
    origin:=o;
    size  :=s;
    setLength(inputPoints,0);
    setLength(outputPoints,0);
  end;

PROCEDURE T_ioBlock.addInput(CONST p: T_point);
  VAR k:longint;
  begin
    k:=length(inputPoints);
    setLength(inputPoints,k+1);
    inputPoints[k]:=p;
  end;

PROCEDURE T_ioBlock.addOutput(CONST p: T_point);
  VAR k:longint;
  begin
    k:=length(outputPoints);
    setLength(outputPoints,k+1);
    outputPoints[k]:=p;
  end;

{ T_wireHelper }
PROCEDURE T_wireHelper.resetGraph;
  VAR block:T_ioBlock;
      x,y:longint;
      p:T_point;
  begin
    enterCriticalSection(criticalSection);
    graph.initDirections;
    for block in blocks do begin
      for x:=block.origin[0] to block.origin[0]+block.size[0] do
      for y:=block.origin[1] to block.origin[1]+block.size[1] do
      graph.dropNode(pointOf(x,y));
      //Input connections:
      for p in block.inputPoints do graph.addUnidirectionalEdge(p+wd_left,wd_right);
      //Output connections:
      for p in block.outputPoints do graph.addUnidirectionalEdge(p,wd_right);

      x:=block.origin[0];
      for y:=block.origin[1]+1 to block.origin[1]+block.size[1]-1 do graph.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
      x:=block.origin[1]+1;
      for y:=block.origin[1]+1 to block.origin[1]+block.size[1]-1 do graph.dropEdges(pointOf(x,y),[wd_leftDown,wd_leftUp,wd_rightDown,wd_rightUp]);
    end;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.sortWires;
  VAR i,j:longint;
      tmp:T_logicWire;
      trip:T_Trip;
      start:T_point;
  begin
    enterCriticalSection(criticalSection);
    for i:=1 to length(logicWires)-1 do
    if (length(logicWires[i].trips)>length(logicWires[i-1].trips))
    then begin
      tmp            :=logicWires[i  ];
      logicWires[i  ]:=logicWires[i+1];
      logicWires[i+1]:=tmp;
    end;
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      start:=startPoint.toOutputPoint(@self);
      for j:=1 to length(trips)-1 do
      if maxNormDistance(trips[j-1].endPoint.toInputPoint(@self),start)>
         maxNormDistance(trips[j  ].endPoint.toInputPoint(@self),start)
      then begin
        trip      :=trips[j  ];
        trips[j  ]:=trips[j-1];
        trips[j-1]:=trip;
      end;
    end;
    leaveCriticalSection(criticalSection);
  end;

FUNCTION wireHelperThread(p:pointer):ptrint;
  VAR i,j:longint;
      start:T_point;
      targets:T_wirePath;
      paths:T_wirePathArray;
      score:double;
  begin with P_wireHelper(p)^ do begin
    resetGraph;
    sortWires;
    score:=0;
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      enterCriticalSection(criticalSection);
      start:=startPoint.toOutputPoint(P_wireHelper(p));
      setLength(targets,length(trips));
      for j:=0 to length(trips)-1 do targets[j]:=trips[j].endPoint.toInputPoint(P_wireHelper(p));
      leaveCriticalSection(criticalSection);

      paths:=graph.findPaths(start,targets);

      enterCriticalSection(criticalSection);
      if length(paths)=length(trips) then
      for j:=0 to length(trips)-1 do begin
        trips[j].path:=paths[j];
        score+=pathScore(paths[j]);
      end else score:=infinity;
      leaveCriticalSection(criticalSection);
    end;

    enterCriticalSection(criticalSection);
    threadIsUp:=false;
    leaveCriticalSection(criticalSection);
    result:=0;
  end; end;

PROCEDURE T_wireHelper.ensureThread;
  begin
    if threadIsUp then exit;
    enterCriticalSection(criticalSection);
    if threadIsUp then begin
      leaveCriticalSection(criticalSection);
      exit;
    end;
    threadIsUp:=true;
    beginThread(@wireHelperThread,@self);

    leaveCriticalSection(criticalSection);
  end;

CONSTRUCTOR T_wireHelper.create;
  begin
    initCriticalSection(criticalSection);
    graph.create;
    setLength(blocks,0);
    currentTotalCost:=infinity;
    setLength(logicWires,0);
    threadKillRequrested:=false;
    threadIsUp          :=false;
    inputChanged        :=false;
    outputChanged       :=false;
  end;

DESTRUCTOR T_wireHelper.destroy;
  VAR i,j:longint;
  begin
    enterCriticalSection(criticalSection);
    if threadIsUp then begin
      threadKillRequrested:=true;
      repeat
        leaveCriticalSection(criticalSection);
        sleep(1);
        enterCriticalSection(criticalSection);
      until not(threadIsUp);
    end;
    graph.destroy;
    setLength(blocks,0);
    for i:=0 to length(logicWires)-1 do with logicWires[i] do begin
      for j:=0 to length(trips)-1 do setLength(trips[j].path,0);
      setLength(trips,0);
    end;
    setLength(logicWires,0);
    leaveCriticalSection(criticalSection);
    doneCriticalSection(criticalSection);
  end;

OPERATOR=(CONST x, y: T_ioBlock): boolean;
  VAR i:longint;
  begin
    result:=(       x.id           =       y.id) and
            (       x.origin       =       y.origin) and
            (       x.size         =       y.size) and
            (length(x.inputPoints )=length(y.inputPoints)) and
            (length(x.outputPoints)=length(y.outputPoints));
    if result then begin
      for i:=0 to length(x.inputPoints )-1 do result:=result and (x.inputPoints [i]=y.inputPoints [i]);
      for i:=0 to length(x.outputPoints)-1 do result:=result and (x.outputPoints[i]=y.outputPoints[i]);
    end;
  end;

PROCEDURE T_wireHelper.updateBlock(CONST block: T_ioBlock);
  VAR i:longint=0;
      modified:boolean=false;
  begin
    enterCriticalSection(criticalSection);
    while (i<length(blocks)) and (blocks[i].id<>block.id) do inc(i);
    if i=length(blocks) then begin
      setLength(blocks,i+1);
      modified:=true;
    end else modified:=blocks[i]<>block;
    blocks[i]:=block;
    if modified and (length(logicWires)>0) then ensureThread;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.deleteBlock(CONST block: T_ioBlock);
  VAR i:longint;
      j:longint=0;
  begin
    enterCriticalSection(criticalSection);
    for i:=0 to length(blocks)-1 do if blocks[i].id<>block.id then begin
      blocks[j]:=blocks[i];
      inc(j);
    end;
    if j<length(blocks) then begin
      setLength(blocks,j);
      if length(logicWires)>0 then ensureThread;
    end;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.addWire(CONST start, goal: T_connector);
  VAR i:longint=0;
      j:longint=0;
      tripAdded:boolean=false;
  begin
    enterCriticalSection(criticalSection);
    while (i<length(logicWires)) and (logicWires[i].startPoint<>start) do inc(i);
    if i>=length(logicWires) then begin
      setLength(logicWires,i+1);
      logicWires[i].startPoint:=start;
      setLength(logicWires[i].trips,0);
    end;
    with logicWires[i] do begin
      while (j<length(trips)) and (trips[j].endPoint<>goal) do inc(j);
      if j>=length(trips) then begin
        setLength(trips,j+1);
        trips[j].endPoint:=goal;
        setLength(trips[j].path,0);
        tripAdded:=true;
      end;
    end;
    if tripAdded then ensureThread;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.deleteWire(CONST start, goal: T_connector);
  VAR i,il:longint;
      j :longint=0;
      jl:longint=0;
  begin
    enterCriticalSection(criticalSection);
    for i:=0 to length(logicWires)-1 do begin
      if logicWires[i].startPoint=start then with logicWires[i] do begin
        for il:=0 to length(trips)-1 do begin
          if trips[il].endPoint<>goal then begin
            if il<>jl then trips[jl]:=trips[il];
            inc(jl);
          end else setLength(trips[il].path,0);
        end;
        if jl<length(trips) then setLength(trips,jl);
      end;
      if length(logicWires[i].trips)>0 then begin
        if j<>i then logicWires[j]:=logicWires[i];
        inc(j);
      end;
    end;
    if j<length(logicWires) then setLength(logicWires,j);
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.logPathsUnchanged;
  begin
    enterCriticalSection(criticalSection);
    outputChanged:=false;
    leaveCriticalSection(criticalSection);
  end;

FUNCTION T_wireHelper.havePathsChanged: boolean;
  begin
    enterCriticalSection(criticalSection);
    result:=outputChanged;
    leaveCriticalSection(criticalSection);
  end;

FUNCTION T_wireHelper.getDrawablePathsFrom(CONST start: T_connector): T_wirePathArray;
  FUNCTION constructFallback(CONST goal:T_connector):T_wirePath;
    begin
      setLength(result,2);
      result[0]:=start.toOutputPoint(@self);
      result[1]:=goal .toInputPoint(@self);
    end;

  VAR i:longint=0;
      j:longint=0;
  begin
    enterCriticalSection(criticalSection);
    i:=0;
    while (i<length(logicWires)) and (logicWires[i].startPoint<>start) do inc(i);
    if i>=length(logicWires)-1 then begin
      setLength(result,0);
      leaveCriticalSection(criticalSection);
      exit(result);
    end;
    with logicWires[i] do begin
      setLength(result,length(trips));
      for j:=0 to length(trips)-1 do begin
        if length(trips[j].path)=0
        then result[j]:=constructFallback(trips[j].endPoint)
        else result[j]:=trips[j].path;
      end;
    end;
    leaveCriticalSection(criticalSection);
  end;

FUNCTION T_wireHelper.getPreviewPath(CONST startPoint, endPoint: T_point
  ): T_wirePath;
  begin
    enterCriticalSection(criticalSection);
   // if threadIsUp then begin
   //   setLength(result,2);
   //   result[0]:=startPoint;
   //   result[1]:=endPoint;
   // end else begin
      result:=graph.findPath(startPoint,endPoint);
   // end;
    leaveCriticalSection(criticalSection);
  end;

PROCEDURE T_wireHelper.clear;
  begin
    if threadIsUp then begin
      setLength(blocks,0);
      setLength(logicWires,0);
      inputChanged:=true;
    end;
  end;

{ T_priorityQueue }

CONSTRUCTOR T_priorityQueue.create;
  begin
    setLength(sortedEntries,0);
    setLength(unsortedEntries,0);
  end;

DESTRUCTOR T_priorityQueue.destroy;
  begin
    setLength(sortedEntries,0);
    setLength(unsortedEntries,0);
  end;

PROCEDURE T_priorityQueue.add(CONST path: T_wirePath; CONST fScore: double);
  VAR k:longint;
  begin
    if (length(sortedEntries)=0) or (fScore<sortedEntries[length(sortedEntries)-1].estimatedCostToGoal)
    then begin
      k:=length(sortedEntries);
      setLength(sortedEntries,k+1);
      sortedEntries[k].path  :=path;
      sortedEntries[k].estimatedCostToGoal:=fScore;
    end else begin
      k:=length(unsortedEntries);
      setLength(unsortedEntries,k+1);
      unsortedEntries[k].path  :=path;
      unsortedEntries[k].estimatedCostToGoal:=fScore;
      if length(unsortedEntries)>16 then cleanup;
    end;
  end;

FUNCTION T_priorityQueue.isEmpty: boolean;
  begin
    result:=(length(sortedEntries)=0) and
          (length(unsortedEntries)=0)
  end;

FUNCTION T_priorityQueue.ExtractMin(OUT fScore: double): T_wirePath;
  VAR k:longint;
  begin
    cleanup;
    k:=length(sortedEntries)-1;
    result:=sortedEntries[k].path;
    fScore:=sortedEntries[k].estimatedCostToGoal;
    setLength(sortedEntries,k);
  end;

PROCEDURE T_priorityQueue.cleanup;
  VAR tmp:T_priorityQueueElement;
      i,j,k:longint;
      copyOfSorted:array of T_priorityQueueElement;
  begin
    //Bubblesort unsorted...
    for j:=1 to length(unsortedEntries)-1 do for i:=0 to j-1 do
    if unsortedEntries[i].estimatedCostToGoal<unsortedEntries[j].estimatedCostToGoal
    then begin
      tmp               :=unsortedEntries[i];
      unsortedEntries[i]:=unsortedEntries[j];
      unsortedEntries[j]:=tmp;
    end;
    //Single Merge (as implemented for mergesort)
    setLength(copyOfSorted,length(sortedEntries));
    for i:=0 to length(sortedEntries)-1 do copyOfSorted[i]:=sortedEntries[i];
    i:=0;
    j:=0;
    k:=0;
    setLength(sortedEntries,length(copyOfSorted)+length(unsortedEntries));
    while (i<length(copyOfSorted)) and (j<length(unsortedEntries)) do
      if copyOfSorted[i].estimatedCostToGoal>=unsortedEntries[j].estimatedCostToGoal
      then begin sortedEntries[k]:=copyOfSorted   [i]; inc(k); inc(i); end
      else begin sortedEntries[k]:=unsortedEntries[j]; inc(k); inc(j); end;
    while (i<length(copyOfSorted   )) do begin sortedEntries[k]:=copyOfSorted   [i]; inc(k); inc(i); end;
    while (j<length(unsortedEntries)) do begin sortedEntries[k]:=unsortedEntries[j]; inc(k); inc(j); end;
    setLength(unsortedEntries,0);
    setLength(copyOfSorted,0);
  end;

CONSTRUCTOR T_wireGraph.create;
  begin
    initDirections;
  end;

DESTRUCTOR T_wireGraph.destroy;
  begin
  end;

PROCEDURE T_wireGraph.initDirections;
  VAR x,y:longint;
  begin
    for x:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do allowedDirectionsPerPoint[x,y]:=AllDirections;
    y:=BOARD_MAX_SIZE_IN_GRID_ENTRIES-1;
    for x:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do begin
      allowedDirectionsPerPoint[x,0]-=[wd_up  ,wd_leftUp  ,wd_rightUp  ];
      allowedDirectionsPerPoint[x,y]-=[wd_down,wd_leftDown,wd_rightDown];
    end;
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
      allowedDirectionsPerPoint[0,y]-=[wd_left ,wd_leftUp ,wd_leftDown ];
    x:=BOARD_MAX_SIZE_IN_GRID_ENTRIES-1;
    for y:=0 to BOARD_MAX_SIZE_IN_GRID_ENTRIES-1 do
      allowedDirectionsPerPoint[x,y]-=[wd_right,wd_rightUp,wd_rightDown];
  end;

PROCEDURE T_wireGraph.dropEdges(CONST i: T_point; CONST dirs: T_wireDirectionSet);
  VAR j:T_point;
      dir:T_wireDirection;
  begin
    allowedDirectionsPerPoint[i[0],i[1]]-=dirs;
    for dir in dirs do begin
      j:=i+dir;
      if (j[0]>=0) and (j[0]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (j[1]>=0) and (j[1]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) then
      Exclude(allowedDirectionsPerPoint[j[0],j[1]],OppositeDirection[dir]);
    end;
  end;

PROCEDURE T_wireGraph.addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
  begin
    if (from[0]<0) or (from[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) or
       (from[1]<0) or (from[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) then exit;
    include(allowedDirectionsPerPoint[from[0],from[1]],dir);
  end;

PROCEDURE T_wireGraph.dropNode(CONST i: T_point);
  begin
    dropEdges(i,AllDirections);
  end;

FUNCTION allPointsAlongWire(CONST path: T_wirePath): T_wirePath;
  VAR resultLen:longint=0;
  PROCEDURE append(CONST q:T_point);
    begin
      if resultLen>=length(result) then setLength(result,length(result)*2+1);
      result[resultLen]:=q;
      inc(resultLen);
    end;

  VAR dir:T_wireDirection;
      p:T_point;
      i,j:longint;
  begin
    setLength(result,length(path));
    if length(path)=0 then exit(result);
    p:=path[0];
    append(p);
    for i:=0 to length(path)-2 do begin
      p:=path[i];
      dir:=directionBetween      (path[i],path[i+1]);
      for j:=1 to maxNormDistance(path[i],path[i+1]) do begin
        p+=dir;
        append(p);
      end;
    end;
    setLength(result,resultLen);
  end;

FUNCTION allPointsBetween(CONST startP, endP: T_point; OUT dir: T_wireDirection
  ): T_wirePath;
  VAR p:T_point;
      len:longint;
      i:longint;
  begin
    len:=maxNormDistance(startP,endP);
    setLength(result,len+1);
    dir:=directionBetween(startP,endP);
    p:=startP;
    for i:=0 to len do begin
      result[i]:=p;
      p+=dir;
    end;
  end;

PROCEDURE T_wireGraph.dropWireSection(CONST a, b: T_point; CONST diagonalsOnly:boolean=false);
  //CONST DIRECTIONS_TO_DROP:array[T_wireDirection] of T_wireDirectionSet=(
  //{wd_left     }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  //{wd_leftDown }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  //{wd_down     }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  //{wd_rightDown}[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp],
  //{wd_right    }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  //{wd_rightUp  }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  //{wd_up       }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  //{wd_leftUp   }[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp]);

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
  begin
    len:=maxNormDistance(a,b);
    if len>1 then begin
      dir:=directionBetween(a,b);
      for i:=1 to len-1 do dropEdges(wireStep(a,dir,i),DIRECTIONS_TO_DROP[dir]);
    end;
    if diagonalsOnly then exit;
    dropNode(a);
    dropNode(b);
  end;

PROCEDURE T_wireGraph.dropWire(CONST path:T_wirePath; CONST diagonalsOnly:boolean=false);
  VAR i:longint;
  begin
    for i:=0 to length(path)-2 do dropWireSection(path[i],path[i+1],diagonalsOnly);
  end;

CONST DirectionCost:array[T_wireDirection] of double=(1,2,
                                                      1,2,
                                                      1,2,
                                                      1,2);
      ChangeDirectionPenalty=1;
      //2+changeDirectionPenalty < diagonalCost+2*changeDirectionPenalty
      //2 < diagonalCost+changeDirectionPenalty

FUNCTION pathScore(CONST path:T_wirePath):double;
  VAR i:longint;
  begin
    if length(path)=0 then exit(infinity);
    result:=(length(path)-1)*ChangeDirectionPenalty;
    for i:=0 to length(path)-2 do result+=maxNormDistance(path[i],path[i+1])*DirectionCost[directionBetween(path[i],path[i+1])];
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point; CONST pathsToPrimeWith:T_wirePathArray): T_wirePath;
  FUNCTION distance(CONST p:T_point):double;
    begin
      result:=sqrt(sqr(p[0]-endPoint[0])+sqr(p[1]-endPoint[1]));
    end;
  VAR nodeMap:T_nodeMap;

  FUNCTION continuePath(CONST previous:T_wirePath; CONST newPoint:T_point; CONST directionChanged:boolean):T_wirePath;
    VAR i:longint;
    begin
      if directionChanged or (length(previous)<=1) then begin
        setLength(result,length(previous)+1);
        for i:=0 to length(previous)-1 do result[i]:=previous[i];
        result[length(result)-1]:=newPoint;
      end else begin
        setLength(result,length(previous));
        for i:=0 to length(previous)-2 do result[i]:=previous[i];
        result[length(result)-1]:=newPoint;
      end;
    end;

  VAR openSet:T_priorityQueue;
      entry  :T_aStarNodeInfo;
  PROCEDURE prime;
    VAR i,j,k:longint;
        dir:T_wireDirection;
        pathUpToHere:T_wirePath;
        pointsBetween:T_wirePath;

    begin
      initialize(pathUpToHere);
      for i:=0 to length(pathsToPrimeWith)-1 do if length(pathsToPrimeWith[i])>0 then begin
        setLength(pathUpToHere,1);
        pathUpToHere[0]:=pathsToPrimeWith[i,0];
        openSet.add(pathUpToHere,distance(pathUpToHere[0]));
        for j:=0 to length(pathsToPrimeWith[i])-2 do begin
          pointsBetween :=allPointsBetween(pathsToPrimeWith[i,j],pathsToPrimeWith[i,j+1],dir);
          for k:=1 to length(pointsBetween)-1 do begin
            try
              pathUpToHere:=continuePath(pathUpToHere,pointsBetween[k],k<=1);
              if not(nodeMap.containsKey(pointsBetween[k],entry)) then begin
                entry.p:=pointsBetween[k];
                entry.cameFrom:=dir;
                entry.costToGetThere:=0;
                entry.estimatedCostToGoal:=distance(pointsBetween[k]);
                nodeMap.put(entry);
                openSet.add(pathUpToHere,entry.estimatedCostToGoal);
              end;
            except
            end;
          end;
        end;
      end;
    end;

  VAR n,neighbor:T_point;
      previousDirection,dir:T_wireDirection;
      score:double;
      scoreBasis:double;
      scanRadius:double;
      directionChanged,knownPrevious,firstStep:boolean;
      allowedByDirection:T_wireDirectionSet;
  begin
    nodeMap.create;
    openSet.create;
    scanRadius:=2*distance(startPoint);

    if length(pathsToPrimeWith)>0 then prime
    else begin
      Initialize(result);
      setLength(result,1);
      result[0]:=startPoint;
      openSet.add(result,distance(startPoint));
    end;

    while not openSet.isEmpty do begin
      result:=openSet.ExtractMin(scoreBasis);
      n:=result[length(result)-1];
      if (n=endPoint) then begin
        openSet.destroy;
        nodeMap.destroy;
        exit(result);
      end;
      scoreBasis-=distance(n);

      if nodeMap.containsKey(n,entry) then begin
         firstStep:=entry.costToGetThere=0;
         knownPrevious:=true;
        previousDirection:=entry.cameFrom;
        allowedByDirection:=allowedSuccessiveDirection[previousDirection];
      end else begin
        firstStep:=false;
        knownPrevious:=false;
        allowedByDirection:=AllDirections;
      end;

      for dir in allowedDirectionsPerPoint[n[0],n[1]]*allowedByDirection do begin
        score:=scoreBasis+DirectionCost[dir];
        if knownPrevious and (dir<>previousDirection) then begin
          if not(firstStep) then score+=ChangeDirectionPenalty;
          directionChanged:=true;
        end else directionChanged:=false;
        neighbor:=n+dir;
        if not(nodeMap.containsKey(neighbor,entry)) or (score<entry.costToGetThere) then begin
          entry.p:=neighbor;
          entry.cameFrom:=dir;
          entry.costToGetThere:=score;
          entry.estimatedCostToGoal:=score+distance(neighbor);
          nodeMap.put(entry);
          if distance(neighbor)<scanRadius then openSet.add(continuePath(result,neighbor,directionChanged),entry.estimatedCostToGoal);
        end;
      end;
    end;
    openSet.destroy;
    nodeMap.destroy;
    setLength(result,0);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint,endPoint:T_point):T_wirePath;
  VAR toPrimeWith:T_wirePathArray;
  begin
    setLength(toPrimeWith,0);
    result:=findPath(startPoint,endPoint,toPrimeWith);
  end;

FUNCTION T_wireGraph.findPaths(CONST startPoint:T_point; CONST endPoints:T_wirePath):T_wirePathArray;
  VAR nextPath:T_wirePath;
      i:longint;
      anyImproved: boolean;
  FUNCTION listExceptEntry(CONST list:T_wirePathArray; CONST indexToDrop:longint):T_wirePathArray;
    VAR i:longint;
        k:longint=0;
    begin
      setLength(result,length(list)-1);
      for i:=0 to length(list)-1 do if i<>indexToDrop then begin
        result[k]:=list[i];
        k+=1;
      end;
    end;

  begin
    setLength(result,0);
    for i:=0 to length(endPoints)-1 do begin
      nextPath:=findPath(startPoint,endPoints[i],result);
      setLength(result,i+1);
      result[i]:=nextPath;
    end;
    if length(endPoints)>1 then repeat
      anyImproved:=false;
      for i:=0 to length(endPoints)-1 do begin
        if length(result[i])>0 then begin
          nextPath:=findPath(startPoint,endPoints[i],listExceptEntry(result,i));
          if pathScore(nextPath)<pathScore(result[i])
          then begin
            result[i]:=nextPath;
            anyImproved:=true;
          end;
        end;
      end;
    until not(anyImproved);

  end;

FUNCTION T_wireGraph.anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
  VAR d:T_wireDirection;
      p:T_point;
  begin
    for d in AllDirections do begin
      p:=endPoint+d;
      if (p[0]>=0) and (p[0]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (p[1]>=0) and (p[1]<BOARD_MAX_SIZE_IN_GRID_ENTRIES) and
         (OppositeDirection[d] in allowedDirectionsPerPoint[p[0],p[1]]) then exit(true);
    end;
    result:=false;
  end;

FUNCTION T_wireGraph.isWireAllowed(CONST path:T_wirePath):boolean;
  VAR p:T_point;
      i,k,len:longint;
      dir:T_wireDirection;
  begin
    for p in path do if (p[0]<0)
    or (p[1]<0)
    or (p[0]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES)
    or (p[1]>=BOARD_MAX_SIZE_IN_GRID_ENTRIES) then exit(false);

    for i:=0 to length(path)-2 do begin
      p:=path[i];
      try
        dir:=directionBetween(p,path[i+1]);
      except
        result:=false;
      end;
      len:=maxNormDistance(path[i],path[i+1]);
      for k:=0 to len-1 do begin
        if not(dir in allowedDirectionsPerPoint[p[0],p[1]]) then exit(false);
        p+=dir;
      end;
    end;
    result:=true;
  end;

{ T_nodeMap }

CONSTRUCTOR T_nodeMap.create;
  begin
    setLength(map,0);
  end;

DESTRUCTOR T_nodeMap.destroy;
  VAR i,j:longint;
  begin
    for i:=0 to length(map)-1 do begin
      for j:=0 to length(map[i])-1 do if map[i,j]<>nil then freeMem(map[i,j],sizeOf(T_aStarNodeInfo));
      setLength(map[i],0);
    end;
    setLength(map,0);
  end;

FUNCTION T_nodeMap.containsKey(CONST p: T_point; OUT value: T_aStarNodeInfo): boolean;
  VAR info:P_aStarNodeInfo;
  begin
    if length(map      )<=p[0] then exit(false);
    if length(map[p[0]])<=p[1] then exit(false);
    info:=map[p[0],p[1]];
    if info=nil
    then exit(false)
    else begin
      value:=info^;
      result:=true;
    end;
  end;

PROCEDURE T_nodeMap.put(CONST value: T_aStarNodeInfo);
  VAR i0,i,j0,j:longint;
  begin
    if length(map)<=value.p[0] then begin
      i0:=length(map);
      setLength(map,value.p[0]+1);
      for i:=i0 to length(map)-1 do setLength(map[i],0);
    end;
    i:=value.p[0];
    if length(map[i])<=value.p[1] then begin
      j0:=length(map[i]);
      setLength(map[i],value.p[1]+1);
      for j:=j0 to length(map[i])-1 do map[i,j]:=nil;
    end;
    j:=value.p[1];
    if map[i,j]=nil then getMem(map[i,j],sizeOf(T_aStarNodeInfo));
    map[i,j]^:=value;
  end;

end.

