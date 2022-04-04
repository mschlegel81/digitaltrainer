UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
USES serializationUtil;
CONST BOARD_MAX_SIZE_IN_GRID_ENTRIES=200;
TYPE T_wireDirection=(wd_left,wd_leftDown,wd_down,wd_rightDown,wd_right,wd_rightUp,wd_up,wd_leftUp);
     T_wireDirectionSet=bitpacked set of T_wireDirection;
     T_point=array[0..1] of longint;
     T_wirePath=array of T_point;

CONST WIRE_DELTA:array[T_wireDirection] of T_point=(
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
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE initDirections;
      PROCEDURE dropEdges(CONST i:T_point; CONST dirs:T_wireDirectionSet);
      PROCEDURE dropNode(CONST i:T_point);
      PROCEDURE addUnidirectionalEdge(CONST from:T_point; CONST dir:T_wireDirection);
      PROCEDURE dropWireSection(CONST a,b:T_point);
      PROCEDURE dropWire(CONST path:T_wirePath);
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
  end;

FUNCTION pointOf(CONST x,y:longint):T_point;
OPERATOR +(CONST x,y:T_point):T_point;
OPERATOR -(CONST x,y:T_point):T_point;
OPERATOR +(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR -(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR =(CONST x,y:T_point):boolean;
OPERATOR *(CONST x:T_point; CONST y:longint):T_point;

PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p:T_point);
FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper):T_point;
FUNCTION maxNormDistance(CONST x,y:T_point):longint;
FUNCTION directionBetween(CONST x,y:T_point):T_wireDirection;
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
    //In this special case, the map can be represented
    //efficiently by a "sparse array".
    //X-Dimension: - allocate all up to needed
    //Y-Dimension: - allocate needed number; maybe retain sorting?!?
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
  end;

FUNCTION pointOf(CONST x,y:longint):T_point;
  begin
    result[0]:=x;
    result[1]:=y;
  end;

OPERATOR +(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
  end;

OPERATOR -(CONST x, y: T_point): T_point;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
  end;

OPERATOR +(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]+WIRE_DELTA[y,0];
    result[1]:=x[1]+WIRE_DELTA[y,1];
  end;

OPERATOR -(CONST x: T_point; CONST y: T_wireDirection): T_point;
  begin
    result[0]:=x[0]-WIRE_DELTA[y,0];
    result[1]:=x[1]-WIRE_DELTA[y,1];
  end;

OPERATOR *(CONST x:T_point; CONST y:longint):T_point;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
  end;

OPERATOR=(CONST x, y: T_point): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]);
  end;

PROCEDURE writePointToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST p: T_point);
  begin
    stream.writeLongint(p[0]);
    stream.writeLongint(p[1]);
  end;

FUNCTION readPoint(VAR stream: T_bufferedInputStreamWrapper): T_point;
  begin
    result[0]:=stream.readLongint;
    result[1]:=stream.readLongint;
  end;

FUNCTION maxNormDistance(CONST x,y:T_point):longint;
  begin
    result:=max(abs(x[0]-y[0]),abs(x[1]-y[1]));
  end;

FUNCTION directionBetween(CONST x,y:T_point):T_wireDirection;
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
    end;
  end;

FUNCTION T_priorityQueue.isEmpty: boolean;
  begin
    result:=(length(sortedEntries)=0) and
          (length(unsortedEntries)=0)
  end;

FUNCTION T_priorityQueue.ExtractMin(OUT fScore:double): T_wirePath;
  PROCEDURE cleanup;
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

  VAR k:longint;
  begin
    cleanup;
    k:=length(sortedEntries)-1;
    result:=sortedEntries[k].path;
    fScore:=sortedEntries[k].estimatedCostToGoal;
    setLength(sortedEntries,k);
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

PROCEDURE T_wireGraph.dropWireSection(CONST a, b: T_point);
  CONST DIRECTIONS_TO_DROP:array[T_wireDirection] of T_wireDirectionSet=(
  {wd_left     }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  {wd_leftDown }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  {wd_down     }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  {wd_rightDown}[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp],
  {wd_right    }[wd_left,wd_leftDown,        wd_rightDown,wd_right,wd_rightUp,      wd_leftUp],
  {wd_rightUp  }[wd_left,wd_leftDown,wd_down,             wd_right,wd_rightUp,wd_up          ],
  {wd_up       }[        wd_leftDown,wd_down,wd_rightDown,         wd_rightUp,wd_up,wd_leftUp],
  {wd_leftUp   }[wd_left,            wd_down,wd_rightDown,wd_right,           wd_up,wd_leftUp]);

  VAR dir:T_wireDirection;
      len,i:longint;
  begin
    dropNode(a);
    dropNode(b);
    len:=maxNormDistance(a,b);
    if len>1 then begin
      dir:=directionBetween(a,b);
      for i:=1 to len-1 do dropEdges(wireStep(a,dir,i),DIRECTIONS_TO_DROP[dir]);
    end;
  end;

PROCEDURE T_wireGraph.dropWire(CONST path:T_wirePath);
  VAR i:longint;
  begin
    for i:=0 to length(path)-2 do dropWireSection(path[i],path[i+1]);
  end;

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point): T_wirePath;
  CONST DirectionCost:array[T_wireDirection] of double=(1,1.8,
                                                        1,1.8,
                                                        1,1.8,
                                                        1,1.8);
        ChangeDirectionPenalty=1;
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
      n,neighbor:T_point;
      dir:T_wireDirection;
      score:double;
      scoreBasis:double;
      directionChanged:boolean;
      resultCandidate:record
        path:T_wirePath;
        score:double;
        found:boolean;
        stepCount:longint;
      end;

  begin
    resultCandidate.found:=false;
    resultCandidate.stepCount:=0;

    nodeMap.create;
    openSet.create;
    setLength(result,1);
    result[0]:=startPoint;
    openSet.add(result,distance(startPoint));
    //TODO: Is there a plausible earlier exit?!?
    while not openSet.isEmpty do begin
      result:=openSet.ExtractMin(scoreBasis);
      n:=result[length(result)-1];
      if n=endPoint then begin
        if not(resultCandidate.found) or (scoreBasis<resultCandidate.score) then begin
          resultCandidate.found:=true;
          resultCandidate.path :=result;
          resultCandidate.score:=scoreBasis;
        end;
      end;
      with resultCandidate do begin
        if found then inc(stepCount);
        if stepCount>10 then begin
          openSet.destroy;
          nodeMap.destroy;
          result:=path;
          exit(result);
        end;
      end;

      scoreBasis-=distance(n);
      for dir in allowedDirectionsPerPoint[n[0],n[1]] do begin
        score:=scoreBasis+DirectionCost[dir];
        if nodeMap.containsKey(n,entry) and (entry.cameFrom<>dir) then begin
          score+=ChangeDirectionPenalty;
          directionChanged:=true;
        end else directionChanged:=false;
        neighbor:=n+dir;
        if not(nodeMap.containsKey(neighbor,entry)) or (score<entry.costToGetThere) then begin
          entry.p:=neighbor;
          entry.cameFrom:=dir;
          entry.costToGetThere:=score;
          entry.estimatedCostToGoal:=score+distance(neighbor);
          nodeMap.put(entry);
          openSet.add(continuePath(result,neighbor,directionChanged),entry.estimatedCostToGoal);
        end;
      end;
    end;
    openSet.destroy;
    nodeMap.destroy;
    with resultCandidate do
    if found
    then result:=path
    else setLength(result,0);
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

