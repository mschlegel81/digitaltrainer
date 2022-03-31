UNIT wiringUtil;

{$mode objfpc}{$H+}

INTERFACE
CONST BOARD_MAX_SIZE_IN_GRID_ENTRIES=2000;
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
      DirectionCost:array[T_wireDirection] of byte=(2,3,2,3,2,3,2,3);

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
      FUNCTION findPath(CONST startPoint,endPoint:T_point):T_wirePath;
      FUNCTION anyEdgeLeadsTo(CONST endPoint:T_point):boolean;
  end;

FUNCTION pointOf(CONST x,y:longint):T_point;
OPERATOR +(CONST x,y:T_point):T_point;
OPERATOR -(CONST x,y:T_point):T_point;
OPERATOR +(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR -(CONST x:T_point; CONST y:T_wireDirection):T_point;
OPERATOR =(CONST x,y:T_point):boolean;
OPERATOR +(CONST x:T_wirePath; CONST y:T_point):T_wirePath;

IMPLEMENTATION
USES math,sysutils;
TYPE
  PP_node = ^P_node;
  P_node = ^T_node;

  T_node = record
    point:T_point;
    cost:double;
    degree: integer;
    parent, child, Left, Right: P_node;
    mark: boolean;
  end;

  T_aStarNodeInfo=record
    p:T_point;
    cameFrom:T_wireDirection;
    gScore:double;
    fScore:double;
  end;

  { T_nodeMap }

  T_nodeMap=object
    //In this special case, the map can be represented
    //efficiently by a "sparse array".
    //X-Dimension: - allocate all up to needed
    //Y-Dimension: - allocate needed number; maybe retain sorting?!?
    map:array of array of T_aStarNodeInfo;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION containsKey(CONST p:T_point; OUT value:T_aStarNodeInfo):boolean;
    PROCEDURE put(CONST value:T_aStarNodeInfo);
  end;

  T_priorityQueueElement=record
    path:T_wirePath;
    fScore:double;
  end;

  { T_priorityQueue }

  T_priorityQueue=object
    unsortedEntries,
    sortedEntries:array of T_priorityQueueElement;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE add(CONST path:T_wirePath; CONST fScore:double);
    FUNCTION isEmpty:boolean;
    FUNCTION ExtractMin:T_wirePath;
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

OPERATOR=(CONST x, y: T_point): boolean;
  begin
    result:=(x[0]=y[0]) and (x[1]=y[1]);
  end;

OPERATOR +(CONST x:T_wirePath; CONST y:T_point):T_wirePath;
  VAR i:longint;
  begin
    setLength(result,length(x)+1);
    for i:=0 to length(x)-1 do result[i]:=x[i];
    result[length(x)]:=y;
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
    if (length(sortedEntries)=0) or (fScore<sortedEntries[length(sortedEntries)-1].fScore)
    then begin
      k:=length(sortedEntries);
      setLength(sortedEntries,k+1);
      sortedEntries[k].path  :=path;
      sortedEntries[k].fScore:=fScore;
    end else begin
      k:=length(unsortedEntries);
      setLength(unsortedEntries,k+1);
      unsortedEntries[k].path  :=path;
      unsortedEntries[k].fScore:=fScore;
    end;
  end;

FUNCTION T_priorityQueue.isEmpty: boolean;
  begin
    result:=(length(sortedEntries)=0) and
          (length(unsortedEntries)=0)
  end;

FUNCTION T_priorityQueue.ExtractMin: T_wirePath;
  PROCEDURE cleanup;
    VAR tmp:T_priorityQueueElement;
        i,j,k:longint;
        copyOfSorted:array of T_priorityQueueElement;
    begin
      //Bubblesort unsorted...
      for j:=1 to length(unsortedEntries)-1 do for i:=0 to j-1 do
      if unsortedEntries[i].fScore<unsortedEntries[j].fScore
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
        if copyOfSorted[i].fScore>=unsortedEntries[j].fScore
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

FUNCTION T_wireGraph.findPath(CONST startPoint, endPoint: T_point): T_wirePath;
  FUNCTION distance(CONST p:T_point):double;
    begin
      //Multiplied with 2 to match direction cost
      result:=2*sqrt(sqr(p[0]-endPoint[0])+sqr(p[1]-endPoint[1]));
    end;
  VAR nodeMap:T_nodeMap;
  FUNCTION reconstructPath(CONST p:T_point):T_wirePath;
    VAR prev:T_point;
        x:T_point;
        entry:T_aStarNodeInfo;
        reversedResult:T_wirePath;
        i:longint;
    begin
      setLength(reversedResult,1);
      reversedResult[0]:=p;
      prev:=p;
      while prev<>startPoint do begin
        if nodeMap.containsKey(prev,entry) then begin
          x:=prev-entry.cameFrom;
          writeln('Reconstruct: ',prev[0],',',prev[1],' came from ',x[0],',',x[1],' via ',entry.cameFrom);
          prev-=entry.cameFrom;
          setLength(reversedResult,length(reversedResult)+1);
          reversedResult[length(reversedResult)-1]:=prev;
        end else raise Exception.create('Path reconstruction failed!');
      end;
      setLength(result,length(reversedResult));
      for i:=0 to length(result)-1 do result[i]:=reversedResult[length(reversedResult)-1-i];
      setLength(reversedResult,0);
    end;

  PROCEDURE simplifyPath(VAR wirePath:T_wirePath);
    VAR i:longint;
        j:longint=1;
        p:T_point;
        dir,newDir:T_wireDirection;
    begin
      if length(wirePath)<=2 then exit;
    end;

  VAR openSet:T_priorityQueue;
      entry  :T_aStarNodeInfo;
      n,neighbor:T_point;
      dir:T_wireDirection;
      score:double;
  begin
    writeln('Find path ',startPoint[0],',',startPoint[1],' - ',endPoint[0],',',endPoint[1]);
    nodeMap.create;
    openSet.create;
    setLength(result,1);
    result[0]:=startPoint;
    openSet.add(result,distance(startPoint));
    while not openSet.isEmpty do begin
      result:=openSet.ExtractMin;
      n:=result[length(result)-1];
      if n=endPoint then begin
//        result:=reconstructPath(endPoint);
//        simplifyPath(result);
        openSet.destroy;
        nodeMap.destroy;
        exit(result);
      end;
      for dir in allowedDirectionsPerPoint[n[0],n[1]] do begin
        score:=DirectionCost[dir];
        // if nodeMap.containsKey(n,entry) and entry.cameFrom<>dir then score+=2;
        neighbor:=n+dir;
        writeln('Examining point ',neighbor[0],',',neighbor[1],' (Coming from ',n[0],',',n[1],' via ',dir,')');
        if not(nodeMap.containsKey(neighbor,entry)) or (score<entry.gScore) then begin
          entry.p:=neighbor;
          entry.cameFrom:=dir;
          entry.gScore:=score;
          entry.fScore:=score+distance(neighbor);
          nodeMap.put(entry);
          openSet.add(result+neighbor,entry.fScore);
        end;
      end;
    end;
    setLength(result,0);
    openSet.destroy;
    nodeMap.destroy;
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
  VAR i:longint;
  begin
    for i:=0 to length(map)-1 do setLength(map[i],0);
    setLength(map,0);
  end;

FUNCTION T_nodeMap.containsKey(CONST p: T_point; OUT value: T_aStarNodeInfo): boolean;
  VAR info:T_aStarNodeInfo;
  begin
    if length(map)<p[0]+1 then exit(false);
    for info in map[p[0]] do if info.p[1]=p[1] then begin
      value:=info;
      exit(true);
    end;
    result:=false;
  end;

PROCEDURE T_nodeMap.put(CONST value: T_aStarNodeInfo);
  VAR i0,i:longint;
  begin
    if length(map)<=value.p[0] then begin
      i0:=length(map);
      setLength(map,value.p[0]+1);
      for i:=i0 to length(map)-1 do setLength(map[i],0);
    end;
    i:=0;
    while i<length(map[value.p[0]]) do begin
      if map[value.p[0],i].p[1]=value.p[1]
      then begin
        map[value.p[0],i]:=value;
        exit;
      end else inc(i);
    end;
    setLength(map[value.p[0]],i+1);
    map[value.p[0],i]:=value;
  end;

end.

