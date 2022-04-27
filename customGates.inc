{$ifdef includeInterface}
T_simpleConnection=record source,sink:T_gateConnector end;

P_customGate=^T_customGate;
T_customGate=object(T_abstractGate)
  prototype  :P_circuitBoard;
  gates      :array of P_abstractGate;
  name       :string;
  connections:array of T_simpleConnection;
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
  lastSimulationStepWasActive:boolean;

  CONSTRUCTOR createFromBoard(CONST origin:P_circuitBoard);
  CONSTRUCTOR createAsClone(CONST origin:P_customGate; CONST includeState:boolean);
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
  FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
  FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
  FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual;

  PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  PROCEDURE countGates(VAR gateCount:T_gateCount); virtual;
end;
{$endif}
{$ifdef includeImplementation}
CONSTRUCTOR T_customGate.createFromBoard(CONST origin: P_circuitBoard);
  TYPE T_annotatedConnection=record
         con:T_simpleConnection;
         srcIdx,tgtIdx:longint;
         level:longint;
       end;

  FUNCTION myCorrespondingGate(CONST original:T_visualGateConnector; OUT gateIndex:longint):T_gateConnector;
    VAR i:longint;
    begin
      for i:=0 to length(origin^.gates)-1 do begin
        if origin^.gates[i]=original.gate
        then begin
          result.gate:=gates[i];
          result.index:=original.index;
          gateIndex:=i;
          exit(result);
        end;
      end;
      raise Exception.create('Connection cannot be reproduced!');
    end;

  VAR gateLevel:array of longint;
      aConnections:array of T_annotatedConnection;
  PROCEDURE addConnection(CONST src,tgt:T_gateConnector; CONST srcIdx,tgtIdx:longint);
    VAR k,ioIdx,i:longint;
    begin
      if src.gate^.gateType=gt_input then begin
        gateLevel[srcIdx]:=-1;
        gateLevel[tgtIdx]:=0;
        ioIdx:=P_inputGate(src.gate)^.ioIndex;
        if length(inputConnections)<=ioIdx then begin
          k:=length(inputConnections);
          setLength(inputConnections,ioIdx+1);
          while k<length(inputConnections) do begin
            setLength(inputConnections[k].goesTo,0);
            for i:=0 to WIRE_MAX_WIDTH-1 do inputConnections[k].value.bit[i]:=tsv_undetermined;
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
        k:=length(aConnections);
        setLength(aConnections,k+1);
        gateLevel[tgtIdx]:=min(gateLevel[tgtIdx],gateLevel[srcIdx]+1);
        aConnections[k].con.source:=src;
        aConnections[k].con.sink  :=tgt;
        aConnections[k].srcIdx:=srcIdx;
        aConnections[k].tgtIdx:=tgtIdx;
      end;
    end;

  PROCEDURE sortConnectionsAndCleanup;
    VAR anyGateLevelSet:boolean=true;
        c:T_annotatedConnection;
        i,j:longint;
    begin
      while anyGateLevelSet do begin
        anyGateLevelSet:=false;
        for c in aConnections do with c do begin
          if gateLevel[tgtIdx]>gateLevel[srcIdx]+1 then begin
            anyGateLevelSet:=true;
            gateLevel[tgtIdx]:=gateLevel[srcIdx]+1;
          end;
        end;
      end;
      for i:=0 to length(aConnections)-1 do with aConnections[i] do level:=gateLevel[srcIdx];

      for i:=1 to length(aConnections)-1 do for j:=0 to i-1 do if aConnections[i].level<aConnections[j].level then begin
        c              :=aConnections[i];
        aConnections[i]:=aConnections[j];
        aConnections[j]:=c;
      end;

      setLength(connections,length(aConnections));
      for i:=0 to length(connections)-1 do connections[i]:=aConnections[i].con;
      setLength(aConnections,0);
      setLength(gateLevel,0);
    end;

  VAR i,j:longint;
      src,tgt:T_gateConnector;
      srcIndex,tgtIndex:longint;
  begin
    inherited create;
    prototype:=origin;

    setLength(connections      ,0);
    setLength(inputConnections ,0);
    setLength(outputConnections,0);
    setLength(aConnections,0);
    setLength(gateLevel,length(origin^.gates));
    for i:=0 to length(gateLevel)-1 do gateLevel[i]:=length(gateLevel);
    setLength(gates    ,length(origin^.gates));
    for i:=0 to length(gates)-1 do gates[i]:=origin^.gates[i]^.behavior^.clone(false);
    name:=origin^.name;

    for i:=0 to length(origin^.logicWires)-1 do begin
      src:=myCorrespondingGate(origin^.logicWires[i].source,srcIndex);
      for j:=0 to length(origin^.logicWires[i].wires)-1 do begin
        tgt:=myCorrespondingGate(origin^.logicWires[i].wires[j].sink,tgtIndex);
        addConnection(src,tgt,srcIndex,tgtIndex);
      end;
    end;

    sortConnectionsAndCleanup;

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
    lastSimulationStepWasActive:=true;
  end;

CONSTRUCTOR T_customGate.createAsClone(CONST origin:P_customGate; CONST includeState:boolean);
  FUNCTION myConn(CONST originConnector:T_gateConnector):T_gateConnector;
    VAR i:longint;
    begin
      for i:=0 to length(origin^.gates)-1 do if origin^.gates[i]=originConnector.gate then begin
        result.gate:=gates[i];
        result.index:=originConnector.index;
        exit(result);
      end;
      raise Exception.create('Invalid state!');
    end;

  VAR i,j:longint;
  begin
    inherited create;
    prototype:=origin^.prototype;
    name:=origin^.name;

    setLength(gates,length(origin^.gates));
    for i:=0 to length(gates)-1 do gates[i]:=origin^.gates[i]^.clone(includeState);

    setLength(connections,length(origin^.connections));
    for i:=0 to length(connections)-1 do begin
      connections[i].source:=myConn(origin^.connections[i].source);
      connections[i].sink  :=myConn(origin^.connections[i].sink);
    end;

    setLength(inputConnections,length(origin^.inputConnections));
    for i:=0 to length(inputConnections)-1 do with inputConnections[i] do begin
      caption:=origin^.inputConnections[i].caption;
      width  :=origin^.inputConnections[i].width;
      value  :=origin^.inputConnections[i].value;

      setLength(goesTo,length(origin^.inputConnections[i].goesTo));
      for j:=0 to length(goesTo)-1 do goesTo[j]:=myConn(origin^.inputConnections[i].goesTo[j]);
    end;

    setLength(outputConnections,length(origin^.outputConnections));
    for i:=0 to length(outputConnections)-1 do with outputConnections[i] do begin
      caption  :=origin^.outputConnections[i].caption;
      width    :=origin^.outputConnections[i].width;
      comesFrom:=myConn(origin^.outputConnections[i].comesFrom);
    end;
    lastSimulationStepWasActive:=true;
    if not(includeState) then reset;
  end;

PROCEDURE T_customGate.reset;
  VAR gate:P_abstractGate;
      i,j:longint;
  begin
    for gate in gates do gate^.reset;
    for i:=0 to length(inputConnections)-1 do begin
      inputConnections[i].value.width:=inputConnections[i].width;
      for j:=0 to WIRE_MAX_WIDTH-1 do inputConnections[i].value.bit[j]:=tsv_undetermined;
    end;
    lastSimulationStepWasActive:=true;
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
    if result or lastSimulationStepWasActive then begin
      for gate in gates do result:=gate^.simulateStep or result;
      for i:=0 to length(connections)-1 do with connections[i] do begin
        v:=source.getOutputValue;
        if isFullyDefined(v)
        then result:=sink.setInputValue(v) or result;
      end;
    end;
    lastSimulationStepWasActive:=result;
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

FUNCTION T_customGate.clone(CONST includeState:boolean): P_abstractGate;
  begin
    new(P_customGate(result),createAsClone(@self,includeState));
  end;

PROCEDURE T_customGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper);
  begin
    inherited;
    stream.writeNaturalNumber(prototype^.paletteIndex);
  end;

PROCEDURE T_customGate.countGates(VAR gateCount:T_gateCount);
  VAR gate:P_abstractGate;
  begin
    for gate in gates do gate^.countGates(gateCount);
  end;
{$endif}
