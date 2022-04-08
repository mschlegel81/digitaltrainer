{$ifdef includeInterface}
P_customGate=^T_customGate;
T_customGate=object(T_abstractGate)
  prototype  :P_circuitBoard;
  gates      :array of P_abstractGate;
  name       :string;
  connections:array of record source,sink:T_gateConnector end;
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
  FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
  FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;
  FUNCTION  clone:P_abstractGate; virtual;
end;
{$endif}
{$ifdef includeImplementation}
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
            for i:=0 to WIRE_MAX_WIDTH-1 do inputConnections[k].value.bit[i]:=tsv_true;
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
{$endif}
