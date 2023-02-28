UNIT compoundGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,logicalGates, serializationUtil,wiringUtil;

TYPE
  P_abstractPrototypeSource=^T_abstractPrototypeSource;
  T_abstractPrototypeSource=object(T_serializable)
    PROCEDURE BringToFront; virtual; abstract;
    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual; abstract;
  end;

  P_compoundGate=^T_compoundGate;

  { T_wire }
  T_wire=object
    source:P_abstractGate;
    sourceOutputIndex:longint;

    sink:array of record
      gate:P_abstractGate;
      gateInputIndex:longint;
      path:T_wirePath;
    end;

    FUNCTION simulateStep:boolean;
  end;

  F_queryLongint=FUNCTION():longint of object;

  { T_compoundGate }
  T_compoundGate=object(T_abstractGate)
    protected
      prototypeSource:P_abstractPrototypeSource;
      myIndex:longint;

      //not persisted:
      lastStepBusy:boolean;
    public
      prototype:P_captionedAndIndexed;
      captionString:string;
      descriptionString:string;
      inputs : array of P_inputGate;
      outputs: array of P_outputGate;
      gates  : array of P_abstractGate;
      wires  : array of T_wire;

      CONSTRUCTOR create(CONST prototypeSrc:P_abstractPrototypeSource);
      DESTRUCTOR destroy; virtual;
      PROCEDURE reset;                   virtual;
      FUNCTION  clone(CONST includeState:boolean):P_abstractGate; virtual;
      FUNCTION  getCaption:shortstring;       virtual;
      FUNCTION  getDescription:string;   virtual;
      FUNCTION  getIndexInPalette: longint; virtual;
      FUNCTION  numberOfInputs :longint; virtual;
      FUNCTION  numberOfOutputs:longint; virtual;
      FUNCTION  getIoLocations:T_ioLocations; virtual;
      FUNCTION  inputWidth (CONST index:longint):byte; virtual;
      FUNCTION  outputWidth(CONST index:longint):byte; virtual;
      FUNCTION  gateType:T_gateType;     virtual;
      FUNCTION  simulateStep:boolean;    virtual;
      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writePrototypeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST acutalIndex:longint);
      FUNCTION readPrototypeFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST acutalIndex:longint):boolean;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;

      PROCEDURE countGates(VAR gateCount:T_gateCount); virtual;
      FUNCTION equals(CONST other:P_abstractGate):boolean; virtual;

      FUNCTION usesPrototype(CONST p:P_captionedAndIndexed):boolean;
      FUNCTION getPrototypeIndex:longint;
    end;

IMPLEMENTATION

{ T_wire }

FUNCTION T_wire.simulateStep: boolean;
  VAR value:T_wireValue;
      i:longint;
  begin
    value:=source^.getOutput(sourceOutputIndex);
    result:=false;
    for i:=0 to length(sink)-1 do if sink[i].gate^.setInput(sink[i].gateInputIndex,value) then result:=true;
  end;

{ T_compoundGate }

CONSTRUCTOR T_compoundGate.create(CONST prototypeSrc: P_abstractPrototypeSource);
  begin
    inherited create;
    prototypeSource:=prototypeSrc;
    setLength(inputs,0);
    setLength(outputs,0);
    setLength(gates,0);
    setLength(wires,0);
    captionString:='';
  end;

DESTRUCTOR T_compoundGate.destroy;
  VAR i:longint;
  begin
    inherited;
    for i:=0 to length(wires)-1 do setLength(wires[i].sink,0);
    setLength(wires,0);
    for i:=0 to length(gates)-1 do dispose(gates[i],destroy);
    setLength(gates,0);
    for i:=0 to length(inputs)-1 do dispose(inputs[i],destroy);
    setLength(inputs,0);
    for i:=0 to length(outputs)-1 do dispose(outputs[i],destroy);
    setLength(outputs,0);
  end;

PROCEDURE T_compoundGate.reset;
  VAR g:P_abstractGate;
  begin
    for g in inputs do g^.reset;
    for g in outputs do g^.reset;
    for g in gates do g^.reset;
    lastStepBusy:=true;
  end;

FUNCTION T_compoundGate.clone(CONST includeState: boolean): P_abstractGate;
  VAR cloned:P_compoundGate;
      i,j:longint;
  FUNCTION gateInClone(CONST gate:P_abstractGate):P_abstractGate;
    VAR i:longint;
    begin
      result:=nil;
      for i:=0 to length(inputs )-1 do if P_abstractGate(inputs [i])=gate then exit(cloned^.inputs[i]);
      for i:=0 to length(outputs)-1 do if P_abstractGate(outputs[i])=gate then exit(cloned^.outputs[i]);
      for i:=0 to length(gates  )-1 do if                gates  [i] =gate then exit(cloned^.gates[i]);
      assert(result<>nil,'Cloning of T_circuitBoard failed');
    end;

  begin
    new(cloned,create(prototypeSource));
    if prototype=nil
    then cloned^.prototype:=@self
    else cloned^.prototype:=prototype;
    setLength(cloned^.inputs ,length(inputs )); for i:=0 to length(inputs )-1 do cloned^.inputs [i]:=P_inputGate (inputs [i]^.clone(includeState));
    setLength(cloned^.outputs,length(outputs)); for i:=0 to length(outputs)-1 do cloned^.outputs[i]:=P_outputGate(outputs[i]^.clone(includeState));
    setLength(cloned^.gates  ,length(gates));   for i:=0 to length(gates  )-1 do cloned^.gates  [i]:=             gates  [i]^.clone(includeState);
    setLength(cloned^.wires,length(wires));
    for i:=0 to length(wires)-1 do begin
      cloned^.wires[i].source:=gateInClone(wires[i].source);
      cloned^.wires[i].sourceOutputIndex:= wires[i].sourceOutputIndex;
      setLength(cloned^.wires[i].sink,length(wires[i].sink));
      for j:=0 to length(wires[i].sink)-1 do begin
        cloned^.wires[i].sink[j].gate:=gateInClone(wires[i].sink[j].gate);
        cloned^.wires[i].sink[j].gateInputIndex:=  wires[i].sink[j].gateInputIndex;
      end;
    end;
    result:=cloned;
  end;

FUNCTION T_compoundGate.getCaption: shortstring;
  begin
    if prototype=nil
    then result:=captionString
    else result:=prototype^.getCaption;
  end;

FUNCTION T_compoundGate.getDescription: string;
  begin
    if prototype=nil
    then result:=descriptionString
    else result:=prototype^.getDescription;
  end;

FUNCTION T_compoundGate.getIndexInPalette: longint;
  begin
    if prototype=nil
    then result:=myIndex
    else result:=prototype^.getIndexInPalette;
  end;

FUNCTION T_compoundGate.numberOfInputs: longint;
  begin
    result:=length(inputs);
  end;

FUNCTION T_compoundGate.numberOfOutputs: longint;
  begin
    result:=length(outputs);
  end;

FUNCTION T_compoundGate.getIoLocations: T_ioLocations;
  VAR i:longint;
  begin
    result.init;
    for i:=0 to length(inputs )-1 do with inputs [i]^ do result.add(gt_input ,onLeftOrRightSide,positionIndex,getCaption);
    for i:=0 to length(outputs)-1 do with outputs[i]^ do result.add(gt_output,onLeftOrRightSide,positionIndex,getCaption);
  end;

FUNCTION T_compoundGate.inputWidth(CONST index: longint): byte;
  begin
    if (index>=0) and (index<length(inputs))
    then result:=inputs[index]^.outputWidth(0)
    else result:=1;
  end;

FUNCTION T_compoundGate.outputWidth(CONST index: longint): byte;
  begin
    if (index>=0) and (index<length(outputs))
    then result:=outputs[index]^.inputWidth(0)
    else result:=1;
  end;

FUNCTION T_compoundGate.gateType: T_gateType;
  begin
    result:=gt_compound;
  end;

FUNCTION T_compoundGate.simulateStep: boolean;
  VAR wire:T_wire;
      g:P_abstractGate;
  begin
    result:=false;
    for wire in wires do if wire.simulateStep then result:=true;
    if not(result) and not(lastStepBusy) then exit(false);
    for g in gates do if g^.simulateStep then result:=true;
    lastStepBusy:=result;
  end;

FUNCTION T_compoundGate.getOutput(CONST index: longint): T_wireValue;
  begin
    if (index>=0) and (index<length(outputs))
    then result:=outputs[index]^.getInput(0);
  end;

FUNCTION T_compoundGate.setInput(CONST index: longint; CONST value: T_wireValue): boolean;
  VAR wire: T_wire;
  begin
    if (index>=0) and (index<length(inputs))
    then result:=inputs[index]^.setInput(0,value)
    else result:=false;
    if result then for wire in wires do if wire.simulateStep then lastStepBusy:=true;
  end;

FUNCTION T_compoundGate.getInput(CONST index: longint): T_wireValue;
  begin
    if (index>=0) and (index<length(inputs))
    then result:=inputs[index]^.getOutput(0);
  end;

PROCEDURE T_compoundGate.writePrototypeToStream(
  VAR stream: T_bufferedOutputStreamWrapper; CONST acutalIndex: longint);
  FUNCTION gateIndexForSerialization(CONST gate:P_abstractGate):longint;
    VAR i: longint;
    begin
      result:=0;
      for i:=0 to length(inputs )-1 do if P_abstractGate(inputs [i])=gate then exit(       i); result+=length(inputs);
      for i:=0 to length(outputs)-1 do if P_abstractGate(outputs[i])=gate then exit(result+i); result+=length(outputs);
      for i:=0 to length(gates  )-1 do if                gates  [i] =gate then exit(result+i); result:=-1;
      assert(result>0,'writeToStream failed for T_circuitBoard');
    end;
  VAR g:P_abstractGate;
      i,j:longint;
  begin
    myIndex:=acutalIndex;
    stream.writeAnsiString(captionString);
    stream.writeAnsiString(descriptionString);
    stream.writeNaturalNumber(length(inputs));  for g in inputs  do g^.writeToStream(stream);
    stream.writeNaturalNumber(length(outputs)); for g in outputs do g^.writeToStream(stream);
    stream.writeNaturalNumber(length(gates));   for g in gates   do g^.writeToStream(stream);
    stream.writeNaturalNumber(length(wires));
    for i:=0 to length(wires)-1 do with wires[i] do begin
      stream.writeNaturalNumber(gateIndexForSerialization(source));
      stream.writeNaturalNumber(sourceOutputIndex);
      stream.writeNaturalNumber(length(sink));
      for j:=0 to length(sink)-1 do with sink[j] do begin
        stream.writeNaturalNumber(gateIndexForSerialization(gate));
        stream.writeNaturalNumber(gateInputIndex);
      end;
    end;
  end;

FUNCTION T_compoundGate.readPrototypeFromStream(
  VAR stream: T_bufferedInputStreamWrapper; CONST acutalIndex: longint
  ): boolean;
  FUNCTION gateFromDeserialization(index:longint):P_abstractGate;
    begin
      assert(index>=0,'Negative gate index!');
      if index<length(inputs ) then exit(inputs [index]); index-=length(inputs);
      if index<length(outputs) then exit(outputs[index]); index-=length(outputs);
      assert(index<length(gates),'Gate index overflow!');
      result:=gates[index];
    end;
  VAR i,j:longint;
  begin
    myIndex:=acutalIndex;
    captionString:=stream.readAnsiString;
    descriptionString:=stream.readAnsiString;
    setLength(inputs ,stream.readNaturalNumber); for i:=0 to length(inputs )-1 do inputs [i]:=P_inputGate (prototypeSource^.readGate(stream));
    setLength(outputs,stream.readNaturalNumber); for i:=0 to length(outputs)-1 do outputs[i]:=P_outputGate(prototypeSource^.readGate(stream));
    setLength(gates  ,stream.readNaturalNumber); for i:=0 to length(gates  )-1 do gates  [i]:=             prototypeSource^.readGate(stream);
    setLength(wires,stream.readNaturalNumber);
    for i:=0 to length(wires) do with wires[i] do begin
      source:=gateFromDeserialization(stream.readNaturalNumber);
      sourceOutputIndex:=stream.readNaturalNumber;
      setLength(sink,stream.readNaturalNumber);
      for j:=0 to length(sink)-1 do with sink[j] do begin
        gate:=gateFromDeserialization(stream.readNaturalNumber);
        gateInputIndex:=stream.readNaturalNumber;
      end;
    end;
  end;

PROCEDURE T_compoundGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false);
  begin
    inherited;
    stream.writeLongint(prototype^.getIndexInPalette);
  end;

PROCEDURE T_compoundGate.readMetaDataFromStream(
  VAR stream: T_bufferedInputStreamWrapper);
  begin
    assert(false,'This should never be called');
  end;

PROCEDURE T_compoundGate.countGates(VAR gateCount: T_gateCount);
  VAR gate: P_abstractGate;
  begin
    inc(gateCount[gt_input ],length(inputs));
    inc(gateCount[gt_output],length(outputs));
    for gate in gates do gate^.countGates(gateCount);
  end;

FUNCTION T_compoundGate.equals(CONST other: P_abstractGate): boolean;
  begin
    result:=(gateType=other^.gateType) and ((P_compoundGate(other)^.prototype=prototype) or (P_compoundGate(other)^.prototype=prototype));
  end;

FUNCTION T_compoundGate.usesPrototype(CONST p: P_captionedAndIndexed): boolean;
  VAR g:P_abstractGate;
  begin
    result:=false;
    if (prototype=p) then exit(true);
    for g in gates do if (g^.gateType=gt_compound) and P_compoundGate(g)^.usesPrototype(p) then result:=true;
  end;

FUNCTION T_compoundGate.getPrototypeIndex: longint;
  begin
    result:=myIndex;
  end;

end.

