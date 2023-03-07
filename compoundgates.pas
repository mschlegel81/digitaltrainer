UNIT compoundGates;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,logicalGates, serializationUtil,wiringUtil;

TYPE
  P_compoundGate=^T_compoundGate;

  T_challengeBoardOption=(co_allGates,co_halfGates,co_ioOnly,co_none);
  T_challengePaletteOption=(co_preconfiguredPaletteWithCounts,
                            co_preconfiguredPalette,
                            co_unconfiguredPaletteWithCounts,
                            co_freePalette);

  P_abstractPrototypeSource=^T_abstractPrototypeSource;

  T_gateInterface=record
    name:string;
    wireWidth:byte;
    representation:T_multibitWireRepresentation;
  end;

  T_gateInterfaces=record
    inputs,outputs:array of T_gateInterface;
  end;

  { T_abstractPrototypeSource }

  T_abstractPrototypeSource=object(T_serializable)
    FUNCTION readGate(VAR stream:T_bufferedInputStreamWrapper):P_abstractGate; virtual; abstract;
    FUNCTION obtainGate(CONST prototypeIndex:longint):P_compoundGate; virtual; abstract;
    PROCEDURE dropPaletteItem(CONST gatePtr:pointer); virtual; abstract;

    FUNCTION hasPrototype(CONST prototypeIndex:longint):boolean; virtual; abstract;
    PROCEDURE addPrototype(CONST prototypeIndex:longint; CONST behavior:P_compoundGate; CONST visible:boolean); virtual; abstract;
    PROCEDURE ensureBaseGate(CONST gate:P_abstractGate; CONST visible:boolean); virtual; abstract;
    PROCEDURE countUpGate(CONST gate:P_abstractGate); virtual; abstract;
    PROCEDURE countDownGate(CONST gate:P_abstractGate); virtual; abstract;
  end;

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
      FUNCTION  exportForChallenge(CONST challengePalette:P_abstractPrototypeSource; CONST addAsVisibleToPalette:boolean):P_abstractGate;

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
      FUNCTION  simulateSteps(CONST count: longint; CONST inputWires: T_wireValueArray; OUT stepsDone: longint): T_wireValueArray;

      FUNCTION  getOutput(CONST index:longint):T_wireValue; virtual;
      FUNCTION  setInput(CONST index:longint; CONST value:T_wireValue):boolean; virtual;
      FUNCTION  getInput(CONST index:longint):T_wireValue; virtual;

      PROCEDURE writePrototypeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST acutalIndex:longint);
      FUNCTION readPrototypeFromStream(VAR stream:T_bufferedInputStreamWrapper; CONST acutalIndex:longint):boolean;

      PROCEDURE writeToStream(VAR stream:T_bufferedOutputStreamWrapper; CONST metaDataOnly:boolean=false); virtual;
      PROCEDURE readMetaDataFromStream(VAR stream:T_bufferedInputStreamWrapper); virtual;

      PROCEDURE countGates(VAR gateCount:T_gateCount); virtual;
      FUNCTION equals(CONST other:P_abstractGate):boolean; virtual;
      FUNCTION equalsInOtherPalette(CONST other:P_abstractGate):boolean;

      FUNCTION usesPrototype(CONST p:P_captionedAndIndexed):boolean;
      PROCEDURE prototypeUpdated(CONST oldPrototype, newPrototype: P_captionedAndIndexed);
      FUNCTION getPrototypeIndex:longint;

      FUNCTION getInterfaces:T_gateInterfaces;
    end;

IMPLEMENTATION
USES math;
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
    prototype:=nil;
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

FUNCTION T_compoundGate.exportForChallenge(CONST challengePalette: P_abstractPrototypeSource; CONST addAsVisibleToPalette:boolean): P_abstractGate;
  VAR cloned:P_compoundGate;
      i,j:longint;
  FUNCTION gateInClone(CONST gate:P_abstractGate):P_abstractGate;
    VAR i:longint;
    begin
      result:=nil;
      for i:=0 to length(inputs )-1 do if P_abstractGate(inputs [i])=gate then exit(cloned^.inputs[i]);
      for i:=0 to length(outputs)-1 do if P_abstractGate(outputs[i])=gate then exit(cloned^.outputs[i]);
      for i:=0 to length(gates  )-1 do if                gates  [i] =gate then exit(cloned^.gates[i]);
      assert(result<>nil,'exportForChallenge of T_circuitBoard failed');
    end;

  begin
    if (challengePalette^.hasPrototype(prototype^.getIndexInPalette))
    then exit(challengePalette^.obtainGate(prototype^.getIndexInPalette));

    new(cloned,create(challengePalette));
    cloned^.prototype:=nil;
    cloned^.captionString:=getCaption;
    cloned^.descriptionString:=getDescription;
    cloned^.myIndex:=prototype^.getIndexInPalette;
    setLength(cloned^.inputs ,length(inputs )); for i:=0 to length(inputs )-1 do cloned^.inputs [i]:=P_inputGate (inputs[i]^.clone(false));
    setLength(cloned^.outputs,length(outputs)); for i:=0 to length(outputs)-1 do cloned^.outputs[i]:=P_outputGate(outputs[i]^.clone(false));
    setLength(cloned^.gates  ,length(gates));   for i:=0 to length(gates  )-1 do begin
      if gates[i]^.gateType=gt_compound
      then cloned^.gates[i]:=P_compoundGate(gates[i])^.exportForChallenge(challengePalette,false)
      else begin
        challengePalette^.ensureBaseGate(gates[i],false);
        cloned^.gates[i]:=gates[i]^.clone(false);
      end;
    end;

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

    challengePalette^.addPrototype(prototype^.getIndexInPalette,cloned,addAsVisibleToPalette);
    result:=challengePalette^.obtainGate(prototype^.getIndexInPalette);
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

FUNCTION T_compoundGate.simulateSteps(CONST count: longint;
  CONST inputWires: T_wireValueArray; OUT stepsDone: longint): T_wireValueArray;
  VAR i:longint;
      changed:boolean=true;
      g: P_abstractGate;
  begin

    for i:=0 to min(length(inputs),length(inputWires))-1 do begin
      assert(inputs[i]^.inputWidth(0)=inputWires[i].width);
      inputs[i]^.setInput(0,inputWires[i]);
    end;

    stepsDone:=0;
    while changed and (stepsDone<count) do begin
      changed:=false;
      for i:=0 to length(wires)-1 do if wires[i].simulateStep then changed:=true;
      for g in gates              do if g^.simulateStep       then changed:=true;
      inc(stepsDone);
    end;

    setLength(result,length(outputs));
    for i:=0 to length(outputs)-1 do result[i]:=outputs[i]^.getInput(0);
  end;

FUNCTION T_compoundGate.getOutput(CONST index: longint): T_wireValue;
  begin
    if (index>=0) and (index<length(outputs))
    then result:=outputs[index]^.getInput(0);
  end;

FUNCTION T_compoundGate.setInput(CONST index: longint; CONST value: T_wireValue
  ): boolean;
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

PROCEDURE T_compoundGate.writePrototypeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST acutalIndex: longint);
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

FUNCTION T_compoundGate.readPrototypeFromStream(VAR stream: T_bufferedInputStreamWrapper; CONST acutalIndex: longint): boolean;
  FUNCTION gateFromDeserialization(index:longint):P_abstractGate;
    begin
      assert(index>=0,'Negative gate index!');

      writeln('Gate for index ',index,' (',length(inputs),'/',length(outputs),'/',length(gates),')');

      if index<length(inputs ) then exit(inputs [index]); index-=length(inputs);
      if index<length(outputs) then exit(outputs[index]); index-=length(outputs);
      assert(index<length(gates),'Gate index overflow!');
      result:=gates[index];
    end;
  VAR i,j:longint;
  begin
    myIndex:=acutalIndex;
    prototype:=nil;
    captionString:=stream.readAnsiString;
    descriptionString:=stream.readAnsiString;
    setLength(inputs ,stream.readNaturalNumber); for i:=0 to length(inputs )-1 do inputs [i]:=P_inputGate (prototypeSource^.readGate(stream));
    setLength(outputs,stream.readNaturalNumber); for i:=0 to length(outputs)-1 do outputs[i]:=P_outputGate(prototypeSource^.readGate(stream));
    setLength(gates  ,stream.readNaturalNumber); for i:=0 to length(gates  )-1 do gates  [i]:=             prototypeSource^.readGate(stream);
    setLength(wires  ,stream.readNaturalNumber);
    for i:=0 to length(wires)-1 do with wires[i] do begin
      source:=gateFromDeserialization(stream.readNaturalNumber);
      sourceOutputIndex:=stream.readNaturalNumber;
      setLength(sink,stream.readNaturalNumber);
      for j:=0 to length(sink)-1 do with sink[j] do begin
        gate:=gateFromDeserialization(stream.readNaturalNumber);
        gateInputIndex:=stream.readNaturalNumber;
      end;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE T_compoundGate.writeToStream(VAR stream: T_bufferedOutputStreamWrapper; CONST metaDataOnly: boolean);
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
    result:=(gateType=other^.gateType) and ((P_compoundGate(other)^.prototype=prototype));
  end;

FUNCTION T_compoundGate.equalsInOtherPalette(CONST other: P_abstractGate): boolean;
  VAR i:longint;
  begin
    result:=(gateType=other^.gateType) and
            (P_compoundGate(other)^.getIndexInPalette=getIndexInPalette) and
            (other^.numberOfInputs=numberOfInputs) and
            (other^.numberOfOutputs=numberOfOutputs);
    if result then for i:=0 to numberOfInputs -1 do result:=result and (inputWidth (i)=other^.inputWidth (i));
    if result then for i:=0 to numberOfOutputs-1 do result:=result and (outputWidth(i)=other^.outputWidth(i));
  end;

FUNCTION T_compoundGate.usesPrototype(CONST p: P_captionedAndIndexed): boolean;
  VAR g:P_abstractGate;
  begin
    result:=false;
    if (prototype=p) then exit(true);
    for g in gates do if (g^.gateType=gt_compound) and P_compoundGate(g)^.usesPrototype(p) then result:=true;
  end;

PROCEDURE T_compoundGate.prototypeUpdated(CONST oldPrototype, newPrototype: P_captionedAndIndexed);
  PROCEDURE rewire(CONST old,new:P_compoundGate);
    VAR i,i_,j,j_:longint;
    begin
      i_:=0;
      for i:=0 to length(wires)-1 do begin
        if wires[i].source=P_abstractGate(old) then begin
          wires[i].source:=new;
          if (wires[i].sourceOutputIndex>=new^.numberOfOutputs) or (old^.outputWidth(wires[i].sourceOutputIndex)<>new^.outputWidth(wires[i].sourceOutputIndex))
          then setLength(wires[i].sink,0);
        end;

        j_:=0;
        for j:=0 to length(wires[i].sink)-1 do begin
          if (wires[i].sink[j].gate=P_abstractGate(old)) then begin
            wires[i].sink[j].gate:=new;
            if (wires[i].sink[j].gateInputIndex>=new^.numberOfInputs) or (old^.inputWidth(wires[i].sink[j].gateInputIndex)<>new^.inputWidth(wires[i].sink[j].gateInputIndex))
            then begin end else begin
              wires[i].sink[j_]:=wires[i].sink[j];
              inc(j_);
            end;
          end else begin
            wires[i].sink[j_]:=wires[i].sink[j];
            inc(j_);
          end;
        end;
        if length(wires[i].sink)>0 then begin
          wires[i_]:=wires[i];
          inc(i_);
        end;
      end;
      setLength(wires,i_);
    end;

  VAR i:longint;
      replacement: P_compoundGate;
  begin
    assert(prototypeSource<>nil);
    for i:=0 to length(gates)-1 do if (gates[i]^.gateType=gt_compound) then begin
      if (P_compoundGate(gates[i])^.prototype=oldPrototype) then begin
        replacement:=prototypeSource^.obtainGate(oldPrototype^.getIndexInPalette);
        rewire(P_compoundGate(gates[i]),replacement);
        dispose(gates[i],destroy);
        gates[i]:=replacement;
      end else P_compoundGate(gates[i])^.prototypeUpdated(oldPrototype,newPrototype);
    end;
  end;

FUNCTION T_compoundGate.getPrototypeIndex: longint;
  begin
    result:=myIndex;
  end;

FUNCTION T_compoundGate.getInterfaces: T_gateInterfaces;
  VAR i:longint;
  begin
    initialize(result);
    setLength(result.inputs,length(inputs));
    for i:=0 to length(inputs)-1 do begin
      result.inputs[i].name     :=inputs[i]^.getCaption;
      result.inputs[i].wireWidth:=inputs[i]^.outputWidth(0);
      if result.inputs[i].wireWidth=1
      then result.inputs[i].representation:=wr_binary
      else result.inputs[i].representation:=wr_decimal;
    end;
    setLength(result.outputs,length(outputs));
    for i:=0 to length(outputs)-1 do begin
      result.outputs[i].name     :=outputs[i]^.getCaption;
      result.outputs[i].wireWidth:=outputs[i]^.inputWidth(0);
      if result.outputs[i].wireWidth=1
      then result.outputs[i].representation:=wr_binary
      else result.outputs[i].representation:=wr_decimal;
    end;
  end;

end.

