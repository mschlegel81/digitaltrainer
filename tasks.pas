UNIT tasks;
{$mode objfpc}{$H+}
INTERFACE
USES serializationUtil,logicGates;
TYPE
  T_ioPair=record
    input,output:array of T_wireValue;
    maximumSteps:longint;
  end;

  T_task=object(T_serializable)
    name,description:string;

    allowedGates:T_gateTypeSet;
    gateNumberLimit:longint;

    expectedBehavior:array of T_ioPair;

  end;

IMPLEMENTATION

end.

